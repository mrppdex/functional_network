library(shiny)
library(visNetwork)
library(dplyr)
library(igraph)

# Define server logic
server <- function(input, output, session) {
    # --- Reactive Values to hold the network data ---
    rv <- reactiveValues(
        nodes = NULL,
        edges = NULL
    )

    # Helper to update all inputs after data change
    update_all_inputs <- function() {
        req(rv$nodes)
        # people_choices removed
        expertise_choices <- rv$nodes %>%
            filter(group == "Expertise") %>%
            pull(id)
        all_nodes <- rv$nodes$id

        # Preserve selections if possible, or reset
        updateSelectInput(session, "selected_expertise", choices = expertise_choices, selected = input$selected_expertise)
        # focus_person removed

        updateSelectInput(session, "remove_node_select", choices = all_nodes)
        updateSelectInput(session, "edge_from", choices = all_nodes)
        updateSelectInput(session, "edge_to", choices = all_nodes)
    }

    # --- Initialization: Load Data ---
    observe({
        isolate({
            # Load data on startup
            data <- load_network_data()

            # Calculate metrics once loaded
            nodes_w_metrics <- calculate_metrics(data$nodes, data$edges)

            rv$nodes <- nodes_w_metrics
            rv$edges <- data$edges

            # Update UI inputs with available choices
            update_all_inputs()
        })
    })

    # --- Persistence: Save Data ---
    observeEvent(input$save_data, {
        req(rv$nodes, rv$edges)
        save_network_data(rv$nodes, rv$edges)
        showNotification("Network data saved successfully!", type = "message")
    })

    # --- Persistence: Reload/Reset Data ---
    observeEvent(input$reload_data, {
        # For demo purposes, this effectively resets to new mock data if file is deleted
        # Or reloads from disk. Let's force a regeneration of mock data for "Reset" behavior
        # to demonstrate dynamic graph changes, or just reload.
        # Let's interpret "Reload/Reset" as "Regenerate Mock Data" for this user request context
        # so they can see different configurations.

        new_data <- generate_mock_data()
        save_network_data(new_data$nodes, new_data$edges) # Save the new fresh batch

        rv$nodes <- calculate_metrics(new_data$nodes, new_data$edges)
        rv$edges <- new_data$edges

        # Update inputs again
        # people_choices removed
        expertise_choices <- rv$nodes %>%
            filter(group == "Expertise") %>%
            pull(id)

        updateSelectInput(session, "selected_expertise", choices = expertise_choices)
        # focus_person removed

        # Update Management Inputs
        all_nodes <- rv$nodes$id
        updateSelectInput(session, "remove_node_select", choices = all_nodes)
        updateSelectInput(session, "edge_from", choices = all_nodes)
        updateSelectInput(session, "edge_to", choices = all_nodes)

        showNotification("Network data reset/reloaded!", type = "warning")
    })


    # --- Management Logic ---

    observeEvent(input$add_node_btn, {
        req(input$new_node_name)
        name <- input$new_node_name
        type <- input$new_node_type

        # Check if exists
        if (name %in% rv$nodes$id) {
            showNotification("Node already exists!", type = "error")
            return()
        }

        # Create new node
        new_node <- tibble(
            id = name,
            label = name,
            group = type,
            title = paste(type, ":", name),
            value = 1, # Initial size
            degree = 0,
            betweenness = 0
        )

        # Update nodes
        rv$nodes <- bind_rows(rv$nodes, new_node)
        update_all_inputs()
        showNotification(paste("Added", type, ":", name), type = "message")
        updateTextInput(session, "new_node_name", value = "")
    })

    observeEvent(input$remove_node_btn, {
        req(input$remove_node_select)
        node_to_remove <- input$remove_node_select

        # Remove node
        rv$nodes <- rv$nodes %>% filter(id != node_to_remove)

        # Remove associated edges
        rv$edges <- rv$edges %>% filter(from != node_to_remove, to != node_to_remove)

        # Recalculate metrics for remaining nodes
        rv$nodes <- calculate_metrics(rv$nodes %>% select(-degree, -betweenness, -value), rv$edges)

        update_all_inputs()
        showNotification(paste("Removed node:", node_to_remove), type = "message")
    })

    observeEvent(input$add_edge_btn, {
        req(input$edge_from, input$edge_to, input$edge_type)
        from <- input$edge_from
        to <- input$edge_to
        type <- input$edge_type

        if (from == to) {
            showNotification("Cannot connect node to itself!", type = "error")
            return()
        }

        # Validation and Auto-Correction
        final_from <- from
        final_to <- to
        final_type <- type

        tryCatch(
            {
                # 1. Enforce Person -> Expertise direction
                is_expertise_from <- any(rv$nodes$group[rv$nodes$id == from] == "Expertise")
                is_person_to <- any(rv$nodes$group[rv$nodes$id == to] == "Person")

                if (is_expertise_from && is_person_to) {
                    final_from <- to
                    final_to <- from
                    showNotification("Auto-corrected direction to Person -> Expertise", type = "warning")
                }

                # 2. Enforce 'has_skill' for Person -> Expertise
                is_person_from <- any(rv$nodes$group[rv$nodes$id == final_from] == "Person")
                is_expertise_to <- any(rv$nodes$group[rv$nodes$id == final_to] == "Expertise")

                if (is_person_from && is_expertise_to) {
                    final_type <- "has_skill"
                }

                # 3. Enforce 'knows' for Person -> Person
                is_person_to <- any(rv$nodes$group[rv$nodes$id == final_to] == "Person")

                if (is_person_from && is_person_to) {
                    final_type <- "knows"
                }

                # Check existence with FINAL values
                exists <- rv$edges %>%
                    filter(from == !!final_from, to == !!final_to, type == !!final_type) %>%
                    nrow() > 0

                if (exists) {
                    showNotification("Connection already exists!", type = "warning")
                    return()
                }

                # Add Edge with optional details
                details <- input$edge_details
                weight_val <- input$edge_weight
                # Use NA_character_ to ensure correct column type
                title_attr <- if (!is.null(details) && details != "") details else NA_character_

                new_edge <- tibble(
                    from = final_from,
                    to = final_to,
                    type = final_type,
                    title = title_attr,
                    color = ifelse(final_type == "knows", "lightblue", "gray"),
                    weight = weight_val
                )

                # Debug: check types
                # print(str(new_edge))

                rv$edges <- bind_rows(rv$edges, new_edge)

                # Recalculate metrics
                # Ensure from/to are first columns for igraph
                edges_for_metrics <- rv$edges %>% select(from, to, everything())
                rv$nodes <- calculate_metrics(rv$nodes %>% select(-degree, -betweenness, -value), edges_for_metrics)

                # Auto-select if connecting to an expertise
                if (final_type == "has_skill") {
                    current_selection <- input$selected_expertise
                    if (!final_to %in% current_selection) {
                        updateSelectInput(session, "selected_expertise", selected = c(current_selection, final_to))
                    }
                }

                edge_count <- nrow(rv$edges)
                showNotification(paste("Added connection:", final_from, "->", final_to, "(Total:", edge_count, ")"), type = "message")
                updateTextInput(session, "edge_details", value = "")
            },
            error = function(e) {
                showNotification(paste("Error adding connection:", e$message), type = "error")
                print(e)
            }
        )
    })

    # --- Network Visualization ---
    output$network_plot <- renderVisNetwork({
        req(rv$nodes, rv$edges)

        nodes_viz <- rv$nodes
        edges_viz <- rv$edges

        # Default styling - Minimalist
        # Default styling - Minimalist
        nodes_viz$color.background <- ifelse(nodes_viz$group == "Person", "#E0F7FA", "#FFF9C4") # Very light Cyan / Light Yellow
        nodes_viz$color.border <- ifelse(nodes_viz$group == "Person", "#006064", "#F57F17") # Dark Cyan / Dark Orange
        nodes_viz$borderWidth <- 1

        # Use simpler shapes (Person=dot, Expertise=box)
        nodes_viz$shape <- ifelse(nodes_viz$group == "Expertise", "box", "dot")

        # Font styling
        nodes_viz$font.face <- "Inter, sans-serif"
        nodes_viz$font.size <- 14
        nodes_viz$font.color <- "#333333"

        # Highlighting Logic
        target_exp <- input$selected_expertise
        # focus_p removed

        if (!is.null(target_exp) && length(target_exp) > 0) {
            # 1. Identify Experts
            experts_edges <- edges_viz %>%
                filter(to %in% target_exp, type == "has_skill")

            expert_ids <- unique(experts_edges$from)

            # Count matches
            expert_counts <- experts_edges %>%
                count(from)

            super_experts <- expert_counts %>%
                filter(n == length(target_exp)) %>%
                pull(from)

            # Color experts - Soft Green
            nodes_viz$color.background[nodes_viz$id %in% expert_ids] <- "#C8E6C9"
            nodes_viz$color.border[nodes_viz$id %in% expert_ids] <- "#2E7D32"

            # Color Super Experts - Slightly darker Green
            if (length(target_exp) > 1) {
                nodes_viz$color.background[nodes_viz$id %in% super_experts] <- "#A5D6A7"
            }

            # Target Skill - Soft Red/Orange
            nodes_viz$color.background[nodes_viz$id %in% target_exp] <- "#FFCCBC"
            nodes_viz$color.border[nodes_viz$id %in% target_exp] <- "#D84315"
        }

        # Focus person logic and highlighting removed

        visNetwork(nodes_viz, edges_viz) %>%
            visOptions(
                highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                nodesIdSelection = TRUE
            ) %>%
            visEdges(
                smooth = list(enabled = TRUE, type = "continuous"),
                color = list(color = "#BDBDBD", highlight = "#5C5C5C"), # Light gray default
                width = edges_viz$weight * 4 + 1 # Scale weight to width (1-5px)
            ) %>%
            visNodes(
                shadow = FALSE, # Clean look
                color = list(highlight = list(border = "#333333", background = "#FFFFFF"))
            ) %>%
            visPhysics(stabilization = FALSE) %>%
            visInteraction(navigationButtons = FALSE) %>% # Cleaner UI
            visLayout(randomSeed = 123)
    })

    # --- Experts Table ---
    output$experts_table <- renderDataTable({
        req(rv$nodes, input$selected_expertise)

        target_exp <- input$selected_expertise
        if (is.null(target_exp) || length(target_exp) == 0) {
            return()
        }

        # Find people with these skills
        experts_edges <- rv$edges %>%
            filter(to %in% target_exp, type == "has_skill")

        # Calculate skill match count
        match_counts <- experts_edges %>%
            group_by(from) %>%
            summarise(
                MatchingSkills = n(),
                Skills = paste(to, collapse = ", ")
            )

        expert_ids <- match_counts$from

        # Filter nodes table and join with counts
        experts_df <- rv$nodes %>%
            filter(id %in% expert_ids) %>%
            select(Name = id, Degree = degree, Betweenness = betweenness) %>%
            left_join(match_counts, by = c("Name" = "from")) %>%
            arrange(desc(MatchingSkills), desc(Degree))

        experts_df
    })

    # --- Path Finding ---
    # --- Path Finding Removed ---

    # --- Path Finding Removed ---
}
