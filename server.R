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
        people_choices <- rv$nodes %>%
            filter(group == "Person") %>%
            pull(id)
        expertise_choices <- rv$nodes %>%
            filter(group == "Expertise") %>%
            pull(id)
        all_nodes <- rv$nodes$id

        # Preserve selections if possible, or reset
        updateSelectInput(session, "selected_expertise", choices = expertise_choices, selected = input$selected_expertise)
        updateSelectInput(session, "focus_person", choices = c("None", people_choices), selected = input$focus_person)

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
        people_choices <- rv$nodes %>%
            filter(group == "Person") %>%
            pull(id)
        expertise_choices <- rv$nodes %>%
            filter(group == "Expertise") %>%
            pull(id)

        updateSelectInput(session, "selected_expertise", choices = expertise_choices)
        updateSelectInput(session, "focus_person", choices = c("None", people_choices), selected = "None")

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
                # Use NA_character_ to ensure correct column type
                title_attr <- if (!is.null(details) && details != "") details else NA_character_

                new_edge <- tibble(
                    from = final_from,
                    to = final_to,
                    type = final_type,
                    title = title_attr,
                    color = ifelse(final_type == "knows", "lightblue", "gray")
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

        # Default styling
        nodes_viz$color.background <- ifelse(nodes_viz$group == "Person", "#97C2FC", "#FFD700") # Blue / Gold
        nodes_viz$shape <- ifelse(nodes_viz$group == "Person", "dot", "diamond")

        # Highlighting Logic
        target_exp <- input$selected_expertise # Can be a vector now
        focus_p <- input$focus_person

        if (!is.null(target_exp) && length(target_exp) > 0) {
            # 1. Identify Experts (match ANY of the selected skills)
            experts_edges <- edges_viz %>%
                filter(to %in% target_exp, type == "has_skill")

            expert_ids <- unique(experts_edges$from)

            # Find people with ALL selected skills (Super Experts)
            # Count how many of the selected skills each person has
            expert_counts <- experts_edges %>%
                count(from)

            super_experts <- expert_counts %>%
                filter(n == length(target_exp)) %>%
                pull(from)

            # Color experts green
            nodes_viz$color.background[nodes_viz$id %in% expert_ids] <- "#7BE141" # Green

            # Color Super Experts (match ALL) darker green
            if (length(target_exp) > 1) {
                nodes_viz$color.background[nodes_viz$id %in% super_experts] <- "#228B22" # ForestGreen
            }

            nodes_viz$color.background[nodes_viz$id %in% target_exp] <- "#FF4500" # OrangeRed
        }

        if (focus_p != "None") {
            # Highlight Focus Person
            nodes_viz$color.background[nodes_viz$id == focus_p] <- "#FB7E81" # Red-ish
            nodes_viz$size[nodes_viz$id == focus_p] <- 30 # Make bigger
        }

        visNetwork(nodes_viz, edges_viz) %>%
            visOptions(
                highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                nodesIdSelection = TRUE
            ) %>%
            visPhysics(stabilization = FALSE) %>%
            visInteraction(navigationButtons = TRUE) %>%
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
    output$path_output <- renderText({
        req(rv$nodes, rv$edges, input$selected_expertise, input$focus_person)

        if (input$focus_person == "None") {
            return("Select a Focus Person to see the path.")
        }

        target_exp <- input$selected_expertise
        if (is.null(target_exp) || length(target_exp) == 0) {
            return("Select expertise.")
        }

        # Find all experts (anyone with AT LEAST ONE of the skills)
        experts_edges <- rv$edges %>%
            filter(to %in% target_exp, type == "has_skill")
        expert_ids <- unique(experts_edges$from)

        if (input$focus_person %in% expert_ids) {
            return("You are already an expert in this field!")
        }

        if (length(expert_ids) == 0) {
            return("No experts found for these skills.")
        }

        # Find shortest path to *any* expert
        shortest_path <- NULL
        best_target <- NULL
        min_dist <- Inf

        # Prioritize "Super Experts" (those with more matching skills) could be an enhancement
        # For now, stick to shortest path to *any* relevant expert

        for (expert in expert_ids) {
            # Refine pathfinding: Use only "knows" edges (people connections)
            # Otherwise people are connected via shared skills
            people_edges <- rv$edges %>% filter(type == "knows")
            path <- find_connection_path(rv$nodes, people_edges, input$focus_person, expert)

            if (!is.null(path)) {
                # Path length (distance)
                dist <- length(path) - 1 # edges
                if (dist < min_dist) {
                    min_dist <- dist
                    shortest_path <- path
                    best_target <- expert
                }
            }
        }

        if (!is.null(shortest_path)) {
            paste("Best path to expert", best_target, ":", paste(shortest_path, collapse = " -> "))
        } else {
            "No connection found to any expert in this network."
        }
    })
}
