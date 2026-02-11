library(shiny)
library(visNetwork)
library(dplyr)
library(igraph)

# Define server logic
server <- function(input, output, session) {
    # --- Reactive Values to hold the network data ---
    rv <- reactiveValues(
        nodes = NULL,
        edges = NULL,
        current_view = "root" # "root" or an expertise node ID
    )

    # Helper to update all inputs after data change
    update_all_inputs <- function() {
        req(rv$nodes)
        expertise_choices <- rv$nodes %>%
            filter(group == "Expertise", is.na(parent)) %>%
            pull(id)

        # Preserve selections if possible, or reset
        updateSelectInput(session, "selected_expertise", choices = expertise_choices, selected = input$selected_expertise)
    }

    # --- Initialization: Load Data ---
    observe({
        isolate({
            data <- load_network_data()
            nodes_w_metrics <- calculate_metrics(data$nodes, data$edges)

            rv$nodes <- nodes_w_metrics
            rv$edges <- data$edges

            # BACKWARD COMPATIBILITY
            if (!"weight" %in% names(rv$edges)) {
                rv$edges$weight <- 0.5
            }
            rv$edges$weight[is.na(rv$edges$weight)] <- 0.5
            if (!"parent" %in% names(rv$nodes)) {
                rv$nodes$parent <- NA_character_
            }

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
        new_data <- generate_mock_data()
        save_network_data(new_data$nodes, new_data$edges)

        rv$nodes <- calculate_metrics(new_data$nodes, new_data$edges)
        rv$edges <- new_data$edges
        rv$current_view <- "root"

        update_all_inputs()
        showNotification("Network data reset/reloaded!", type = "warning")
    })

    # =========================================================================
    # MANAGEMENT LOGIC
    # =========================================================================

    # --- Root: Add Node ---
    observeEvent(input$add_node_btn, {
        req(input$new_node_name)
        name <- input$new_node_name
        type <- input$new_node_type

        if (name %in% rv$nodes$id) {
            showNotification("Node already exists!", type = "error")
            return()
        }

        new_node <- tibble(
            id = name, label = name, group = type,
            title = paste(type, ":", name),
            value = 1, degree = 0, betweenness = 0,
            parent = NA_character_
        )

        rv$nodes <- bind_rows(rv$nodes, new_node)
        update_all_inputs()
        showNotification(paste("Added", type, ":", name), type = "message")
        updateTextInput(session, "new_node_name", value = "")
    })

    # --- Root: Remove Node ---
    observeEvent(input$remove_node_btn, {
        req(input$remove_node_select)
        node_to_remove <- input$remove_node_select

        # Remove child nodes first
        child_ids <- rv$nodes %>%
            filter(!is.na(parent) & parent == node_to_remove) %>%
            pull(id)
        if (length(child_ids) > 0) {
            rv$nodes <- rv$nodes %>% filter(!id %in% child_ids)
            rv$edges <- rv$edges %>% filter(!from %in% child_ids, !to %in% child_ids)
        }

        rv$nodes <- rv$nodes %>% filter(id != node_to_remove)
        rv$edges <- rv$edges %>% filter(from != node_to_remove, to != node_to_remove)

        if (nrow(rv$nodes) > 0 && nrow(rv$edges) > 0) {
            rv$nodes <- calculate_metrics(rv$nodes %>% select(-degree, -betweenness, -value), rv$edges)
        }

        update_all_inputs()
        showNotification(paste("Removed node:", node_to_remove), type = "message")
    })

    # --- Root: Add Edge ---
    observeEvent(input$add_edge_btn, {
        req(input$edge_from, input$edge_to, input$edge_type)
        from <- input$edge_from
        to <- input$edge_to
        type <- input$edge_type

        if (from == to) {
            showNotification("Cannot connect node to itself!", type = "error")
            return()
        }

        final_from <- from
        final_to <- to
        final_type <- type

        tryCatch(
            {
                # Auto-correction
                is_expertise_from <- any(rv$nodes$group[rv$nodes$id == from] == "Expertise")
                is_person_to <- any(rv$nodes$group[rv$nodes$id == to] == "Person")

                if (is_expertise_from && is_person_to) {
                    final_from <- to
                    final_to <- from
                    showNotification("Auto-corrected direction to Person -> Expertise", type = "warning")
                }

                is_person_from <- any(rv$nodes$group[rv$nodes$id == final_from] == "Person")
                is_expertise_to <- any(rv$nodes$group[rv$nodes$id == final_to] == "Expertise")

                if (is_person_from && is_expertise_to) final_type <- "has_skill"

                is_person_to <- any(rv$nodes$group[rv$nodes$id == final_to] == "Person")
                if (is_person_from && is_person_to) final_type <- "knows"

                exists <- rv$edges %>%
                    filter(from == !!final_from, to == !!final_to, type == !!final_type) %>%
                    nrow() > 0

                if (exists) {
                    showNotification("Connection already exists!", type = "warning")
                    return()
                }

                details <- input$edge_details
                weight_val <- input$edge_weight
                title_attr <- if (!is.null(details) && details != "") details else NA_character_

                new_edge <- tibble(
                    from = final_from, to = final_to, type = final_type,
                    title = title_attr,
                    color = ifelse(final_type == "knows", "lightblue", "gray"),
                    weight = weight_val
                )

                rv$edges <- bind_rows(rv$edges, new_edge)

                edges_for_metrics <- rv$edges %>% select(from, to, everything())
                rv$nodes <- calculate_metrics(rv$nodes %>% select(-degree, -betweenness, -value), edges_for_metrics)

                if (final_type == "has_skill") {
                    current_selection <- input$selected_expertise
                    if (!final_to %in% current_selection) {
                        updateSelectInput(session, "selected_expertise", selected = c(current_selection, final_to))
                    }
                }

                showNotification(paste("Added connection:", final_from, "->", final_to), type = "message")
                updateTextInput(session, "edge_details", value = "")
            },
            error = function(e) {
                showNotification(paste("Error:", e$message), type = "error")
            }
        )
    })

    # --- Drill-down: Add Sub-Expertise ---
    observeEvent(input$add_sub_expertise_btn, {
        req(input$new_sub_expertise_name, rv$current_view != "root")
        name <- input$new_sub_expertise_name
        parent_id <- rv$current_view

        if (name %in% rv$nodes$id) {
            showNotification("Node already exists!", type = "error")
            return()
        }

        new_node <- tibble(
            id = name, label = name, group = "Expertise",
            title = paste("Sub-Expertise:", name, "(within", parent_id, ")"),
            value = 2, degree = 0, betweenness = 0,
            parent = parent_id
        )

        rv$nodes <- bind_rows(rv$nodes, new_node)
        showNotification(paste("Added sub-expertise:", name, "inside", parent_id), type = "message")
        updateTextInput(session, "new_sub_expertise_name", value = "")
    })

    # --- Drill-down: Remove Sub-Node ---
    observeEvent(input$remove_internal_btn, {
        req(input$remove_internal_select, input$remove_internal_select != "")
        node_id <- input$remove_internal_select

        rv$nodes <- rv$nodes %>% filter(id != node_id)
        rv$edges <- rv$edges %>% filter(from != node_id, to != node_id)

        showNotification(paste("Removed sub-node:", node_id), type = "message")
    })

    # --- Drill-down: Connect Person to Sub-Expertise ---
    observeEvent(input$add_internal_edge_btn, {
        req(
            input$internal_edge_person, input$internal_edge_sub,
            input$internal_edge_person != "", input$internal_edge_sub != ""
        )

        person_id <- input$internal_edge_person
        sub_id <- input$internal_edge_sub
        weight_val <- input$internal_edge_weight

        exists <- rv$edges %>%
            filter(from == !!person_id, to == !!sub_id) %>%
            nrow() > 0

        if (exists) {
            showNotification("Connection already exists!", type = "warning")
            return()
        }

        new_edge <- tibble(
            from = person_id, to = sub_id, type = "has_skill",
            title = NA_character_, color = "gray", weight = weight_val
        )

        rv$edges <- bind_rows(rv$edges, new_edge)
        showNotification(paste("Connected", person_id, "->", sub_id), type = "message")
    })

    # =========================================================================
    # DYNAMIC EDIT NETWORK UI
    # =========================================================================
    output$edit_network_ui <- renderUI({
        req(rv$nodes)

        if (rv$current_view == "root") {
            # ===== ROOT VIEW =====
            tagList(
                layout_columns(
                    col_widths = c(6, 6),
                    card(
                        card_header("Add Node"),
                        card_body(
                            textInput("new_node_name", "Name/Skill"),
                            selectInput("new_node_type", "Type", choices = c("Person", "Expertise")),
                            actionButton("add_node_btn", "Add Node", class = "btn-success")
                        )
                    ),
                    card(
                        card_header("Remove Node"),
                        card_body(
                            selectInput("remove_node_select", "Select Node", choices = rv$nodes$id),
                            actionButton("remove_node_btn", "Remove Node", class = "btn-danger")
                        )
                    )
                ),
                card(
                    card_header("Add Connection"),
                    card_body(
                        layout_columns(
                            col_widths = c(4, 4, 4),
                            selectInput("edge_from", "From", choices = rv$nodes$id),
                            selectInput("edge_to", "To", choices = rv$nodes$id),
                            selectInput("edge_type", "Relationship",
                                choices = c("knows" = "knows", "has_skill" = "has_skill")
                            )
                        ),
                        sliderInput("edge_weight", "Strength/Weight", min = 0, max = 1, value = 0.5, step = 0.1),
                        textInput("edge_details", "Details/Proficiency (Optional)"),
                        actionButton("add_edge_btn", "Add Connection", class = "btn-success w-100"),
                        helpText("Direction will be auto-corrected (Person -> Expertise).")
                    )
                )
            )
        } else {
            # ===== DRILL-DOWN VIEW: Sub-expertise management =====
            expertise_id <- rv$current_view

            # People connected to this expertise
            skill_edges <- rv$edges %>% filter(to == expertise_id, type == "has_skill")
            connected_people <- unique(skill_edges$from)

            # Sub-nodes of this expertise
            sub_nodes <- rv$nodes %>% filter(!is.na(parent) & parent == expertise_id)
            sub_node_ids <- sub_nodes$id

            tagList(
                # Context banner
                div(
                    style = "padding: 10px 14px; margin-bottom: 14px; background: linear-gradient(135deg, #FFF3E0, #FFE0B2); border-radius: 10px; border-left: 4px solid #F57F17;",
                    tags$strong(
                        style = "color: #E65100; font-size: 15px;",
                        icon("layer-group"), paste(" Editing inside:", expertise_id)
                    ),
                    tags$p(
                        class = "text-muted mb-0 mt-1", style = "font-size: 13px;",
                        paste(
                            length(connected_people), "people connected \u2022",
                            length(sub_node_ids), "sub-expertises"
                        )
                    )
                ),
                layout_columns(
                    col_widths = c(6, 6),

                    # Add Sub-Expertise
                    card(
                        card_header(tags$span(icon("sitemap"), " Add Sub-Expertise")),
                        card_body(
                            textInput("new_sub_expertise_name", "Sub-Expertise Name"),
                            helpText(paste("Will be created inside", expertise_id)),
                            actionButton("add_sub_expertise_btn", "Add Sub-Expertise", class = "btn-warning")
                        )
                    ),

                    # Remove Sub-Node
                    card(
                        card_header("Remove Sub-Node"),
                        card_body(
                            selectInput("remove_internal_select", "Select Sub-Node",
                                choices = if (length(sub_node_ids) > 0) sub_node_ids else c("(none)" = "")
                            ),
                            actionButton("remove_internal_btn", "Remove", class = "btn-danger")
                        )
                    )
                ),

                # Connect Person to Sub-Expertise
                card(
                    card_header(tags$span(icon("link"), " Connect Person to Sub-Expertise")),
                    card_body(
                        layout_columns(
                            col_widths = c(6, 6),
                            selectInput("internal_edge_person", "Person",
                                choices = if (length(connected_people) > 0) connected_people else c("(none)" = "")
                            ),
                            selectInput("internal_edge_sub", "Sub-Expertise",
                                choices = if (length(sub_node_ids) > 0) sub_node_ids else c("(none)" = "")
                            )
                        ),
                        sliderInput("internal_edge_weight", "Strength/Weight", min = 0, max = 1, value = 0.5, step = 0.1),
                        actionButton("add_internal_edge_btn", "Connect", class = "btn-success w-100"),
                        helpText("Link a person to a specific sub-expertise within this area.")
                    )
                )
            )
        }
    })

    # =========================================================================
    # VIEW HEADER & NAVIGATION
    # =========================================================================
    output$view_header <- renderUI({
        if (rv$current_view != "root") {
            div(
                style = "display: flex; align-items: center; gap: 12px; margin-bottom: 8px;",
                actionButton("back_to_root",
                    label = tagList(icon("arrow-left"), " Back to Full Network"),
                    class = "btn-outline-secondary btn-sm"
                ),
                tags$span(
                    style = "font-size: 16px; font-weight: 600; color: #F57F17;",
                    paste("Inside:", rv$current_view)
                )
            )
        } else {
            NULL
        }
    })

    observeEvent(input$back_to_root, {
        rv$current_view <- "root"
    })

    # --- Click Handler: Drill into Expertise ---
    observeEvent(input$network_plot_click, {
        click_info <- input$network_plot_click
        if (!is.null(click_info) && !is.null(click_info$nodes) && length(click_info$nodes) > 0) {
            clicked_node <- click_info$nodes[[1]]
            node_row <- rv$nodes %>% filter(id == clicked_node)
            if (nrow(node_row) > 0 && node_row$group[1] == "Expertise" && rv$current_view == "root") {
                rv$current_view <- clicked_node
            }
        }
    })

    # =========================================================================
    # NETWORK VISUALIZATION
    # =========================================================================
    output$network_plot <- renderVisNetwork({
        req(rv$nodes, rv$edges)

        if (rv$current_view == "root") {
            # ===== ROOT VIEW =====
            nodes_viz <- rv$nodes %>% filter(is.na(parent))

            visible_ids <- nodes_viz$id
            edges_viz <- rv$edges %>%
                filter(from %in% visible_ids, to %in% visible_ids)

            # Styling
            nodes_viz$color.background <- ifelse(nodes_viz$group == "Person", "#E0F7FA", "#FFF9C4")
            nodes_viz$color.border <- ifelse(nodes_viz$group == "Person", "#006064", "#F57F17")
            nodes_viz$borderWidth <- 1
            nodes_viz$shape <- ifelse(nodes_viz$group == "Expertise", "box", "dot")
            nodes_viz$font.face <- "Inter, sans-serif"
            nodes_viz$font.size <- 14
            nodes_viz$font.color <- "#333333"

            # Highlighting
            target_exp <- input$selected_expertise
            if (!is.null(target_exp) && length(target_exp) > 0) {
                experts_edges <- edges_viz %>%
                    filter(to %in% target_exp, type == "has_skill")
                expert_ids <- unique(experts_edges$from)

                expert_counts <- experts_edges %>% count(from)
                super_experts <- expert_counts %>%
                    filter(n == length(target_exp)) %>%
                    pull(from)

                nodes_viz$color.background[nodes_viz$id %in% expert_ids] <- "#C8E6C9"
                nodes_viz$color.border[nodes_viz$id %in% expert_ids] <- "#2E7D32"

                if (length(target_exp) > 1) {
                    nodes_viz$color.background[nodes_viz$id %in% super_experts] <- "#A5D6A7"
                }

                nodes_viz$color.background[nodes_viz$id %in% target_exp] <- "#FFCCBC"
                nodes_viz$color.border[nodes_viz$id %in% target_exp] <- "#D84315"
            }

            edges_viz$weight <- as.numeric(edges_viz$weight)
            edges_viz$width <- edges_viz$weight * 4 + 1

            visNetwork(nodes_viz, edges_viz) %>%
                visOptions(
                    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                    nodesIdSelection = TRUE
                ) %>%
                visEdges(
                    smooth = list(enabled = TRUE, type = "continuous"),
                    color = list(color = "#BDBDBD", highlight = "#5C5C5C")
                ) %>%
                visNodes(
                    shadow = FALSE,
                    color = list(highlight = list(border = "#333333", background = "#FFFFFF"))
                ) %>%
                visPhysics(stabilization = FALSE) %>%
                visInteraction(navigationButtons = FALSE) %>%
                visLayout(randomSeed = 123) %>%
                visEvents(click = "function(params) {
                    if (params.nodes.length > 0) {
                        Shiny.setInputValue('network_plot_click', {nodes: params.nodes}, {priority: 'event'});
                    }
                }")
        } else {
            # ===== DRILL-DOWN VIEW =====
            expertise_id <- rv$current_view

            # People connected to this expertise
            skill_edges <- rv$edges %>%
                filter(to == expertise_id, type == "has_skill")
            connected_people_ids <- unique(skill_edges$from)

            # Sub-nodes (children of this expertise)
            sub_nodes <- rv$nodes %>%
                filter(!is.na(parent) & parent == expertise_id)
            sub_node_ids <- sub_nodes$id

            # Build visible node set
            all_internal_ids <- c(connected_people_ids, sub_node_ids)
            nodes_viz <- rv$nodes %>% filter(id %in% all_internal_ids)

            # Size people by their connection weight
            if (nrow(nodes_viz) > 0) {
                nodes_viz$value <- 15
                for (i in seq_len(nrow(nodes_viz))) {
                    nid <- nodes_viz$id[i]
                    if (nid %in% connected_people_ids) {
                        w <- skill_edges$weight[skill_edges$from == nid]
                        if (length(w) > 0) {
                            w_norm <- max(0.5, min(1.0, as.numeric(w[1])))
                            nodes_viz$value[i] <- w_norm * 40
                        }
                    }
                }
            }

            # Edges between internal nodes
            edges_viz <- rv$edges %>%
                filter(from %in% all_internal_ids, to %in% all_internal_ids)

            # Styling
            nodes_viz$color.background <- ifelse(nodes_viz$group == "Person", "#B2EBF2", "#FFF9C4")
            nodes_viz$color.border <- ifelse(nodes_viz$group == "Person", "#00838F", "#F57F17")
            nodes_viz$borderWidth <- 2
            nodes_viz$shape <- ifelse(nodes_viz$group == "Person", "dot", "diamond")
            nodes_viz$font.face <- "Inter, sans-serif"
            nodes_viz$font.size <- 16
            nodes_viz$font.color <- "#333333"

            edges_viz$weight <- as.numeric(edges_viz$weight)
            edges_viz$width <- edges_viz$weight * 4 + 1

            visNetwork(nodes_viz, edges_viz) %>%
                visOptions(
                    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                    nodesIdSelection = TRUE
                ) %>%
                visEdges(
                    smooth = list(enabled = TRUE, type = "continuous"),
                    color = list(color = "#90CAF9", highlight = "#1565C0")
                ) %>%
                visNodes(
                    shadow = list(enabled = TRUE, size = 8, x = 2, y = 2),
                    color = list(highlight = list(border = "#333333", background = "#FFFFFF"))
                ) %>%
                visPhysics(
                    solver = "forceAtlas2Based",
                    forceAtlas2Based = list(
                        gravitationalConstant = -30,
                        centralGravity = 0.01,
                        springLength = 150
                    ),
                    stabilization = list(iterations = 100)
                ) %>%
                visInteraction(navigationButtons = FALSE) %>%
                visLayout(randomSeed = 42)
        }
    })

    # =========================================================================
    # EXPERTS TABLE
    # =========================================================================
    output$experts_table <- renderDataTable({
        req(rv$nodes, input$selected_expertise)

        target_exp <- input$selected_expertise
        if (is.null(target_exp) || length(target_exp) == 0) {
            return()
        }

        experts_edges <- rv$edges %>%
            filter(to %in% target_exp, type == "has_skill")

        match_counts <- experts_edges %>%
            group_by(from) %>%
            summarise(
                MatchingSkills = n(),
                Skills = paste(to, collapse = ", ")
            )

        expert_ids <- match_counts$from

        experts_df <- rv$nodes %>%
            filter(id %in% expert_ids) %>%
            select(Name = id, Degree = degree, Betweenness = betweenness) %>%
            left_join(match_counts, by = c("Name" = "from")) %>%
            arrange(desc(MatchingSkills), desc(Degree))

        experts_df
    })
}
