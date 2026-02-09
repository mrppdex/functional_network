library(shiny)
library(visNetwork)

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Expertise Network Analysis"),
    sidebarLayout(
        sidebarPanel(
            h4("Search & Filter"),
            helpText("Find the best person for a task based on expertise and network connectivity."),

            # Select input for Expertise
            selectInput("selected_expertise",
                "Select Required Expertise (Multi-select):",
                choices = NULL, # Populated server-side
                multiple = TRUE,
                selected = NULL
            ),

            # Select input for Focus Person (Ego Network)
            selectInput("focus_person",
                "Focus Person (Optional - You):",
                choices = c("None", "Alice", "Bob"), # Populated server-side
                selected = "None"
            ),
            hr(),
            h4("Actions"),
            actionButton("reset_view", "Reset View", icon = icon("refresh")),
            br(), br(),
            actionButton("save_data", "Save Network Data", icon = icon("save"), class = "btn-primary"),
            actionButton("reload_data", "Reload/Reset Data", icon = icon("undo"), class = "btn-danger"),
            hr(),
            h4("Legend"),
            tags$ul(
                tags$li(span(style = "color: blue; font-weight: bold;", "Blue Nodes"), ": People"),
                tags$li(span(style = "color: orange; font-weight: bold;", "Orange Nodes"), ": Expertise")
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Network Diagram",
                    visNetworkOutput("network_plot", height = "600px")
                ),
                tabPanel(
                    "Analysis & Recommendations",
                    h4("Best Points of Contact"),
                    p("People with the selected expertise, ranked by their network centrality (influence)."),
                    dataTableOutput("experts_table"),
                    hr(),
                    h4("Path to Expert"),
                    textOutput("path_output")
                ),
                tabPanel(
                    "Management",
                    h3("Manage Network"),
                    p("Add or remove people and expertise, and create connections."),
                    fluidRow(
                        column(
                            4,
                            wellPanel(
                                h4("Add Node"),
                                textInput("new_node_name", "Name/Label"),
                                selectInput("new_node_type", "Type", choices = c("Person", "Expertise")),
                                actionButton("add_node_btn", "Add Node", class = "btn-success")
                            )
                        ),
                        column(
                            4,
                            wellPanel(
                                h4("Add Connection"),
                                selectInput("edge_from", "From", choices = NULL),
                                selectInput("edge_to", "To", choices = NULL),
                                selectInput("edge_type", "Relationship",
                                    choices = c("knows" = "knows", "has skill" = "has_skill")
                                ),
                                textInput("edge_details", "Details/Proficiency (Optional)"),
                                actionButton("add_edge_btn", "Add Connection", class = "btn-success")
                            )
                        ),
                        column(
                            4,
                            wellPanel(
                                h4("Remove Node"),
                                selectInput("remove_node_select", "Select Node", choices = NULL),
                                actionButton("remove_node_btn", "Remove Node", class = "btn-danger")
                            )
                        )
                    )
                )
            )
        )
    )
)
