library(shiny)
library(visNetwork)
library(bslib)

# Define UI with bslib
ui <- page_sidebar(
    title = "Expertise Network",
    theme = bs_theme(
        version = 5,
        bootswatch = "zephyr", # Clean, minimalist theme
        primary = "#007bff",
        "navbar-bg" = "#ffffff"
    ),
    sidebar = sidebar(
        title = "Controls",
        width = 300,

        # Search & Filter Section
        h5("Search & Filter"),
        selectInput("focus_person",
            "Focus Person (You):",
            choices = NULL
        ), # Populated server-side

        selectInput("selected_expertise",
            "Required Expertise:",
            choices = NULL, # Populated server-side
            multiple = TRUE
        ),
        hr(),

        # Actions
        actionButton("save_data", "Save Network", class = "btn-primary w-100 mb-2"),
        actionButton("reload_data", "Reload/Reset Data", class = "btn-outline-secondary w-100")
    ),

    # Main Content Area
    navset_card_underline(
        title = "Analysis",

        # Tab 1: Network Visualization
        nav_panel(
            "Network Diagram",
            card_body(
                min_height = "600px",
                visNetworkOutput("network_plot", height = "600px")
            ),
            card_footer(
                "Tip: Scroll to zoom. Click and drag to pan. Click nodes to select."
            )
        ),

        # Tab 2: Experts Table
        nav_panel(
            "Experts List",
            card_body(
                h4("Recommended Experts"),
                dataTableOutput("experts_table")
            )
        ),

        # Tab 3: Path Finding
        nav_panel(
            "Connection Path",
            card_body(
                h4("Shortest Path to Expert"),
                verbatimTextOutput("path_output"),
                helpText("Shows the most efficient referral chain from your Focus Person to an expert.")
            )
        ),

        # Tab 4: Management
        nav_panel(
            "Edit Network",
            layout_columns(
                col_widths = c(6, 6),

                # Column 1: Add Node
                card(
                    card_header("Add Node"),
                    card_body(
                        textInput("new_node_name", "Name/Skill"),
                        selectInput("new_node_type", "Type", choices = c("Person", "Expertise")),
                        actionButton("add_node_btn", "Add Node", class = "btn-success")
                    )
                ),

                # Column 2: Remove Node
                card(
                    card_header("Remove Node"),
                    card_body(
                        selectInput("remove_node_select", "Select Node", choices = NULL),
                        actionButton("remove_node_btn", "Remove Node", class = "btn-danger")
                    )
                )
            ),
            card(
                card_header("Add Connection"),
                card_body(
                    layout_columns(
                        col_widths = c(4, 4, 4),
                        selectInput("edge_from", "From", choices = NULL),
                        selectInput("edge_to", "To", choices = NULL),
                        selectInput("edge_type", "Relationship",
                            choices = c("knows" = "knows", "has_skill" = "has_skill")
                        )
                    ),
                    textInput("edge_details", "Details/Proficiency (Optional)"),
                    actionButton("add_edge_btn", "Add Connection", class = "btn-success w-100"),
                    helpText("Direction will be auto-corrected (Person -> Expertise).")
                )
            )
        )
    )
)
