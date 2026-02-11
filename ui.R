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
        # Focus Person removed as requested

        selectInput("selected_expertise",
            "Required Expertise:",
            choices = NULL, # Populated server-side
            multiple = TRUE
        ),
        sliderInput("network_spread", "Spread / Relax",
            min = 1, max = 10, value = 3, step = 1,
            post = "x"
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
                uiOutput("view_header"),
                visNetworkOutput("network_plot", height = "550px")
            ),
            card_footer(
                "Tip: Scroll to zoom. Click and drag to pan. Click an Expertise node to drill in."
            )
        ),

        # Tab 2: Experts Table
        nav_panel(
            "Experts List",
            card_body(
                h4("Recommended Experts"),
                p(class = "text-muted", "Metrics Explanation:"),
                tags$ul(
                    tags$li(tags$strong("Degree:"), " The number of direct connections a person has. Higher degree means they are more connected."),
                    tags$li(tags$strong("Betweenness:"), " A measure of influence. People with high betweenness act as 'bridges' between different parts of the network.")
                ),
                dataTableOutput("experts_table")
            )
        ),

        # Tab 3: Path Finding
        # Connection Path tab removed as requested

        # Tab 4: Management (Dynamic based on current view)
        nav_panel(
            "Edit Network",
            uiOutput("edit_network_ui")
        )
    )
)
