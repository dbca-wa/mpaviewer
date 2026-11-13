# TEMPLATE for a new asset module (e.g. seagrass ). Not loaded by the package (lives in dev/).
#
# HOW TO USE
# 1. Copy to R/mod_<asset>.R and find/replace `template` with your asset name (lower case, no dots).
# 2. Wire it in (the three "WIRE" comments at the bottom).
# 3. Delete the example pieces you don't need.
#
# RULES / GOTCHAS
# * UI: every id goes through ns("id"). Forgetting ns() = output silently never appears.
# * Server: use input$id / output$id with the plain id (moduleServer scopes them for you).
# * Inputs created inside renderUI() need ns() too: inputId = ns("id"), or create_dropdown(input, ns, "id", ...).
# * conditionalPanel conditions are JavaScript, so they need the namespaced id:
#     conditionalPanel(paste0("input['", ns("metric"), "'] == 'Cover'"), ...)
# * Use req() before using an input that may not exist yet (dropdowns made by renderUI start as NULL).
# * `mpa_data` is shared by all sessions: read from it, never modify it.
# * For pre-rendered plots use a spec table + register_png_plots() instead of one renderUI per plot
#   (see fish_png_spec in R/mod_fish.R). PNGs must exist in inst/app/www/plots (generate_plots()).

mod_template_ui <- function(id) {
  ns <- NS(id) # makes ids unique per module: "template-total_number" etc.

  tabItem(
    tabName = "templatetab", # must equal the menuItem tabName in app_ui.R (this one is NOT wrapped in ns())
    fluidRow(
      tabBox(
        title = "Choose a spatial scale to plot", width = 8, id = ns("tabset"), height = "78vh",
        tabPanel("State-wide summary",
                 valueBoxOutput(ns("total_number"), width = 6)),
        tabPanel("Marine park",
                 uiOutput(ns("park_dropdown")), # dropdown is built in the server from the data
                 withSpinner(uiOutput(ns("park_png"))),
                 plotOutput(ns("park_plot")))
      ),
      box(width = 4, title = "Sampling locations",
          leafletOutput(ns("map"), height = "78vh"))
    )
  )
}

# One row per pre-rendered PNG plot: file is Template_<park>_<suffix>.png (add "method" via the
# `method` argument of register_png_plots() if the file names contain one, as for fish).
template_png_spec <- tibble::tribble(
  ~id,        ~suffix,   ~title, ~methods,
  "park_png", "overview", NA,    NA
)

mod_template_server <- function(id, mpa_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # needed for inputs created in renderUI()

    # Alias for the selected park; NULL until the dropdown has rendered
    park <- reactive(input$park)

    # Summary box. Field names (total_number_template) come from generate_data(); add yours there.
    output$total_number <- renderValueBox(
      valueBox(prettyNum(mpa_data$total_number_template, big.mark = ","), "Things counted", color = "yellow")
    )

    # Dropdown that keeps the current choice if the choices change
    output$park_dropdown <- renderUI(
      create_dropdown(input, ns, "park", sort(unique(mpa_data$template_metadata$marine_park)),
                      "Choose a marine park:", FALSE)
    )

    # Static PNG plots from the spec table above
    register_png_plots(output, template_png_spec, "Template", park)

    # Example of a live ggplot instead of a PNG: slower, so prefer PNGs for anything heavy
    output$park_plot <- renderPlot({
      req(park())
      # filter mpa_data$... to park() and plot
    })

    output$map <- renderLeaflet(leaflet() %>% addTiles())
  })
}

# WIRE 1 - app_ui.R, in dashboardSidebar(sidebarMenu(...)):
#   menuItem(tags$div(tags$i(icon("leaf")), tags$span("Template")), tabName = "templatetab")
# WIRE 2 - app_ui.R, inside tabItems(...):
#   mod_template_ui("template")
# WIRE 3 - app_server.R, inside app_server():
#   mod_template_server("template", mpa_data)
