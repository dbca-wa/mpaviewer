library(shiny)
# library(data.table)
library(dplyr)

# Create small data table

dat1 <- data.table::data.table(x = c(1, 2, 3), y = c("a", "b", "c"), z = c("ID1", "ID2", "ID2"))
dat2 <- data.table::data.table(x = c(2, 4, 6), y = c("1", "2", "3"), z = c("ID1", "ID2", "ID2"))

glimpse(dat1)

# set key as all variables
data.table::setkey(dat1)

# Check key worked
data.table::key(dat1)

mpa_data <- structure(list(dat1 = dat1,
                           dat2 = dat2),
                      class = "mpa_data")

# Save data table to file
save(mpa_data, file = "example.Rdata")

# Define UI
ui <- fluidPage(
  selectInput(
    "input1",
    "Dropdwon 1",
    c("ID1","ID2"),
    selected = "ID1",
    multiple = FALSE
  ),

  htmlOutput("fish.state.method.dropdown", multiple = FALSE)
)

# Define server
server <- function(input, output, session) {

  load("example.Rdata")
  # data.table::key(mpa_data)

  ###### ►  Create method dropdown ----
  output$fish.state.method.dropdown <- renderUI({

    print(data.table::key(mpa_data$dat1))

    dat <- mpa_data$dat1[z %in% input$input1]

    options <- dat %>%
      dplyr::distinct(y) %>%
      dplyr::pull("y")

    shinyWidgets::pickerInput(
      inputId = "fish.state.method.dropdown",
      label = "Choose method to plot:",
      choices = sort(options),
      multiple = FALSE,
      selected = sort(options)[1],
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )

    # create_dropdown("fish.state.method.dropdown", options, "Choose a method:", FALSE)
  })
}

# Run app
shinyApp(ui, server)
