# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Chord Similarity tool"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      selectInput("chord", label = "chord",
                  choices = NULL, selected = "C"),
      selectInput("keys", label = "keys",
                  choices = NULL, selected = "all"),
      selectInput("chord_type", label = "chord type",
                  choices = NULL, selected = "all")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      #plotOutput(outputId = "distPlot")
      DT::dataTableOutput("sim_table")

    )
  )
)
