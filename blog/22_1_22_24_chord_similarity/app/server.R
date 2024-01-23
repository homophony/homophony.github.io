# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  # Read the data
  df_first_order <- rio::import("first_order.csv")

  #update selectors
  chord_names <- unique(df_first_order$chord_names)

  updateSelectizeInput(session,'chord', label = "Select chord",
                       choices = chord_names)

  key_names <- unique(df_first_order$key)

  updateSelectizeInput(session,'keys', label = "Select Key",
                       choices = c("all",key_names))

  chord_types <- unique(df_first_order$type)

  updateSelectizeInput(session,'chord_type', label = "Select type",
                       choices = c("all",chord_types))

  observe({

    validate(
      need(input$chord != "","none")
    )

    selected_data <- df_first_order %>%
      select(type,key,chord_names,input$chord)


    if(input$keys != "all"){
      selected_data <- selected_data %>%
        filter(key == input$keys)
    }

    if(input$chord_type != "all"){
      selected_data <- selected_data %>%
        filter(type == input$chord_type)
    }

    output$sim_table <- DT::renderDT({

      return(selected_data)

    }, options = list(pageLength=10, order = list(3,'desc')), rownames=FALSE,
    escape=FALSE, selection = 'none')

  })



}
