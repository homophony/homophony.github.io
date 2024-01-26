
library(tidyverse)
# pre-processing to get the chord vectors

# load chord vectors
c_chord_excel <- rio::import("chord_vectors.xlsx")

# grab feature vectors
c_chord_matrix <- as.matrix(c_chord_excel[,4:15])

# assign row names to the third column containing chord names
row.names(c_chord_matrix) <- c_chord_excel[,3]

# define all keys
keys <- c("C","Db","D","Eb","E","F","Gb","G","Ab","A","Bb","B")


# the excel sheet only has chords in C
# loop through the keys, permute the matrix to get the chords in the next key
# add the permuted matrix to new rows in the overall chord_matrix
for (i in 1:length(keys)) {

  if (i == 1) {
    # initialize chord_matrix with C matrix
    chord_matrix <- c_chord_matrix

  } else {
    #permute the matrix as a function of iterator
    new_matrix <- cbind(c_chord_matrix[, (14-i):12],c_chord_matrix[, 1:(13-i)] )

    # rename the rows with the new key
    new_names <- gsub("C", keys[i], c_chord_excel[,3])
    row.names(new_matrix) <- new_names

    # append the new_matrix to chord_matrix
    chord_matrix <- rbind(chord_matrix,new_matrix)

  }
}

chord_properties <- tibble(
  type = rep(c_chord_excel$type,length(keys)),
  key = rep(keys, each = dim(c_chord_matrix)[1]),
  chord_names  = row.names(chord_matrix),
  synonyms = list(NA),
  database_chord = FALSE
)

first_order <- lsa::cosine(t(chord_matrix))

# find repeats and build synonym list
repeat_indices <- c()

first_occurrence <- c()

for(i in 1:dim(chord_matrix)[1]){
  # get the current row
  evaluate_row <- first_order[i,]

  # don't count the current item as a repeat
  evaluate_row[i] <- 0

  # repeats are the ids for any other 1s found
  repeats <- which(evaluate_row == 1 )

  if(length(repeats) == 0){
  }

  if(length(repeats) > 0){
    #add to list of repeat items
    repeat_indices <- c(repeat_indices,repeats)

    # add synonyms
    chord_properties$synonyms[i] <- list(synonyms = row.names(chord_matrix)[repeats])
  }

  if(i %in% first_occurrence == FALSE){
    if(i %in% repeat_indices == FALSE){
      first_occurrence <- c(first_occurrence,i)
      chord_properties$database_chord[i] <- TRUE
    }
  }
}

# keep only unique chord, recompute similarities
chord_matrix_no_repeats <- chord_matrix[first_occurrence,]
first_order_no_repeats <- lsa::cosine(t(chord_matrix_no_repeats))
second_order_no_repeats <- lsa::cosine(first_order_no_repeats)

# remove scales and individual notes
only_chords <- chord_properties %>%
  filter(type!="scale",
         type!="key",
         database_chord == TRUE)

second_order_chords <- second_order_no_repeats[only_chords$chord_names,
                                               only_chords$chord_names]

#################################
# FUNCTIONS
#################################

get_most_similar <- function(x,m,r_names,top_n=1){
  r_names <- row.names(m)
  all_sim <- tibble(similarities = RsemanticLibrarian::cosine_x_to_m(x,m),
                    items = r_names) %>%
    dplyr::arrange(desc(similarities))
  return(all_sim[1:top_n,])
}

generate_intermediate_chords <- function(start,
                                         target,
                                         steps,
                                         chord_similarity_matrix,
                                         method,
){

  if(method == 1 ){
    ordered_similarities <- sort(chord_similarity_matrix[start,],decreasing =T)
    target_id <- which(names(ordered_similarities) == target)
    progression <- ordered_similarities[floor(seq(1,target_id,length.out = 2+steps ))]
    return(progression)
  }

  if(method ==2){
    num_chords <- steps+2
    num_intervals <- num_chords - 1
    interval_proportion <- 1/num_intervals
    chord_similarity_matrix[start,]

    middle <- (chord_similarity_matrix[start,]*.5) + (chord_similarity_matrix[target,]*.5)


    most_sim <- get_most_similar(middle,chord_similarity_matrix,top_n=10)


  }

}

generate_intermediate_chords_

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Between two chords"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(

        selectizeInput("start_chord",
                       label = "start chord",
                       choices = NULL),
        selectizeInput("target_chord",
                        label = "target chord",
                        choices = NULL),
        selectInput("chord_steps",
                    label = "num chord steps",
                    choices = 1:20,
                    selected = "1")
      ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("chord_progression"),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  #update selectors
  chord_names <- only_chords$chord_names

  updateSelectizeInput(session,'start_chord', label = "Start chord",
                       choices = chord_names,
                       selected = "C7")

  updateSelectizeInput(session,'target_chord', label = "Target chord",
                        choices = chord_names,
                        selected = "G7")

  observe({

    validate(
      need(input$start_chord != "","none"),
      need(input$target_chord != "","none"),
      need(input$chord_steps != "","none"),
      need(input$start_chord != input$target_chord,"none")
    )

    print(input$start_chord)
    print(input$target_chord)
    print(input$chord_steps)

    suggestion <- generate_intermediate_chords(start = input$start_chord,target = input$target_chord,steps = as.numeric(input$chord_steps),chord_similarity_matrix = second_order_chords)

    output$chord_progression <- renderText({ names(suggestion) })

  })


}

# Run the application
shinyApp(ui = ui, server = server)
