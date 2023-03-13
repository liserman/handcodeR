#' handcode: Classifying text into pre-defined categories.
#'
#' `handcode` opens a Shiny app which allows for handcoding strings of text into pre-defined categories. You can code between one and three variables at a time. It returns an updated data.frame with your handcoded classifications.
#'
#' @param data A dataframe initialized via the init_data() function. If you want to continue an ongoing handcoding process, you can also load your most recent handcode() output.
#' @param start A numeric value indicating the line in which you want to start handcoding. Alternatively, you can set start to "first_empty" to automatically start handcoding in the first line that has not been handcoded yet.
#'
# Importing dependencies with roxygen2
#' @import shiny
# Export function
#' @export

handcode <- function(data, start = "first_empty") {

  # Checks -----------------------------------------------------------------------

  # Check if data is dataframe
  if(!is.data.frame(data)) stop("data must be a dataframe initialized via the init_data() function.")

  # Check if first column of data is texts and character
  if(names(data)[1] != "texts" | !is.character(data[,1]) ) stop("First column of data must be character vector named texts. You can initialize your dataframe via the init_data() function.")

  # Check if all columns except the first one are factors
  if(!all(sapply(data[, -1], is.factor))) stop("All columns except the first one in the dataframe must be factors.")

  # Check if there is at least one classification variable
  if (ncol(data) < 2) stop("The dataframe must contain at least one classification variable.")

  # Check if there are at max three classification variables
  if (ncol(data) > 4) stop("The dataframe can have at most three classification variables.")

  # check if start is a single value
  if(length(start) > 1) stop("start must be a single value.")

  # Check if start is numeric or "first_empty"
  if(!is.numeric(start) & start!="first_empty") stop("start must be numeric or 'first_empty'.")

  # Check if there is uncoded data when start = "first_empty"
  if(all(!do.call(paste0,data.frame(data[,-1], helper=""))=="")) stop("All your data is already classified. Please provide unclassified data if you want to proceed.")

  # Check if interactive
  if(!interactive()) stop("handcode() can only be used in an interactive R session.")

  # Check if shiny is installed
  if (system.file(package="shiny") == "") stop("handcode() needs the package shiny installed. You can run install.packages(\"shiny\") to install shiny.")


# Initialize -------------------------------------------------------------------

  # Initialize environment e
  e <- new.env()

  # Set start to first empty row of data
  if(start == "first_empty"){
    start <- min((1:nrow(data))[do.call(paste0,data.frame(data[,-1], helper=""))==""])
  }


  # List to store classifications and their categories
  classifications <- vector("list", length = ncol(data)-1)

  # Name list
  names(classifications) <- names(data)[-1]

  # Fill with categories
  for (i in seq_along(classifications)) {
    classifications[[i]] <- levels(data[,i+1])
  }

  # Initialize container for classification
  container <- data.frame(kat1 = factor(rep("", nrow(data))), kat2 = factor(""), kat3 = factor(""))

  for (i in seq_along(classifications)){
    container[,i] <- data[,i+1]
    names(container)[i] <- names(classifications)[[i]]
    levels(container[,i]) <- c(classifications[[i]])
  }



  # UI ---------------------------------------------------------------------------

  ui <- shiny::fluidPage(

    # Generate html-class that hides output
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(".hide-checkbox {display: none;}")
      )
    ),


    # Title
    shiny::titlePanel("Text Annotation App"),

    # Main Panel
    shiny::mainPanel(

      # Text Statement
      shiny::h3("Statement:"),
      shiny::verbatimTextOutput("statement"),

      # Coding Categories
      shiny::fluidRow(
        shiny::column(width = 3,
                      shiny::radioButtons(
                        "code1",
                        names(classifications)[1],
                        c(classifications[[1]]),
                        selected = ""
                      )
        ),
        # Invisible checkbox for optional categories
        shiny::div(shiny::checkboxInput(
          "add2",
          "Add second classification",
          value = length(classifications)>1),
          class = "hide-checkbox"
        ),
        shiny::conditionalPanel(
          condition = "input.add2",
        shiny::column(width = 3,
                      shiny::radioButtons(
                        "code2",
                        try(names(classifications)[2], silent = TRUE),
                        try(c(classifications[[2]]), silent = TRUE),
                        selected = ""
                      )
        )),
        shiny::div(shiny::checkboxInput(
          "add3",
          "Add third classification",
          value = length(classifications)>2),
          class = "hide-checkbox"
        ),
        shiny::conditionalPanel(
          condition = "input.add3",
          shiny::column(width = 3,
                        shiny::radioButtons(
                          "code3",
                          try(names(classifications)[3], silent = TRUE),
                          try(c(classifications[[3]]), silent = TRUE),
                          selected = ""
                        )
          )
        )
      ),

      # Space and Enter to press next and previous
      shiny::tags$head(
        shiny::tags$script(
          '$(document).on("keyup", function (e) {
          if ( e.which == 32 ) document.getElementById("previouspage").click();
          if ( e.which == 13 ) document.getElementById("nextpage").click();
        });'
        )
      ),

      # Buttons for next and previous
      shiny::actionButton(
        "previouspage",
        "Previous",
        class = "btn btn-primary",
        style = "position: absolute; bottom: 20px; right: 80px;"
      ),
      shiny::actionButton("nextpage", "Next", class = "btn btn-primary",
                          style = "position: absolute; bottom: 20px; right: 20px;"),

      # Help Text
      shiny::helpText(
        sprintf(
          'Use Space and Enter keys to go to "%s" or "%s" page, respectively.',
          "Previous",
          "Next"
        )
      ),

      # Button for save and exit
      shiny::actionButton("save", "Save and exit", class = "btn btn-primary",
                          style = "position: absolute; bottom: 100px; right: 20px;")
    )
  )




  # Server -----------------------------------------------------------------------

  server <- function(input, output, session) {

    # Function for output generation
    gen_output <- function(data, values){

      # Generate final data.frame to be displayed as output
      final <- data.frame(texts = data$texts, kat1 = values$code1, kat2 = values$code2, kat3 = values$code3)

      # Reduce to size of original dataframe
      final <- final[,1:ncol(data)]

      # Take names from original dataframe
      names(final) <- names(data)

      # Make sure all NA is saved as ""
      final[is.na(final)] <- ""

      # Return
      return(final)
    }

    # Initialize reactiveValues
    values <- shiny::reactiveValues(
      # Set counter to start
      counter = start,

      # Initialize coding as existing classification
      code1 = container[,1],
      code2 = container[,2],
      code3 = container[,3]
    )

    # Initialize radioButtons
    observe({
      shiny::updateRadioButtons(session, "code1", selected = values$code1[values$counter])
      shiny::updateRadioButtons(session, "code2", selected = values$code2[values$counter])
      shiny::updateRadioButtons(session, "code3", selected = values$code3[values$counter])
    })


    # Behaviour when previous is clicked
    shiny::observeEvent(input$previouspage, {

      # Save current coding
      values$code1[values$counter] <- input$code1
      try(values$code2[values$counter] <- input$code2, silent = TRUE)
      try(values$code3[values$counter] <- input$code3, silent = TRUE)

      # Update the counter value by substracting 1 but only if page is bigger 1
      if (values$counter > 1) {
        values$counter <- values$counter - 1
      }

      # Set the selected value of radioButtons to "" or already coded category
      shiny::updateRadioButtons(session, "code1", selected = ifelse(is.na(values$code1[values$counter]), "", values$code1[values$counter]))
      shiny::updateRadioButtons(session, "code2", selected = ifelse(is.na(values$code2[values$counter]), "", values$code2[values$counter]))
      shiny::updateRadioButtons(session, "code3", selected = ifelse(is.na(values$code3[values$counter]), "", values$code3[values$counter]))
    })

    # Behaviour when next is clicked
    shiny::observeEvent(input$nextpage, {

      # Save current coding
      values$code1[values$counter] <- input$code1
      try(values$code2[values$counter] <- input$code2, silent = TRUE)
      try(values$code3[values$counter] <- input$code3, silent = TRUE)

      # If max pages reached, save and exit
      if (values$counter == length(data$texts)) {

        # Print the coding for all text samples when the app is closed
        shiny::observeEvent(session$onSessionEnded, {

          final <- gen_output(data, values)

          # Print
          if (nrow(final) <= 20){
            print(final)
          }

          # Write final to e
          e$output <- final

        }, once = TRUE)


        # Stop the shiny app
        shiny::stopApp()
      }

      # Update the counter value by adding 1
      values$counter <- values$counter + 1

      # Set the selected value of radioButtons to "" or already coded category
      shiny::updateRadioButtons(session, "code1", selected = ifelse(is.na(values$code1[values$counter]), "", values$code1[values$counter]))
      shiny::updateRadioButtons(session, "code2", selected = ifelse(is.na(values$code2[values$counter]), "", values$code2[values$counter]))
      shiny::updateRadioButtons(session, "code3", selected = ifelse(is.na(values$code3[values$counter]), "", values$code3[values$counter]))
    })

    # Update text displayed
    current_text <- reactive({
      data$texts[values$counter]
    })

    output$statement <- renderText({
      current_text()
    })


    # Behaviour when save is clicked
    shiny::observeEvent(input$save, {

      # Save current coding
      values$code1[values$counter] <- input$code1
      try(values$code2[values$counter] <- input$code2, silent = TRUE)
      try(values$code3[values$counter] <- input$code3, silent = TRUE)

      # Print the coding for all text samples when the app is closed
      shiny::observeEvent(session$onSessionEnded, {

        final <- gen_output(data, values)

        # Print output
        if (nrow(final) <= 20){
        print(final)
        }

        # Write final to e
        e$output <- final
      }, once = TRUE)

      # Stop the shiny app
      shiny::stopApp()
    })

  }

  # Run App ----------------------------------------------------------------------

  app <- shiny::shinyApp(ui = ui, server = server)

  shiny::runApp(app)

  return(e$output)
}
