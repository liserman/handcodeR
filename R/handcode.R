#' handcode: Classifying text into pre-defined categories.
#'
#' `handcode` opens a Shiny app which allows for handcoding of texts into pre-defined categories. The results will be saved as a new object to the global environment.
#'
#' @param data A dataframe initialized via the init_data() function.
#' @param start A numeric value indicating the line in which you want to start handcoding. Alternatively, you can set start to "first_empty" to automatically start handcoding in the first line that has not been handcoded yet.
#' @param newname A character string indicating a name for a new object that saves the handcoding to your global environment
#'
# Importing dependencies with roxygen2
#' @import shiny
# Export function
#' @export

handcode <- function(data, start = 1, newname = "data_new") {

  # Checks -----------------------------------------------------------------------

  # Check if shiny is installed
  if (!"shiny" %in% rownames(installed.packages())) stop("handcode() needs the package shiny installed. You can run install.packages(\"shiny\") to install shiny.")

  # Check if data is dataframe
  if(!is.data.frame(data)) stop("data must be a dataframe initialized via the init_data() function.")

  # Check if first column of data is texts and character
  if(names(data)[1] != "texts" | !is.character(data[,1]) ) stop("First column of data must be character vector named texts. You can initialize your dataframe via the init_data() function.")

  # Check if there is more than one category to code
  if(ncol(data) < 4) stop("Too few categories provided. You can initialize your dataframe via the init_data() function.")

  # Check if last column of data is classification and character
  if(names(data)[ncol(data)] != "classification" | !is.character(data[,ncol(data)]) ) stop("Last column of data must be character vector named classification. You can initialize your dataframe via the init_data() function.")

  # Check if start is numeric or "first_empty"
  if(!is.numeric(start) & start!="first_empty") stop("start must be numeric or 'first_empty'.")

  # check if start is a single value
  if(length(start) > 1) stop("start must be a single value.")

  # Check if newname is character
  if(!is.character(newname)) stop("newname must be character. newname gives the name of a new object in your environment that saves your classifications.")

  # check if newname is a single input
  if(length(newname) > 1) stop("newname must be a single input")

  # Check if interactive
  if(!interactive()) stop("handcode() can only be used in an interactive R session.")


  # Initialize -------------------------------------------------------------------

  # Set start to first empty row of data
  if(start == "first_empty")
    start <- min((1:nrow(data))[data$classification == ""])

  # Object texts to store texts
  texts <- data$texts

  # Object categories to store categories
  categories <- names(data)[2:(ncol(data)-1)]


  # UI ---------------------------------------------------------------------------

  ui <- shiny::fluidPage(

    # Title
    shiny::titlePanel("Text Annotation App"),

    # Main Panel
    shiny::mainPanel(

      # Text Statement
      h3("Statement:"),
      shiny::verbatimTextOutput("statement"),

      # Coding Categories
      shiny::radioButtons(
        "code",
        "Coding for the current text sample:",
        c("", "Not applicable", categories),
        selected = ""
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

    # Initialize reactiveValues
    values <- shiny::reactiveValues(
      # Set counter to start
      counter = start,

      # Initialize coding as existing classification
      coding = data$classification
    )


    # Behaviour when previous is clicked
    shiny::observeEvent(input$previouspage, {

      # Save current coding
      values$coding[values$counter] <- input$code

      # Update the counter value by substracting 1 but only if page is bigger 1
      if (values$counter > 1) {
        values$counter <- values$counter - 1
      }

      # Set the selected value of radioButtons to "" or already coded category
      shiny::updateRadioButtons(session, "code", selected = values$coding[values$counter])
    })

    # Behaviour when next is clicked
    shiny::observeEvent(input$nextpage, {

      # Save current coding
      values$coding[values$counter] <- input$code

      # If max pages reached, save and exit
      if (values$counter == length(texts)) {

        # Print the coding for all text samples when the app is closed
        shiny::observeEvent(session$onSessionEnded, {

          # Generate final data.frame to be displayed as output
          final <- data

          # Overwrite classification with coded content
          final$classification <- values$coding

          # Overwrite categories with TRUE/FALSE
          for (i in 2:(ncol(data)-1)) {
            final[,i] <- names(final)[i] == values$coding
            final[values$coding == "", i] <- NA
          }

          # Print
          print(final)

          # Write final to global environment
          assign(newname, final, envir = .GlobalEnv)

        }, once = TRUE)


        # Stop the shiny app
        session$close()
      }

      # Update the counter value by adding 1
      values$counter <- values$counter + 1

      # Set the selected value of radioButtons to "" or already coded category
      shiny::updateRadioButtons(session, "code", selected = values$coding[values$counter])
    })

    # Update text displayed
    current_text <- reactive({
      texts[values$counter]
    })

    output$statement <- renderText({
      current_text()
    })


    # Behaviour when save is clicked
    shiny::observeEvent(input$save, {

      # Save the coding for the current text sample
      values$coding[values$counter] <- input$code

      # Print the coding for all text samples when the app is closed
      shiny::observeEvent(session$onSessionEnded, {

        # Generate final data.frame to be displayed as output
        final <- data

        # Overwrite classification with coded content
        final$classification <- values$coding

        # Overwrite categories with TRUE/FALSE
        for (i in 2:(ncol(data)-1)) {
          final[,i] <- names(final)[i] == values$coding
          final[values$coding == "", i] <- NA
        }

        # Print output
        print(final)

        # Write final to global environment
        assign(newname, final, envir = .GlobalEnv)
      }, once = TRUE)

      # Stop the shiny app
      session$close()
    })

  }

  # Run App ----------------------------------------------------------------------
  shiny::runApp(shiny::shinyApp(ui = ui, server = server))
}
