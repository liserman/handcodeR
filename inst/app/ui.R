ui <- shiny::fluidPage(

  # Generate html-class that hides output
  shiny::tags$head(
    shiny::tags$style(
      shiny::HTML(".hide-checkbox {display: none;}"),
    )
  ),

  shiny::tags$head(
    shiny::tags$style(
      shiny::HTML("
        /* Increase spacing after htmlOutput('statement') */
        .statement-output {
          margin-bottom: 20px;
        }

        /* Add box around htmlOutput('statement') */
        .statement-output {
          border: 1px solid #ccc;
          padding: 10px;
        }
      ")
    )
  ),


  # Title
  shiny::titlePanel("handcodeR Text Annotation App"),

  # Main Panel
  shiny::mainPanel(

    # Text Statement
    shiny::h3("Statement:"),
    shiny::div(
    shiny::htmlOutput("statement"),
    class = "statement-output"),

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
