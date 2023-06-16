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
    shiny::HTML("<br>"),

    # Coding Categories
    shiny::fluidRow(
      shiny::column(width = 3,
                    shiny::radioButtons(
                      "code1",
                      names(e$classifications)[1],
                      c(e$classifications[[1]]),
                      selected = ""
                    )
      ),
      # Invisible checkbox for optional categories
      shiny::div(shiny::checkboxInput(
        "add2",
        "Add second classification",
        value = length(e$classifications)>1),
        class = "hide-checkbox"
      ),
      shiny::conditionalPanel(
        condition = "input.add2",
        shiny::column(width = 3,
                      shiny::radioButtons(
                        "code2",
                        try(names(e$classifications)[2], silent = TRUE),
                        try(c(e$classifications[[2]]), silent = TRUE),
                        selected = ""
                      )
        )),
      shiny::div(shiny::checkboxInput(
        "add3",
        "Add third classification",
        value = length(e$classifications)>2),
        class = "hide-checkbox"
      ),
      shiny::conditionalPanel(
        condition = "input.add3",
        shiny::column(width = 3,
                      shiny::radioButtons(
                        "code3",
                        try(names(e$classifications)[3], silent = TRUE),
                        try(c(e$classifications[[3]]), silent = TRUE),
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
    shiny::HTML("<br><br>"),



    # Buttons for next and previous
    shiny::actionButton("previouspage","Previous", class = "btn btn-primary",
                        style = "position: absolute; bottom: 60px; right: 90px;",
                        icon = shiny::icon("backward")),

    shiny::actionButton("nextpage", "Next", class = "btn btn-primary",
                        style = "position: absolute; bottom: 60px; right: 10px;",
                        icon = shiny::icon("forward")),

    # Button for save and exit
    shiny::actionButton("save", "Save and exit", class = "btn btn-primary",
                        style = "position: absolute; bottom: 60px; left: 10px;",
                        icon = shiny::icon("floppy-disk")),

    # Help Text
    shiny::helpText(
      sprintf(
        'Use Space and Enter keys to go to "%s" or "%s" page, respectively.',
        "Previous",
        "Next"
      )
    ),

    # Progress bar
    shinyWidgets::progressBar(
      id = "progress",
      value = e$start_app,
      total = nrow(e$data_app),
      size = "xxs"
    ),
    tags$head(tags$style(HTML('.progress-number {position: absolute; bottom: -10px; right: -10px; color: white;}'))),
    tags$head(tags$style(HTML('.progress {height: 6px; margin-top: -21px;}')))
  )
)



