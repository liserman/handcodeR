library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  titlePanel("handcodeR – Web annotator"),

  uiOutput("main_or_app")

)

server <- function(input, output, session) {

  # ============================================================
  # 0. App mode switch
  # ============================================================
  launch_app <- reactiveVal(FALSE)

  observeEvent(input$launch, {
    launch_app(TRUE)
  })

  # ============================================================
  # 1. UI SWITCH
  # ============================================================
  output$main_or_app <- renderUI({
    if(!launch_app()) {
    sidebarLayout(
      # ------------------------------------------------------------------
      # SIDEBAR
      # ------------------------------------------------------------------
      sidebarPanel(
        width = 3,
        h3("Data input"),

        # File Upload
        fileInput(
          "upload", "Upload CSV",
          accept = c(".csv")
        ),

        # Use dataset as input
        uiOutput("switch_existing_dataset"),
        tags$hr(),

        # Select text column
        uiOutput("select_text_col"),

        # Select comparison column
        uiOutput("select_comparison_col"),

        # Select pre / post columns
        tags$hr(),
        uiOutput("pre_post_inputs"),

        # Select comparison pre / post columns
        tags$hr(),
        uiOutput("comparison_pre_post_inputs")
      ),

      # ------------------------------------------------------------------
      # MAIN PANEL
      # ------------------------------------------------------------------
      mainPanel(
        width = 8,

        # ------------------------------------------------------------------
        # Live command preview
        # ------------------------------------------------------------------
        verbatimTextOutput("handcode_preview"),
        tags$hr(),

        # ------------------------------------------------------------------
        # Annotation variables
        # ------------------------------------------------------------------
        h3("Annotation Variables"),
        uiOutput("annotation_vars"),

        tags$hr(),

        # ---- Compact Switches Row ----
        fluidRow(
          column(
            3,
            switchInput(
              "start_mode",
              label = "Start value",
              value = FALSE,               # FALSE = first_empty, TRUE = all_empty
              offLabel = "first_empty",
              onLabel = "all_empty",
              size = "small"
            ),
            textInput(
              "start_numeric",
              label = NULL,
              placeholder = "start [numeric]",
              value = "",
              width = "100%"
            )
          ),
          column(
            3,
            switchInput(
              "context",
              label = "Show context",
              value = FALSE,
              onLabel = "Yes",
              offLabel = "No",
              size = "small"  # smaller size
            )
          ),
          column(
            3,
            switchInput(
              "randomize",
              label = "Randomize order",
              value = FALSE,
              onLabel = "Yes",
              offLabel = "No",
              size = "small"
            )
          ),
          column(
            3,
            switchInput(
              "add_notes",
              label = "Add notes",
              value = FALSE,
              onLabel = "Yes",
              offLabel = "No",
              size = "small"
            )
          )
        ),

        tags$hr(),

        # ---- Missing input ----
        uiOutput("missing_input"),


        # ---- Start value + Launch App ----
        fluidRow(
          column(
            width = 6,
            offset = 6, # pushes the column to the right half
            div(
              style = "
        display: flex;
        justify-content: flex-end;
        align-items: flex-end;
        height: 100%;
        padding-top: 25px;",
              actionButton(
                "launch",
                "Launch App",
                class = "btn-primary",
                style = "width: 300px;"
              )
            )
          )
        ),

        tags$hr(),
      ))
    } else {

      handcodeR:::handcoder_ui(a_obj(), mode = "web")

    }
    })


  # ============================================================
  # 2. App SETUP
  # ============================================================


  ### ------ Data loading

  data_uploaded <- reactive({
    req(input$upload)
    read.csv(input$upload$datapath, stringsAsFactors = FALSE)
  })

  # Define all text columns to limit choices for all variables requiring text input
  char_columns <- reactive({
    req(data_uploaded())
    names(data_uploaded())[sapply(data_uploaded(), is.character)]
  })


  ### ------ Switch to use dataset as input

  output$switch_existing_dataset <- renderUI({
    req(data_uploaded())

    is_handcode_like <- all(c("texts" %in% names(data_uploaded()),
                              sum(!names(data_uploaded()) %in% c("texts", "comparison",
                                                                 "pre", "post",
                                                                 "comparison_pre", "comparison_post",
                                                                 "notes")) > 1,  # at least one variable
                              sum(!names(data_uploaded()) %in% c("texts", "comparison",
                                                                 "pre", "post",
                                                                 "comparison_pre", "comparison_post",
                                                                 "notes")) <= 6  # at most 6 variables
                              # texts is character vector
                              # if comparison exists, it is character vector
                              # if pre and post exist, they are character vectors
                              # if comparison_pre and post exist, they are character vectors
                              # if notes exists, it is character vector
                              # all other variables are factors

    ))

    if (!is_handcode_like) return(NULL)

    switchInput(
      "use_existing",
      label = "Use as handcodeR dataset?",
      value = TRUE, size = "small",
      onLabel = "Yes", offLabel = "No"
    )
  })

  ### ------ Select texts column

  output$select_text_col <- renderUI({
    req(char_columns())
    selectInput("text_col", "Select text column",
                choices = c("", char_columns()), selected = "")
  })

  ### ------ Select comparison column

  output$select_comparison_col <- renderUI({
    req(char_columns())
    selectInput("comparison_col", "Select comparison column (optional)",
                choices = c("", char_columns()), selected = "")
  })

  ### ------ Annotation columns

  max_vars <- 6
  current_var_count <- reactiveVal(1)

  observeEvent(input$add_var_btn, {
    if (current_var_count() < max_vars) {
      current_var_count(current_var_count() + 1)
    }
  })

  output$annotation_vars <- renderUI({
    n <- current_var_count()

    # Build UI with empty value fields (they will be updated below)
    ui_list <- tagList(
      lapply(1:n, function(i) {
        fluidRow(
          column(4, textInput(paste0("var_name_", i), label = NULL,
                              value = "",  # will be overwritten
                              placeholder = paste("Variable", i))),
          column(1, HTML("<b>=</b>")),
          column(7, textInput(paste0("var_levels_", i), label = NULL,
                              value = "",  # will be overwritten
                              placeholder = "comma,separated,levels"))
        )
      }),
      if (n < max_vars)
        div(
          style = "text-align:left;",
          actionButton("add_var_btn", "+", class = "btn btn-light")
        )
    )

    ui_list
  })

  # Restore previous inputs after UI redraw
  observe({
    n <- current_var_count()

    for (i in 1:n) {
      isolate({
        if (!is.null(input[[paste0("var_name_", i)]])) {
          updateTextInput(
            session,
            paste0("var_name_", i),
            value = input[[paste0("var_name_", i)]]
          )
        }
        if (!is.null(input[[paste0("var_levels_", i)]])) {
          updateTextInput(
            session,
            paste0("var_levels_", i),
            value = input[[paste0("var_levels_", i)]]
          )
        }
      })
    }
  })


  ### ------ Pre / Post input

  output$pre_post_inputs <- renderUI({
    req(input$context)
    req(char_columns())

    tagList(
      h5("Custom context for data:"),
      selectInput("pre_col", "Select PRE column (optional)",
                  choices = c("", char_columns()), selected = ""),
      selectInput("post_col", "Select POST column (optional)",
                  choices = c("", char_columns()), selected = "")
    )
  })


  ### ------ Comparison Pre / Post Inputs

  output$comparison_pre_post_inputs <- renderUI({
    req(input$context & input$comparison_col != "")
    req(char_columns())

    tagList(
      h5("Custom context for comparison:"),
      selectInput("comparison_pre_col", "Select comparison PRE column (optional)",
                  choices = c("", char_columns()), selected = ""),
      selectInput("comparison_post_col", "Select comparison POST column (optional)",
                  choices = c("", char_columns()), selected = "")
    )
  })

  ### ------ Missing Values

  output$missing_input <- renderUI({
    req(data_uploaded())  # show only after data exists

    textInput(
      "missing",
      "Missing values (comma-separated):",
      placeholder = "e.g. not applicable, n/a, none",
      value = "Not applicable"
    )
  })

  ### ------ Start Value

  start_value <- reactive({

    # Parse the numeric input from textInput
    numeric_val <- if (input$start_numeric == "" || is.null(input$start_numeric)) {
      NA
    } else {
      as.numeric(input$start_numeric)
    }

    # If numeric input is valid, use it
    if (!is.na(numeric_val) && numeric_val > 0) {
      return(numeric_val)
    }

    # Otherwise use the switch (TRUE = all_empty, FALSE = first_empty)
    if (isTRUE(input$start_mode)) {
      return("all_empty")
    } else {
      return("first_empty")
    }
  })

  ### ------ Code display

  output$handcode_preview <- renderText({

    req(data_uploaded())

    # ---- Start building the command ----
    cmd <- "handcode("

    # DATA
    if (!is.null(input$text_col)) {
      cmd <- paste0(cmd, "\n  data = ", input$text_col)
    }

    # ANNOTATION VARIABLES (dynamic list)
    n <- current_var_count()

    for (i in seq_len(n)) {
      name <- input[[paste0("var_name_", i)]]
      levels <- input[[paste0("var_levels_", i)]]

      if (!is.null(name) && nchar(name) > 0 && !is.null(levels) && nchar(levels) > 0) {

        # Clean and quote levels
        lev_vec <- trimws(unlist(strsplit(levels, ",")))
        lev <- paste0("\"", lev_vec, "\"", collapse = ", ")

        cmd <- paste0(cmd, ",\n  ", name, " = c(", lev, ")")
      }
    }

    # START VALUE
    val <- start_value()

    # Only display in function call if not the default "first_empty"
    if (is.numeric(val)) {
      cmd <- paste0(cmd, ",\n  start = ", val)
    } else if (!identical(val, "first_empty")) {
      cmd <- paste0(cmd, ",\n  start = \"", val, "\"")
    }

    # RANDOMIZE
    if (isTRUE(input$randomize)) {
      cmd <- paste0(cmd, ",\n  randomize = TRUE")
    }

    # CONTEXT
    if (isTRUE(input$context)) {
      cmd <- paste0(cmd, ",\n  context = TRUE")
    }

    # MISSING VALUES
    if (!is.null(input$missing)) {

      # Split & trim
      miss_levels <- trimws(strsplit(input$missing, ",")[[1]])

      # Default is exactly one value: "not applicable"
      default_missing <- c("Not applicable")

      # Only display if user changed the values
      if (!identical(miss_levels, default_missing)) {

        miss_txt <- paste0("\"", miss_levels, "\"", collapse = ", ")

        cmd <- paste0(cmd, ",\n  missing = c(", miss_txt, ")")
      }
    }


    # PRE
    if (!is.null(input$pre_col) && nzchar(input$pre_col)) {
      cmd <- paste0(cmd, ",\n  pre = ", input$pre_col)
    }

    # POST
    if (!is.null(input$post_col) && nzchar(input$post_col)) {
      cmd <- paste0(cmd, ",\n  post = ", input$post_col)
    }

    # COMPARISON
    if (!is.null(input$comparison_col) && nzchar(input$comparison_col)) {
      cmd <- paste0(cmd, ",\n  comparison = ", input$comparison_col)
    }

    # COMPARISON PRE
    if (!is.null(input$comparison_pre_col) && nzchar(input$comparison_pre_col)) {
      cmd <- paste0(cmd, ",\n  comparison_pre = ", input$comparison_pre_col)
    }

    # COMPARISON POST
    if (!is.null(input$comparison_post_col) && nzchar(input$comparison_post_col)) {
      cmd <- paste0(cmd, ",\n  comparison_post = ", input$comparison_post_col)
    }

    # ADD NOTES
    if (isTRUE(input$add_notes)) {
      cmd <- paste0(cmd, ",\n  add_notes = TRUE")
    }

    cmd <- paste0(cmd, "\n)\n")

    cmd
  })



  # ============================================================
  # 3. Build data for handcoder app
  # ============================================================

  a_obj <- reactive({
    # Required inputs
    req(launch_app())
    req(data_uploaded())
    req(input$text_col)
    req(current_var_count())

    # Extract text vector (required)
    data_vec <- data_uploaded()[[input$text_col]]

    # Optional column: comparison
    comparison_vec <- NULL
    if (!is.null(input$comparison_col) &&
        nzchar(input$comparison_col)) {

      comparison_vec <- data_uploaded()[[input$comparison_col]]
    }

    # Build annotation variable list (all optional)
    classifications <- list()
    for (i in seq_len(current_var_count())) {

      varname <- input[[paste0("var_name_", i)]]
      levels  <- input[[paste0("var_levels_", i)]]

      # Skip incomplete variables
      if (is.null(varname) || !nzchar(varname)) next
      if (is.null(levels)  || !nzchar(levels))  next

      classifications[[varname]] <- strsplit(levels, ",")[[1]]
    }

    # Optional columns
    pre_vec <- if (!is.null(input$pre_col) &&
                   nzchar(input$pre_col)) {
      data_uploaded()[[input$pre_col]]
    } else NULL

    post_vec <- if (!is.null(input$post_col) &&
                    nzchar(input$post_col)) {
      data_uploaded()[[input$post_col]]
    } else NULL

    comparison_pre_vec <- if (!is.null(input$comparison_pre_col) &&
                              nzchar(input$comparison_pre_col)) {
      data_uploaded()[[input$comparison_pre_col]]
    } else NULL

    comparison_post_vec <- if (!is.null(input$comparison_post_col) &&
                               nzchar(input$comparison_post_col)) {
      data_uploaded()[[input$comparison_post_col]]
    } else NULL

    # Build data object
    data <- handcodeR:::character_to_data(
      data       = data_vec,
      comparison = comparison_vec,
      arg_list   = classifications,
      missing    = input$missing,
      add_notes  = input$add_notes
    )

    # Data for app
    handcodeR:::data_for_app(
      data           = data,
      start          = start_value(),
      randomize      = input$randomize,
      context        = input$context,
      pre            = pre_vec,
      post           = post_vec,
      comparison_pre = comparison_pre_vec,
      comparison_post = comparison_post_vec
    )
  })

  # ============================================================
  # 4. Attach handcoder_server
  # ============================================================

  observeEvent(input$launch, {
    handcodeR:::handcoder_server(input, output, session, a_obj(), mode = "web")
  })

}


shinyApp(ui, server)
