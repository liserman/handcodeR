# ============================================================================ #
# handcodeR Web App — Docker / hosted build                                    #
# ---------------------------------------------------------------------------- #
# Self-contained. Sequential phases: configurator -> package coder (own runApp, #
# return caught) -> download (CSV + RDS). Zero package changes; stopApp kept.    #
# Public/untrusted hardening per App/README.md §11. Container entry:             #
#   Rscript app_docker.R       (binds 0.0.0.0, PORT env or 3838)                #
# ============================================================================ #

suppressMessages({
  library(shiny)
  library(shinyWidgets)
  # Production: handcodeR is installed. Dev fallback: load it from source.
  if (!requireNamespace("handcodeR", quietly = TRUE)) {
    for (p in c("..", ".")) {
      if (file.exists(file.path(p, "DESCRIPTION")) && requireNamespace("pkgload", quietly = TRUE)) {
        pkgload::load_all(p, quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
        break
      }
    }
  }
})

# Public hosting: cap uploads (README §11). Generous limit per owner decision.
options(shiny.maxRequestSize = 50 * 1024^2)
MAX_ROWS <- 50000L

# ============================================================================ #
# Inlined helpers                                                              #
# ============================================================================ #

`%||%` <- function(a, b) if (is.null(a)) b else a

HANDCODE_RESERVED <- c(
  "texts", "comparison", "pre", "post",
  "comparison_pre", "comparison_post", "notes",
  "before", "after", "id"
)

BINARY_DEFAULT_COLORS <- list(left = "#10b981", right = "#dc2626")

# CSV formula-injection guard (§11): prefix risky leading chars so spreadsheets
# treat the cell as text. RDS is unaffected.
.escape_formula <- function(x) {
  x <- as.character(x)
  risky <- !is.na(x) & substr(x, 1, 1) %in% c("=", "+", "-", "@", "\t", "\r")
  x[risky] <- paste0("'", x[risky])
  x
}
.escape_df_for_csv <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]]) || is.factor(df[[col]])) df[[col]] <- .escape_formula(df[[col]])
  }
  df
}

# Replicates handcode()/handcode_binary() validate -> normalize -> assemble for
# all four modes, reaching package internals through its namespace.
build_app_data <- function(config, save_loc = NULL) {
  ns <- asNamespace("handcodeR")
  mode           <- config$mode
  is_binary      <- grepl("binary", mode)
  has_comparison <- grepl("comparison", mode)
  prefix         <- if (is_binary) "bin" else "cat"
  missing        <- if (length(config$missing)) config$missing else "Not applicable"
  arg_list       <- config$arg_list

  comp_vec <- if (!is.null(config$comparison_col) && nzchar(config$comparison_col)) {
    config$data[[config$comparison_col]]
  } else {
    NULL
  }

  if (isTRUE(config$is_resume)) {
    data <- config$data
    if (!"texts" %in% names(data)) stop("resume dataset must contain a 'texts' column.")
    if (names(data)[1] != "texts") {
      data <- data[, c("texts", setdiff(names(data), "texts")), drop = FALSE]
    }
    if (length(arg_list) == 0) {
      for (col in setdiff(names(data), HANDCODE_RESERVED)) {
        vals <- unique(as.character(data[[col]]))
        vals <- vals[nzchar(vals) & !grepl("^_.*_$", vals)]
        if (length(vals)) arg_list[[col]] <- vals
      }
    }
  } else {
    data <- NULL
  }

  if (is_binary) ns$.check_binary_args(arg_list) else ns$.check_cat_args(arg_list, missing)
  if (is_binary) {
    ns$.check_binary_params(missing, config$multifactorial, config$enable_numeric, arg_list, config$quickcode)
    colors <- utils::modifyList(BINARY_DEFAULT_COLORS, config$colors %||% list())
    ns$.check_colors_bin(colors)
  }

  if (is.null(data)) {
    text_vec <- config$data[[config$text_col]]
    if (has_comparison) ns$.check_comparison_args(comp_vec, text_vec)
    data <- ns$.character_to_data(text_vec, arg_list, missing,
                                  prefix = prefix,
                                  comparison = if (has_comparison) comp_vec else NULL)
  }

  data <- ns$.relevel_data_factors(data, arg_list, missing)

  if (has_comparison) {
    ns$.check_comparison_col(data)
    pre_comparison  <- if (!is.null(config$comparison_pre_col)  && nzchar(config$comparison_pre_col))  config$data[[config$comparison_pre_col]]  else NULL
    post_comparison <- if (!is.null(config$comparison_post_col) && nzchar(config$comparison_post_col)) config$data[[config$comparison_post_col]] else NULL
    data <- ns$.init_comparison_context(data, config$context, pre_comparison, post_comparison)
  }

  pre  <- if (!is.null(config$pre_col)  && nzchar(config$pre_col))  config$data[[config$pre_col]]  else NULL
  post <- if (!is.null(config$post_col) && nzchar(config$post_col)) config$data[[config$post_col]] else NULL
  ns$.check_common_params(data, config$start, config$randomize, config$context, pre, post)

  extra_exclude <- if (has_comparison) c("comparison", "before_comparison", "after_comparison") else character(0)
  prepared <- ns$.prepare_data(data, config$start, config$randomize, config$context, pre, post, extra_exclude)

  factor_levels <- list()
  for (col in prepared$class_cols) {
    if (is.factor(prepared$data[[col]])) {
      lv <- levels(prepared$data[[col]])
      lv <- lv[!grepl("^_.*_$|^$", lv)]
      if (length(lv)) factor_levels[[col]] <- lv
    }
  }
  if (!is_binary) ns$.check_cat_numeric_param(config$enable_numeric, factor_levels)

  if (is_binary) {
    list(
      data = prepared$data, original_data = prepared$original_data, start_val = prepared$start_val,
      context = config$context, classifications = factor_levels, missing = missing,
      original_name = "handcode_data", multifactorial = config$multifactorial,
      enable_numeric = config$enable_numeric, quickcode = config$quickcode,
      colors = colors, add_notes = config$add_notes, save_loc = save_loc
    )
  } else {
    list(
      data = prepared$data, original_data = prepared$original_data, start_val = prepared$start_val,
      context = config$context, classifications = factor_levels, missing = missing,
      original_name = "handcode_data", add_notes = config$add_notes,
      save_loc = save_loc, enable_numeric = config$enable_numeric
    )
  }
}

# Phase 3 download app over the caught annotated frame.
download_app <- function(annotated) {
  ui <- fluidPage(
    titlePanel("Coding complete"),
    p("Your annotated data is ready. Download it below."),
    div(style = "display:flex; gap:12px; margin-bottom:16px;",
        downloadButton("dl_csv", "Download CSV", class = "btn-primary"),
        downloadButton("dl_rds", "Download RDS")),
    tags$hr(), h5("Preview (first 10 rows)"), tableOutput("preview")
  )
  server <- function(input, output, session) {
    output$preview <- renderTable(utils::head(annotated, 10))
    dl_name <- function(ext) paste0("handcode_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    output$dl_csv <- downloadHandler(
      filename = function() dl_name("csv"),
      content = function(file) utils::write.csv(.escape_df_for_csv(annotated), file, row.names = FALSE, fileEncoding = "UTF-8")
    )
    output$dl_rds <- downloadHandler(
      filename = function() dl_name("rds"),
      content = function(file) saveRDS(annotated, file)
    )
  }
  shinyApp(ui, server)
}

# ============================================================================ #
# Configurator (Phase 1) — plain single-file ui/server                         #
# ============================================================================ #

configurator_ui <- fluidPage(
  titlePanel("handcodeR — annotate text data"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data input"),
      fileInput("file", "Upload CSV", accept = ".csv"),
      actionButton("demo", "Load example data", class = "btn btn-light"),
      uiOutput("resume_note"),
      tags$hr(),
      uiOutput("text_col_ui"),
      uiOutput("comparison_col_ui"),
      uiOutput("context_cols_ui")
    ),
    mainPanel(
      width = 9,
      h4("Mode"),
      radioButtons("mode", NULL, c("Categorial" = "categorial", "Comparison" = "comparison",
                                   "Binary" = "binary", "Binary comparison" = "binary_comparison"), inline = TRUE),
      tags$hr(),
      fluidRow(
        column(3, switchInput("start_mode", "Start value", value = FALSE, onLabel = "all_empty", offLabel = "first_empty", size = "small"),
                  textInput("start_numeric", NULL, placeholder = "start [numeric]", width = "100%")),
        column(3, switchInput("context", "Show context", value = FALSE, onLabel = "Yes", offLabel = "No", size = "small")),
        column(3, switchInput("randomize", "Randomize order", value = FALSE, onLabel = "Yes", offLabel = "No", size = "small")),
        column(3, switchInput("add_notes", "Add notes", value = FALSE, onLabel = "Yes", offLabel = "No", size = "small"))
      ),
      fluidRow(column(3, switchInput("enable_numeric", "Numeric keys", value = FALSE, onLabel = "Yes", offLabel = "No", size = "small"))),
      textInput("missing", "Missing values (comma-separated)", value = "Not applicable"),
      uiOutput("binary_opts"),
      tags$hr(),
      h4("Annotation variables"),
      uiOutput("var_rows"),
      actionButton("add", "+ Add variable", class = "btn btn-light"),
      tags$hr(),
      h5("Equivalent R command"),
      verbatimTextOutput("cmd"),
      tags$hr(),
      div(style = "display:flex; justify-content:flex-end;",
          actionButton("launch", "Launch coder", class = "btn-primary", style = "width:240px;")),
      uiOutput("launch_error")
    )
  )
)

configurator_server <- function(input, output, session) {
  raw <- reactiveVal(NULL)
  observeEvent(input$file, {
    req(input$file)
    raw(utils::read.csv(input$file$datapath, stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
  })
  observeEvent(input$demo, {
    raw(utils::read.csv("example.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
  })
  data <- reactive(raw())
  char_columns <- reactive({ req(data()); names(data())[vapply(data(), is.character, logical(1))] })
  is_resume <- reactive({ req(data()); "texts" %in% names(data()) && sum(!names(data()) %in% HANDCODE_RESERVED) >= 1 })

  output$resume_note <- renderUI({
    req(data())
    if (is_resume()) div(class = "alert alert-info", style = "margin-top:10px;padding:8px;",
                         "Detected a handcodeR dataset — you can continue coding it.")
  })
  output$text_col_ui <- renderUI({
    req(char_columns())
    sel <- if ("texts" %in% char_columns()) "texts" else ""
    selectInput("text_col", "Text column", choices = c("", char_columns()), selected = sel)
  })
  output$comparison_col_ui <- renderUI({
    req(char_columns())
    if (!grepl("comparison", input$mode %||% "")) return(NULL)
    sel <- if ("comparison" %in% char_columns()) "comparison" else ""
    selectInput("comparison_col", "Comparison column", choices = c("", char_columns()), selected = sel)
  })
  output$context_cols_ui <- renderUI({
    req(char_columns())
    if (!isTRUE(input$context)) return(NULL)
    ui <- tagList(tags$hr(), h5("Custom context (optional)"),
                  selectInput("pre_col", "Pre column", choices = c("", char_columns())),
                  selectInput("post_col", "Post column", choices = c("", char_columns())))
    if (grepl("comparison", input$mode %||% "")) {
      ui <- tagList(ui,
                    selectInput("comparison_pre_col", "Comparison pre column", choices = c("", char_columns())),
                    selectInput("comparison_post_col", "Comparison post column", choices = c("", char_columns())))
    }
    ui
  })

  output$binary_opts <- renderUI({
    if (!grepl("binary", input$mode %||% "")) return(NULL)
    tagList(tags$hr(), h5("Binary options"),
      fluidRow(
        column(3, switchInput("multifactorial", "Multifactorial", value = TRUE, onLabel = "Yes", offLabel = "No", size = "small")),
        column(3, switchInput("quickcode", "Quickcode", value = FALSE, onLabel = "Yes", offLabel = "No", size = "small")),
        column(3, textInput("color_left", "Left color (hex)", value = "#10b981")),
        column(3, textInput("color_right", "Right color (hex)", value = "#dc2626"))
      ))
  })

  n_vars <- reactiveVal(1L)
  observeEvent(input$add, n_vars(n_vars() + 1L))
  output$var_rows <- renderUI({
    placeholder <- if (grepl("binary", input$mode %||% "")) "exactly,two" else "comma,separated,levels"
    tagList(lapply(seq_len(n_vars()), function(i) fluidRow(
      column(4, textInput(paste0("name_", i), NULL, value = isolate(input[[paste0("name_", i)]]) %||% "", placeholder = paste("Variable", i))),
      column(1, HTML("<b>=</b>")),
      column(7, textInput(paste0("levels_", i), NULL, value = isolate(input[[paste0("levels_", i)]]) %||% "", placeholder = placeholder))
    )))
  })
  arg_list <- reactive({
    out <- list()
    for (i in seq_len(n_vars())) {
      nm <- input[[paste0("name_", i)]]; lv <- input[[paste0("levels_", i)]]
      if (is.null(nm) || !nzchar(nm)) next
      if (is.null(lv) || !nzchar(lv)) next
      out[[nm]] <- trimws(strsplit(lv, ",")[[1]])
    }
    out
  })

  start_value <- reactive({
    nv <- suppressWarnings(as.numeric(input$start_numeric))
    if (!is.null(input$start_numeric) && nzchar(input$start_numeric) && !is.na(nv) && nv > 0) return(nv)
    if (isTRUE(input$start_mode)) "all_empty" else "first_empty"
  })
  missing_vec <- reactive({ vals <- trimws(strsplit(input$missing %||% "", ",")[[1]]); vals[nzchar(vals)] })

  quote_vec <- function(x) paste0("c(", paste0("\"", x, "\"", collapse = ", "), ")")
  output$cmd <- renderText({
    fn <- if (grepl("binary", input$mode %||% "")) "handcode_binary" else "handcode"
    lines <- character(0)
    tc <- input$text_col
    lines <- c(lines, paste0("  data = ", if (is.null(tc) || !nzchar(tc)) "<text column>" else tc))
    for (nm in names(arg_list())) lines <- c(lines, paste0("  ", nm, " = ", quote_vec(arg_list()[[nm]])))
    sv <- start_value()
    if (is.numeric(sv)) lines <- c(lines, paste0("  start = ", sv)) else if (!identical(sv, "first_empty")) lines <- c(lines, paste0("  start = \"", sv, "\""))
    if (isTRUE(input$randomize)) lines <- c(lines, "  randomize = TRUE")
    if (isTRUE(input$context)) lines <- c(lines, "  context = TRUE")
    if (isTRUE(input$add_notes)) lines <- c(lines, "  add_notes = TRUE")
    if (isTRUE(input$enable_numeric)) lines <- c(lines, "  enable_numeric = TRUE")
    mv <- missing_vec()
    if (length(mv) && !identical(mv, "Not applicable")) lines <- c(lines, paste0("  missing = ", quote_vec(mv)))
    if (!is.null(input$comparison_col) && nzchar(input$comparison_col)) lines <- c(lines, paste0("  comparison = ", input$comparison_col))
    if (!is.null(input$pre_col) && nzchar(input$pre_col)) lines <- c(lines, paste0("  pre = ", input$pre_col))
    if (!is.null(input$post_col) && nzchar(input$post_col)) lines <- c(lines, paste0("  post = ", input$post_col))
    if (grepl("binary", input$mode %||% "")) {
      if (isFALSE(input$multifactorial)) lines <- c(lines, "  multifactorial = FALSE")
      if (isTRUE(input$quickcode)) lines <- c(lines, "  quickcode = TRUE")
      cl <- input$color_left %||% "#10b981"; cr <- input$color_right %||% "#dc2626"
      if (!identical(cl, "#10b981") || !identical(cr, "#dc2626")) lines <- c(lines, paste0("  colors = list(left = \"", cl, "\", right = \"", cr, "\")"))
    }
    paste0(fn, "(\n", paste(lines, collapse = ",\n"), "\n)")
  })

  launch_problem <- reactive({
    if (is.null(data())) return("Upload a CSV or load the example data first.")
    if (nrow(data()) > MAX_ROWS) return(paste0("Dataset too large (max ", MAX_ROWS, " rows)."))
    if (is.null(input$text_col) || !nzchar(input$text_col)) return("Select a text column.")
    if (length(arg_list()) < 1) return("Define at least one annotation variable (name and levels).")
    NULL
  })
  output$launch_error <- renderUI({
    req(input$launch)
    if (!is.null(launch_problem())) div(class = "alert alert-danger", style = "margin-top:10px;", launch_problem())
  })
  observeEvent(input$launch, {
    if (!is.null(launch_problem())) return(NULL)
    stopApp(list(
      mode = input$mode, data = data(), is_resume = is_resume(),
      text_col = input$text_col, comparison_col = input$comparison_col,
      pre_col = input$pre_col, post_col = input$post_col,
      comparison_pre_col = input$comparison_pre_col, comparison_post_col = input$comparison_post_col,
      arg_list = arg_list(), missing = missing_vec(), start = start_value(),
      randomize = isTRUE(input$randomize), context = isTRUE(input$context), add_notes = isTRUE(input$add_notes),
      enable_numeric = isTRUE(input$enable_numeric), multifactorial = isTRUE(input$multifactorial),
      quickcode = isTRUE(input$quickcode), colors = list(left = input$color_left, right = input$color_right)
    ))
  })
}

# ============================================================================ #
# Orchestration — sequential phases, hosted                                    #
# ============================================================================ #

run_docker <- function() {
  host <- "0.0.0.0"
  port <- as.integer(Sys.getenv("PORT", "3838"))
  ns <- asNamespace("handcodeR")

  config <- runApp(shinyApp(configurator_ui, configurator_server), host = host, port = port)
  app_data <- build_app_data(config, save_loc = NULL)  # web: quicksave off

  build_ui <- switch(config$mode,
    categorial = ns$.build_categorial_ui, comparison = ns$.build_comparison_ui,
    binary = ns$.build_binary_ui, binary_comparison = ns$.build_binary_comparison_ui,
    stop("unknown mode: ", config$mode))
  build_server <- switch(config$mode,
    categorial = ns$.categorial_server, comparison = ns$.comparison_server,
    binary = ns$.binary_server, binary_comparison = ns$.binary_comparison_server)

  # Own runApp -> stopApp(annotated) return is caught here.
  annotated <- runApp(shinyApp(build_ui(app_data), build_server(app_data)), host = host, port = port)

  runApp(download_app(annotated), host = host, port = port)
}

# HANDCODER_DEFS_ONLY is the test/inspection hatch (load definitions, don't run).
if (!nzchar(Sys.getenv("HANDCODER_DEFS_ONLY"))) run_docker()
