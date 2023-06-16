#' handcode: Classifying text into pre-defined categories.
#'
#' `handcode` opens a Shiny app which allows for handcoding strings of text into pre-defined categories. You can code between one and three variables at a time. It returns an updated data.frame with your annotated classifications.
#'
#' @param data A character vector of texts you want to annotate or a data.frame returned from the handcode() function.
#' @param ... Between one and three named character vectors indicating different variables and categories you want to use for your annotation. Only needed if data a new character vector of texts.
#' @param start A numeric value indicating the line in which you want to start handcoding. Alternatively, you can set start to "first_empty" to automatically start handcoding in the first line that has not been handcoded yet, or to "all_empty" to automatically handcode all lines that have not been handcoded yet.
#' @param randomize A logical value indicating whether you want to randomize the order in which texts are shown to the coder.
#' @param context A logical value indicating whether you want the coder to see the previous and next text alongside the text that is currently coded. If TRUE, the function will show the previous and next text in light gray.
#'
# Importing dependencies with roxygen2
#' @import shiny
#' @importFrom shinyWidgets progressBar
#' @importFrom shinyWidgets updateProgressBar
# Export function
#' @export

handcode <- function(data, ... , start = "first_empty", randomize = FALSE, context = FALSE) {

  # Initialize ...
  arg_list <- list(...)

  # Check if data is either data.frame or character vector of texts
  if(!is.data.frame(data) & !is.character(data)) stop("data must be a character vector of texts you want to annotate or a data.frame() that has been returned in an earlier run of this function.")


  # Checks and data handling if data is character vector
  if(is.character(data)){

      # Check if items in arg_list are named vectors
      if(!all(vapply(arg_list, is.character, logical(1))) | !all(vapply(arg_list, is.vector, logical(1)))) {
        stop("All arguments in ... must be named character vectors.")
      }

      # Check that there are between 1 and 3 named character vectors given
      if(length(arg_list) < 1 | length(arg_list) > 3) {
        stop("If data is a character vector of texts to annotate, you must provide between 1 and 3 named character vectors of annotation categories.")
      }

      # Check if "" and "Not applicable" are in list of categories
      if(TRUE %in% sapply(sapply(arg_list, function(x) x %in% c("", "Not applicable")), function(x) TRUE %in% x)) stop("The default categories \"\" and \"Not applicable\" are automatically added handcode(). You cannot add them as input to the function.")

      # Check for duplicate categories
      for (i in seq_along(arg_list)) {
        if(length(unique(arg_list[[i]]))<length(arg_list[[i]])) stop("You cannot set duplicate categories for a variable. Please provide unique categories for classification.")
      }


    # Data handling

    # Initialize empty dataframe
    output <- data.frame(matrix(factor(""), nrow = length(data), ncol = length(arg_list)))

    # Name variables according to arg_list
    names(output) <- names(arg_list)

    # For every categorisation, set levels of factor
    for (i in seq_along(arg_list)){
      output[,i] <- factor("", levels = c("", "Not applicable", arg_list[[i]]))
    }

    # Paste texts object to dataframe
   data <- data.frame(texts = data, output)

  }

  # Checks -----------------------------------------------------------------------

  # Check if first column of data is texts and character
  if(names(data)[1] != "texts" | !is.character(data[,1]) ) stop("data must be a character vector of texts you want to annotate or a data.frame() that has been returned in an earlier run of this function.")

  # Check if all columns except the first one are factors
  if(!all(sapply(data[, -1], is.factor))) stop("data must be a character vector of texts you want to annotate or a data.frame() that has been returned in an earlier run of this function.")

  # Check if there are min 1 and  max 3 classification variables
  if (ncol(data) < 2 | ncol(data) > 4) stop("data must be a character vector of texts you want to annotate or a data.frame() that has been returned in an earlier run of this function.")

  # check if start is a single value
  if(length(start) > 1) stop("start must be a single value.")

  # Check if start is numeric or "first_empty"
  if(!is.numeric(start) & !start%in%c("first_empty", "all_empty")) stop("start must be numeric, 'first_empty', or 'all_empty'.")

  # Check if there is uncoded data when start = "first_empty"
  if(all(!do.call(paste0,data.frame(data[,-1], helper=""))=="")) stop("All your data is already classified. Please provide unclassified data if you want to proceed.")

  # Check if randomize is single value
  if(length(randomize)>1) stop("randomize must be a single value.")

  # Check if randomize is logical
  if(!is.logical(randomize)) stop("randomize must be either TRUE or FALSE.")

  # Check if context is single value
  if(length(context)>1) stop("context must be a single value.")

  # Check if context is logical
  if(!is.logical(context)) stop("context must be either TRUE or FALSE.")

  # Check if interactive
  if(!interactive()) stop("handcode() can only be used in an interactive R session.")

  # Check if shiny is installed
  if (system.file(package="shiny") == "") stop("handcode() needs the package shiny installed. You can run install.packages(\"shiny\") to install shiny.")


  # Initialize -------------------------------------------------------------------

  # Initialize environment e
  e <<- new.env()


  # Add context to dataframe
  data <- data.frame(before = c("", data$texts[1:nrow(data)-1]), after = c(data$texts[2:nrow(data)], ""), data)

  # Add id variable to data
  data <- cbind(id = (1:nrow(data)), data)

  # if start == "all_empty", reorder data
  if(start == "all_empty"){
  data <- data[order(do.call(paste0,data.frame(data[,-c(1:4)], helper=""))==""), ]
  }


  # Set start to first empty row of data
  if(start %in% c("first_empty", "all_empty")){
    start <- min((1:nrow(data))[do.call(paste0,data.frame(data[,-c(1:4)], helper=""))==""])
  }

  # If randomize is TRUE, randomize order after start value
  if(randomize){
    data <- data[c(1:start-1, sample(start:nrow(data), nrow(data)-(start-1))),]
  }

  # List to store classifications and their categories
  classifications <- vector("list", length = ncol(data)-4)

  # Name list
  names(classifications) <- names(data)[-c(1:4)]

  # Fill with categories
  for (i in seq_along(classifications)) {
    classifications[[i]] <- levels(data[,i+4])
  }

  # Initialize container for classification
  container <- data.frame(kat1 = factor(rep("", nrow(data))), kat2 = factor(""), kat3 = factor(""))

  for (i in seq_along(classifications)){
    container[,i] <- data[,i+4]
    names(container)[i] <- names(classifications)[[i]]
    levels(container[,i]) <- c(classifications[[i]])
  }

  # Pass to app
  e$container <- container
  e$data_app <- data
  e$start_app <- start
  e$classifications <- classifications
  e$context_app <- context


  # Run App ----------------------------------------------------------------------

  shiny::runApp("inst/app")

  # Reorder output
  e$output <- e$output[order(e$output$id),]

  # Delete id
  e$output <- e$output[,-1]

  cat("Please cite: Isermann, Lukas. 2023. handcodeR: Text annotation app. R package version 0.1.0. https://github.com/liserman/handcodeR")

  return(e$output)
}
