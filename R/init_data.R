#' init_data: Initialize dataframe for handcode().
#'
#' `init_data` initializes a dataframe to be passed to handcode(). The function takes a vector of texts and between one and three named vectors of categories as input. It returns a dataframe that can be used as data input in handcode().
#' @param texts A vector of character strings you want to classify by hand.
#' @param ... Between one and three named character vectors indicating different variables and categories that you whish to classify.
#'
# Export function
#' @export

init_data <- function(texts, ...) {

  # Initialize ...
  arg_list <- list(...)

# Checks -----------------------------------------------------------------------

  # Check if texts is vector of texts
  if(!is.character(texts) | !is.vector(texts)) stop("texts must be character vector indicating the texts you whish to classify.")

  # Check if items in arg_list are named vectors
  if(!all(vapply(arg_list, is.character, logical(1))) | !all(vapply(arg_list, is.vector, logical(1)))) {
    stop("All arguments in ... must be named character vectors.")
  }

  # Check that there are between 1 and 3 named character vectors given
  if(length(arg_list) < 1 | length(arg_list) > 3) {
    stop("There must be between 1 and 3 named character vectors given.")
  }


# Main function ----------------------------------------------------------------

  # Initialize empty dataframe
  output <- data.frame(matrix(factor(""), nrow = length(texts), ncol = length(arg_list)))

  # Name variables according to arg_list
  names(output) <- names(arg_list)

  # For every categorisation, set levels of factor
  for (i in seq_along(arg_list)){
    output[,i] <- factor("", levels = c("", "Not applicable", arg_list[[i]]))
  }

  # Paste texts object to dataframe
  output <- data.frame(texts, output)

  # Return dataframe
  return(output)

}
