#' init_data: Initialize dataframe for handcode().
#'
#' `init_data` initializes a dataframe to be passed to handcode(). The function takes a vector of texts and a number of categories by which you want to classify the texts as input and returns a dataframe that can be used as data input in handcode().
#'
#' @param texts A vector of character strings you want to classify by hand.
#' @param categories A vector of character strings giving names for different categories you want to classify.
#'
# Export function
#' @export

init_data <- function(texts, categories) {

# Checks -----------------------------------------------------------------------

  # Check if texts is vector of texts
  if(!is.character(texts) | !is.vector(texts)) stop("texts must be character vector indicating the texts you whish to classify.")

  # Check if categories is vector of strings
  if(!is.character(categories) | !is.vector(categories)) stop("categories must be character vector indicating the names for the categories you whish to classify.")

# Main function ----------------------------------------------------------------

  # Make sure categories are lowercase and have no spaces
  categories <- tolower(sub(" ", "_", categories))

  # Initialize categories as dataframe
  catframe <- data.frame(matrix(nrow = length(texts), ncol = length(categories)))

  # Set variable names
  names(catframe) <- categories

  # Define output dataframe
  output <- data.frame(texts = texts, catframe, classification = "")

  return(output)


}
