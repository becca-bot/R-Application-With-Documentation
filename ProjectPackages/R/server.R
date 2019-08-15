#' Displaying Prediction to Shiny Application
#'
#' This function is used to display the prediction to the shiny application.
#' @param input
#' Keep this as input, This will be using the input from the prediction model
#' @param output
#' Keep this as output. This is what will be displayed to the user.
#' @param round
#' Round the number that will be displayed
#' @param PredictionModel
#' Calling the function where the user inputs are stored. Add here what your fuction is called.
#' @param digits
#' Rounding numbers to the nearest digits after the decimal point.
#' @examples
#'(input, output)
#'(round((ForestModel(input)),digits=2))
#'
#'
  server <- function(input, output) {

  output$Predicted <- renderText({
    paste("Your House Has a Predicted Value of $ ", as.character(round((PredictionModel(input)),digits=-2)))
  })
}
