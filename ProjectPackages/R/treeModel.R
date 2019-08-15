#' Adding UserInput into random forest
#'
#' This function is adding user inputs into the random forest model
#' This is done by creating a list with all the user inputs making
#' them into a dataframe then feeding this dataframe into the model.
#' @param input
#' This is referring to the input that will be given to the shiny application. Keep as input.
#' @param column_name<-input$coloum_name
#' Set the column name that the user input will be going to.
#' @param inputList
#' add the name of the varible you set the list of user inputs to be.
#' @param fit
#' This is the name of tree fit that was created.
#' @param column~column
#' Should include column that data is dependant on then columns to be predicted.
#' @param data=
#' Name of the dataframe used should be after the =.
#' @param type=Vector
#' Class or Vector
#' @param prediction
#' This is the name of the variable you set your tree model prediction to be.
#' @return
#' The predicted house price based on user input
#' @examples
#'
#' fit <- rpart( column ~ column, data = dataframe_name, cp =.02)
#' inputList <- list(column_name<-input$Column_name,)
#' inputList <- as.data.frame(inputList) = predict(fit, inputList, type = "vector")
#' return(prediction)
#

  TreeModel<-function(input){
  fit <- rpart( column ~ column, data = dataframe_name, cp =.02)

  inputList <- list(column_name<-input$Column_name,)

  inputList <- as.data.frame(inputList)
  prediction = predict(fit, inputList, type = "vector")
  return(prediction)

}
