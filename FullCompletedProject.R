#install.packages("RMariaDB")
#install.packages("rpart.plot")
#install.packages("randomForest")
#install.packages("Class")


library(rpart.plot)
library(class)
library(randomForest)
library(RMariaDB)

options(scipen = 999)

#connect sql database
connect <- dbConnect(RMariaDB::MariaDB(),
                     user="root", password="root",
                     dbname="house_database", host="localhost")

dbListTables(connect)
#SQL query to get all data frome database
select_all_query <- "SELECT * FROM house_database.housedata";


#Turn out SQL query into a dataframe
cut_data <- dbFetch(dbSendQuery(connect, select_all_query))
on.exit(dbDisconnect(connect))
#Cutting the data. Removing uneeded columns
cut_data<-cut_data[,-c(1,12)]

#Renaming column
train<-cut_data


ui <- fluidPage(
  titlePanel("Lowa House Price Prediction"),
  
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      numericInput(inputId = "LotArea", label = "Your House Lot Area", value = 1000, min = 1000, max = 300000, step = 1),
      sliderInput(inputId = "OverallCond", label = "Overall Condition of Home", min = 0, value = 0, step = 1, max = 10),
      numericInput(inputId = "YearBuilt", label = "What Year Was Your House Built?", value = 1850, min = 1850, max = 2010, step = 1),
      numericInput(inputId = "X1stFlrSF", label = "Total Size of Your First Floor in Square Foot", value = 1000, min = 1000, step = 1),
      numericInput(inputId = "X2ndFlrSF", label = "Total Size of Your Second Floor in Square Foot", value = 1000, min = 1000, step = 1),
      sliderInput(inputId = "BsmtFullBath", label = "How Many Fully Completed Basement Bathrooms Do You Have?", min = 0, value = 0, step = 1, max = 5),
      sliderInput(inputId = "FullBath", label = "How Many Fully Completed Bathrooms Do You Have?", min = 0, value = 0, step = 1, max = 5),
      sliderInput(inputId = "TotRmsAbvGrd", label = "How Many of The Rooms In Your House Are Above Grade?", min = 0, value = 0, step = 1, max = 20),
      sliderInput(inputId ="GarageCars", label = "How Many Cars Can You Fit In Your Garage?", min = 0, value = 0, step = 1, max = 5)
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      textOutput(outputId = "Predicted")
      
    )
  )
)
#Model that puts our user input into the random forest
TreeModel<-function(input){
  
  
  #Fit for the random forest
  fit3 <- rpart(SalePrice ~ LotArea + OverallCond + YearBuilt + X1stFlrSF +
                  X2ndFlrSF + BsmtFullBath + FullBath + TotRmsAbvGrd + GarageCars, data = train, cp =.02)
  
  #List of the user inputs which are liked to columns in dataframe
  inputList <- list(LotArea<-input$LotArea,
                    OverallCond<-input$OverallCond,
                    YearBuilt<-input$YearBuilt,
                    X1stFlrSF<-input$X1stFlrSF,
                    X2ndFlrSF<-input$X2ndFlrSF,
                    BsmtFullBath<-input$BsmtFullBath,
                    FullBath<-input$FullBath,
                    TotRmsAbvGrd<-input$TotRmsAbvGrd,
                    GarageCars<-input$GarageCars)
  
  #Making inputlist into a dataframe
  inputList <- as.data.frame(inputList)
  #Random Forest Prediction
  prediction = predict(fit3, inputList, type = "vector")
  return(prediction)
  
}
#Taking in our inputs and outputting the prediction
server <- function(input, output) {
  
  output$Predicted <- renderText({
    paste("Your House Has a Predicted Value of $ ", as.character(round((TreeModel(input)),digits=2)))
  })
}

shinyApp(ui = ui, server = server)



