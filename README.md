# Iowa House R Project


## How to Set up The Application
We want to install the following programs for this application to Work.

Download [R](https://cran.r-project.org/) (Download this before you download and set with up before you set up R Studio.

Download [R Studio](https://www.rstudio.com/products/rstudio/download/#download). Choose the version that is compatible with your 
operating system. 

Download [MySQL](https://dev.mysql.com/downloads/mysql/). Download this and you will be able to load the Database model into MySQL.
After MySQL has downloaded go into environment variables on your computer and edit the path and add the path directory of your MySQL Server. E.g. **C:\Program Files\MySQL\MySQL Server 8.0\bin**

To see the model of the MySQL database which is on the cloud download the house.mwb file and open it in the MySQL Workbench.


## Downloading R Packages 
Once in the application is loaded within R studio add the following lines of code to the R script (if not already present)
 

	install.packages("RMariaDB")
    install.packages("rpart.plot")
    install.packages("randomForest")
    install.packages('class')
    install.packages('Shiny')
	install.packages("devtools")
	
devtools::install_github("https://github.com/becca-bot/Shiny-Application-Cloud/tree/master/ProjectPackages")

Once these have been installed we now have to add the libraries to be used. Add the following code to the R script (if not already present).

	library(rpart.plot)
	library(class)
	library(randomForest)
	library(RMariaDB)
	Library(Shiny)
	Library(ProjectPackages)

To see what the functions in the application do type in ?treeModel or ?Server.

## KKN and Decision Models
Look to files KKNAttempt.R for my original KNN Model and see randomForest.R for the actual model.

 - The train.csv will need to be downloaded for these models to work.
   
 - Set the working directory to the path you have the file.  
 - **E.g. setwd("C:/Users/Admin/Documents")**
 - **train <- read.csv("C:/Users/Admin/Documents/train.csv")****
