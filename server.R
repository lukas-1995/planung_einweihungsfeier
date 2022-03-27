##################################################################################################################################
###################################### SERVER-FILE ###############################################################################
##################################################################################################################################


##################################
######## Load libraries ##########
##################################

library(tictoc)
library(scales)
library(grid)
library(ggplot2)
library(mise)
library(shiny)
library(shinyjs)
library(rdrop2)
library(stringi)
library(shinyWidgets)
library(shinydashboard)
library(shinyalert)
library(odbc)
library(RMySQL)

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL server",
                      Server   = "v2202104145393150207.luckysrv.de",
                      Database = "siedler_app",
                      UID      = "SA",
                      PWD      = "nzdpXjmJaeh^X*5k3u^G2quZ6H@N3P",
                      Port     = 1433)





#############################################
######## Initialise server funtion ##########
############################################# 

function(input,output, session){
  
  


  ##############################################
  ######## Initialise reactive values ##########
  ##############################################
  
  waits <- reactiveValues() # reactive to store all reactive variables
  waits$resetindicator<-0   # used to change button labels
  waits$temp <- 0
  waits$inp <- 0
  waits$m <- 0
  waits$n <- 1
  waits$t1 <- 0
  waits$t2 <- 0

  
  #############################
  ######## Load data ##########
  #############################
  
  user <- dbGetQuery(con, "SELECT * FROM Siedler")
  names(user) <- "Users"
  
  

}
