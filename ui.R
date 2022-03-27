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
library(RPostgreSQL)

Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
mise()


dashboardPage(skin="blue",
              dashboardHeader(title = "Einweihungsfeier Planungstool",
                              titleWidth = '100%',
                              dropdownMenuOutput("messageMenu")),
              dashboardSidebar(disable=T),
              dashboardBody(setBackgroundImage(
                src = "frankfurt.jpg", shinydashboard = T
              ),
              fluidRow(column(12,
                              tabBox(width='100%',
                                     tabPanel("Anmeldung",  
                                              fluidRow(column(12, align="center", 
                                                              h3("Histogram", align="center"),
                                                              plotOutput("siedler"),
                                                              helpText("Die dunklen Balken stellen das Histogram der tatsächlichen Würfe dar, während die hellblauen
                                          die theoretische Verteilung zeigen."),
                                                              h3("Statistiken", align="center"), #   tableOutput("view")),
                                                              tableOutput("summary")))
                                              
                                     ),
                                     tabPanel("Teilnehmerübersicht",
                                              fluidRow(column(12, align="center",
                                                              h3("Information über teilnehmen Personen"),
                                                              tableOutput("teilnehmer")
                                     ),
                                     tabPanel("Schlafübersicht",
                                              fluidRow(column(12, align="center",
                                                              h3("Daten"),
                                                              tableOutput("statdata"))))
                                     
                              )
                       ))
              ))