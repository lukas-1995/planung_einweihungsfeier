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

# fluidPage(
#   surveyOutput(df = df,
#                survey_title = "Hello, World!",
#                survey_description = "Welcome! This is a demo survey showing off the {shinysurveys} package.")
# )

dashboardPage(skin="blue",
              dashboardHeader(title = "Einweihungsfeier Planungstool",
                              titleWidth = '100%',
                              dropdownMenuOutput("messageMenu")),
              dashboardSidebar(disable=T),
              dashboardBody(setBackgroundImage(
                src = "frankfurt.jpg", shinydashboard = T
              ),
              shinyjs::useShinyjs(),
              fluidRow(column(2),
                       column(8,
                              tabBox(width='100%',
                              tabPanel("Anmeldung",
                                       uiOutput("initiate_data"),
                                       uiOutput("check_data", align = "center"),
                                       uiOutput("change_data")),
                              tabPanel("Teilnehmerübersicht",
                                        fluidRow(column(12, align="center",
                                                        h3("Information über teilnehmende Personen"),
                                                        tableOutput("teilnehmer")
                               ))),
                               tabPanel("Schlafübersicht",
                                        fluidRow(h3("Informationen über Schlafplätze"), align = "center",
                                                 column(5, 
                                                        tableOutput("sleep_booking")),
                                                 column(7, align="center",
                                                        imageOutput("grundriss"))))),
                              # tabPanel("Anfahrtsbeschreibung",
                              #          fluidRow(column(12, align="center",
                              #                          h3("Daten"),
                              #                          tableOutput("statdata"))))
                              

              
                       column(2)))))