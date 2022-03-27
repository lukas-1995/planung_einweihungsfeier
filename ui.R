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
                              tabBox(width='1000%',
                              tabPanel("Anmeldung",
                                       uiOutput("initiate_data"),
                                       uiOutput("check_data"),
                                       uiOutput("change_data")
                                     # tabPanel("Teilnehmerübersicht",
                                     #          fluidRow(column(12, align="center",
                                     #                          h3("Information über teilnehmen Personen"),
                                     #                          tableOutput("teilnehmer")
                                     # ),
                                     # tabPanel("Schlafübersicht",
                                     #          fluidRow(column(12, align="center",
                                     #                          h3("Daten"),
                                     #                          tableOutput("statdata"))))

              ))),
                       column(2))))