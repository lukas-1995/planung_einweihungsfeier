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
                                                 column(7,
                                                        box(imageOutput("grundriss", height = "70%", width = "70%"), width = "300px")))),
                              tabPanel("Anfahrtsbeschreibung",
                                       fluidRow(column(12, 
                                                       h3("Anfahrtsbeschreibung", align="center"),
                                                       h4("Per Zug:"),
                                                       p("Die Anreise mit dem Zug erfolgt über den Hauptbahnhof Frankfurt.
                                                         Von hier aus gelangt man nun am schnellsten mit der Straßenbahn zu uns. Die Straßenbahn 20, bzw. 21
                                                         Richtung Stadion fährt direkt vor dem Bahnhof ab und kann bis zur Haltstelle 'Niederräder Landstraße'
                                                         genutzt werden, ebenso wie die Straßenbahn 12. Diese fährt allerdings in der Münchener Straße ab, eine 
                                                         Seitenstaße des Hauptbahnhofes."),
                                                       br(),
                                                       p("Von der Haltestelle Niederräder Landstraße aus sind es dann noch wenige Meter zu Fuß, wie auf dem folgenden
                                                         Bild zu sehen:"),
                                                       fluidRow(align = "center",
                                                       tags$iframe(
                                                         seamless = "seamless",
                                                         src = "https://www.google.com/maps/d/embed?mid=1Fb6_gVA0ObyyT6s0-T5A8IfXywZydtHf&ehbc=2E312F&z=16",
                                                         height = 480, width = "80%")),
                                                       br(),
                                                       h4("Per Auto:"),
                                                       p("Die Anreise mit dem Auto erfolgt am einfachsten über die Anschlusstellen Niederrad an der A5,
                                                         bzw. Frankfurt-Süd an der A3. Es steht eine begrenzte Anzahl an Parkplätzen vor dem Haus zur Verfügung, die 
                                                         Nachbarstraßen können allerdings auch abgesucht werden, es gibt keine Anwohnerparkzone und keine
                                                         Notwendigkeit für einen Parkschein oder ähnliches. Falls nichts zu finden ist, so sollte man an der Rennbahn
                                                         auf den unten zu sehenden, markierten Plätzen suchen. Hier findet sich eigentlich immer ein Parkplatz"),
                                                       fluidRow(align = "center",
                                                      tags$iframe(
                                                         seamless = "seamless",
                                                         src = "https://www.google.com/maps/d/embed?mid=12Y7X60cX0a0L288H0pAv6GKKKa4RlM1x&hl=de&ehbc=2E312F&z=16",
                                                         height = 480, width = "80%"))
                                                      )))),
                       column(2)))))