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
                                                      ))),
                              tabPanel("Informationen",
                                       fluidRow(column(12, 
                                                       h3("Veranstaltungsinformationen", align="center"),
                                                       h4("Herzlich Willkommen beim Planungsportal für unsere Einweihungsfeier!"),
                                                       h4("Wir würden gerne mit euch gemeinsam am", strong("Samstag, den 09. April 2022"), "ab", strong("18 Uhr"), "in der", strong("Treburer Str. 14"), "in",
                                                          strong("60528 Frankfurt"), "feiern. Bitte gebt hier im Tool eure Daten ein, damit wir genauer planen können."),
                                                       h4("Falls ihr bei uns 
                                                          übernachten wollt und falls Bedarf besteht, kann nur entweder eine Isomatte oder eine Bettdecke/ ein Schlafsack gewählt werden,
                                                          damit möglichst viele von euch ein bisschen was abbekommen. Falls ihr gerne übernachten würdet, aber nichts von beidem mitbringen 
                                                          könnt, wendet euch einfach an uns."),
                                                       h4("Aufgrund der aktuellen Lage mit hohen Infektionszahlen und positiven Coronafällen an jeder Ecke, findet die Veranstaltung", 
                                                          strong("2G+"), "statt. Wir werden keine Tests überprüfen, aber bitte testet euch möglichst kurz vor der Veranstaltung."),
                                                       h4("Solltet ihr aufgrund Corona abends nicht teilnehmen wollen, könnt ihr auch gerne schon nachmittags (gegen 15 Uhr) vorbeikommen und die 
                                                          Nachmittagssonne auf dem Balkon bei einem Stück Kuchen in kleinerer Runde genießen. Das könnt ihr natürlich auch zusätzlich zur Abendveranstaltung tun,
                                                          bitte beachtet jedoch, dass wir die Runde nachmittags etwas kleiner halten wollen. Gebt uns einfach kurz Bescheid, falls Interesse daran besteht."),
                                                       h4("Für Essen und Getränke ist gesorgt, ihr dürft aber natürlich gerne noch mehr mitbringen."),
                                                       br(),
                                                       h4("Wir hoffen auf zahlreiche Gäste, gutes Wetter und einen schönen Abend und würden uns freuen, möglichst viele von euch begrüßen zu können."),
                                                       br(),
                                                       h4("Nina und Lukas")
                                       )))),
                       column(2)))))