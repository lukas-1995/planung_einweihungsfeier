##################################################################################################################################
###################################### SERVER-FILE ###############################################################################
##################################################################################################################################
 
# What the indicator stands for:
# 
# 0 <- nothing has been decided yet
# 1 <- the input was confirmed
# 2 <- the input is changed


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
library(RMySQL)


options(mysql = list(
  "host" = "v2202104145393150207.luckysrv.de",
  "port" = 3306,
  "user" = "root",
  "password" = "nzdpXjmJaeh^X*5k3u^G2quZ6H@N3P"
))
databaseName <- "Einweihungsfeier"


saveData <- function(data, table) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  # Construct the update query by looping over the data fields
  # Submit the update query and disconnect
  # dbGetQuery(db, sprintf("DROP TABLE %s", table))
  # dbGetQuery(db, query)
  dbWriteTable(db, table, data, overwrite = T, row.names = F, col.names = T)
  dbDisconnect(db)
}


loadData <- function(table) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}




#############################################
######## Initialise server funtion ##########
############################################# 

function(input,output, session){
  
  waits <- reactiveValues() # reactive to store all reactive variables
  waits$user <- 0
  waits$resetindicator <- 0
  data_drink <- NULL
  data_name <- NULL
  data_schlafen <- NULL
  data_partner <- NULL
  num_schlafen <- NULL
  
  user <- loadData("user")
  
  
  query_modal <- modalDialog(
    title = "Anmelden",
    helpText("Bitte erstelle hier eine Nutzerkonto mit individuellem Anmeldenamen.",
             "um deine Daten später nochmals bearbeiten zu können.",
             "Falls du bereits ein Konto angelegt hast, wähle DEINEN bereits erstellten aus."),
    selectInput('user_choose','Teilnehmerkonto:', user$userid),
    textInput("new_user", "Neuen Teilnehmer anlegen und einloggen:"),
    easyClose = F,
    footer = tagList(
      actionButton("run", "Einloggen")
    )
  )
  
  showModal(query_modal)
  
  observeEvent(input$run,{
    removeModal()
    waits$user <- ifelse(input$new_user!="", input$new_user, input$user_choose)
    if (waits$user == input$new_user) {
      user <- data.frame(c(user$userid, waits$user))
      names(user) <- "userid"
      saveData(user, "user")
    }
    })
  
  
  output$messageMenu <- renderMenu({
    dropdownMenu(type = "messages", badgeStatus = NULL, headerText="",
                 messageItem(from = "Benutzer", 
                             message = tags$div(paste0("Der aktuelle Nutzer ist ", waits$user, "."),
                                                style = "display: inline-block; vertical-align: middle; color:grey")),
                 messageItem(
                   from = "Support",
                   message = tags$div("Falls ein Problem auftritt, wende",
                                      tags$br(),
                                      "dich an lukas.f@tutanota.com",
                                      style = "display: inline-block; vertical-align: middle; color:grey"),
                   icon = icon("life-ring"),
                   href = "mailto:lukas.f@tutanota.com"))
  })
  
  observe({
    if (waits$resetindicator==0) {
      shinyjs::show("initiate_data")
    } else {
      shinyjs::hide("initiate_data")
    }
  })

  observe({
    if (waits$resetindicator==1) {
      shinyjs::show("check_data")
    } else {
      shinyjs::hide("check_data")
    }
  })
  
  observe({
    if (waits$resetindicator==2) {
      shinyjs::show("change_data")
    } else {
      shinyjs::hide("change_data")
    }
  })
  
  
    
  output$initiate_data<-renderUI({
    tagList(
      textInput("name", h3("Mein Name ist:"),
              placeholder = "Bei Standardnamen bitte auch mit Nachnamenkürzel",
              width = "100%"),
    textInput("drink", h3("Ich habe einen speziellen Getränkewunsch:"),
              placeholder = "Bspw. heiße Schokolade, Ahoj-Brause, Champagner...",
              width = "100%"),
    textInput("partner", h3("Ich bringe noch eine weitere Person mit, welche nicht direkt eingeladen wurde:"),
              placeholder = "Bspw. Partner",
              width = "100%"),
    pickerInput(inputId = "schlafen", 
                label = h3("Möchtest du bei uns schlafen?"), 
                choices = c("Ja"="1",
                            "Nein"="2"),
                options = pickerOptions(
                  actionsBox = TRUE, 
                  size = 10,
                  title = "Wähle 'Ja' oder 'Nein' aus."), 
                multiple = F, width = "100%"),
    actionButton("confirm", "Bestätigen"))
  })
  
  data_overview <- function(){
    output$check_data <- renderUI({
      if (nchar(input$name_change)==0) {
        if (nchar(input$drink)==0) {
          data_drink <<- "Kein Getränkewunsch."
        } else {
          data_drink <<- input$drink
        }
        
        if (nchar(input$partner)==0) {
          data_partner <<- "Keine weitere Person kommt."
        } else {
          data_partner <<- input$partner
        }
        
        if (input$schlafen == 1) {
          data_schlafen <<- "Ja"
        } else {
          data_schlafen <<- "Nein"
        }
        
        data_name <<- input$name
        
        tagList(h2("Übersicht"),
                HTML(paste0("<h4>", "<b>","Name:","</b>", " ", data_name, "</h4>")),
                HTML(paste0("<h4>", "<b>","Getränkewünsche:","</b>", " ", data_drink, "</h4>")),
                HTML(paste0("<h4>", "<b>","Partner:","</b>", " ", data_partner, "</h4>")),
                HTML(paste0("<h4>", "<b>","Übernachtung:","</b>", " ", data_schlafen, "</h4>")),
                actionButton("change", "Auswahl ändern"))
        
      
    } else {
      if (nchar(input$drink_change)==0) {
        data_drink <<- "Kein Getränkewunsch."
      } else {
        data_drink <<- input$drink_change
      }
      
      if (nchar(input$partner_change)==0) {
        data_partner <<- "Keine weitere Person kommt."
      } else {
        data_partner <<- input$partner_change
      }
      
      if (input$schlafen_change == 1) {
        data_schlafen <<- "Ja"
      } else {
        data_schlafen <<- "Nein"
      }
      
      data_name <<- input$name_change
      
      tagList(h2("Übersicht"),
              HTML(paste0("<h4>", "<b>","Name:","</b>", " ", data_name, "</h4>")),
              HTML(paste0("<h4>", "<b>","Getränkewünsche:","</b>", " ", data_drink, "</h4>")),
              HTML(paste0("<h4>", "<b>","Partner:","</b>", " ", data_partner, "</h4>")),
              HTML(paste0("<h4>", "<b>","Übernachtung:","</b>", " ", data_schlafen, "</h4>")),
              actionButton("change", "Auswahl ändern"))
      
    

      }})
  }

  
  
  observeEvent(input$confirm,{
    if (nchar(input$name)==0) {
      show_alert(
        title = "Fehlender Name",
        text = "Du hast keinen Namen eingegeben. Bitte gib einen Namen ein.",
        type = "error"
      )
    }

    req(nchar(input$name)>0)
    
    
    if (nchar(input$schlafen)==0) {
      show_alert(
        title = "Fehlende Angabe zur Übernachtung",
        text = "Du hast noch keine Präferenz bezüglich deiner Übernachtung eingegeben. Bitte tue das, um Fortzufahren.",
        type = "error"
      )
    }
    
    req(input$schlafen>0)
    

    upload_data()
    
    waits$resetindicator <- 1
    
    data_overview()
    
  })
  
  observeEvent(input$confirm_change,{
    if (nchar(input$name)==0) {
      show_alert(
        title = "Fehlender Name",
        text = "Du hast keinen Namen eingegeben. Bitte gib einen Namen ein.",
        type = "error"
      )
    }
    
    req(nchar(input$name)>0)
    
    
    if (nchar(input$schlafen)==0) {
      show_alert(
        title = "Fehlende Angabe zur Übernachtung",
        text = "Du hast noch keine Präferenz bezüglich deiner Übernachtung eingegeben. Bitte tue das, um Fortzufahren.",
        type = "error"
      )
    }
    
    req(input$schlafen>0)
    
    
    upload_data()
    
    waits$resetindicator <- 1
    
    data_overview()
    
  })
  
  observeEvent(input$change, {
    waits$resetindicator <- 2
    
    if (data_schlafen == "Ja") {
      num_schlafen <- 1
    } else {
      num_schlafen <- 2
    }

    output$change_data<-renderUI({
      tagList(
        textInput("name_change", h3("Mein Name ist:"),
                  value = data_name,
                  width = "100%"),
        textInput("drink_change", h3("Ich habe einen speziellen Getränkewunsch:"),
                  value = data_drink,
                  width = "100%"),
        textInput("partner_change", h3("Ich bringe noch eine weitere Person mit, welche nicht direkt eingeladen wurde:"),
                  value = data_partner,
                  width = "100%"),
        pickerInput(inputId = "schlafen_change", 
                    label = h3("Möchtest du bei uns schlafen?"), 
                    choices = c("Ja"="1",
                                "Nein"="2"),
                    selected = num_schlafen,
                    options = pickerOptions(
                      actionsBox = TRUE, 
                      size = 10,
                      title = "Wähle 'Ja' oder 'Nein' aus."), 
                    multiple = F, width = "100%"),
        actionButton("confirm_change", "Bestätigen"))
    })
  })
  
  upload_data <- function(){
    daten <- loadData("daten")
    userid_new <- waits$user
    name_new <- input$name
    drink_new <- input$drink
    partner_new <- input$partner
    schlafen_new <- input$schlafen
    
    daten_new <- daten
    
    if (is.element(waits$user, daten_new$userid)) {
      line <- which(daten_new$userid == waits$user)
      daten_new <- daten_new[-line,]
    }
    
    daten_new[nrow(daten_new)+1, ] <- c(userid_new, drink_new, partner_new, schlafen_new, name_new)
    
    saveData(daten_new, "daten")
    
    show_alert(
      title = "Fragebogen ausgefüllt",
      text = paste0("Vielen Dank ", input$name, "! Du hast erfolgreich deine Daten übermittelt!"),
      type = "success"
    )
  }
  
  

}

