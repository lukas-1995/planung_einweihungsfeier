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

my_dbWriteTable <- function(conn, name, value, ..., field.types = NULL) {
  cl <- match.call(expand.dots = TRUE)
  cl[[1]] <- substitute(dbWriteTable)
  if (!is.null(field.types) && ".default" %in% names(field.types)) {
    othernames <- setdiff(colnames(value), names(field.types))
    cl$field.types <- c(
      field.types[ setdiff(names(field.types), ".default") ],
      setNames(rep(field.types[".default"], length(othernames)), othernames)
    )
  }
  eval.parent(cl)
}


saveData <- function(data, table) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  # Construct the update query by looping over the data fields
  # Submit the update query and disconnect
  # dbGetQuery(db, sprintf("DROP TABLE %s", table))
  # dbGetQuery(db, query)
  my_dbWriteTable(db, table, data, overwrite = T, row.names = F, col.names = T,
               field.types = c(.default="varchar(1111)"))
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
  waits$new <- 0
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
    if (is.element(input$new_user, user$userid)) {
      show_alert(
        title = "Fehlerhafter Nutzername",
        text = "Dieser Nutzer existiert bereits. Wähle bitte deinen Nutzer aus oder suche dir einen neuen Namen.",
        type = "error")
    }
    req(!is.element(input$new_user, user$userid))
    removeModal()
    waits$user <- ifelse(input$new_user!="", input$new_user, input$user_choose)
    if (waits$user == input$new_user) {
      user <- data.frame(c(user$userid, waits$user))
      names(user) <- "userid"
      saveData(user, "user")
    } else {
      daten <- loadData("daten")
      if (is.element(waits$user, daten$userid)) {
        waits$resetindicator <- 1
        data_overview()
      }
    }
    update_teilnehmerlist()
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
  
  observe({
    if (is.null(input$schlafen)){
      shinyjs::hide("box")
    }
  })

  observeEvent(input$schlafen,{
    if (!is.null(input$schlafen)) {
      if (input$schlafen == 1) {
        shinyjs::show("box")
      } else {
        shinyjs::hide("box")
      }
    }
  })

  observeEvent(input$schlafen_change,{
    if (!is.null(input$schlafen_change)) {
      if (input$schlafen_change == 1) {
        shinyjs::show("box_change")
      } else {
        shinyjs::hide("box_change")
      }
    }
  })
  

  output$initiate_data<-renderUI({
    
    sleep_data <- loadData("sleep_space")
    Encoding(sleep_data$bed) <- "UTF-8"
    
    sleep_choices <- sleep_data$bed[nchar(sleep_data$userid) == 0]
    sleep_choices <- sleep_choices[!grepl("(Vorläufig reserviert)", sleep_choices)]
    
    equip_data <- loadData("sleep_equip")
    
    equip_choices <- equip_data$equip[nchar(equip_data$userid) == 0]
    
    
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
    radioButtons(inputId = "schlafen", 
                 label = h3("Möchtest du bei uns schlafen?"), 
                 choices = c("Ja"="1",
                             "Nein"="2"),
                 selected = character(0),
                 width = "100%"),
    box(id = "box", width = 12,
      fluidRow(column(7,
                      pickerInput(inputId = "bed_choice",
                                  label = h3("Wo möchtest du schlafen?"),
                                  choices = sleep_choices,
                                  options = pickerOptions(
                                    actionsBox = TRUE,
                                    size = 10,
                                    title = "Wähle deinen Schlafplatz aus.",
                                    multiple = F
                                  ),
                                  width = "100%"),
                      pickerInput(inputId = "equip_choice",
                                  label = h3("Benötigst du Bettzeug? Bitte antworte hier nur, falls du wirklich nichts mitbringen kannst."),
                                  choices = equip_choices,
                                  options = pickerOptions(
                                    actionsBox = TRUE,
                                    size = 10,
                                    title = "Wähle dein Bettzeug aus.",
                                    multiple = F
                                  ),
                                  width = "100%")),
               column(5,
                      renderImage({
                        filename <- normalizePath(file.path('./www', paste('grundriss', input$n, '.png', sep='')))
                        
                        # Return a list containing the filename and alt text
                        list(src = filename,
                             width = "100%",
                             height = "100%",
                             alt = paste("Grundriss", input$n))
                        
                      }, deleteFile = FALSE)))
                    ),
    
           
    # pickerInput(inputId = "schlafen", 
    #             label = h3("Möchtest du bei uns schlafen?"), 
    #             choices = c("Ja"="1",
    #                         "Nein"="2"),
    #             options = pickerOptions(
    #               actionsBox = TRUE, 
    #               size = 10,
    #               title = "Wähle 'Ja' oder 'Nein' aus."), 
    #             multiple = F, width = "100%"),
    actionButton("confirm", "Bestätigen"))
  })
  
  data_overview <- function(){
    output$check_data <- renderUI({
      if (is.null(input$name_change) & waits$new == 1) {
        if (nchar(input$drink)==0) {
          data_drink <<- "Kein Getränkewunsch"
        } else {
          data_drink <<- input$drink
        }
        if (nchar(input$partner)==0) {
          data_partner <<- "Keine weitere Person kommt"
        } else {
          data_partner <<- input$partner
        }
        
        data_name <<- input$name
        if (input$schlafen == 1) {
          data_schlafen <<- "Ja"
          if (length(input$bed_choice)==0) {
            data_bed <<- "Kein Schlafplatz benötigt"
          } else {
            data_bed <<- input$bed_choice
          }
          if (length(equip_use$equip)==0) {
            data_equip <<- "Kein Bettzeug benötigt"
          } else {
            data_equip <<- input$equip_choice
          }

          data_show <- data.frame(matrix(c(paste0("<strong>","Name:","</strong>"),data_name,
                                           paste0("<strong>","Getränkewünsche:","</strong>"), data_drink,
                                           paste0("<strong>","Partner:","</strong>"), data_partner,
                                           paste0("<strong>","Übernachtung:","</strong>"), data_schlafen,
                                           paste0("<strong>","Übernachtungsplatz:","</strong>"), data_bed,
                                           paste0("<strong>","Bettzeug:","</strong>"), data_equip), nrow=6, byrow = T))
        } else {
          data_schlafen <<- "Nein"
          data_show <- data.frame(matrix(c(paste0("<strong>","Name:","</strong>"),data_name,
                                           paste0("<strong>","Getränkewünsche:","</strong>"), data_drink,
                                           paste0("<strong>","Partner:","</strong>"), data_partner,
                                           paste0("<strong>","Übernachtung:","</strong>"), data_schlafen), nrow=4, byrow = T))
        }
        
        names(data_show) <- NULL
        
        tagList(h2("Übersicht"),
                renderTable({data_show},sanitize.text.function=function(x){x}),
                actionButton("change", "Auswahl ändern"))      
        
      
    } else {
      daten <- loadData("daten")
      
      sleep_data <- loadData("sleep_space")
      Encoding(sleep_data$bed) <- "UTF-8"
      
      sleep_choices <- sleep_data$bed[nchar(sleep_data$userid) == 0]
      sleep_choices <- sleep_choices[!grepl("(Vorläufig reserviert)", sleep_choices)]
      
      equip_data <- loadData("sleep_equip")
      
      equip_choices <- equip_data$equip[nchar(equip_data$userid) == 0]
      
      
      line <- which(daten$userid == waits$user)
      daten_use <- daten[line,]
      
      sleep_use <- sleep_data[sleep_data$userid == waits$user,]
      equip_use <- equip_data[equip_data$userid == waits$user,]
      
      if (nchar(daten_use$drinkwish)==0) {
        data_drink <<- "Kein Getränkewunsch"
      } else {
        data_drink <<- daten_use$drinkwish
      }
      
      if (nchar(daten_use$partner)==0) {
        data_partner <<- "Keine weitere Person kommt"
      } else {
        data_partner <<- daten_use$partner
      }
      
      data_name <<- daten_use$name
      

      if (daten_use$sleep == 1) {
        data_schlafen <<- "Ja"
        if (length(sleep_use$bed)==0) {
          data_bed <<- "Kein Schlafplatz benötigt"
        } else {
          data_bed <<- sleep_use$bed
        }
        if (length(equip_use$equip)==0) {
          data_equip <<- "Kein Bettzeug benötigt"
        } else {
          data_equip <<- equip_use$equip
        }
        
        
        data_show <- data.frame(matrix(c(paste0("<strong>","Name:","</strong>"),data_name,
                                         paste0("<strong>","Getränkewünsche:","</strong>"), data_drink,
                                         paste0("<strong>","Partner:","</strong>"), data_partner,
                                         paste0("<strong>","Übernachtung:","</strong>"), data_schlafen,
                                         paste0("<strong>","Übernachtungsplatz:","</strong>"), data_bed,
                                         paste0("<strong>","Bettzeug:","</strong>"), data_equip), nrow=6, byrow = T))
      } else {
        data_schlafen <<- "Nein"
        data_show <- data.frame(matrix(c(paste0("<strong>","Name:","</strong>"),data_name,
                                         paste0("<strong>","Getränkewünsche:","</strong>"), data_drink,
                                         paste0("<strong>","Partner:","</strong>"), data_partner,
                                         paste0("<strong>","Übernachtung:","</strong>"), data_schlafen), nrow=4, byrow = T))
      }
      names(data_show) <- NULL
      
      tagList(h2("Übersicht"),
              renderTable({data_show},sanitize.text.function=function(x){x}),
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
    
    if (nchar(input$schlafen)==1 & length(input$bed_choice) == 0) {
      show_alert(
        title = "Fehlende Angabe zum Schlafplatz",
        text = "Du hast noch keine Präferenz bezüglich deines Schlafplatzes angegeben. Bitte tue das, um Fortzufahren.",
        type = "error"
      )
    }
    
    req(length(input$bed_choice) > 0 | nchar(input$schlafen)==2)
    
    if (nchar(input$schlafen)==1 & length(input$equip_choice) == 0) {
      show_alert(
        title = "Fehlende Angabe zum Bettzeug",
        text = "Du hast noch keine Präferenz bezüglich deines benötigten Bettzeugs angegeben. Bitte tue das, um Fortzufahren.",
        type = "error"
      )
    }
    
    req(length(input$equip_choice) > 0 | nchar(input$schlafen)==2)
    
    
    waits$new <- 1

    upload_data()
     
    waits$resetindicator <- 1
    
    data_overview()
    
    update_teilnehmerlist()
    
  })
  
  observeEvent(input$confirm_change,{
    
    if (is.null(input$name_change)) {
      if (nchar(input$name)==0) {
        show_alert(
          title = "Fehlender Name",
          text = "Du hast keinen Namen eingegeben. Bitte gib einen Namen ein.",
          type = "error"
        )
      }
      print("1")
      req(nchar(input$name_change)>0)
      
      
      if (nchar(input$schlafen_change)==0) {
        show_alert(
          title = "Fehlende Angabe zur Übernachtung",
          text = "Du hast noch keine Präferenz bezüglich deiner Übernachtung eingegeben. Bitte tue das, um Fortzufahren.",
          type = "error"
        )
      }
      print("2")
      req(input$schlafen_change>0)
      
      if (nchar(input$schlafen_change)==1 & length(input$bed_choice_change) == 0) {
        show_alert(
          title = "Fehlende Angabe zum Schlafplatz",
          text = "Du hast noch keine Präferenz bezüglich deines Schlafplatzes angegeben. Bitte tue das, um Fortzufahren.",
          type = "error"
        )
      }
      
      req(length(input$bed_choice_change) > 0 | nchar(input$schlafen_change)==2)
      print("3")
      if (nchar(input$schlafen_change)==1 & length(input$equip_choice_change) == 0) {
        show_alert(
          title = "Fehlende Angabe zum Bettzeug",
          text = "Du hast noch keine Präferenz bezüglich deines benötigten Bettzeugs angegeben. Bitte tue das, um Fortzufahren.",
          type = "error"
        )
      }
      
      req(length(input$equip_choice_change) > 0 | nchar(input$schlafen_change)==2)
      print("4")
      
    } else {
      if (nchar(input$name_change)==0) {
        show_alert(
          title = "Fehlender Name",
          text = "Du hast keinen Namen eingegeben. Bitte gib einen Namen ein.",
          type = "error"
        )
      }
      
      req(nchar(input$name_change)>0)
      print("5")
      
      if (nchar(input$schlafen_change)==0) {
        show_alert(
          title = "Fehlende Angabe zur Übernachtung",
          text = "Du hast noch keine Präferenz bezüglich deiner Übernachtung eingegeben. Bitte tue das, um Fortzufahren.",
          type = "error"
        )
      }
      
      req(input$schlafen_change>0)
      print("6")

      if (nchar(input$schlafen_change)==1 & length(input$bed_choice_change) == 0) {
        show_alert(
          title = "Fehlende Angabe zum Schlafplatz",
          text = "Du hast noch keine Präferenz bezüglich deines Schlafplatzes angegeben. Bitte tue das, um Fortzufahren.",
          type = "error"
        )
      }
      
      req(length(input$bed_choice_change) > 0 | nchar(input$schlafen_change)==2)
      print("7")
      if (nchar(input$schlafen_change)==1 & length(input$equip_choice_change) == 0) {
        show_alert(
          title = "Fehlende Angabe zum Bettzeug",
          text = "Du hast noch keine Präferenz bezüglich deines benötigten Bettzeugs angegeben. Bitte tue das, um Fortzufahren.",
          type = "error"
        )
      }
      
      req(length(input$equip_choice_change) > 0 | nchar(input$schlafen_change)==2)
      print("8")
    }

    upload_data()
    print("9")
    waits$resetindicator <- 1
    
    data_overview()
    print("10")
    update_teilnehmerlist()
    
  })
  
  observeEvent(input$change, {
    waits$resetindicator <- 2
    
    if (data_schlafen == "Ja") {
      num_schlafen <- 1
    } else {
      num_schlafen <- 2
    }

    output$change_data<-renderUI({
      
      sleep_data <- loadData("sleep_space")
      Encoding(sleep_data$bed) <- "UTF-8"
      
      sleep_choices <- sleep_data$bed[nchar(sleep_data$userid) == 0]
      sleep_choices <- sleep_choices[!grepl("(Vorläufig reserviert)", sleep_choices)]
      
      equip_data <- loadData("sleep_equip")
      
      equip_choices <- equip_data$equip[nchar(equip_data$userid) == 0]
      
      
      
      tagList(
        textInput("name_change", h3("Mein Name ist:"),
                  value = data_name,
                  width = "100%"),
        if (data_drink == "Kein Getränkewunsch") {
          textInput("drink_change", h3("Ich habe einen speziellen Getränkewunsch:"),
                    placeholder = "Bspw. heiße Schokolade, Ahoj-Brause, Champagner...",
                    width = "100%")
        } else {
          textInput("drink_change", h3("Ich habe einen speziellen Getränkewunsch:"),
                    value = data_drink,
                    width = "100%")
        },
        if (data_partner == "Keine weitere Person kommt") {
          textInput("partner_change", h3("Ich bringe noch eine weitere Person mit, welche nicht direkt eingeladen wurde:"),
                    placeholder = "Bspw. Partner",
                    width = "100%")
        } else {
          textInput("partner_change", h3("Ich bringe noch eine weitere Person mit, welche nicht direkt eingeladen wurde:"),
                    value = data_partner,
                    width = "100%")
        },
        radioButtons(inputId = "schlafen_change", 
                     label = h3("Möchtest du bei uns schlafen?"), 
                     choices = c("Ja"="1",
                                 "Nein"="2"),
                     selected = num_schlafen,
                     width = "100%"),
        box(id = "box_change", width = 12,
            fluidRow(column(7,
                            pickerInput(inputId = "bed_choice_change",
                                        label = h3("Wo möchtest du schlafen?"),
                                        choices = c(sleep_choices, data_bed),
                                        selected = data_bed,
                                        options = pickerOptions(
                                          actionsBox = TRUE,
                                          size = 10,
                                          title = "Wähle deinen Schlafplatz aus.",
                                          multiple = F
                                        ),
                                        width = "100%"),
                            pickerInput(inputId = "equip_choice_change",
                                        label = h3("Benötigst du Bettzeug? Bitte antworte hier nur, falls du wirklich nichts mitbringen kannst."),
                                        choices = c(equip_choices, data_equip),
                                        selected = data_equip,
                                        options = pickerOptions(
                                          actionsBox = TRUE,
                                          size = 10,
                                          title = "Wähle dein Bettzeug aus.",
                                          multiple = F
                                        ),
                                        width = "100%")),
                     column(5,
                            renderImage({
                              filename <- normalizePath(file.path('./www', paste('grundriss', input$n, '.png', sep='')))
                              
                              # Return a list containing the filename and alt text
                              list(src = filename,
                                   width = "100%",
                                   height = "100%",
                                   alt = paste("Grundriss", input$n))
                              
                            }, deleteFile = FALSE)))
        ),
        
        
        
        # pickerInput(inputId = "schlafen_change", 
        #             label = h3("Möchtest du bei uns schlafen?"), 
        #             choices = c("Ja"="1",
        #                         "Nein"="2"),
        #             selected = num_schlafen,
        #             options = pickerOptions(
        #               actionsBox = TRUE, 
        #               size = 10,
        #               title = "Wähle 'Ja' oder 'Nein' aus."), 
        #             multiple = F, width = "100%"),
        actionButton("confirm_change", "Bestätigen"))
    })
  })
  
  upload_data <- function(){
    daten <- loadData("daten")
    
    sleep_data <- loadData("sleep_space")
    Encoding(sleep_data$bed) <- "UTF-8"
    
    equip_data <- loadData("sleep_equip")
    
    print("11")
    
    if (is.null(input$name_change)) {
      userid_new <- waits$user
      name_new <- input$name
      drink_new <- input$drink
      partner_new <- input$partner
      schlafen_new <- input$schlafen
      bed_choice_new <- input$bed_choice
      equip_choice_new <- input$equip_choice
    } else {
      userid_new <- waits$user
      name_new <- input$name_change
      drink_new <- input$drink_change
      partner_new <- input$partner_change
      schlafen_new <- input$schlafen_change
      bed_choice_new <- input$bed_choice_change
      equip_choice_new <- input$equip_choice_change
      
    }
    print("12")
    
    if (schlafen_new == "2") {
      bed_choice_new <- character()
      equip_choice_new <- character()
    }

    if (is.element(waits$user, sleep_data$userid)) {
      sleep_data$userid[sleep_data$userid == waits$user] <- ""
    }
    
    if (is.element(waits$user, equip_data$userid)) {
      equip_data$userid[equip_data$userid == waits$user] <- ""
    }
    print("15")
    
        
    print(sleep_data$userid)
    print(sleep_data$bed)
    print(bed_choice_new)
    print(sleep_data$userid[sleep_data$bed == bed_choice_new])
    if (length(sleep_data$userid[sleep_data$bed == bed_choice_new])>0) {
      show_alert(
        title = "Schlafplatz wurde schon gebucht",
        text = "Der ausgewählte Schlafplatz wurde gerade von einer anderen Person gebucht. Bitte wähle einen neuen Schlafplatz aus.",
        type = "error")
    }
    req(length(sleep_data$userid[sleep_data$bed == bed_choice_new]) == 0)
    print("13")
    
    if (length(equip_data$userid[equip_data$equip == equip_choice_new])>0) {
      show_alert(
        title = "Dieses Bettzeug wurde schon gebucht",
        text = "Das ausgewählte Bettzeug wurde gerade von einer anderen Person gebucht. Bitte wähle anderes Bettzeug aus.",
        type = "error")
    }
    req(length(equip_data$userid[equip_data$equip == equip_choice_new]) == 0)
    print("14")
    
    
    sleep_data$userid[sleep_data$bed == bed_choice_new] <- waits$user
    equip_data$userid[equip_data$equip == equip_choice_new] <- waits$user
    
    saveData(sleep_data, "sleep_space")
    saveData(equip_data, "sleep_equip")
    
    
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
  
  update_teilnehmerlist <- function(){
    output$teilnehmer <- renderTable({
      daten <- loadData("daten")
      data.frame("Name"=daten$name, 
                 "Getränkewunsch"=daten$drinkwish,
                 check.names=FALSE)
      
    })
    
  }
  
  output$grundriss <- renderImage({
    filename <- normalizePath(file.path('./www', paste('grundriss', input$n, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         width = "100%",
         height = "100%",
         alt = paste("Grundriss", input$n))
    
  }, deleteFile = FALSE)
  
  output$sleep_booking <- renderTable({
    sleep_space <- loadData("sleep_space")
    Encoding(sleep_space$bed) <- "UTF-8"
    data.frame("Schlafplatz"=sleep_space$bed, 
               "Person"=sleep_space$userid,
               check.names=FALSE)
    
  })
  

}


