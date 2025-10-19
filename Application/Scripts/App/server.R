# nolint start
library(pacman)

# Library loading:
p_load(shiny, data.table, rtracklayer, ggplot2, ggthemes, plyranges, ggpubr,
       BRGenomics, reshape2, plotly, heatmaply, dplyr, gplots, genomation,
       Biostrings, scales, GenomicRanges, DT, shinythemes, shinycustomloader,
       ggseqlogo, ChIPseeker, tools, reactable, annotables, enrichplot,
       clusterProfiler, shinyalert, rjson, ensembldb, deepredeff, rBLAST)

library(shinyjs)

p_load(DBI, RMySQL, shinyFeedback)

# Declaration of options:
options(scipen = 100)
options(shiny.maxRequestSize = 300 * 1024 ^ 2)

# Path declarations:
PROJECT           <- "./Application/"
SCRIPTS           <- paste0(PROJECT, "Scripts/")
APP               <- paste0(SCRIPTS, "App/")
FUNCTIONS         <- paste0(SCRIPTS, "functions.R")
VALIDATIONS       <- paste0(SCRIPTS, "validations.R")
INPUT             <- paste0(PROJECT, "Input/")
DATABASES         <- paste0(PROJECT, "Databases/")
MODULES           <- paste0(APP, "Modules/")
PATIENT_MODULE    <- paste0(MODULES, "Patient/")
DOCTOR_MODULE     <- paste0(MODULES, "Doctor/")
RESEARCHER_MODULE <- paste0(MODULES, "Researcher/")

print(RESEARCHER_MODULE)

source(FUNCTIONS)
source(VALIDATIONS)

ERRORS <- c()

server <- function(input, output, session) {
  output$main_ui <- renderUI({
    navbarPage(
    "Pseudonimizuota genetinių duomenų keitimosi sistema",
    theme = shinytheme("cosmo"),
    id = "main_nav",
    includeCSS(paste0(APP, "styles.css")),
    tabPanel(
      "Titulinis"
    ),
    tabPanel(
      "Registracija",
      useShinyFeedback(),
      fluidRow(
        column(4, offset = 4,
          wellPanel(
            p(class = "titles", "Sistemos naudotojo paskyros sukūrimas"),
            textInput("name", "Vardas:"),
            textInput("surname", "Pavardė:"),
            textInput("personal_code", "Asmens kodas:"),
            textInput("signup_email", "El. pašto adresas:"),
            textInput("signup_phone", "Tel. numeris:"),
            textInput("address", "Adresas:"),
            selectInput("gender", "Lytis:",
                        list("Moteris" = "Moteris", "Vyras" = "Vyras",
                            "Kita" = "Kita", "Nenurodyta" = "Nenurodyta"),
                        selected = "Undefined"),
            selectInput("role", "Kategorija:",
                        list("Pacientas" = "Pacientas", "Gydytojas" = "Gydytojas",
                            "Tyrėjas" = "Tyrėjas", "Nenurodyta" = "Nenurodyta"),
                        selected = "Undefined", multiple = FALSE),
            uiOutput("extraFields"),
            br(), br(),
            passwordInput("password", "Slaptažodis:"),
            
            actionButton("signup_btn", "Registruotis",
                        class = "btn-primary btn-block"),
            br(), br(),
            textOutput("signup_message")
          )
        )
      )
    ),
    tabPanel(
      "Prisijungimas",
      useShinyFeedback(),
      fluidRow(
        column(4, offset = 4,
          wellPanel(
            p(class = "titles", "Prisijungimas prie sistemos"),
            textInput("login_email", "El. pašto adresas:"),
            textInput("login_phone", "Tel. numeris:"),
            passwordInput("password", "Slaptažodis:"),
            actionButton("login_btn", "Prisijungti",
                        class = "btn-primary btn-block"),
            br(), br(),
            textOutput("login_message")
          )
        )
      )
    )
  )})

  ######################### REGISTRACIJOS LANGO LOGIKA #########################
  observeEvent(input$signup_btn, {
    con <-
      dbConnect(RMySQL::MySQL(), dbname = "GeneticDataExchangeSystem",
                host = "127.0.0.1", user = "root",
                password = "daniele_Vostro000804",
                client.flag = CLIENT_MULTI_STATEMENTS + CLIENT_LOCAL_FILES)
    
    ERRORS <- c(ERRORS, validate_name(input$name))
    ERRORS <- c(ERRORS, validate_surname(input$surname))
    ERRORS <- c(ERRORS, validate_personal_code(input$personal_code))
    ERRORS <- c(ERRORS, validate_email("signup_email", input$signup_email))
    ERRORS <- c(ERRORS, validate_phone("signup_phone", input$signup_phone))
    ERRORS <- c(ERRORS, validate_address(input$address))
    ERRORS <- c(ERRORS, validate_gender(input$gender))
    ERRORS <- c(ERRORS, validate_role(input$role))
    
    if (input$role == "Gydytojas") {
      ERRORS <- c(ERRORS, validate_licence(input$licence))
      ERRORS <-
        c(ERRORS, validate_text("institution_doc", input$institution_doc))
      ERRORS <-
        c(ERRORS, validate_text("specialization", input$specialization))
    }
    else if (input$role == "Tyrėjas") {
      ERRORS <-
        c(ERRORS, validate_text("institution_res", input$institution_res))
    }

    if (length(ERRORS) == 0) {
      new_user <- data.frame(
        Vardas = input$name,
        Pavarde = input$surname,
        Asmens_kodas = input$personal_code,
        El_pastas = input$signup_email,
        Tel_numeris = gsub(" ", "", input$signup_phone),
        Adresas = input$address,
        Lytis = input$gender,
        Kategorija = input$role,
        Registracijos_data = Sys.time(),
        stringsAsFactors = FALSE
      )

      tryCatch({
        dbWriteTable(con, "ASMUO", new_user, append = TRUE, row.names = FALSE)
        showNotification("Naudotojas sėkmingai sukurtas!", type = "message",
                         closeButton = TRUE, duration = 2)

        if (input$role == "Gydytojas") {
          source(paste0(DOCTOR_MODULE, "ui.R"), local = TRUE)
          output$main_ui <- renderUI({doctor_ui("doctor")})
          source(paste0(DOCTOR_MODULE, "server.R"), local = TRUE)
          callModule(doctor_server, "doctor")
        }
        else if (input$role == "Tyrėjas") {
          source(paste0(RESEARCHER_MODULE, "ui.R"), local = TRUE)
          output$main_ui <- renderUI({researcher_ui("researcher")})
          source(paste0(RESEARCHER_MODULE, "server.R"), local = TRUE)
          callModule(researcher_server, "researcher")
        }
      }, error = function(e) {
        print(e)
      }, finally = {
        dbDisconnect(con)
      })
    } else {
      showNotification("Klaida registruojant naudotoją!", type = "error",
                        closeButton = TRUE, duration = 2)
    }
  })

  ######################### PRISIJUNGIMO LANGO LOGIKA ##########################
  observeEvent(input$login_btn, {
    con <-
      dbConnect(RMySQL::MySQL(), dbname = "GeneticDataExchangeSystem",
                host = "127.0.0.1", user = "root",
                password = "daniele_Vostro000804",
                client.flag = CLIENT_MULTI_STATEMENTS + CLIENT_LOCAL_FILES)

    if (input$login_phone == "" && input$login_email != "") {
        ERRORS <- c(ERRORS, validate_email("login_email", input$login_email))
      }
      if (input$login_phone != "" && input$login_email == "") {
        ERRORS <- c(ERRORS, validate_phone("login_phone", input$login_phone))
      }
      if (input$login_phone == "" && input$login_email == "") {
        ERRORS <- c(ERRORS, phone_email_exists("login_phone", input$login_phone))
        ERRORS <- c(ERRORS, phone_email_exists("login_email", input$login_email))
      }

      else {
        ERRORS <- c(ERRORS, validate_email("login_email", input$login_email))
        ERRORS <- c(ERRORS, validate_phone("login_phone", input$login_phone))
      }
    
    
    if (length(ERRORS) == 0) {
      tryCatch({
        user <-
          dbGetQuery(con,
                     paste0("SELECT Vardas FROM ASMUO WHERE Tel_numeris = '",
                     input$login_phone, "';")
          )
      }, error = function(e) {
        print(e)
        
      }, finally = {
        dbDisconnect(con)
      })
    } else {
      showNotification("Klaida bandant prisijungti!", type = "error",
                        closeButton = TRUE, duration = 2)
    }
    
    
    
  })

  output$extraFields <- renderUI({
    if (input$role == "Gydytojas") {
      wellPanel(
        p(class = "titles-2", "Papildoma informacija apie gydytoją"),
        textInput("licence", "Licencijos numeris:"),
        textInput("institution_doc", "Institucija:"),
        textInput("specialization", "Specializacija:")
      )
    }
    else if (input$role == "Tyrėjas") {
      wellPanel(
        p(class = "titles-2", "Papildoma informacija apie tyrėją"),
        textInput("institution_res", "Institucija:")
      )
    }
  })
}
# nolint end
