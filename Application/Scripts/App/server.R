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
RESEARCHER_MODULE <- paste0(MODULES, "Researcher/")
PATIENT_MODULE    <- paste0(MODULES, "Patient/")
DOCTOR_MODULE     <- paste0(MODULES, "Doctor/")

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
            textInput("email", "El. pašto adresas:"),
            textInput("phone", "Tel. numeris:"),
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
    )
  )})

  ############### REGISTRACIJOS LANGO LOGIKA ###############
  observeEvent(input$signup_btn, {
    con <- dbConnect(RMySQL::MySQL(), dbname = "GeneticDataExchangeSystem",
                host = "127.0.0.1", user = "root",
                password = "daniele_Vostro000804",
                client.flag = CLIENT_MULTI_STATEMENTS + CLIENT_LOCAL_FILES)
    
    ERRORS <- c(ERRORS, validate_name(input$name))
    ERRORS <- c(ERRORS, validate_surname(input$surname))
    ERRORS <- c(ERRORS, validate_personal_code(input$personal_code))
    ERRORS <- c(ERRORS, validate_email(input$email))
    ERRORS <- c(ERRORS, validate_phone(input$phone))
    ERRORS <- c(ERRORS, validate_address(input$address))
    ERRORS <- c(ERRORS, validate_gender(input$gender))
    ERRORS <- c(ERRORS, validate_role(input$role))
    
    if (input$role == "Gydytojas") {
      ERRORS <- c(ERRORS, validate_licence(input$licence))
      ERRORS <-
        c(ERRORS, validate_text("institution_doc", input$institution_doc))
      ERRORS <-
        c(ERRORS, validate_text("specialization", input$specialization))
    } else if (input$role == "Tyrėjas") {
      ERRORS <-
        c(ERRORS, validate_text("institution_res", input$institution_res))
    }

    if (length(ERRORS) == 0) {
      new_user <- data.frame(
        Vardas = input$name,
        Pavarde = input$surname,
        Asmens_kodas = input$personal_code,
        El_pastas = input$email,
        Tel_numeris = input$phone,
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

        if (input$role == "Tyrėjas") {
          source(paste0(RESEARCHER_MODULE, "researcher_ui.R"), local = TRUE)
          output$main_ui <- renderUI({researcher_ui("researcher")})
          source(paste0(RESEARCHER_MODULE, "researcher_server.R"), local = TRUE)
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
