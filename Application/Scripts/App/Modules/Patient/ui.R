library(pacman)
p_load(shiny, shinythemes, shinydashboard, shinycustomloader, reactable, shinyjs)

patient_ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    includeCSS("Application/Scripts/App/styles.css"),
    title =
    div(
    class = "module-titles",
    "Pseudonimizuota genetinių duomenų keitimosi sistema - Pacientų modulis",
    div(
      p("Sveiki prisijungę, ", 
        class = "module-subtitles",
        textOutput(ns("pat_user"), inline = TRUE), "!")
    )),
    theme = shinytheme("cosmo"),
    tabPanel(
      "Paskyra",
      sidebarLayout(
        sidebarPanel(
          width = 6,
          fluidRow(
            column(
              width = 5,
              p("Asmeniniai duomenys",
                style = "font-weight:bold; font-size:17px; margin-left:0px"),
              textInput(
                inputId = ns("pat_name"),
                label = "Vardas:",
                value = "",
                width = "100%"
              ),
              textInput(
                inputId = ns("pat_surname"),
                label = "Pavardė:",
                value = "",
                width = "100%"
              ),
              textInput(
                inputId = ns("pat_code"),
                label = "Asmens kodas:",
                value = "",
                width = "100%"
              ),
              selectInput(
                inputId = ns("pat_gender"),
                label = "Lytis:",
                choices = list("Moteris" = "Moteris", "Vyras" = "Vyras",
                               "Kita" = "Kita"),
                selected = "",
                width = "100%"
              ),
              textInput(
                inputId = ns("pat_address"),
                label = "Gyvenamoji vieta (adresas):",
                value = "",
                width = "100%"
              ),
              br(),
              textInput(
                inputId = ns("pat_phone"),
                label = "Telefono numeris:",
                value = "",
                width = "100%"
              ),
              textInput(
                inputId = ns("pat_email"),
                label = "Elektroninis paštas:",
                value = "",
                width = "100%"
              ),
              br(), br(),
              actionButton("edit_profile_btn", "Redaguoti duomenis",
                           class = "btn-primary btn-block"),
              textOutput("profile_edit_message")
            ),
            column(width = 1),
            column(
              width = 5,
              p("Slaptažodžio keitimas",
                style = "font-weight:bold; font-size:17px; margin-left:0px"),
              textInput(
                inputId = ns("pat_name"),
                label = "Dabartinis slaptažodis:",
                value = "",
                width = "100%"
              ),
              textInput(
                inputId = ns("pat_surname"),
                label = "Naujas slaptažodis:",
                value = "",
                width = "100%"
              ),
              br(), br(),
              actionButton("change_pass_btn", "Pakeisti slaptažodį",
                           class = "btn-primary btn-block"),
              textOutput("password_change_message")
            )
          )
        ),
        mainPanel(
          width = 8,
          # tableOutput(ns("table0")),
          # shinydashboard::box(
          #   width = 12,
          #   withLoader(plotOutput(ns("plot7"), width = "40%", height = "250px"),
          #             type = "html", loader = "dnaspin")
          # ),
          # DT::dataTableOutput(ns("samples"))
        )
      )
    ),
    tabPanel(
      "Genetiniai duomenys",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          fileInput(
            inputId = ns("bigbed"),
            label = "Įkelkite BED formato failą (-us)*:",
            multiple = TRUE,
            buttonLabel = "Ieškoti failo",
            placeholder = "Failas nepasirinktas"
          ),
          selectInput(
            inputId = ns("organism"),
            label = "Nurodykite, iš kokio organizmo išgauti mėginiai:",
            choices = c(
              "Mus musculus", "Homo sapiens", "Rattus norvegicus", "Danio rerio",
              "Bos taurus", "Drosophila melanogaster", "Gallus gallus",
              "Macaca mulatta", "Pan troglodytes", "Sus scrofa", "Nenurodyta"
            ),
            selected = "Nenurodyta"
          ),
          fileInput(
            inputId = ns("pwm"),
            label = "Įkelkite transkripcijos faktoriaus PSM matricą:",
            multiple = FALSE,
            buttonLabel = "Ieškoti failo",
            placeholder = "Failas nepasirinktas"
          ),
          textInput(
            inputId = ns("tf_options"), 
            label = "Įveskite transkripcijos faktoriaus pavadinimą:",
            value = "Nenurodyta", 
            width = "100%",
            placeholder = "Transkripcijos faktoriaus pavadinimas..."
          ),
          p("* - privalomas įvesties laukas", class = "info_text"),
          br(),
          br(),
          actionButton(
            inputId = ns("sample_data"),
            label = "Pavyzdiniai duomenys",
            icon = icon("th"), 
            onclick = "window.open('https://vult-my.sharepoint.com/:f:/g/personal/daniele_stasiunaite_mif_stud_vu_lt/EtEGQ8POkapLhPv6eHvl48cB-jmes81M0JPW8PVWTz2QgA?e=wjSSKJ', '_blank')"
          )
        ),
        mainPanel(
          width = 8,
          tableOutput(ns("table0")),
          shinydashboard::box(
            width = 12,
            withLoader(plotOutput(ns("plot7"), width = "40%", height = "250px"),
                      type = "html", loader = "dnaspin")
          ),
          DT::dataTableOutput(ns("samples"))
        )
      )
    )
  )
}