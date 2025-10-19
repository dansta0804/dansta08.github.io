library(pacman)
p_load(shiny, shinythemes, shinydashboard, shinycustomloader, reactable)


doctor_ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    "Pseudonimizuota genetinių duomenų keitimosi sistema",
    theme = shinytheme("cosmo"),
    includeCSS("Application/Scripts/App/styles.css"),
    tabPanel(
      "Medicininės kortelės",
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
    ),
    tabPanel(
      "Užklausos sukūrimas",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          p("Pasirinkite vieną arba kelis pateiktus mėginius, kurių duomenų
            kokybę vertinsite:", style = "font-weight:bold; font-size:17px;
            margin-left:0px"),
          DT::dataTableOutput(ns("samples2"))
        ),
        mainPanel(
          width = 8,
          tabsetPanel(
            tabPanel(
              "Pikų skaičius mėginiuose", 
              p("Pateiktoje stulpelinėje diagramoje pavaizduota, kiek yra pikų
                kiekviename pateiktame BED formato faile:"),
              shinydashboard::box(
                width = 12, 
                withLoader(
                  plotOutput(ns("plot1")), type = "html", loader = "dnaspin"
                ),
                downloadButton("download1", "Atsisiųsti vaizdą")
              )
            )
          )
        )
      )
    )
  )
}