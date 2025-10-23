library(pacman)
p_load(shiny, shinythemes, shinydashboard, shinycustomloader, reactable)

researcher_ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    includeCSS("Application/Scripts/App/styles.css"),
    title =
    div(
    class = "module-titles",
    "Pseudonimizuota genetinių duomenų keitimosi sistema - Tyrėjų modulis",
    div(
      p("Sveiki prisijungę, ", 
        class = "module-subtitles",
        textOutput(ns("res_user"), inline = TRUE), "!")
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
                inputId = ns("res_name"),
                label = "Vardas:",
                value = "",
                width = "100%"
              ),
              textInput(
                inputId = ns("res_surname"),
                label = "Pavardė:",
                value = "",
                width = "100%"
              ),
              textInput(
                inputId = ns("res_code"),
                label = "Asmens kodas:",
                value = "",
                width = "100%"
              ),
              selectInput(
                inputId = ns("res_gender"),
                label = "Lytis:",
                choices = list("Moteris" = "Moteris", "Vyras" = "Vyras",
                               "Kita" = "Kita"),
                selected = "",
                width = "100%"
              ),
              textInput(
                inputId = ns("res_address"),
                label = "Gyvenamoji vieta (adresas):",
                value = "",
                width = "100%"
              ),
              textInput(
                inputId = ns("res_institution"),
                label = "Darbovietė:",
                value = "",
                width = "100%"
              ),
              br(),
              textInput(
                inputId = ns("res_phone"),
                label = "Telefono numeris:",
                value = "",
                width = "100%"
              ),
              textInput(
                inputId = ns("res_email"),
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
              # textInput(
              #   inputId = ns("pat_name"),
              #   label = "Dabartinis slaptažodis:",
              #   value = "",
              #   width = "100%"
              # ),
              # textInput(
              #   inputId = ns("pat_surname"),
              #   label = "Naujas slaptažodis:",
              #   value = "",
              #   width = "100%"
              # ),
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
      "Gautos užklausos",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          p("Aktualios analizių užklausos pateiktos žemiau:",
            style = "font-weight:bold; font-size:17px; margin-left:0px"),
          DT::dataTableOutput(ns("samples3"))
        ),
        mainPanel(
          width = 8,
          tabsetPanel(
            tabPanel(
              title = "Kokybės vertinimas",
              value = "kokybes_vertinimas",
              br(),
              tabsetPanel(
                tabPanel(
                  title = "Pikų skaičius mėginiuose",
                  value = "piku_skaicius_meginiuose",
                  p("Pateiktoje stulpelinėje diagramoje pavaizduota, kiek yra pikų
                    kiekviename pateiktame BED formato faile:"),
                  shinydashboard::box(
                    width = 12, 
                    withLoader(
                      plotOutput(ns("plot1")), type = "html", loader = "dnaspin"
                    ),
                    downloadButton("download1", "Atsisiųsti vaizdą")
                  )
                ),
                tabPanel(
                  title = "Pikų skaičius chromosomose",
                  value = "piku_skaicius_chr",
                  p("Pateiktose stulpelinėse diagramose pavaizduota, kaip pikų
                    skaičius pasiskirstęs skirtingose chromosomose:"),
                  shinydashboard::box(
                    width = 12,
                    withLoader(
                      plotOutput(ns("plot2")), type = "html", loader = "dnaspin"
                    ),
                    downloadButton("download2", "Atsisiųsti vaizdą")
                  )
                ),
                tabPanel(
                  title = "Genominė distribucija",
                  value = "genomine_distribucija",
                  p("Pateiktame grafike pavaizduota pasirinktų mėginių genominių
                    elementų procentinė dalis:"),
                  shinydashboard::box(
                    width = 12,
                    withLoader(
                      plotOutput(ns("plot4")), type = "html", loader = "dnaspin"
                    ),
                    downloadButton("download4", "Atsisiųsti vaizdą")
                  )
                )
              )
            ),
            tabPanel(
              title = "Biologinės analizės",
              value = "biologines_analizes",
              br(),
              tabsetPanel(
                tabPanel(
                  title = "PSM matricos atitikimai",
                  value = "psm_atitikimai",
                  p("Pateiktoje stulpelinėje diagramoje pavaizduota, kokią procentinę
                    dalį sudaro įkeltą transkripcijos faktoriaus pozicinę svorių
                    matricą atitinkantys sekų fragmentai, palyginus su bendru pikų
                    skaičiumi:"),
                  shinydashboard::box(
                    width = 12,
                    withLoader(
                      plotOutput(ns("plot8")), type = "html", loader = "dnaspin"
                    ),
                    downloadButton("download8", "Atsisiųsti vaizdą")
                  )
                ),
                tabPanel(
                  title = "GO analizė",
                  value = "go_analize",
                  tabsetPanel(
                    id = "go",
                    tabPanel(
                      "GO lentelė",
                      id = "go_table",
                      p("Biologinių procesų (BP) genų ontologijos rezultatai:"),
                      width = 12,
                      withLoader(
                        reactableOutput(ns("table1")), type = "html",
                        loader = "dnaspin"
                      ),
                      p("Molekulinių funkcijų (MF) genų ontologijos rezultatai:"),
                      width = 12,
                      withLoader(
                        reactableOutput(ns("table2")), type = "html",
                        loader = "dnaspin"
                      ),
                      p("Ląstelinių komponentų (CC) genų ontologijos rezultatai:"),
                      width = 12,
                      withLoader(
                        reactableOutput(ns("table3")), type = "html",
                        loader = "dnaspin"
                      )
                    ),
                    tabPanel(
                      "GO aciklinis grafas",
                      id = "go_graph",
                      p("Biologinių procesų (BP) kryptinis aciklinis grafas:"),
                      width = 12,
                      withLoader(
                        plotOutput(ns("plot9")), type = "html", loader = "dnaspin"
                      ),
                      p("Molekulinių funkcijų (MF) kryptinis aciklinis grafas:"),
                      width = 12,
                      withLoader(
                        plotOutput(ns("plot10")), type = "html", loader = "dnaspin"
                      ),
                      p("Ląstelinių komponentų (CC) kryptinis aciklinis grafas:"),
                      width = 12,
                      withLoader(
                        plotOutput(ns("plot11")), type = "html", loader = "dnaspin"
                      )
                    ),
                    tabPanel(
                      "GO medžio struktūra",
                      id = "go_tree",
                      p("Biologinių procesų (BP) hierarchinis klasterizavimas:"),
                      width = 12,
                      withLoader(
                        plotOutput(ns("plot12")), type = "html", loader = "dnaspin"
                      ),
                      p("Molekulinių funkcijų (MF) hierarchinis klasterizavimas:"),
                      width = 12,
                      withLoader(
                        plotOutput(ns("plot13")), type = "html", loader = "dnaspin"
                      ),
                      p("Ląstelinių komponentų (CC) hierarchinis klasterizavimas:"),
                      width = 12,
                      withLoader(
                        plotOutput(ns("plot14")), type = "html", loader = "dnaspin"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}