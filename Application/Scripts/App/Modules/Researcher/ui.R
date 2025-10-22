library(pacman)
p_load(shiny, shinythemes, shinydashboard, shinycustomloader, reactable)


researcher_ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    "Pseudonimizuota genetinių duomenų keitimosi sistema - Tyrėjų modulis",
    theme = shinytheme("cosmo"),
    includeCSS("Application/Scripts/App/styles.css"),
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