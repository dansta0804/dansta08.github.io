# nolint start
library(pacman)

# Library loading:
p_load(shiny, data.table, rtracklayer, ggplot2, ggthemes, plyranges, ggpubr,
       BRGenomics, reshape2, plotly, heatmaply, dplyr, gplots, genomation,
       Biostrings, scales, GenomicRanges, DT, shinythemes, shinycustomloader,
       ggseqlogo, ChIPseeker, tools, reactable, annotables, enrichplot,
       clusterProfiler, shinyalert, rjson, ensembldb, deepredeff, rBLAST,
       shinyjs, DBI, RMySQL, shinyFeedback)

# Declaration of options:
options(scipen = 100)
options(shiny.maxRequestSize = 300 * 1024 ^ 2)

# Path declarations:
PROJECT       <- "./Application/"

doctor_server <- function(input, output, session, user) {
  output$doc_user <- renderText({
    paste0(user$Vardas, " ", user$Pavarde)
  })

  print(user)
  
  updateTextInput(session, "doc_name", value = user$Vardas)
  updateTextInput(session, "doc_surname", value = user$Pavarde)
  updateTextInput(session, "doc_code", value = user$Asmens_kodas)
  updateSelectInput(session, "doc_gender", selected = user$Lytis)
  updateTextInput(session, "doc_address", value = user$Adresas)
  updateTextInput(session, "doc_phone", value = user$Tel_numeris)
  updateTextInput(session, "doc_email", value = user$El_pastas)
  updateTextInput(session, "doc_licence", value = user$Licencijos_Nr)
  updateTextInput(session, "doc_specialization", value = user$Specializacija)
  updateTextInput(session, "doc_institution", value = user$Institucija)
}
# nolint end
