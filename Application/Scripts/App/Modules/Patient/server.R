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

patient_server <- function(input, output, session, user) {
  output$pat_user <- renderText({
    paste0(user$Vardas, " ", user$Pavarde)
  })
  
  updateTextInput(session, "pat_name", value = user$Vardas)
  updateTextInput(session, "pat_surname", value = user$Pavarde)
  updateTextInput(session, "pat_code", value = user$Asmens_kodas)
  updateSelectInput(session, "pat_gender", selected = user$Lytis)
  updateTextInput(session, "pat_address", value = user$Adresas)
  updateTextInput(session, "pat_phone", value = user$Tel_numeris)
  updateTextInput(session, "pat_email", value = user$El_pastas)
}
# nolint end
