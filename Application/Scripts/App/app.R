# nolint start
library(pacman)

# Library loading:
p_load(shiny, data.table, rtracklayer, ggplot2, ggthemes, plyranges, ggpubr,
       BRGenomics, reshape2, plotly, heatmaply, dplyr, gplots, genomation,
       Biostrings, scales, GenomicRanges, DT, shinythemes, shinycustomloader,
       ggseqlogo, ChIPseeker, tools, reactable, annotables, enrichplot,
       clusterProfiler, shinyalert, rjson, shiny, shinythemes, shinydashboard)

library(BiocManager)
options(repos = BiocManager::repositories())

# Path declarations:
PROJECT <- "./Application/"
source(paste0(PROJECT, "Scripts/App/ui.R"))
source(paste0(PROJECT, "Scripts/App/server.R"))

# source(paste0(PROJECT, "ui.R"))
# source(paste0(PROJECT, "server.R"))

# A function that starts an app:
shinyApp(ui = ui, server = server)
# nolint end

################################ TO DO LIST ################################
# 1. Reikia padaryti, kad asmens kodas būtų ne int tipo, bet string, nes int
# max reikšmė yra 4,294,967,295, todėl ne visada telpa asmens kodas. [DONE]

# 2. Sutvarkyti DB stulpelių apribojimus, kad atitiktų tai, kaip aprašyta
# reikalavimų specifikacijoje skyriuje "7.2.2 Duomenų modelio specifikacija". [DONE]

# 3. Slaptažodžių saugojimas DB. Reikia išsiaiškinti, kaip geriausia
# saugoti slaptažodžius DB (hash + salt?).