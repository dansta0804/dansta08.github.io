# nolint start
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

# 4. Pereiti prie gydytojų modulio kūrimo:
#   4.1. Sukurti gydytojo UI (panašiai kaip researcher_ui.R), kad būtų galima
#        pasirinkti pacientą, sukurti paciento kortelę, pateikti užklausą
#        tyrėjui.

# 5. Praplėsti tyrėjų modulį:
#   5.1. Sukurti skiltį, kurioje būtų galima matyti visas gautas užklausas ir
#        inicijuoti analizių, nurodytų užklausoje, vykdymą.

# 6. Reikia pakoreguoti validacijų logiją, nes dabar kiek kreivai veikia. [DONE]