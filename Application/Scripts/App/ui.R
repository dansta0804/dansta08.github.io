# nolint start
library(pacman)
p_load(shiny, shinythemes, shinydashboard, shinycustomloader, reactable)

ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),
  uiOutput("main_ui")
)

# nolint end