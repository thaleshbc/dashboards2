#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_inicio_server("inicio_1")
  mod_no_mundo_server("no_mundo_1")
  mod_por_pais_server("por_pais_1")
}
