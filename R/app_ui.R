#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::bs4DashPage(
      bs4Dash::bs4DashNavbar(
        title = "Geração de Energia"
      ),
      bs4Dash::bs4DashSidebar(
        bs4Dash::bs4SidebarMenu(
          bs4Dash::bs4SidebarMenuItem(
            text = "Sobre",
            tabName = "inicio",
            icon = icon("info")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "No mundo",
            tabName = "no_mundo",
            icon = icon("globe-americas")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "Por país",
            tabName = "por_pais",
            icon = icon("flag")
          )
        )
      ),
      bs4Dash::bs4DashBody(
        bs4Dash::bs4TabItems(
          bs4Dash::bs4TabItem(
            tabName = "inicio",
            mod_inicio_ui("inicio_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "no_mundo",
            mod_no_mundo_ui("no_mundo_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "por_pais",
            mod_por_pais_ui("por_pais_1")
          )

        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "dashboards2"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
