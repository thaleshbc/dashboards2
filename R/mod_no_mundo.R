#' no_mundo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_no_mundo_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      bs4Dash::bs4Card(
        title = "Filtros",
        width = 12,
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::selectInput(
              inputId = ns("ano"),
              label = "Selecione o ano:",
              choices = unique(sort(dados_energia |> dplyr::filter(year >= 1965) |> dplyr::pull(year))),
              selected = 2021
            )
          ),
          shiny::column(
            width = 6,
            shiny::selectInput(
              inputId = ns("tipo"),
              label = "Selecione um tipo de geração de energia:",
              choices = c("Geração hidrelétrica", "Geração eólica",
                          "Geração solar", "Geração a partir de petróleo",
                          "Geração a partir de gás", "Geração a partir de carvão",
                          "Geração nuclear", "Geração a partir de biocombustíveis"),
              selected = "Geração hidrelétrica"
            )
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        echarts4r::echarts4rOutput(
          outputId = ns("mapa")
        )
      )
    )
  )
}

#' no_mundo Server Functions
#'
#' @noRd
mod_no_mundo_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$mapa <- echarts4r::renderEcharts4r({


      tooltip <- htmlwidgets::JS(
        "function (params) {
        console.log(params);
        tx = '<strong>' + params.name + '</strong>' + '<br>' + parseFloat(params.value).toFixed(2) + ' TWh';
        return(tx);
      }")



      if (input$tipo == "Geração hidrelétrica") {

        dados_energia |>
          dplyr::filter(year == input$ano) |>
          dplyr::select(country, iso_code, year, hydro_electricity) |>
          dplyr::right_join(mundo, by = c("iso_code" = "shapeISO")) |>
          sf::st_as_sf() |>
          echarts4r::e_charts(country) |>
          echarts4r::e_map(hydro_electricity) |>
          echarts4r::e_visual_map(
            min = 0,
            max = 1400,
            left = "center",
            orient = "horizontal",
            itemHeight = 350,
            inRange = list(
              color = c("#AED6F1", "#85C1E9", "#5DADE2", "#3498DB",
                        "#2E86C1", "#2874A6", "#21618C", "#1B4F72")
            )
          ) |>
          echarts4r::e_theme("infographic") |>
          echarts4r::e_title(
            text = glue::glue("Geração hidrelétrica no ano de {input$ano}."),
            textStyle = list(
              color = "#1B4F72",
              fontWeight = "bolder",
              fontSize = 20,
              fontFamily = "monospace"
            ),
            subtext = "Medida em terawatt-horas.",
            subtextStyle = list(
              color = "#2874A6",
              fontWeight = "bolder",
              fontSize = 10,
              fontFamily = "monospace"
            ),
          ) |>
          echarts4r::e_tooltip(
            trigger = "item",
            backgroundColor = "#3498DB",
            borderColor = "#1B4F72",
            textStyle = list(
              color = "#FDFEFE"
            ),
            formatter = tooltip
          )

      } else if (input$tipo == "Geração eólica") {

        dados_energia |>
          dplyr::filter(year == input$ano) |>
          dplyr::select(country, iso_code, year, wind_electricity) |>
          dplyr::right_join(mundo, by = c("iso_code" = "shapeISO")) |>
          sf::st_as_sf() |>
          echarts4r::e_charts(country) |>
          echarts4r::e_map(wind_electricity) |>
          echarts4r::e_visual_map(
            min = 0,
            max = 700,
            left = "center",
            orient = "horizontal",
            itemHeight = 500,
            inRange = list(
              color = c("#a3e4d7", "#76d7c4", "#48c9b0", "#1abc9c",
                        "#17a589", "#148f77", "#117864", "#0e6251")
            )
          ) |>
          echarts4r::e_theme("infographic") |>
          echarts4r::e_title(
            text = glue::glue("Geração eólica no ano de {input$ano}."),
            textStyle = list(
              color = "#0e6251",
              fontWeight = "bolder",
              fontSize = 20,
              fontFamily = "monospace"
            ),
            subtext = "Medida em terawatt-horas.",
            subtextStyle = list(
              color = "#148f77",
              fontWeight = "bolder",
              fontSize = 10,
              fontFamily = "monospace"
            ),
          ) |>
          echarts4r::e_tooltip(
            trigger = "item",
            backgroundColor = "#1abc9c",
            borderColor = "#0e6251",
            textStyle = list(
              color = "#FDFEFE"
            ),
            formatter = tooltip
          )

      } else if (input$tipo == "Geração solar") {

        dados_energia |>
          dplyr::filter(year == input$ano) |>
          dplyr::select(country, iso_code, year, solar_electricity) |>
          dplyr::right_join(mundo, by = c("iso_code" = "shapeISO")) |>
          sf::st_as_sf() |>
          echarts4r::e_charts(country) |>
          echarts4r::e_map(solar_electricity) |>
          echarts4r::e_visual_map(
            min = 0,
            max = 350,
            left = "center",
            orient = "horizontal",
            itemHeight = 500,
            inRange = list(
              color = c("#fad7a0", "#f8c471", "#f5b041", "#f39c12",
                        "#d68910", "#b9770e", "#9c640c", "#7e5109")
            )
          ) |>
          echarts4r::e_theme("infographic") |>
          echarts4r::e_title(
            text = glue::glue("Geração solar no ano de {input$ano}."),
            textStyle = list(
              color = "#7e5109",
              fontWeight = "bolder",
              fontSize = 20,
              fontFamily = "monospace"
            ),
            subtext = "Medida em terawatt-horas.",
            subtextStyle = list(
              color = "#b9770e",
              fontWeight = "bolder",
              fontSize = 10,
              fontFamily = "monospace"
            ),
          ) |>
          echarts4r::e_tooltip(
            trigger = "item",
            backgroundColor = "#f39c12",
            borderColor = "#7e5109",
            textStyle = list(
              color = "#FDFEFE"
            ),
            formatter = tooltip
          )

      } else if (input$tipo == "Geração a partir de petróleo") {

        dados_energia |>
          dplyr::filter(year == input$ano) |>
          dplyr::select(country, iso_code, year, oil_electricity) |>
          dplyr::right_join(mundo, by = c("iso_code" = "shapeISO")) |>
          sf::st_as_sf() |>
          echarts4r::e_charts(country) |>
          echarts4r::e_map(oil_electricity) |>
          echarts4r::e_visual_map(
            min = 0,
            max = 140,
            left = "center",
            orient = "horizontal",
            itemHeight = 500,
            inRange = list(
              color = c("#D2B4DE", "#BB8FCE", "#A569BD", "#8E44AD",
                        "#7D3C98", "#6C3483", "#5B2C6F", "#4A235A")
            )
          ) |>
          echarts4r::e_theme("infographic") |>
          echarts4r::e_title(
            text = glue::glue("Geração a partir do petróleo no ano de {input$ano}."),
            textStyle = list(
              color = "#4A235A",
              fontWeight = "bolder",
              fontSize = 20,
              fontFamily = "monospace"
            ),
            subtext = "Medida em terawatt-horas.",
            subtextStyle = list(
              color = "#6C3483",
              fontWeight = "bolder",
              fontSize = 10,
              fontFamily = "monospace"
            ),
          ) |>
          echarts4r::e_tooltip(
            trigger = "item",
            backgroundColor = "#8E44AD",
            borderColor = "#4A235A",
            textStyle = list(
              color = "#FDFEFE"
            ),
            formatter = tooltip
          )

      } else if (input$tipo == "Geração a partir de gás") {

        dados_energia |>
          dplyr::filter(year == input$ano) |>
          dplyr::select(country, iso_code, year, gas_electricity) |>
          dplyr::right_join(mundo, by = c("iso_code" = "shapeISO")) |>
          sf::st_as_sf() |>
          echarts4r::e_charts(country) |>
          echarts4r::e_map(gas_electricity) |>
          echarts4r::e_visual_map(
            min = 0,
            max = 1700,
            left = "center",
            orient = "horizontal",
            itemHeight = 500,
            inRange = list(
              color = c("#f5b7b1", "#f1948a", "#ec7063", "#e74c3c",
                        "#cb4335", "#b03a2e", "#943126", "#78281f")
            )
          ) |>
          echarts4r::e_theme("infographic") |>
          echarts4r::e_title(
            text = glue::glue("Geração a partir do gás no ano de {input$ano}."),
            textStyle = list(
              color = "#78281f",
              fontWeight = "bolder",
              fontSize = 20,
              fontFamily = "monospace"
            ),
            subtext = "Medida em terawatt-horas.",
            subtextStyle = list(
              color = "#b03a2e",
              fontWeight = "bolder",
              fontSize = 10,
              fontFamily = "monospace"
            ),
          ) |>
          echarts4r::e_tooltip(
            trigger = "item",
            backgroundColor = "#e74c3c",
            borderColor = "#78281f",
            textStyle = list(
              color = "#FDFEFE"
            ),
            formatter = tooltip
          )

      } else if (input$tipo == "Geração a partir de carvão") {

        dados_energia |>
          dplyr::filter(year == input$ano) |>
          dplyr::select(country, iso_code, year, coal_electricity) |>
          dplyr::right_join(mundo, by = c("iso_code" = "shapeISO")) |>
          sf::st_as_sf() |>
          echarts4r::e_charts(country) |>
          echarts4r::e_map(coal_electricity) |>
          echarts4r::e_visual_map(
            min = 0,
            max = 5400,
            left = "center",
            orient = "horizontal",
            itemHeight = 500,
            inRange = list(
              color = c("#e6b0aa", "#d98880", "#cd6155", "#c0392b",
                        "#a93226", "#922b21", "#7b241c", "#641e16")
            )
          ) |>
          echarts4r::e_theme("infographic") |>
          echarts4r::e_title(
            text = glue::glue("Geração a partir do carvão no ano de {input$ano}."),
            textStyle = list(
              color = "#641e16",
              fontWeight = "bolder",
              fontSize = 20,
              fontFamily = "monospace"
            ),
            subtext = "Medida em terawatt-horas.",
            subtextStyle = list(
              color = "#922b21",
              fontWeight = "bolder",
              fontSize = 10,
              fontFamily = "monospace"
            ),
          ) |>
          echarts4r::e_tooltip(
            trigger = "item",
            backgroundColor = "#c0392b",
            borderColor = "#641e16",
            textStyle = list(
              color = "#FDFEFE"
            ),
            formatter = tooltip
          )

      } else if (input$tipo == "Geração nuclear") {

        dados_energia |>
          dplyr::filter(year == input$ano) |>
          dplyr::select(country, iso_code, year, nuclear_electricity) |>
          dplyr::right_join(mundo, by = c("iso_code" = "shapeISO")) |>
          sf::st_as_sf() |>
          echarts4r::e_charts(country) |>
          echarts4r::e_map(nuclear_electricity) |>
          echarts4r::e_visual_map(
            min = 0,
            max = 900,
            left = "center",
            orient = "horizontal",
            itemHeight = 500,
            inRange = list(
              color = c("#a9cce3", "#7fb3d5", "#5499c7", "#2980b9",
                        "#2471a3", "#1f618d", "#1a5276", "#154360")
            )
          ) |>
          echarts4r::e_theme("infographic") |>
          echarts4r::e_title(
            text = glue::glue("Geração nuclear no ano de {input$ano}."),
            textStyle = list(
              color = "#154360",
              fontWeight = "bolder",
              fontSize = 20,
              fontFamily = "monospace"
            ),
            subtext = "Medida em terawatt-horas.",
            subtextStyle = list(
              color = "#1f618d",
              fontWeight = "bolder",
              fontSize = 10,
              fontFamily = "monospace"
            ),
          ) |>
          echarts4r::e_tooltip(
            trigger = "item",
            backgroundColor = "#2980b9",
            borderColor = "#154360",
            textStyle = list(
              color = "#FDFEFE"
            ),
            formatter = tooltip
          )

      } else if (input$tipo == "Geração a partir de biocombustíveis") {

        dados_energia |>
          dplyr::filter(year == input$ano) |>
          dplyr::select(country, iso_code, year, biofuel_electricity) |>
          dplyr::right_join(mundo, by = c("iso_code" = "shapeISO")) |>
          sf::st_as_sf() |>
          echarts4r::e_charts(country) |>
          echarts4r::e_map(biofuel_electricity) |>
          echarts4r::e_visual_map(
            min = 0,
            max = 170,
            left = "center",
            orient = "horizontal",
            itemHeight = 500,
            inRange = list(
              color = c("#f9e79f", "#f7dc6f", "#f4d03f", "#f1c40f",
                        "#d4ac0d", "#b7950b", "#9a7d0a", "#7d6608")
            )
          ) |>
          echarts4r::e_theme("infographic") |>
          echarts4r::e_title(
            text = glue::glue("Geração a partir de bicombustíveis no ano de {input$ano}."),
            textStyle = list(
              color = "#7d6608",
              fontWeight = "bolder",
              fontSize = 20,
              fontFamily = "monospace"
            ),
            subtext = "Medida em terawatt-horas.",
            subtextStyle = list(
              color = "#b7950b",
              fontWeight = "bolder",
              fontSize = 10,
              fontFamily = "monospace"
            ),
          ) |>
          echarts4r::e_tooltip(
            trigger = "item",
            backgroundColor = "#f1c40f",
            borderColor = "#7d6608",
            textStyle = list(
              color = "#FDFEFE"
            ),
            formatter = tooltip
          )

       }

    })

  })
}

## To be copied in the UI
# mod_no_mundo_ui("no_mundo_1")

## To be copied in the server
# mod_no_mundo_server("no_mundo_1")
