#' por_pais UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_por_pais_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        bs4Dash::bs4Card(
          title = "Filtros",
          width = 12,
          shiny::selectInput(
            inputId = ns("ano_1"),
            label = "Selecione o ano:",
            choices = unique(sort(dados_energia |> dplyr::filter(year >= 1965) |> dplyr::pull(year))),
            selected = 1965
          ),
          shiny::selectInput(
            inputId = ns("pais"),
            label = "Selecione o país:",
            choices = unique(sort(dados_energia$country)),
            selected = "Brazil"
          )

        )
      ),
      shiny::column(
        width = 9,
        echarts4r::echarts4rOutput(
          outputId = ns("grafico_1")
        ),
        echarts4r::echarts4rOutput(
          outputId = ns("grafico_2")
        )
      )
    )
  )
}

#' por_pais Server Functions
#'
#' @noRd
mod_por_pais_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$grafico_1 <- echarts4r::renderEcharts4r({

      dados_energia |>
        tidyr::replace_na(
          replace = list(
            hydro_electricity = 0, wind_electricity = 0,
            solar_electricity = 0, oil_electricity = 0,
            gas_electricity = 0, coal_electricity = 0,
            nuclear_electricity = 0, biofuel_electricity = 0
          )
        ) |>
        dplyr::filter(country == input$pais) |>
        dplyr::filter(year >= input$ano_1) |>
        dplyr::select(
          country, iso_code, year, hydro_electricity, wind_electricity,
          solar_electricity, oil_electricity, gas_electricity,
          coal_electricity, nuclear_electricity, biofuel_electricity
        ) |>
        dplyr::rename(
          `Hidrelétrica` = hydro_electricity, `Gás` = gas_electricity,
          `Biocombustíveis` = biofuel_electricity, `Eólica` = wind_electricity,
          `Petróleo` = oil_electricity, `Carvão` = coal_electricity,
          Solar = solar_electricity, Nuclear = nuclear_electricity
        ) |>
        dplyr::mutate(
          year = lubridate::make_date(year)
        ) |>
        echarts4r::e_chart(year) |>
        echarts4r::e_area(`Hidrelétrica`, symbol = "none", stack = "a") |>
        echarts4r::e_area(`Gás`, symbol = "none", stack = "a") |>
        echarts4r::e_area(`Biocombustíveis`, symbol = "none", stack = "a") |>
        echarts4r::e_area(`Eólica`, symbol = "none", stack = "a") |>
        echarts4r::e_area(`Petróleo`, symbol = "none", stack = "a") |>
        echarts4r::e_area(`Carvão`, symbol = "none", stack = "a") |>
        echarts4r::e_area(Solar, symbol = "none", stack = "a") |>
        echarts4r::e_area(Nuclear, symbol = "none", stack = "a") |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_title(
          text = glue::glue("Produção de eletricidade por fonte desde {input$ano_1}, {input$pais}."),
          textStyle = list(
            fontWeight = "bolder",
            fontSize = 20,
            fontFamily = "monospace"
          )
        ) |>
        echarts4r::e_grid(height = "70%", top = "15%") |>
        echarts4r::e_legend(top = "7%", right = "10%")

    })


    output$grafico_2 <- echarts4r::renderEcharts4r({

      dados_energia |>
        tidyr::replace_na(
          replace = list(
            hydro_electricity = 0, wind_electricity = 0,
            solar_electricity = 0, oil_electricity = 0,
            gas_electricity = 0, coal_electricity = 0,
            nuclear_electricity = 0, biofuel_electricity = 0
          )
        ) |>
        dplyr::filter(country == input$pais) |>
        dplyr::filter(year == input$ano_1) |>
        dplyr::select(
          country, year,
          hydro_electricity, wind_electricity,
          solar_electricity, oil_electricity,
          gas_electricity, coal_electricity,
          nuclear_electricity, biofuel_electricity
        ) |>
        dplyr::mutate(
          total = hydro_electricity + wind_electricity + solar_electricity +
            oil_electricity + gas_electricity + coal_electricity +
            nuclear_electricity + biofuel_electricity,
          hydro_electricity = round((hydro_electricity/total) * 100, 2),
          wind_electricity = round((wind_electricity/total) * 100, 2),
          solar_electricity = round((solar_electricity/total) * 100, 2),
          oil_electricity = round((oil_electricity/total) * 100, 2),
          gas_electricity = round((gas_electricity/total) * 100, 2),
          coal_electricity = round((coal_electricity/total) * 100, 2),
          nuclear_electricity = round((nuclear_electricity/total) * 100, 2),
          biofuel_electricity = round((biofuel_electricity/total) * 100, 2)
        ) |>
        dplyr::rename(
          `Hidrelétrica` = hydro_electricity, `Gás` = gas_electricity,
          `Biocombustíveis` = biofuel_electricity, `Eólica` = wind_electricity,
          `Petróleo` = oil_electricity, `Carvão` = coal_electricity,
          Solar = solar_electricity, Nuclear = nuclear_electricity
        ) |>
        echarts4r::e_chart(country) |>
        echarts4r::e_bar(`Hidrelétrica`, symbol = "none", stack = "a") |>
        echarts4r::e_bar(`Gás`, symbol = "none", stack = "a") |>
        echarts4r::e_bar(`Biocombustíveis`, symbol = "none", stack = "a") |>
        echarts4r::e_bar(`Eólica`, symbol = "none", stack = "a") |>
        echarts4r::e_bar(`Petróleo`, symbol = "none", stack = "a") |>
        echarts4r::e_bar(`Carvão`, symbol = "none", stack = "a") |>
        echarts4r::e_bar(Solar, symbol = "none", stack = "a") |>
        echarts4r::e_bar(Nuclear, symbol = "none", stack = "a") |>
        echarts4r::e_tooltip(trigger = "item") |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_grid(height = "15%", top = "20%") |>
        echarts4r::e_legend(top = "10%", right = "10%") |>
        echarts4r::e_title(
          text = glue::glue("Composição da matriz de eletricidade (%) em {input$ano_1}, {input$pais}."),
          textStyle = list(
            fontWeight = "bolder",
            fontSize = 20,
            fontFamily = "monospace"
          )
        )

    })


  })
}

## To be copied in the UI
# mod_por_pais_ui("por_pais_1")

## To be copied in the server
# mod_por_pais_server("por_pais_1")
