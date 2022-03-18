library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(dashHtmlComponents)
library(plotly)
library(purrr)
library(dplyr)
library(lubridate)

colony <- read.csv("data/colony.csv")
stressor <- read.csv("data/stressor.csv")
state_info <- read.csv("data/state_info.csv")

# Wrangle data
colony <- colony %>%
  dplyr::mutate(start_month = stringr::str_split(colony$months, "-")) %>%
  tidyr::unnest(cols = c(start_month)) %>%
  dplyr::mutate(
    time = lubridate::ym(paste(year, start_month)),
    period = lubridate::quarter(time, type = "year.quarter")
  ) %>%
  dplyr::select(state, colony_n, colony_lost_pct, time, period) %>%
  dplyr::distinct(state, period, .keep_all = TRUE)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      htmlH1('Bee Colony Dashboard'),
      dbcRow(
        list(
          dbcCol(
            list(
              htmlLabel('Select the period for map'),
              dccDropdown(
                id = 'map_widget',
                options = colony$period %>%
                  unique() %>%
                  purrr::map(function(p)
                    list(
                      label = stringr::str_replace(as.character(p), stringr::fixed("."), "Q"),
                      value = p
                    )),
                value = 2015.1
              )
            )
          ),
          dccLoading(
            dccGraph(id = 'map')
          )
        )
      )
    )
  )
)

app$callback(
  output('map', 'figure'),
  list(input('map_widget', 'value')),
  function(str_period, month) {
    df <- colony %>%
      filter(period == str_period)
    target_df <- left_join(state_info, df, by='state')
    target_df['colony_lost_pct'][is.na(target_df['colony_lost_pct'])] <- 0
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )

    plot_ly(target_df) %>%
      layout(geo = g) %>%
      add_trace(type = "choropleth", locationmode = 'USA-states',
                locations = ~abbr,
                z = ~colony_lost_pct,
                color = ~colony_lost_pct, autocolorscale = TRUE) %>%
      add_trace(type = "scattergeo", locationmode = 'USA-states',
                locations = ~abbr, text = ~colony_lost_pct,
                mode = "text",
                textfont = list(color = rgb(0,0,0), size = 12))
  }
)

app$run_server(debut=T)
