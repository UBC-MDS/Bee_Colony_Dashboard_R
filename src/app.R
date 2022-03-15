library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(dashHtmlComponents)
library(ggplot2)
library(ggthemes)
library(plotly)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# Read in global data
colony <-  readr::read_csv("data/colony.csv")
stressor <- readr::read_csv("data/stressor.csv")

# Wrangle data
colony <- colony %>%
  dplyr::mutate(start_month = stringr::str_split(colony$months, "-")) %>%
  tidyr::unnest(cols = c(start_month)) %>%
  dplyr::mutate(
    time = lubridate::ym(paste(year, start_month)),
    period = lubridate::quarter(time, type = "year.quarter")
  ) %>% 
  dplyr::select(state, colony_n, time, period) %>%
  dplyr::distinct(state, period, .keep_all = TRUE)

stressor['start_month'] <- str_split_fixed(stressor$months, '-', 2)[,1]
stressor['year'] <- as.character(stressor$year)
stressor['start_month'] <- as.integer(factor(stressor$start_month, levels = month.name))
stressor["time"] <- paste0(stressor$year,"-",stressor$start_month,"-","01")
stressor <- stressor %>% select(-c(year, months, start_month))
stressor["period"] = paste0(year(stressor$time), "-", quarter(stressor$time)) 


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
        dbcRow(
            list(
                dbcCol(
                    list(
                        htmlH1(
                            "Bee Colony Dashboard",
                            style=list(
                                "background-color"= "#E9AB17",
                                "font-family"= "Roboto",
                                "textAlign"= "center",
                                "font-weight"= "800",
                                "border"= "2px solid #000000",
                                "border-radius"= "5px"
                            )
                        ),
                        htmlH4(
                            "Select the period for map... (not in use for now)",
                            style=list("font-family"= "Roboto", "font-weight"= "600")
                        ),
                        dbcRow(
                            dccDropdown(
                                id='select_state_2',
                                value='Alabama',
                                options = state,
                                className = 'text-dark',
                                style=list(
                                    "height"= "50px",
                                    "vertical-align"= "middle",
                                    "font-family"= "Roboto",
                                    "font-size"= "28px",
                                    "textAlign"= "center",
                                    "border-radius"= "10px"
                                ),
                                placeholder="Select a period"
                            )
                        ),
                        htmlBr(),
                        htmlH4(
                            "Select a state for trend and stressor...",
                            style=list("font-family"= "Roboto", "font-weight"= "600")
                        ),
                        dbcRow(
                            dccDropdown(
                                id='state-widget',
                                value='Alabama',
                                options = unique(colony$state),
                                className = 'text-dark',
                                style=list(
                                    "height"= "50px",
                                    "vertical-align"="middle",
                                    "font-family"= "Roboto",
                                    "font-size"= "28px",
                                    "textAlign"= "center",
                                    "border-radius"= "10px"
                                ),
                                placeholder="Select a state"
                            )
                        ),
                        htmlBr(),
                        htmlH4(
                            "Select the period for trend and stressor...", 
                            style=list("font-family"= "Roboto", "font-weight"= "600"),
                        ),
                        dbcRow(
                            list(
                                dbcCol(
                                    dccDropdown(
                                        id='start-date-widget',
                                        options = colony$period %>%
                                                    unique() %>%
                                                    purrr::map(function(p)
                                                        list(
                                                        label = stringr::str_replace(as.character(p), stringr::fixed("."), "Q"),
                                                        value = p
                                                        )),
                                        className = 'text-dark', 
                                        style=list(
                                            "height"= "50px",
                                            "vertical-align"= "middle",
                                            "font-family"= "Roboto",
                                            "font-size"= "28px",
                                            "textAlign"= "center",
                                            "border-radius"= "10px"
                                        ),
                                        placeholder="Select a year",
                                        value=2015.1
                                    )
                                ),
                                dbcCol(
                                    dccDropdown(
                                        id='end-date-widget',
                                        options = colony$period %>%
                                                    unique() %>%
                                                    purrr::map(function(p)
                                                        list(
                                                        label = stringr::str_replace(as.character(p), stringr::fixed("."), "Q"),
                                                        value = p
                                                        )),
                                        className = 'text-dark',
                                        style=list(
                                            "height"= "50px",
                                            "vertical-align"= "middle",
                                            "font-family"= "Roboto",
                                            "font-size"= "28px",
                                            "textAlign"= "center",
                                            "border-radius"= "10px"
                                        ),
                                        placeholder="Select a time period",
                                        value=2015.4
                                    )
                                )
                              ),
                            className="g-0",
                        )
                    ),
                    md=6,
                    align="start",
                ),
                htmlBr(),
                dbcCol(
                    dbcCard(
                        list(
                            dbcCardHeader(
                                
                                    htmlH4(
                                        "Bee colony loss percentages by state (not in use for now)", 
                                        style=list(
                                            "font-family"= "Roboto",
                                            "font-weight"= "600"
                                        )
                                    ),
                                
                            ),
                            dbcCardBody(
                                htmlIframe(
                                    id="plot-area-3",
                                    style=list("width"= "100%", "height"= "320px")
                                )
                            )
                        ), 
                        style=list(
                            "width"= "100%",
                            "height"= "400px",
                            "background-color"= "#FBE7A1",
                            "border"= "2px solid #000000",
                            "border-radius"= "5px"
                        )
                    )
                )
            )
        ),
        htmlBr(),
        dbcRow(
          list(
                dbcCol(
                    (
                        dbcCard(
                            list(
                                dbcCardHeader(
                                    htmlH4(
                                        "Number of bee colonies over time",
                                        style=list(
                                            "font-family"= "Roboto",
                                            "font-weight"= "600"
                                        )
                                    )
                                ),
                                dbcCardBody(
                                    dccGraph(
                                        id="ncolony_chart",
                                        style=list("width"= "100%", "height"= "300px")
                                    )
                                )
                            ), 
                            style=list(
                                "width"= "100%",
                                "height"= "400px",
                                "background-color"= "#FBE7A1",
                                "border"= "2px solid #000000",
                                "border-radius"= "5px"
                            ),
                        )
                    ),
                    md=6,
                ),
                dbcCol(
                    list(
                        dbcCard(
                            list(
                                dbcCardHeader(
                                    
                                        htmlH4(
                                            "Bee colony stressors",
                                            style=list(
                                                "font-family"= "Roboto",
                                                "font-weight"= "600"
                                            )
                                        )
                                    
                                ),
                                dbcCardBody(
                                    list(
                                        dccGraph(
                                            id="plot-area",
                                            style=list("width"= "100%", "height"= "270px")
                                        ),
                                        htmlH6(
                                            "Note that the percentage will add to more than 100 as a colony can be affected by multiple stressors in the same quarter.",
                                            style=list("font-family"= "Roboto")
                                        )
                                    )
                                )
                            ),
                            style=list(
                                "width"= "100%",
                                "height"= "400px",
                                "background-color"= "#FBE7A1",
                                "border"= "2px solid #000000",
                                "border-radius"= "5px"
                            )
                        )
                    )
                )
            )
        )
    )
    # style={"background-color"= "#FFF8DC"}
  )
)

app$callback(
  output('plot-area', 'figure'),
  list(input('state-widget', 'value'),
       input('start-date-widget', 'value'),
       input('end-date-widget', 'value')),
  function(state, start_date, end_date) {
    start_date <- lubridate::ym(start_date)
    end_date <- lubridate::ym(end_date)
    p <- stressor %>%  filter(state == {{state}} & (period >= {{start_date}} & period <= {{end_date}})) %>%
      ggplot(aes(x = period,
                 y = stress_pct,
                 fill = stressor,
                 text = stress_pct)) +
      geom_bar(position="stack", stat="identity") + 
      labs(title = 'Bee colony stressors', x = 'Time period', y = 'Impacted colonies(%)')
  ggplotly(p)
  }
)

app$callback(
  output("ncolony_chart", "figure"),
  list(
    input("state-widget", "value"),
    input("start-date-widget", "value"),
    input("end-date-widget", "value")
  ),
  plot_timeseries <- function(state_arg, start_date, end_date) {
    start_date <- lubridate::ym(start_date)
    end_date <- lubridate::ym(end_date)
    
    data <- colony %>%
      dplyr::filter(
        state == state_arg,
        lubridate::ym(period) %within% lubridate::interval(start = start_date, end = end_date)
      )
    
    time_series <- data %>% ggplot2::ggplot() +
      ggplot2::aes(x = time, y = colony_n) +
      ggplot2::geom_line(size = 2) +
      ggplot2::geom_point(size = 4) +
      ggplot2::labs(x = "Time", y = "Count") +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 12),
        axis.text.x = ggplot2::element_text(angle = -30, hjust = 0),
        axis.title = ggplot2::element_text(size = 14),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black"),
        plot.background = ggplot2::element_rect(fill = '#fffadc', colour = '#fffadc')
      ) +
      ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
      ggplot2::scale_x_date(date_labels = "%b %Y")
    
    time_series
    # , width = 700, height = 400
    
    ggplotly(time_series + aes(text = colony_n), tooltip = "text") %>%
      layout(plot_bgcolor = '#fffadc')
  
  }
)

app$callback(
  output('plot-area-3', 'figure'),
  list(input("state-widget", "value"),
    input("start-date-widget", "value"),
    input("end-date-widget", "value")),
  function(state, start_date, end_date) {
    p <- stressor %>%  filter(state == {{state}} & (period >= {{start_date}} & period <= {{end_date}})) %>%
      ggplot(aes(x = period,
                 y = stress_pct,
                 fill = stressor,
                 text = stress_pct)) +
      geom_bar(position="stack", stat="identity") + 
      labs(title = 'Bee colony stressors', x = 'Time period', y = 'Impacted colonies(%)')
  ggplotly(p)
  }
)
# app$run_server(debug = T)
app$run_server(host = '0.0.0.0')