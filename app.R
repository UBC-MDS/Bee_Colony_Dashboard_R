library(dash)
library(dashHtmlComponents)
library(lubridate)
library(plotly)

# Read in global data

colony <-  readr::read_csv("data/colony.csv")
colony

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

# Create app and widgets

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      dccGraph(id = "ncolony_chart"),
      dccDropdown(
        id = "state-widget",
        options = unique(colony$state),
        value = "Alabama"
      ),
      dccDropdown(
        id = "start-date-widget",
        options = unique(colony$period),
        value = 2015.1
      ),
      dccDropdown(
        id = "end-date-widget",
        options = unique(colony$period),
        value = 2015.4
      )
    )
  )
)


# Plot time series


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
    
    ggplotly(time_series + aes(text = colony_n), tooltip = "text", width = 700, height = 400) %>%
      layout(plot_bgcolor = '#fffadc')
  
  }
)

app$run_server(debug = T)