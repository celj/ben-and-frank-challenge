source('src/data.R')

# Ventas por días y por tienda (agregado y proporciones)
sales <- data |>
    drop_na() |> # para cada gráfico, elimina valores NA pues solo interesa la columna 'Total'
    group_by(date) |>
    ggplot() +
    geom_bar(aes(x = date,
                 y = Total,
                 fill = Location),
             stat = 'identity',
             position = 'stack') +
    scale_x_date(date_breaks = '1 week', date_labels =  '%e %b') +
    scale_y_continuous(breaks = seq(0, 360000, by = 25000),
                       labels = scales::dollar_format()) +
    labs(x = 'Fecha',
         y = 'Ventas') +
    theme(legend.title = element_blank(),
          legend.position = 'top')

# Plot de ventas por día de la semana
weekly <- data |>
    drop_na() |>
    filter(weekday %in% week) |>
    group_by(weekday, Location) |>
    summarise(Sales = sum(Total) / weeks) |> # Promedia el volumen total de ventas por semana para cada día
    ggplot() +
    geom_line(aes(
        x = weekday,
        y = Sales,
        group = Location,
        color = Location
    )) +
    scale_y_continuous(breaks = seq(0, 220000, by = 20000),
                       labels = scales::dollar_format()) +
    labs(x = NULL,
         y = 'Ventas promedio') +
    theme(legend.title = element_blank(),
          legend.position = 'top')

hourly <- data |>
    drop_na() |>
    group_by(hour, Location) |>
    summarise(Sales = sum(Total) / days) |> # Promedia el volumen total de ventas por día para cada hora
    ggplot() +
    geom_line(aes(
        x = hour,
        y = Sales,
        group = Location,
        color = Location
    )) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    scale_y_continuous(breaks = seq(0, 20000, by = 2000),
                       labels = scales::dollar_format()) +
    labs(x = 'Hora',
         y = NULL) +
    theme(legend.title = element_blank(),
          legend.position = 'top') +
    coord_polar()

# Mismo procedimiento anterior, filtrando por día de la semana
hourly_daily <- function (x) {
    data |>
        drop_na() |>
        filter(weekday == x) |>
        group_by(hour, Location) |>
        summarise(Sales = sum(Total) / weeks) |>
        ggplot() +
        geom_line(aes(
            x = hour,
            y = Sales,
            group = Location,
            color = Location
        )) +
        scale_x_continuous(breaks = seq(0, 24, by = 1)) +
        scale_y_continuous(breaks = seq(0, 50000, by = 2500),
                           labels = scales::dollar_format()) +
        labs(x = 'Hora',
             y = NULL,
             title = x) +
        theme(legend.title = element_blank(),
              legend.position = 'left') +
        coord_polar()
}

# Aplica la función construida sobre cada día de la semana
hourly.per.day <- lapply(week, hourly_daily)

# Une en un solo gráfico los resultados entre semana
hourly.workdays <- (hourly.per.day[[1]] + hourly.per.day[[2]]) /
    (hourly.per.day[[3]] + hourly.per.day[[4]])
# Une en un solo gráfico los resultados en fin de semana
hourly.weekend <- hourly.per.day[[5]] | hourly.per.day[[6]] | hourly.per.day[[7]]
