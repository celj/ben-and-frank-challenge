source('src/data.R')

sales <- data |>
    drop_na() |>
    group_by(date) |>
    ggplot() +
    geom_bar(aes(x = date,
                 y = Total,
                 fill = Location),
             stat = 'identity',
             position = 'stack') +
    scale_x_date(date_breaks = '1 week', date_labels =  '%e %b') +
    scale_y_continuous(breaks = seq(0, 360000, by = 20000),
                       labels = scales::dollar_format()) +
    labs(x = 'Fecha',
         y = 'Ventas') +
    theme(legend.title = element_blank(),
          legend.position = 'top')

weekly.A <- data |>
    drop_na() |>
    filter(weekday %in% week, Location == 'Tienda A') |>
    group_by(weekday) |>
    summarise(Sales = sum(Total) / weeks) |>
    ggplot(aes(x = weekday,
               y = Sales,
               group = 1)) +
    geom_line() +
    scale_y_continuous(breaks = seq(0, 220000, by = 20000),
                       labels = scales::dollar_format()) +
    labs(x = NULL,
         y = 'Ventas promedio',
         title = 'Tienda A')

weekly.B <- data |>
    drop_na() |>
    filter(weekday %in% week, Location == 'Tienda B') |>
    group_by(weekday) |>
    summarise(Sales = sum(Total) / weeks) |>
    ggplot(aes(x = weekday,
               y = Sales,
               group = 1)) +
    geom_line() +
    scale_y_continuous(breaks = seq(0, 120000, by = 10000),
                       labels = scales::dollar_format()) +
    labs(x = NULL,
         y = 'Ventas promedio',
         title = 'Tienda B')

weekly.C <- data |>
    drop_na() |>
    filter(weekday %in% week, Location == 'Tienda C') |>
    group_by(weekday) |>
    summarise(Sales = sum(Total) / weeks) |>
    ggplot(aes(x = weekday,
               y = Sales,
               group = 1)) +
    geom_line() +
    scale_y_continuous(breaks = seq(0, 24000, by = 2000),
                       labels = scales::dollar_format()) +
    labs(x = NULL,
         y = 'Ventas promedio',
         title = 'Tienda C')

weekly <- data |>
    drop_na() |>
    filter(weekday %in% week) |>
    group_by(weekday, Location) |>
    summarise(Sales = sum(Total) / weeks) |>
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
    summarise(Sales = sum(Total) / days) |>
    ggplot() +
    geom_line(aes(
        x = hour,
        y = Sales,
        group = Location,
        color = Location
    )) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    scale_y_continuous(breaks = seq(0, 20000, by = 1000),
                       labels = scales::dollar_format()) +
    labs(x = 'Hora',
         y = NULL) +
    theme(legend.title = element_blank(),
          legend.position = 'top') +
    coord_polar()

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
        scale_y_continuous(breaks = seq(0, 50000, by = 2000),
                           labels = scales::dollar_format()) +
        labs(x = 'Hora',
             y = NULL,
             title = x) +
        theme(legend.title = element_blank(),
              legend.position = 'top') +
        coord_polar()
}

hourly.per.day <- lapply(week, hourly_daily)

# weekly.A / weekly.B / weekly.C
sales
weekly
hourly

hourly.workdays <- (hourly.per.day[[1]] + hourly.per.day[[2]]) /
    (hourly.per.day[[3]] + hourly.per.day[[4]])
hourly.weekends <- hourly.per.day[[5]] +
    hourly.per.day[[6]] +
    hourly.per.day[[7]]
