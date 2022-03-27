source('src/data.R')

sales.plot <- data |>
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
    theme(legend.title = element_blank())

sales.plot

data |>
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
    # scale_y_continuous(breaks = seq(1800, 2100, by = 25),
    #                    labels = scales::dollar_format()) +
    labs(x = NULL,
         y = 'Ventas promedio') +
    theme(legend.title = element_blank())
