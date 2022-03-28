source('src/config.R')

week <- c('lunes',
          'martes',
          'miércoles',
          'jueves',
          'viernes',
          'sábado',
          'domingo')

workweek <- week[1:4]
weekend <- week[5:7]

data <- read_csv('data/sales.csv')

data$`Created_at (UTC)` <- as.POSIXct(data$`Created_at (UTC)`,
                                      format = '%d/%m/%Y %H:%M:%S',
                                      tz = 'UTC')
data$time <- with_tz(data$`Created_at (UTC)`,
                     tz = 'America/Mexico_City')
data$cancelled <- ifelse(is.na(data$Total),
                         NA,
                         ifelse(is.na(data$`Cancelled at`),
                                'no',
                                'yes'))
data$date <- as.Date(data$time)
data$hour <- as.numeric(format(data$time, '%H'))
data$weekday <-
    factor(weekdays(data$time),
           levels = week)

data <- subset(data, select = -`Cancelled at`)

days <- as.numeric(difftime(max(data$date),
                            (min(data$date)),
                            units = 'days'))

weeks <- days / 7

payment.method <- data |>
    drop_na() |>
    filter(`Payment Method` != 'manual') |>
    group_by(`Payment Method`) |>
    summarise(sales_volume = sum(Total)) |>
    kable(col.names = c('Método de pago', 'Volumen de ventas'),
          align = 'cc',
          format.args = list(big.mark = ','))

cancelled.props <-
    c(length(which(data$cancelled == 'yes')) / length(which(!is.na(data$cancelled))),
      length(which(data$cancelled == 'no')) / length(which(!is.na(data$cancelled))))
