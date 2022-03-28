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

# Obtener la órdenes creadas en horario de la CDMX
data$time <- with_tz(data$`Created_at (UTC)`,
                     tz = 'America/Mexico_City')

# Determinar si la orden fue cancelada a partir de la fecha de cancelación
data$cancelled <- ifelse(is.na(data$Total),
                         NA,
                         ifelse(is.na(data$`Cancelled at`),
                                'no',
                                'yes'))

# Fecha de la orden
data$date <- as.Date(data$time)

# Hora de la orden
data$hour <- as.numeric(format(data$time, '%H'))

# Día de la semana en que se hizo la orden
data$weekday <-
    factor(weekdays(data$time),
           levels = week)

# Elimina fecha de cancelación (ya se creó la variable categórica)
data <- subset(data, select = -`Cancelled at`)

# Días transcurridos en el periodo de análisis
days <- as.numeric(difftime(max(data$date),
                            (min(data$date)),
                            units = 'days'))

# Semanas en el periodo de análisis
weeks <- days / 7

# Renombrar efectivo y crédito externo
data$`Payment Method` <- replace(data$`Payment Method`,
                                 data$`Payment Method` == 'External Credit + Cash',
                                 'Cash + External Credit')

# Tabla de conteo de métodos de pago en volumen de ventas
payment.method <- data |>
    drop_na() |>
    filter(`Payment Method` != 'manual') |>
    group_by(`Payment Method`) |>
    summarise(sales_volume = sum(Total)) |>
    kable(col.names = c('Método de pago', 'Volumen de ventas'),
          align = 'cc',
          format.args = list(big.mark = ','))

# Distribución de cancelaciones
cancelled.props <-
    c(length(which(data$cancelled == 'yes')) / length(which(!is.na(data$cancelled))),
      length(which(data$cancelled == 'no')) / length(which(!is.na(data$cancelled))))
