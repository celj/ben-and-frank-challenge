if (!require('pacman')) {
    install.packages('pacman')
}

pacman::p_load(rmarkdown,
               tidymodels,
               tidyverse,
               knitr,
               lubridate,
               patchwork)
