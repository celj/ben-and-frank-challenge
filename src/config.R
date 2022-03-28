source('src/pkgs.R')

knitr::opts_chunk$set(
    echo = FALSE,
    cache = FALSE,
    dpi = 300,
    fig.align = 'center',
    fig.height = 8,
    fig.width = 12,
    out.width = '80%'
)

options(digits = 4)

Sys.setlocale("LC_TIME", "es_ES")

theme_set(theme_minimal())
