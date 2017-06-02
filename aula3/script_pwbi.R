library(tidyverse)
library(stringr)

tipos_decisao <- function(decisoes) {
  negaram <- regex('negaram', ignore_case = TRUE)
  parcial <- regex('parcial', ignore_case = TRUE)
  deram <- regex('deram', ignore_case = TRUE)
  case_when(
    str_detect(decisoes, negaram) ~ 'negado',
    str_detect(decisoes, parcial) ~ 'parcial',
    str_detect(decisoes, deram) ~ 'provido',
    TRUE ~ 'outros'
  )
}

output <- dataset %>%
  mutate(n_processo = str_replace_all(arq, '[^0-9]', '')) %>%
  filter(situacao == 'Julgado') %>%
  distinct(n_processo, .keep_all = TRUE) %>%
  mutate(tipo_decisao = tipos_decisao(decisao))
