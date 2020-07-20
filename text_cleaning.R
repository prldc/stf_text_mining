require(tidyverse)
require(data.table)
require(textmineR)
require(openxlsx)
require(reticulate)

plan2020 <- read.csv('planilha2020.csv')

# FINDING CITED ADI CASES:

set_lists_to_chars <- function(x) {
  if(class(x) == 'list') {
    y <- paste(unlist(x[1]), sep='', collapse=', ')
  } else {
    y <- x
  }
  return(y)
}

plan2020 <- plan2020 %>% 
  mutate(ac = str_match_all(pattern = "ADI[\\s|-]\\d++(?![\\s|-][MC|QO|AgR|ED])", plan2020$obs))  # Finds all matches of ADI followed by a sequence of numbers and NOT followed by "MC", "QO, "AgR' or "ED".

acordaos_citados <- 1:2136
ac <- tibble(acordaos_citados)
for(i in 1:nrow(plan2020))
{ac[i] = set_lists_to_chars(plan2020$ac[i])}
acordaos_citados <- t(ac[1,])
acordaos_citados <- tibble(acordaos_citados)
plan2020 <- bind_cols(plan2020, acordaos_citados)
plan2020 <- plan2020 %>% select(-ac)

# FILTERING ADI BY WHETHER THEY CHALLENGE STATE OR FEDERAL LAW:

plan2020 <- plan2020 %>%
  mutate(origem = case_when(str_detect(pattern = "(LEG-EST)|(LEG-DIS)", plan2020$legislacao) ~ 'estadual',
                            !str_detect(pattern = "LEG-EST|LEG-DIS", plan2020$legislacao) ~ 'federal'))

# FINDING REMOTE TRIAL SESSIONS:

plan2020 <- plan2020 %>% 
  mutate(virtual = str_detect(plan2020$decisao, "Sessão Virtual"))

# CLASSIFYING OPINIONS BY WHETHER THEY WERE JUDGED REMOTELY ("virtual"), FAST-TRACKED ("lista") OR NEITHER ("tradicional"):

plan2020 <- plan2020 %>%
  mutate(tipo_julgamento = case_when(lista ~ "lista",
                                     virtual ~ "virtual",
                                     T ~ 'tradicional'))

# DROPPING 'acordaos_mesmo_sentido', SINCE 'acordaos_citados' CONTAINS MORE INFORMATION:

plan2020 <- plan2020 %>%
  select(-acordaos_mesmo_sentido)


# CLASSIFYING COURT OPINIONS BY PERIOD:

plan2020 <- plan2020 %>% mutate(periodo = 
                                  case_when(plan2020$data_julgamento < as.Date('1995-01-01') ~ 1,
                                            ((plan2020$data_julgamento > as.Date('1994-12-31'))&(plan2020$data_julgamento < as.Date('2003-01-01'))) ~ 2,
                                            ((plan2020$data_julgamento > as.Date('2002-12-31'))&(plan2020$data_julgamento < as.Date('2007-01-01'))) ~ 3,
                                            ((plan2020$data_julgamento > as.Date('2006-12-31'))&(plan2020$data_julgamento < as.Date('2013-01-01'))) ~ 4,
                                            ((plan2020$data_julgamento > as.Date('2012-12-31'))&(plan2020$data_julgamento < as.Date('2021-01-01'))) ~ 5
                                  ))


# EXPORTING TO CSV:

write.csv(plan2020, 'planilha2020_processada.csv', row.names = F)
write.xlsx(plan2020, 'planilha2020_processada.xlsx', row.names = F)

# NOW RUN text_normalizing.py TO NORMALIZE TEXT ENTRIES:



