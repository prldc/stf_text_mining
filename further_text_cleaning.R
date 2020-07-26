## RUN ONLY AFTER text_normalizing.py

require(tidyverse)
require(naniar)

plan2020 <- read.csv('planilha2020_processada.csv')


# STRIPPING 'decisao' OF IRRELEVANT INFORMATION, SAVING ENTRY AS 'decisao limpa':

plan2020 <- plan2020 %>%
  mutate(decisao_limpa = case_when(str_detect(pattern = "Decisão:", plan2020$decisao) ~ str_extract(pattern = "(?<=Decisão:).*$", plan2020$decisao), 
                                   T ~ plan2020$decisao))

# FINDING 'MODULAÇÃO DE EFEITOS' CASES:

plan2020 <- plan2020 %>%
  mutate(apreciou_modulacao = case_when(str_detect(plan2020$decisao, regex(pattern = "ex nunc", ignore_case = T)) ~ T,
                                        str_detect(plan2020$ementa, regex("modul*", ignore_case = T)) ~ T,
                                        str_detect(plan2020$indexacao, regex(pattern = "modul*", ignore_case = T)) ~ T,
                                        T ~ F))

# EXTRACTING RESULT FROM decisao_limpa, REMOVING COLUMN:

plan2020 <- plan2020 %>%
  mutate(dispositivo = case_when(str_detect(plan2020$decisao_limpa, regex('julgou', ignore_case = T)) ~ str_extract(pattern = "[^.!?;]*(julgou)[^.?!]*[.?!;]", plan2020$decisao_limpa), 
                                        T ~ plan2020$decisao_limpa)) %>% select(-decisao_limpa)


# FINDING UNANIMOUS VERDICTS:

plan2020 <- plan2020 %>%
  mutate(placar = case_when(str_detect(plan2020$dispositivo, "((unânime)|(unanimidade))&(maioria)") ~ "complexa",
                            str_detect(plan2020$dispositivo, "(unânime)|(unanimidade)") ~ 'unânime',
                            is.na(plan2020$decisao) ~ "remove_me",
                            T ~ "maioria"))

plan2020 <- plan2020 %>% 
  replace_with_na_at(.vars = "dispositivo", condition = ~.x == 'nan')

plan2020 <- plan2020 %>%
  mutate(placar = case_when(is.na(plan2020$dispositivo) ~ 'remove_me',
                            T ~ plan2020$placar))

plan2020 <- plan2020 %>% 
  replace_with_na_at(.vars = "placar", condition = ~.x == 'remove_me')


# COUNTING NUMBER OF COURT SESSIONS PER TRIAL:

plan2020 <- plan2020 %>%
  mutate(numero_sessoes = case_when(str_detect(pattern = "Decisão:", plan2020$decisao) ~ as.double(str_count(pattern = "Decisão:", plan2020$decisao)),
                                    T ~ 1))

# CLASSIFYING JUDICIAL OPINIONS BY RESULT:

plan2020 <- plan2020 %>%
  mutate(resultado = case_when(str_detect(pattern = "(procedente)&(improcedente)", plan2020$dispositivo) ~ 'complexa',
                               str_detect(pattern = "extinta|extinto", plan2020$dispositivo) ~ 'extinta',
                               str_detect(pattern = "(prejudicada)|(prejudicado)", plan2020$dispositivo) ~ 'prejudicada',
                               str_detect(pattern = "(não conheceu)|(não conhecendo)", plan2020$dispositivo) ~ 'não_conheceu',
                               str_detect(pattern = "improcedente", plan2020$dispositivo) ~ 'improcedente',
                               str_detect(pattern = "parcialmente procedente", plan2020$dispositivo) ~ 'parcialmente_procedente',
                               str_detect(pattern = "procedente", plan2020$dispositivo) ~ 'procedente'))

                               

write.csv(plan2020, 'planilha2020_processada.csv', row.names = F)
write.xlsx(plan2020, 'planilha2020_processada.xlsx', row.names = F)

