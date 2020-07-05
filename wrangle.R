library(tidyverse)
planilha <- planilha2020_uv_ %>%
  select(-virtual, - unanimidade) %>%
  mutate(unanimidade = str_detect(p$decisao, "(unânime)|(unanimidade)")) %>%
  mutate(virtual = str_detect(p$decisao, "Sessão Virtual"))
# mutate(administrativo = str_detect(planilha_2020nuv$ementa, "administrativo")) %>% 
#  mutate(tributario = str_detect(planilha_2020nuv$ementa, "tributário")) %>%
#  mutate(constitucional = str_detect(planilha_2020nuv$ementa, "direito constitucional")) %>%
#  mutate(penal = str_detect(planilha_2020nuv$ementa, "penal")) 
write.csv(planilha, 'planilha2020uv.csv', row.names = F)
