library(tidyverse)
library(readtext)
library(quanteda)
library(data.table)
library(topicmodels)
library(tidytext)
library(viridis)

# CREATING THE LDA MODEL:

stop <- c("poder", "cf", "moreira", "alves", "tribunal", "infraestrutura", "dje", "l", "uma", "um", "muito", "há", "temos", "dizer", 
          "essa", "me", "nós", "eu", "essa", "esta", "muito", "ó", "quando", "quanto", "rtj", 
          "mas", "foi", "ao", "par", "se", "88", "crfb", "c.f", "os", "que", "não", "b", "c", 
          "britto", "ilmar", "gallotti", "galvão", "silveira", "sydney", "néri", "dr", "cezar", 
          "velloso","lewandowski", "ministra", "senhora", "paulo", "5º", "iii", "janeiro", 
          "celso", "mello", "iv", "r", "outro", "voto", "luiz", "roberto", "mendes", "p", "4º", "dj",  
          "vossa", "excelência", "legislativa", "adin", "quanto", "fls", "rio", "sr", "stf", 
          "sepúlveda", "peluso", "carlos", "pertence", "dias", "artigo", "aqui", "então", 
          "constitucional", "estado", "brasileira", "estadual", "públicas", "direito", 
          "direitos", "df", "federal", "lei", "adi", "constituição", "supremo", "
          tribunal", "3º", "apenas", "pedido", "institui", "ministro", "senhor", 
          "caso", "rel", "aurélio", "qualquer", "ainda", "medida", "assim", "sobre", 
          "assim", "2º", "ii", "i", "1º", "mp", "sob", "porque", "teor", "nº", "conforme", 
          "inteiro", "gilmar", "moraes", "alexandre", "rosa", "lúcia", "cármen", "fachin", 
          "toffoli", "barroso", "pode", "fux", "página", "senha", "código", "2.200-2", 
          "chaves", "eletrônico", "endereço", "acessado", "documento", "REQTE", "ADV", 
          "DOS", "DO", "A", "MIN", "DA", "DAS", "INTDO", "Acórdão", "RELATORA","PLENÁRIO", 
          "RELATOR","INCONSTITUCIONALIDADE","DIRETA", "AÇÃO", "DE","EMENTA", "é", "e", "ser", 
          "assinado", "digitalmente", "art", "número", "n", "s", "v", "icp-brasil", "portal", "portal_autenticacao", "www_jus", 
          "autenticacao", "br_portal", "www", "http_www", "ministério_público", "jus_br",
          "autenticacao_autenticardocumento", "autenticardocumento", "autenticacao_autenticardocumento, 
          autenticardocumento", "autenticardocumento_asp", "net", "hdl", "https", "net_https", "https_hdl",
          "santa", "catarina", "santa_catarina", "sc", "paraíba", "ceará", "rondônia", "amapá", "mato", "grosso", 
          "mt", "mato_grosso", "handle_handle", "re_re", "norte", "grande_norte", "grande" , "rn", 
          "santo", "espírito", "espírito_santo", "es", "minas", "minas_gerais", "gerais", "mg", "sul", "grande_sul",
          "rs", "tocantins", "amazonas")

plan2020 <- read_csv("~/R Projects/stf_text_mining/planilha2020_processada_bytopic.csv")
plan2020_text <- readtext("~/R Projects/stf_text_mining/planilha2020_processada.csv", text_field = "acordao")  # Indicates the column with the full extent of the judicial opinion as the text field.
corpus <- corpus(plan2020_text)
docnames(corpus) <- plan2020_text$nome
tokens <- tokens(corpus, remove_punct = T, remove_symbols = T, remove_url = T, remove_numbers = T)
tokens <- tokens_remove(tokens, stopwords("portuguese"))  # Removes common portuguese stopwords.
tokens <- tokens_remove(tokens, pattern = stop)  # Removes our custom, domain-specific, stopwords, as defined above.
dfm <- dfm(tokens, tolower = T, remove = stop, case_insensitive = T)
topfeatures(dfm, 50)   # Allows us to glimpse the 50 most common words of our dfm.
lda <- LDA(dfm, k = 6, control = list(seed = pi))  # Creates a LDA model with 6 topics.
doc_lda <- tidy(lda, matrix = "gamma")
terms(lda, 50)  # Shows 50 most common words of each topic.

## CREATES GAMMA TABLE, CATEGORIZES EACH OPINION BY DOMINANT TOPIC:
gamma_table <- doc_lda %>% pivot_wider(names_from = topic, values_from = gamma)
gamma_table[, "main_topic"] <- apply(gamma_table[, 2:7], FUN = which.max, MARGIN = 1)  # Categorizes each opinion by dominant topic
main_topic <- gamma_table %>% select(document, main_topic)
plan2020 <- plan2020 %>% inner_join(main_topic, by = c("nome" = "document"))
write.csv(plan2020, "planilha2020_processada_bytopic.csv", row.names = F)

# VISUALIZING THE TOPICS:

## TOP 15 WORDS BY TOPIC:
lda_topics <- lda %>% tidy(matrix = 'beta')
word_probs <- lda_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(terms = reorder(term, beta))
p <- ggplot(word_probs, aes(terms, beta, fill= factor(topic)))
p + geom_col(show.legend=F) + facet_wrap(~topic, scales = "free") + coord_flip()

## DISTRIBUTION OF PROBABILITY PER TOPIC:
lda_gamma <- tidy(lda, matrix = "gamma")
p <- ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) 
p + geom_histogram(show.legend = FALSE) + 
  facet_wrap(~ topic, ncol = 2) +
  scale_y_log10() +
  labs(title = "Distribuição de probabilidade por tópico", y = "Número de documentos", x = expression(gamma))

# ESTADUAL VERSUS FEDERAL:

federal_estadual <- corpus_subset(corpus, origem %in% c("federal", "estadual")) 
dfm_fe <- dfm(corpus, groups = "origem", tolower = T, remove = stop, case_insensitive = T,
                      remove_punct = TRUE, remove_symbols = T, remove_url = T, remove_numbers = T)  # Constructs DFM for lt_federal.
dfm_fe <- dfm_remove(dfm_fe, pattern = "198092re")
keyness_fe <- textstat_keyness(dfm_fe, target = "federal")
keyness_fe_lr <- textstat_keyness(dfm_fe, target = "federal", measure = "lr")
textplot_keyness(keyness_fe, color = c("blue", "red")) 
textplot_keyness(keyness_fe_lr, color = c("blue", "red")) 

## sfs

dfm_weighted <- dfm %>% dfm_weight(scheme = "prop")
freq_weight <- textstat_frequency(dfm, n = 15, groups = "origem")

ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")

# LISTA VERSUS TRADICIONAL:

estadual <- corpus_subset(corpus, origem == "estadual")  # Only cases that discuss state legislation.
lt_estadual <- corpus_subset(estadual, tipo_julgamento %in% c("lista", "tradicional"))  # Selects cases judged either "em lista" or tradionally.
dfm_lt_estadual <- dfm(lt_estadual, groups = "tipo_julgamento", tolower = T, remove = stop, case_insensitive = T,
                       remove_punct = TRUE, remove_symbols = T, remove_url = T, remove_numbers = T)  # Constructs DFM for lt_estadual.
keyness_lt_estadual <- textstat_keyness(dfm_lt_estadual, target = "lista")

federal <- corpus_subset(corpus, origem == "federal")  # Only cases that discuss federal legislation.
lt_federal <- corpus_subset(federal, tipo_julgamento %in% c("lista", "tradicional"))  # Selects cases judged either "em lista" or tradionally.
dfm_lt_federal <- dfm(lt_federal, groups = "tipo_julgamento", tolower = T, remove = stop, case_insensitive = T,
                      remove_punct = TRUE, remove_symbols = T, remove_url = T, remove_numbers = T)  # Constructs DFM for lt_federal.
dfm_lt_federal <- dfm_remove(dfm_lt_federal, pattern = c("6baa-5d1a-8eaf-5827", "3b45-f891-9b0e-2352", 
                                                         "cda3-6f46-1044-1b5f", "f624-2e17-bd4b-5bf0", "a2b2-4bd2-d7f7-43ac", 
                                                         "c581-8d42-1b70-16fc", "0716-43fd-06aa-84c6", "e2b7-54de-3124-e570"))
keyness_lt_federal <- textstat_keyness(dfm_lt_federal, target = "lista")

## PLOTS RESULTS:
textplot_keyness(keyness_lt_estadual) 
textplot_keyness(keyness_lt_federal)  


## VERIFYING HYPOTHESIS:
plan2020_topic <- read_csv("~/R Projects/stf_text_mining/planilha2020_processada_bytopic.csv")
only_lista_state <- plan2020_topic %>% filter(origem == "estadual", tipo_julgamento == "lista")
only_lista_federal <- plan2020_topic %>% filter(origem == "federal", tipo_julgamento == "lista")
only_state <- plan2020_topic %>% filter(origem == "estadual")
only_federal <- plan2020_topic %>% filter(origem == "federal")
count_state <- only_state %>% group_by(main_topic) %>% count()
count_federal <- only_federal %>% group_by(main_topic) %>% count()
count_le <- only_lista_state %>% group_by(main_topic) %>% count()
count_lf <- only_lista_federal %>% group_by(main_topic) %>% count()
count_geral <- plan2020_topic %>% group_by(main_topic) %>% count()

## LISTA VERSUS TRADICIONAL USING KEYWORDS:


