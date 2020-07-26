## RUN ONLY AFTER further_text_cleaning.R

require(tidyverse)
require(readtext)
require(quanteda)
require(topicmodels)
require(tidytext)
require(viridis)
require(data.table)
require(textmineR)
require(openxlsx)


stop <- c("poder", "cf", "moreira", "alves", "tribunal", "infraestrutura", "dje", "l",
          "uma", "um", "muito", "há", "temos", "dizer", 
          "essa", "me", "nós", "eu", "essa", "esta", "muito", "ó", "quando", "quanto", "rtj", 
          "mas", "foi", "ao", "par", "se", "88", "crfb", "c.f", "os", "que", "não",  
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
          "assinado", "digitalmente", "art", "número", "icp-brasil", "portal", 
          "portal_autenticacao", "www_jus", 
          "autenticacao", "br_portal", "www", "http_www", "ministério_público", "jus_br",
          "autenticacao_autenticardocumento", "autenticardocumento", "autenticacao_autenticardocumento, 
          autenticardocumento", "autenticardocumento_asp", "net", "hdl", "https", "net_https", "https_hdl",
          "santa", "catarina", "santa_catarina", "sc", "paraíba", "ceará", "rondônia", "amapá", "mato", "grosso", 
          "mt", "mato_grosso", "handle_handle", "re_re", "norte", "grande_norte", "grande" , "rn", 
          "santo", "espírito", "espírito_santo", "es", "minas", "minas_gerais", "gerais", "mg", "sul", "grande_sul",
          "rs", "tocantins", "amazonas", "xx", "pp", "a", "b", "c", "d", "f", "g", "h", "j", "k", "l", "m",
          "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "rj", "i", "ii", "iii", "iv", "v",
          "vi", "vii", "viii", "ix", "xi", "xii", "xiii", "ba", "ce", "ac", "am", "rr", "pa", "ap", "xiv",
          "to", "ma", "pi", "pe", "al", "se", "sp", "pr", "go", "la", "ex", "ª")


plan2020 <- read_csv("~/R Projects/stf_text_mining/planilha2020_processada*.csv")  # Reads the table.

dtm <- CreateDtm(doc_vec = plan2020$acordao, # character vector of documents
                 doc_names = plan2020$nome, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("pt"), stop),
                 lower = TRUE, 
                 remove_punctuation = TRUE, 
                 remove_numbers = TRUE
)

tf_mat <- TermDocFreq(dtm)

tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf   # Calculates tf-idf.

tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))  # Calculates cosine similarity.

csim <- csim %*% t(csim)  

cdist <- as.dist(1 - csim)  # Converts cosine similarity to cosine distance

hc <- hclust(cdist, "ward.D")  # Performs hierarchical clustering using "ward.D" method.

# clustering <- cutree(hc, h = 0.3)  # Cuts dendrogram at height 0.3

cluster_2 <- cutree(hc, 2)  # Cuts dendrogram in 2 clusters
cluster_4 <- cutree(hc, 4)  # Cuts dendrogram in 4 clusters
cluster_8 <- cutree(hc, 8)  # Cuts dendrogram in 8 clusters
cluster_16 <- cutree(hc, 16)  # Cuts dendrogram in 16 clusters
cluster_32 <- cutree(hc, 32)  # Cuts dendrogram in 32 clusters
cluster_64 <- cutree(hc, 64)  # Cuts dendrogram in 64 clusters
cluster_128 <- cutree(hc, 128)  # Cuts dendrogram in 128 clusters
cluster_256 <- cutree(hc, 256)  # Cuts dendrogram in 256 clusters
cluster_512 <- cutree(hc, 512)  # Cuts dendrogram in 512 clusters
cluster_50pct <- cutree(hc, 900)  # Cuts dendrogram in 50% 
cluster_60pct <- cutree(hc, 1081)  # Cuts dendrogram in 40%
cluster_70pct <- cutree(hc, 1261)  # Cuts dendrogram in 30% 
cluster_80pct <- cutree(hc, 1441)  # Cuts dendrogram in 20%
cluster_90pct <- cutree(hc, 1621)  # Cuts dendrogram in 10% 


# PLOTS DENDROGRAM

plot(hc, main = "Clusters de ADIs",
     ylab = "h", xlab = "", labels=FALSE)

rect.hclust(hc, 2, border = "purple")  # Draws clusters.
rect.hclust(hc, 4, border = "red")  # Draws clusters.
rect.hclust(hc, 8, border = "blue")  # Draws clusters.
rect.hclust(hc, 16, border = "orange")  # Draws clusters.
rect.hclust(hc, 32, border = "green")  # Draws clusters.
rect.hclust(hc, 64, border = "firebrick")  # Draws clusters.
rect.hclust(hc, 128, border = "turquoise")  # Draws clusters.
# rect.hclust(hc, 256, border = "pink")  # Draws clusters.
# rect.hclust(hc, 512, border = "brown")  # Draws clusters.
# rect.hclust(hc, 1081, border = "dark green")  # Draws clusters.
# rect.hclust(hc, 1287, border = "orange")  # Draws clusters.
# rect.hclust(hc, 1495, border = "blue")  # Draws clusters.
# rect.hclust(hc, 1709, border = "red")  # Draws clusters.
# rect.hclust(hc, 1621, border = "deepskyblue")  # Draws clusters.




# CREATES LINES AT HEIGHT h:

# abline(h= 0,col = 'deepskyblue')
# abline(1024, col = 'red')
# abline(h= 14,col = 'green')


# TOP 10 MOST FREQUENT WORDS PER CLUSTER

# cluster = cluster_90pct
# p_words <- colSums(dtm) / sum(dtm)
# 
# cluster_words <- lapply(unique(cluster), function(x){
#   rows <- dtm[ cluster == x , ]
#   rows <- rows[ , colSums(rows) > 0 ]
#   colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
# })
# 
# cluster_summary <- data.frame(cluster = unique(cluster),
#                               size = as.numeric(table(cluster)),
#                               top_words = sapply(cluster_words, function(d){
#                                 paste(
#                                   names(d)[ order(d, decreasing = TRUE) ][ 1:10 ],
#                                   collapse = ", ")
#                               }),
#                               stringsAsFactors = FALSE)


# CONVERTS CLUSTERS TO TIBBLES

clusters_2 <- as.data.frame(cluster_2)
clusters_2 <- tibble::rownames_to_column(clusters_2, "nome")

clusters_4 <- as.data.frame(cluster_4)
clusters_4 <- tibble::rownames_to_column(clusters_4, "nome")

clusters_8 <- as.data.frame(cluster_8)
clusters_8 <- tibble::rownames_to_column(clusters_8, "nome")

clusters_16 <- as.data.frame(cluster_16)
clusters_16 <- tibble::rownames_to_column(clusters_16, "nome")

clusters_32 <- as.data.frame(cluster_32)
clusters_32 <- tibble::rownames_to_column(clusters_32, "nome")

clusters_64 <- as.data.frame(cluster_64)
clusters_64 <- tibble::rownames_to_column(clusters_64, "nome")

clusters_128 <- as.data.frame(cluster_128)
clusters_128 <- tibble::rownames_to_column(clusters_128, "nome")

clusters_256 <- as.data.frame(cluster_256)
clusters_256 <- tibble::rownames_to_column(clusters_256, "nome")

clusters_512 <- as.data.frame(cluster_512)
clusters_512 <- tibble::rownames_to_column(clusters_512, "nome")

clusters_50pct <- as.data.frame(cluster_50pct)
clusters_50pct <- tibble::rownames_to_column(clusters_50pct, "nome")

clusters_60pct <- as.data.frame(cluster_60pct)
clusters_60pct <- tibble::rownames_to_column(clusters_60pct, "nome")

clusters_70pct <- as.data.frame(cluster_70pct)
clusters_70pct <- tibble::rownames_to_column(clusters_70pct, "nome")

clusters_80pct <- as.data.frame(cluster_80pct)
clusters_80pct <- tibble::rownames_to_column(clusters_80pct, "nome")

clusters_90pct <- as.data.frame(cluster_90pct)
clusters_90pct <- tibble::rownames_to_column(clusters_90pct, "nome")

# ADDING CLUSTERS TO TABLE

plan2020 <- plan2020 %>%
  inner_join(clusters_2, by = c("nome" = "nome")) %>%
  inner_join(clusters_4, by = c("nome" = "nome")) %>%
  inner_join(clusters_8, by = c("nome" = "nome")) %>%
  inner_join(clusters_16, by = c("nome" = "nome")) %>%
  inner_join(clusters_32, by = c("nome" = "nome")) %>%
  inner_join(clusters_64, by = c("nome" = "nome")) %>%
  inner_join(clusters_128, by = c("nome" = "nome")) %>%
  inner_join(clusters_256, by = c("nome" = "nome")) %>%
  inner_join(clusters_512, by = c("nome" = "nome")) %>%
  inner_join(clusters_50pct, by = c("nome" = "nome")) %>%
  inner_join(clusters_60pct, by = c("nome" = "nome")) %>%
  inner_join(clusters_70pct, by = c("nome" = "nome")) %>%
  inner_join(clusters_80pct, by = c("nome" = "nome")) %>%
  inner_join(clusters_90pct, by = c("nome" = "nome"))

write.csv(plan2020, 'planilha2020_processada*.csv', row.names = F)
write.xlsx(plan2020, 'planilha2020_processada*.xlsx', row.names = F)

# EXPORTING cluster_128 FOR QUALITATIVE ANALYSIS

nome_ementa <- plan2020 %>% filter(duplicado == F) %>% select(nome, data_julgamento, ementa, tipo_julgamento, cluster_128, dispositivo, resultado)
nome_ementa <- nome_ementa %>% inner_join(cluster_summary, by = c("cluster_128" = "cluster"))

write.xlsx(nome_ementa, 'clusters_128.xlsx', row.names = F)


lista <- plan2020 %>% filter(tipo_julgamento == "lista")
n_distinct(lista$cluster_128)
n_distinct(lista$cluster_90pct)
taxa_128_lista <- nrow(lista)/n_distinct(lista$cluster_128)
taxa_90pct_lista <- nrow(lista)/n_distinct(lista$cluster_90pct)
taxa_unanimidade_lista <- sum(lista$unanimidade)/nrow(lista)
virtual <- plan2020 %>% filter(tipo_julgamento == "virtual")
n_distinct(virtual$cluster_128)
n_distinct(virtual$cluster_90pct)
taxa_unanimidade_virtual <- sum(virtual$unanimidade)/nrow(virtual)
taxa_128_virtual <- nrow(virtual)/n_distinct(virtual$cluster_128)
taxa_90pct_virtual <- nrow(virtual)/n_distinct(virtual$cluster_90pct)
tradicional <- plan2020 %>% filter(tipo_julgamento == "tradicional", periodo == 5)
n_distinct(tradicional$cluster_128)
n_distinct(tradicional$cluster_90pct)
taxa_128_trad <- nrow(tradicional)/n_distinct(tradicional$cluster_128)
taxa_90pct_trad <- nrow(tradicional)/n_distinct(tradicional$cluster_90pct)
taxa_unanimidade_trad <- sum(tradicional$unanimidade)/nrow(tradicional)




