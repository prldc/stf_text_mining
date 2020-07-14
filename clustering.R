library(tidyverse)
library(readtext)
library(quanteda)
library(data.table)
library(topicmodels)
library(tidytext)
library(viridis)
library(textmineR)


stop <- c("poder", "cf", "moreira", "alves", "tribunal", "infraestrutura", "dje", "l",
          "uma", "um", "muito", "há", "temos", "dizer", 
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
          "assinado", "digitalmente", "art", "número", "n", "s", "v", "icp-brasil", "portal", 
          "portal_autenticacao", "www_jus", 
          "autenticacao", "br_portal", "www", "http_www", "ministério_público", "jus_br",
          "autenticacao_autenticardocumento", "autenticardocumento", "autenticacao_autenticardocumento, 
          autenticardocumento", "autenticardocumento_asp", "net", "hdl", "https", "net_https", "https_hdl",
          "santa", "catarina", "santa_catarina", "sc", "paraíba", "ceará", "rondônia", "amapá", "mato", "grosso", 
          "mt", "mato_grosso", "handle_handle", "re_re", "norte", "grande_norte", "grande" , "rn", 
          "santo", "espírito", "espírito_santo", "es", "minas", "minas_gerais", "gerais", "mg", "sul", "grande_sul",
          "rs", "tocantins", "amazonas")

plan2020 <- read_csv("~/R Projects/stf_text_mining/planilha2020_processada.csv")  # Reads the table.

dtm <- CreateDtm(doc_vec = plan2020$acordao, # character vector of documents
                 doc_names = plan2020$nome, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("pt"), stop), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
)

tf_mat <- TermDocFreq(dtm)

tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf   # Calculates tf-idf.

tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))  # Calculates cosine similarity.

csim <- csim %*% t(csim)  

cdist <- as.dist(1 - csim)  # Converts cosine similarity to cosine distance

hc <- hclust(cdist, "ward.D")  # Performs hierarchical clustering using "ward.D" method.

# clustering <- cutree(hc, h = 0.3)  # Cuts dendrogram at height 0.3

clustering <- cutree(hc, 128)  # Cuts dendrogram in 128 clusters


# PLOTS DENDROGRAM

plot(hc, main = "Clusters de ADIs",
     ylab = "h", xlab = "", labels=FALSE)

rect.hclust(hc, 128, border = "red")  # Draws clusters.

# CREATES LINES AT HEIGHT h:

# abline(h= 0.3,col = 'blue')
# abline(h= 1,col = 'red')
# abline(h= 10,col = 'green')


# TOP 10 MOST FREQUENT WORDS PER CLUSTER

p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  rows <- rows[ , colSums(rows) > 0 ]
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})

cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:10 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)

write.csv(cluster_summary, "clusters.csv", row.names = F)  # Exports to csv,

# ADDS CLUSTER GROUPS TO MAIN TABLE

clusters <- as.data.frame(clustering)
clusters <- tibble::rownames_to_column(clusters, "nome")

plan2020 <- plan2020 %>%
  inner_join(clusters, by = c("nome" = "nome"))

# write.csv(plan2020, 'plan2020_clusters.csv', row.names = F)


