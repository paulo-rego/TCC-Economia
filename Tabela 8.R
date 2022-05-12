# Tabela 8

library(tidyverse)

caminho_arquivo <- "C:\\Users\\pj_re\\Documents\\TCC - Paulo Rêgo\\dados\\cews_p1-p20_tbl1-en.csv"

# Leitura do arquivo
dados <- read.csv(caminho_arquivo,skip = 2)

# Remove notas de rodapé
dados <- dados[1:315,]

