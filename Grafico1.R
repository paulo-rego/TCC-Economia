# Gráfico 1

library(tidyverse)
library(forcats)

caminho_arquivo <- "C:\\Users\\pj_re\\Documents\\TCC - Paulo Rêgo\\dados\\OECD 2017 - Número de empresas por porte.xlsx"

dados <- openxlsx::read.xlsx(caminho_arquivo)

dados_long <- reshape2::melt(dados %>% select(-Sigla), id = "Nome", value.name = "porcentagem")

dados_long$variable <- forcats::fct_rev(dados_long$variable)

dados_long$porcentagem <- dados_long$porcentagem/100
plot <- ggplot(dados_long, aes(x = fct_reorder2(Nome,variable,porcentagem), y = porcentagem, fill = variable)) +
        geom_col(position = "fill", width = 0.8) +
        labs(fill = "Número de empregados") +
        ylab("% de empresas") +
        xlab(element_blank())+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_fill_brewer(palette="Set2") +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = "Quantidade de empresas por número de empregados",
             subtitle = "Países seletos")

plot

fct_reorder(Nome, porcentagem)

