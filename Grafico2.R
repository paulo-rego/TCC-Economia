# Gráfico 2

library(tidyverse)

# caminho_arquivo <- "C:\\Users\\pj_re\\Documents\\TCC - Paulo Rêgo\\dados\\2019 MSME-EI Database.xlsx"

caminho_arquivo <- "C:\\Users\\pj_re\\Documents\\TCC - Paulo Rêgo\\dados\\SSIS_BSC_ISIC4_19102021155521251.csv"

caminho_paises <- "C:\\Users\\pj_re\\Documents\\TCC - Paulo Rêgo\\dados\\Tabela de Códigos de Países.xlsx"

paises <- openxlsx::read.xlsx(caminho_paises) %>% select(1,3)

names(paises) = c("País","Codigo")

dados <- readr::read_csv(caminho_arquivo)

dados <- dados[!is.na(dados$Value),]

dados2 <- dados %>%
          group_by(Country, Time, `Size Class`) %>%
          summarise(empregados = sum(Value)) %>%
          filter(empregados > 0)

dados3 <- dados2 %>%
          group_by(Country, `Size Class`) %>%
          slice(which.max(Time))

totais <- dados3 %>%
          group_by(Country) %>%
          filter(`Size Class` != "1-249 Empregados (MPEs)") %>%
          summarise(total = sum(empregados))

dados3 <- dados3 %>%
          left_join(totais)

dados3 <- dados3 %>% mutate(porcentagem = empregados/total)

dados_long <- reshape2::melt(dados3 %>% select(-Time, -empregados, -total), id = c("Country", "Size Class"), value.name = "porcentagem") %>%  select(-variable)

dados_long2 <- dados_long %>%
               filter(`Size Class` %in% c("1-249 Empregados (MPEs)", "250+ Empregados"))

plot <- ggplot(dados_long2, aes(x = fct_reorder2(Country,`Size Class`,-porcentagem), y = porcentagem, fill = `Size Class`)) +
        geom_col(position = "fill", width = 0.8) +
        labs(fill = "Tamanho das empresas") +
        ylab("% de empresas") +
        xlab(element_blank())+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_fill_brewer(palette="Set2") +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = "Empregados por tamanho da empresa",
             subtitle = "Países seletos")


plot




cond_iniciais <- data.frame(matrix(ncol = 7, nrow = 2))
names(cond_iniciais) <- c("Pop. Adulta","Pop. Total","PEA","PO", "Produto", "Produto por Adultos", "Prod per capita")
