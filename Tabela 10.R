# Gráficos 6 e 7

library(tidyverse)
library(fs)
library(hrbrthemes)


pasta_dados <- "C:\\Users\\pj_re\\Documents\\TCC - Paulo Rêgo\\dados\\IFData"

caminho_arquivos <- fs::dir_ls(pasta_dados)

dados <- caminho_arquivos %>%
         purrr::map_dfr(readr::read_csv2, na = c("","NA","NI"))


transf_eixo_bilhoes <- scales::number_format(scale = 1e-9, accuracy = .1, decimal.mark = ",")

carteiras_por_porte <- dados %>%
                       select(Data,Micro, Pequena, Média, Grande) %>%
                       group_by(Data) %>%
                       summarise(across(everything(), sum, na.rm = T)) %>%
                       filter(!is.na(Data))

carteiras_long <- carteiras_por_porte %>%
                  pivot_longer(names = c("Data", "Porte", "Valor"),cols = c(Micro,Pequena,Média, Grande))

carteiras_long$Data <- parse_date(carteiras_long$Data, format = "%b/%y", locale = locale("pt"))


carteiras_long <- carteiras_long %>%
                  mutate(value = 1000*value)

carteiras_long <-  carteiras_long %>%
                   group_by(Data) %>%
                   mutate(Total = sum(value)) %>%
                   mutate(prop = value/Total)

names(carteiras_long) <- c("Data", "Porte", "Valor", "Total Mês", "Proporção")

carteiras_long$Porte_factor <- factor(carteiras_long$Porte, levels = c("Micro","Pequena","Média","Grande"))





plot1 <- ggplot(data = carteiras_long, aes(x = Data, y = Valor, fill = Porte_factor)) +
         #geom_col(position = "dodge") +
         geom_area(position = position_stack(reverse = T)) +
         scale_fill_brewer(palette="Set2") +
         scale_y_continuous(labels = transf_eixo_bilhoes) +
         scale_x_date( date_labels = "%m/%Y",breaks = unique(carteiras_long$Data)) +
         ylab(element_blank()) +
         labs(title="Carteira Ativa de Crédito PJ por porte do tomador (R$ Bilhões)", fill = "Porte") +
         theme_classic()

plot2 <- ggplot(data = carteiras_long, aes(x = Data, y = Proporção, fill = Porte_factor)) +
         geom_col(position = "dodge") +
         scale_fill_brewer(palette="Set2") +
         scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.6, by =0.05 )) +
         scale_x_date( date_labels = "%m/%Y",breaks = unique(carteiras_long$Data)) +
         ylab(element_blank()) +
         labs(title="Participação na Carteira Ativa PJ por porte", fill = "Porte") +
         theme_classic()


plot1
plot2


