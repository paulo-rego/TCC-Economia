# Gráficos 8 e 9

library(rbcb)
library(ggplot2)
library(dplyr)
library(ggpubr)


selic <- rbcb::get_series(c("Selic" = 432), start_date = "2018-01-01", end_date = "2021-12-31") %>%
         mutate(Selic = Selic/100)

juros_pequeno_porte <- rbcb::get_series(c("Juros PJ - Pequeno Porte" = 26429), start_date = "2018-01-01", end_date = "2021-12-31")

juros_micro_porte <- rbcb::get_series(c("Juros PJ - Médio Porte" = 26428), start_date = "2018-01-01", end_date = "2021-12-31")

# Gráfico 8

juros_pequeno_porte$Porte <- "Pequeno Porte"

juros_micro_porte$Porte <- "Microempresa"

selic$Porte <- "Selic"

names(juros_pequeno_porte) <- names(juros_micro_porte) <- c("Data", "Taxa Média de Juros", "Porte")

juros <- rbind(juros_pequeno_porte,juros_micro_porte) %>%
         mutate(`Taxa Média de Juros` = `Taxa Média de Juros`/100)

plot1 <- ggplot() +
         geom_line(data = juros, aes(x = Data, y = `Taxa Média de Juros`, color = Porte), size = 2) +
         geom_line(data = selic, aes(x = date, y = Selic, color = Porte), size = 2) +
         scale_color_brewer(palette="Set2") +
         scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.60, by = 0.02)) +
         scale_x_date( date_labels = "%m/%Y", date_breaks = "3 months") +
         labs(title = "Taxa média de juros das operações de crédito por porte (%a.a)", color = element_blank()) +
         ylab(element_blank()) +
         xlab(element_blank()) +
         theme_classic() +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot1

# Gráfico 9

qtd_operacoes_micro <- rbcb::get_series(c("Quantidade de Operações - Micro" = 25713), start_date = "2018-01-01", end_date = "2021-12-31")

qtd_operacoes_pequena <- rbcb::get_series(c("Quantidade de Operações - Pequeno Porte" = 25714), start_date = "2018-01-01", end_date = "2021-12-31")

qtd_operacoes_micro$Porte <- "Microempresa"

qtd_operacoes_pequena$Porte <- "Pequeno Porte"

names(qtd_operacoes_micro) <- names(qtd_operacoes_pequena) <- c("Data","Quantidade de operações de crédito", "Porte")

qtd_operacoes <- rbind(qtd_operacoes_micro, qtd_operacoes_pequena)


plot2 <- ggplot() +
  geom_col(data = qtd_operacoes, aes(x = Data, y = `Quantidade de operações de crédito`, fill= Porte), position = "dodge") +
  scale_color_brewer(palette="Set2") +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.60, by = 0.02)) +
  scale_x_date( date_labels = "%m/%Y", date_breaks = "3 months") +
  labs(title = "Quantidade operações de crédito por porte (milhões)", color = element_blank()) +
  ylab(element_blank()) +
  xlab(element_blank()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


plot2



# Gráfico 10

valor_medio_operacoes_micro <- rbcb::get_series(c("Valor Médio das Operações" = 25715), start_date = "2018-01-01", end_date = "2021-12-31")

valor_medio_operacoes_pequena <- rbcb::get_series(c("Valor Médio das Operações" = 25716), start_date = "2018-01-01", end_date = "2021-12-31")

valor_medio_operacoes_micro$Porte <- "Microempresa"

valor_medio_operacoes_pequena$Porte <- "Pequeno Porte"


valor_medio_operacoes <- rbind(valor_medio_operacoes_micro,valor_medio_operacoes_pequena)

names(valor_medio_operacoes) <- c("Data", "Valor Médio das Operações", "Porte")

plot3 <- ggplot() +
  geom_line(data = valor_medio_operacoes, aes(x = Data, y = `Valor Médio das Operações`, color = Porte), size = 2) +
  scale_color_brewer(palette="Set2") +
  scale_x_date( date_labels = "%m/%Y", date_breaks = "3 months") +
  labs(title = "Valor Médio das Operações por porte (R$)", color = element_blank()) +
  ylab(element_blank()) +
  xlab(element_blank()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3

# Gráfico 11


inadimplencia_micro <- rbcb::get_series(c("Taxa de Inadimplência" = 26202), start_date = "2018-01-01", end_date = "2021-12-31")

inadimplencia_pequena <- rbcb::get_series(c("Taxa de Inadimplência" = 26203), start_date = "2018-01-01", end_date = "2021-12-31")

inadimplencia_micro$Porte <- "Microempresa"

inadimplencia_pequena$Porte <- "Pequeno Porte"

names(inadimplencia_micro) <- names(inadimplencia_pequena) <- c("Data", "Taxa de inadimplência", "Porte")

inadimplencia <- rbind(inadimplencia_micro,inadimplencia_pequena) %>%
                 mutate(`Taxa de inadimplência` = `Taxa de inadimplência`/100)

plot4 <- ggplot() +
  geom_line(data = inadimplencia, aes(x = Data, y = `Taxa de inadimplência`, color = Porte), size = 2) +
  scale_color_brewer(palette="Set2") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.14, by = 0.01)) +
  scale_x_date( date_labels = "%m/%Y", date_breaks = "3 months") +
  labs(title = "Taxa de inadimplência (% de operações com mais de 90 dias de atraso)", color = element_blank()) +
  ylab(element_blank()) +
  xlab(element_blank()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4


plot5 <- ggarrange(plot1,plot2,plot3,plot4)


plot5

inadimplencia_mpes <- rbcb::get_series(c("Taxa de Inadimplência - MPEs" = 27703), start_date = "2018-01-01", end_date = "2021-12-31")

inadimplencia_grande <- rbcb::get_series(c("Taxa de Inadimplência - MPEs" = 27704), start_date = "2018-01-01", end_date = "2021-12-31")

plot(inadimplencia_micro)
plot(inadimplencia_grande)
