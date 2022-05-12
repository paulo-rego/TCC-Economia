# Gráfico 3

library(tidyverse)
library(lubridate)
library(ggplot2)

caminho_arquivo <- "C:\\Users\\pj_re\\Documents\\TCC - Paulo Rêgo\\dados\\PESE.xlsx"

dados <- readxl::read_xlsx(caminho_arquivo, sheet = 2)

dados$`Data Movto 2` <- lubridate::as_date(dados$`Data Movto`, format = "%d/%m/%y")

dados_todos <- dados %>%
               filter(`Inst. Financ.` == 'TODOS' & UF == 'BR') %>%
               arrange(`Data Movto 2`)

transf_eixo_bilhoes <- scales::number_format(scale = 1e-9, accuracy = .1, decimal.mark = ",")

transf_eixo_milhoes <- scales::number_format(scale = 1e-6, accuracy = .1, decimal.mark = ",")

transf_eixo_milhares <- scales::number_format(scale = 1e-3, accuracy = .1, decimal.mark = ",")


plot1 <- ggplot(data = dados_todos, aes(x= `Data Movto 2`, y = `Vl. Financiado (Acum.)`)) +
        geom_line(size = 2, aes(color = "darkgreen")) +
        #geom_point(color = "green") +
        geom_col(aes(y = `Vl. Financiado`, color = "gray")) +
        ylab("Valor Concedido (R$ Bilhões)") +
        xlab(element_blank()) +
        theme_classic() +
        scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y") +
        scale_y_continuous(labels = transf_eixo_bilhoes) +
        labs(title = "Valor Concedido no Pese") +
        scale_color_identity( name = "Valores", breaks = c("darkgreen", "lightgreen"), labels = "Acumulado", "Diário")

plot2 <- ggplot(data = dados_todos, aes(x= `Data Movto 2`, y = `#Empregados (Acum.)` )) +
  geom_line(size = 2, color = "darkgreen") +
  #geom_point(color = "green") +
  geom_col(aes(y = `#Empregados`)) +
  ylab("Número de Empregados Cobertos (milhões)") +
  xlab(element_blank()) +
  theme_classic() +
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y") +
  scale_y_continuous(labels = transf_eixo_milhoes) +
  labs(title = "Empregados Beneficiados")

plot3 <- ggplot(data = dados_todos, aes(x= `Data Movto 2`, y = `#Empresas (Acum.)` )) +
  geom_line(size = 2, color = "darkgreen") +
  #geom_point(color = "green") +
  geom_col(aes(y = `#Empresas`)) +
  ylab("Número de empresas tomadoras (milhares)") +
  xlab("Data") +
  theme_classic() +
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y") +
  scale_y_continuous(labels = transf_eixo_milhares) +
  labs(title = "Empresas beneficiadas")


ggarrange(plot1,plot2,plot3, ncol = 1, nrow = 3)

plot1

