# Gráfico 5

caminho_arquivo <- "C:\\Users\\pj_re\\Documents\\TCC - Paulo Rêgo\\dados\\FGO - Pronampe.xlsx"

dados <- openxlsx::read.xlsx(caminho_arquivo)

dados$`Mês/Ano` <- as.Date(dados$`Mês/Ano`, origin = "1899-12-30")

dados$Valor.Contratado <- dados$Valor.Contratado * 1000

dados$`Qtd. de Operações (acum.)` <- cumsum(dados$Qtde.de.Operações)

dados$`Valor.Contratado (acum.)` <- cumsum(dados$Valor.Contratado)

transf_eixo_bilhoes <- scales::number_format(scale = 1e-9, accuracy = .1, decimal.mark = ",")

transf_eixo_milhares <- scales::number_format(scale = 1e-3, accuracy = .1, decimal.mark = ",")

plot1 <- ggplot(data = dados, aes(x= `Mês/Ano`, y = `Valor.Contratado (acum.)`)) +
         geom_line(size = 2,aes(color = "Acumulado")) +
         geom_col(aes(y = Valor.Contratado, fill = "Mensal"), alpha = 0.5) +
         ylab("Valor Contratado (R$ Bilhões)") +
         xlab(element_blank()) +
         theme_classic() +
         #scale_color_brewer(palette="Set2") +
         scale_x_date(date_breaks = "1 month", date_labels = "%m/%y") +
         scale_y_continuous(labels = transf_eixo_bilhoes) +
         labs(title = "Valores Contratados no Pronampe (R$ Bilhões)") +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
         scale_fill_manual(name = NULL, values = c("Mensal" = "green")) +
         scale_color_manual(name = NULL, values = c("Acumulado" = "darkgreen"))


plot2 <- ggplot(data = dados, aes(x= `Mês/Ano`, y = `Qtd. de Operações (acum.)`)) +
         geom_line(size = 2,aes(color = "Acumulado")) +
         geom_col(aes(y = Qtde.de.Operações, fill = "Mensal"), alpha = 0.5) +
         ylab(element_blank()) +
         xlab(element_blank()) +
         theme_classic() +
         scale_x_date(date_breaks = "1 month", date_labels = "%m/%y") +
         scale_y_continuous(labels = transf_eixo_milhares) +
         labs(title = "Quantidade de Operações Contratadas (milhares)") +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
         scale_fill_manual(name = NULL, values = c("Mensal" = "green")) +
         scale_color_manual(name = NULL, values = c("Acumulado" = "darkgreen"))

ggarrange(plot1,plot2, ncol = 1,common.legend = T,legend = "right")

