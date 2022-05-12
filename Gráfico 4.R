# Gráfico 4

library(rbcb)

selic <- rbcb::get_series(c("Selic" = 432), start_date = "2020-01-01", end_date = "2021-12-31")

taxas <- selic

taxas$adicional_pronampe <- 1.25

taxas$adicional_pronampe[which(taxas$date > "2020-12-31")] <- 6.0

taxas <- taxas %>%
         mutate(`Pronampe` = Selic + adicional_pronampe)

taxas_long <- taxas %>%
              select(-adicional_pronampe) %>%
              pivot_longer(cols = c(Selic, `Pronampe`)) %>%
              mutate(value = value/100)

plot1 <- ggplot(taxas_long, aes(x = date, color = name, y = value)) +
         geom_line(size = 2) +
         scale_color_brewer(palette="Set2") +
         scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.18, by = 0.01)) +
         scale_x_date( date_labels = "%m/%Y", date_breaks = "1 month") +
         labs(title = "Taxa Selic e Taxa Máxima do Pronampe (% a.a.)", color = element_blank()) +
         ylab(element_blank()) +
         xlab(element_blank()) +
         theme_classic() +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot1

