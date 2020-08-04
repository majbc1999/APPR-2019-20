# 4. faza: Analiza podatkov

# Model rasti za elektriko in zemeljski plin
elektrika_TD <- elektrika_TDa
plin_TD <- plin_TDa

names(elektrika_TD) <- c("a","b")
elektrika_TD$a <- as.numeric(as.character(elektrika_TD$a))
elektrika_TD$b <- as.numeric(as.character(elektrika_TD$b))

names(plin_TD) <- c("a","b")
plin_TD$a <- as.numeric(as.character(plin_TD$a))
plin_TD$b <- as.numeric(as.character(plin_TD$b))



modelelektrika <- lm(data=elektrika_TD, b ~ a)
napoved <- predict(modelelektrika, data.frame(a=c(2020, 2021, 2022, 2023, 2024, 2025)))
elektrika_TD1 <- elektrika_TD
elektrika_TD1 <- data.frame(a = c(2020, 2021, 2022, 2023, 2024, 2025), 
                            b = c(napoved[1], napoved[2], napoved[3], napoved[4], napoved[5], napoved[6]))

names(elektrika_TD1) <- c("a", "b")

elektrika_TD$vrsta1 <- "podatki"
elektrika_TD1$vrsta1 <- "napoved"
elektrika_TD <- rbind(elektrika_TD, elektrika_TD1)

names(elektrika_TD) <- c("leto_in_cetrtletje", "cena_elektrike_(EUR/kWh)", "vrsta1")





modelplin <- lm(data=plin_TD, b ~ a)
napoved2 <- predict(modelplin, data.frame(a=c(2020, 2021, 2022, 2023, 2024, 2025)))
plin_TD1 <- plin_TD
plin_TD1 <- data.frame(a = c(2020, 2021, 2022, 2023, 2024, 2025), 
                            b = c(napoved2[1], napoved2[2], napoved2[3], napoved2[4], napoved2[5], napoved2[6]))

names(plin_TD1) <- c("a", "b")

plin_TD$vrsta2 <- "podatki"
plin_TD1$vrsta2 <- "napoved"
plin_TD <- rbind(plin_TD, plin_TD1)

names(plin_TD) <- c("leto_in_cetrtletje", "cena_plina_(EUR/kWh)", "vrsta2")


rownames(elektrika_TD) <- 1:nrow(elektrika_TD)
rownames(plin_TD) <- 1:nrow(plin_TD)

tabelanapovedi <- merge(elektrika_TD, plin_TD, by="leto_in_cetrtletje")
tabelanapovedi$leto_in_cetrtletje <- as.numeric(as.character(tabelanapovedi$leto_in_cetrtletje))
tabelanapovedi$`cena_elektrike_(EUR/kWh)` <- as.numeric(as.character(tabelanapovedi$`cena_elektrike_(EUR/kWh)`))
tabelanapovedi$`cena_plina_(EUR/kWh)` <- as.numeric(as.character(tabelanapovedi$`cena_plina_(EUR/kWh)`))

# Grafi

graf3 <- ggplot(data=tabelanapovedi, aes(x=leto_in_cetrtletje)) +
  geom_point(aes(y= `cena_elektrike_(EUR/kWh)`), colour = "red") +
  geom_line(aes(y= `cena_elektrike_(EUR/kWh)`), colour = "red") +
  geom_point(aes(y= `cena_plina_(EUR/kWh)`), colour = "blue") +
  geom_line(aes(y= `cena_plina_(EUR/kWh)`), colour = "blue") +
  xlab('leto') + ylab('cena (EUR/kWh)') + ggtitle('Napoved gibanja cene električne energije in plina') +
  geom_smooth(method="lm", aes(y= `cena_elektrike_(EUR/kWh)`, colour="blue")) +
  geom_smooth(method="lm", aes(y= `cena_plina_(EUR/kWh)`, colour="red"))
