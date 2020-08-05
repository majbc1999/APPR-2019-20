# 4. faza: Analiza podatkov

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Model rasti za elektriko in zemeljski plin
#-----------------------------------------------------------------------------------------------------------------------------------------------

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

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Model rasti za deleže gospodinjstev
#-----------------------------------------------------------------------------------------------------------------------------------------------

poraba_TD1 <- TD_poraba_gospodinjstev %>% filter(energetski_vir == "Toplota iz okolice (TJ)")
poraba_TD2 <- TD_poraba_gospodinjstev %>% filter(energetski_vir == "Sončna energija (TJ)")
poraba_TD3 <- TD_poraba_gospodinjstev %>% filter(energetski_vir == "Daljinska toplota (TJ)")
poraba_TD1$energetski_vir <- NULL
poraba_TD2$energetski_vir <- NULL
poraba_TD3$energetski_vir <- NULL
poraba_TD1$leto <- as.numeric(as.character(poraba_TD1$leto))
poraba_TD2$leto <- as.numeric(as.character(poraba_TD1$leto))
poraba_TD3$leto <- as.numeric(as.character(poraba_TD1$leto))


# Toplota
modelporabetoplote <- lm(data=poraba_TD1, poraba ~ log(leto))
napoved3 <- predict(modelporabetoplote, data.frame(leto=c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030)))
poraba_TD4 <- poraba_TD1
poraba_TD4 <- data.frame(leto = c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030), 
                            poraba = c(napoved3[1], napoved3[2], napoved3[3], napoved3[4], napoved3[5], napoved3[6], napoved3[7], napoved3[8], 
                                       napoved3[9], napoved3[10], napoved3[11], napoved3[12]))
poraba_TD1 <- rbind(poraba_TD1, poraba_TD4)
names(poraba_TD1) <- c("leto", "porabatoplote")

# Sončna energija
modelporabesonca <- lm(data=poraba_TD2, poraba ~ log(leto))
napoved4 <- predict(modelporabesonca, data.frame(leto=c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030)))
poraba_TD5 <- poraba_TD2
poraba_TD5 <- data.frame(leto = c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030), 
                         poraba = c(napoved4[1], napoved4[2], napoved4[3], napoved4[4], napoved4[5], napoved4[6], napoved4[7], napoved4[8], 
                                    napoved4[9], napoved4[10], napoved4[11], napoved4[12]))
poraba_TD2 <- rbind(poraba_TD2, poraba_TD5)
names(poraba_TD2) <- c("leto", "porabasoncne")

# Daljinska toplota
modelporabedaljinske <- lm(data=poraba_TD3, poraba ~ log(leto))
napoved5 <- predict(modelporabedaljinske, data.frame(leto=c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030)))
poraba_TD6 <- poraba_TD3
poraba_TD6 <- data.frame(leto = c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030), 
                         poraba = c(napoved5[1], napoved5[2], napoved5[3], napoved5[4], napoved5[5], napoved5[6], napoved5[7], napoved5[8], 
                                    napoved5[9], napoved5[10], napoved5[11], napoved5[12]))
poraba_TD3 <- rbind(poraba_TD3, poraba_TD6)
names(poraba_TD3) <- c("leto", "porabadaljinske")

tabelanapovedi2 <- merge(poraba_TD1, poraba_TD2, by=c("leto"))
tabelanapovedi2 <- merge(tabelanapovedi2, poraba_TD3, by=c("leto"))

tabelanapovedi2$celotnaporaba <- with(tabelanapovedi2, porabatoplote + porabasoncne + porabadaljinske)
tabelanapovedi2$od1 <- with(tabelanapovedi2, 100 * porabatoplote / celotnaporaba)
tabelanapovedi2$od2 <- with(tabelanapovedi2, 100 * porabasoncne / celotnaporaba)
tabelanapovedi2$od3 <- with(tabelanapovedi2, 100 * porabadaljinske / celotnaporaba)

names(tabelanapovedi2) <- c("leto", "poraba1", "poraba2", "poraba3", "celotnaporaba", "Toplota iz okolice (%)", "Sončna energija (%)", "Daljinska toplota (%)")

tabelanapovedi2 <- tabelanapovedi2 %>% gather("energent","odstotek", 6:8)

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Grafi
#-----------------------------------------------------------------------------------------------------------------------------------------------

graf3 <- ggplot(data=tabelanapovedi, aes(x=leto_in_cetrtletje)) +
  geom_point(aes(y= `cena_elektrike_(EUR/kWh)`), colour = "red") +
  geom_line(aes(y= `cena_elektrike_(EUR/kWh)`), colour = "red") +
  geom_point(aes(y= `cena_plina_(EUR/kWh)`), colour = "blue") +
  geom_line(aes(y= `cena_plina_(EUR/kWh)`), colour = "blue") +
  xlab('leto') + ylab('cena (EUR/kWh)') + ggtitle('Napoved gibanja cene električne energije in plina') +
  geom_smooth(method="lm", aes(y= `cena_elektrike_(EUR/kWh)`, colour="blue")) +
  geom_smooth(method="lm", aes(y= `cena_plina_(EUR/kWh)`, colour="red"))

graf6 <- ggplot(data=tabelanapovedi2, aes(x=leto, y=odstotek, fill=energent)) + 
  geom_bar(stat="identity", palette = "Blues") + xlab('leto') + ylab('letna poraba (%)') + ggtitle('Deleži porab, merljivih v TJ') +
  scale_fill_manual("legend", values = c("Toplota iz okolice (%)" = "#173F5F", "Sončna energija (%)" = "#20639B", "Daljinska toplota (%)" = "#3CAEA3"))
