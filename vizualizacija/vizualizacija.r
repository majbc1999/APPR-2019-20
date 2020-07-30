# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid(
  "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", "ne_50m_admin_0_countries", encoding="UTF-8")

# Pretvorimo v numeriko

TD_cena_energentov$leto_in_cetrtletje <- as.numeric(as.character(TD_cena_energentov$leto_in_cetrtletje))


# Ustvarjanje novih podatkov
samo_elektrika <- TD_cena_energentov %>% filter(energent == "Električna energija")
names(samo_elektrika) <- c("energent", "leto", "cena_elektrika")
samo_plin <- TD_cena_energentov %>% filter(energent == "Zemeljski plin")
names(samo_plin) <- c("energent", "leto", "cena_plin")
primerjava_cen <- merge(samo_elektrika,samo_plin,by="leto")
primerjava_cen[,c(2,4)] <- NULL
primerjava_cen$razlika <- with(primerjava_cen, cena_elektrika - cena_plin)

# Tukaj so sedaj grafi

graf1 <- ggplot(data=TD_cena_energentov, aes(x=leto_in_cetrtletje, y=`cena_(EUR/kWh)`, col=energent)) + 
  geom_point() + geom_line() +xlab('leto') + ylab('cena (EUR/kWh)') + ggtitle('Gibanje cene električne energije in plina skozi čas')


graf2 <- ggplot(data=primerjava_cen, aes(x=leto, y=razlika)) + 
  geom_path(color="blue") + xlab('leto') + ylab('cena elektrike - cena plina (EUR/kWh)') + ggtitle('Razlika cen elektrike in plina skozi čas')
