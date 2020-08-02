# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid(
  "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", "ne_50m_admin_0_countries", encoding="UTF-8")

# Pretvorimo v numeriko

TD_cena_energentov$leto_in_cetrtletje <- as.numeric(as.character(TD_cena_energentov$leto_in_cetrtletje))
TD_slo_obnovljivi$`proizvodnja_(GWh)` <- as.numeric(as.character(TD_slo_obnovljivi$`proizvodnja_(GWh)`))
TD_slo_obnovljivi[is.na(TD_slo_obnovljivi)] <- 0

TD_world_obnovljivi$`obnovljiva_energija_(GWh)` <- as.numeric(gsub(",","",TD_world_obnovljivi$`obnovljiva_energija_(GWh)`))
TD_world_obnovljivi$`%_obnovljive_energije_iz_hidroelektrarn` <- as.numeric(gsub("%","",TD_world_obnovljivi$`%_obnovljive_energije_iz_hidroelektrarn`))


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
  geom_path(size=2, color="blue") + xlab('leto') + ylab('cena elektrike - cena plina (EUR/kWh)') + ggtitle('Razlika cen elektrike in plina skozi čas')

#graf3 <- napredna analiza

graf4 <- ggplot(data=TD_poraba_gospodinjstev %>% filter(
  energetski_vir == "Utekočinjeni naftni plin (t)" | energetski_vir == "Premog (t)" | 
  energetski_vir == "Lesni sekanci, briketi in ostanki (t)" | energetski_vir == "Lesni peleti (t)" |
  energetski_vir == "Ekstra lahko kurilno olje (t)" | energetski_vir == "Drva - polena (t)"), aes(x=leto, y=poraba, fill=energetski_vir)) + 
  geom_bar(stat="identity") + xlab('leto') + ylab('letna poraba (t)') + ggtitle('Poraba energetskih virov skozi čas')

graf5 <- ggplot(data=TD_poraba_gospodinjstev %>% filter(
  energetski_vir == "Toplota iz okolice (TJ)" | energetski_vir == "Sončna energija (TJ)" | 
  energetski_vir == "Daljinska toplota (TJ)"), aes(x=leto, y=poraba, fill=energetski_vir)) + 
  geom_bar(stat="identity") + xlab('leto') + ylab('letna poraba (TJ)') + ggtitle('Poraba energetskih virov skozi čas')

#graf6 <- napredna analiza

graf7 <- ggplot(data = TD_slo_obnovljivi %>% filter(
  elektrarna == "Hidroelektrarne (GWh)" | elektrarna == "Termoelektrarne (GWh)" |
  elektrarna == "Jedrske elektrarne (GWh)" | elektrarna == "Sončne elektrarne (GWh)"| 
  elektrarna == "Vetrne elektrarne (GWh)"), aes(x=leto, y=`proizvodnja_(GWh)`, fill=elektrarna)) + 
  geom_area(aes(group=`elektrarna`)) + xlab('leto') + ylab('proizvodnja (GWh)') + ggtitle('Proizvodnja obnovljive energija po elektrarnah') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

graf8 <- ggplot(data = TD_slo_obnovljivi %>% filter(
  leto == 2018, elektrarna == "Hidroelektrarne (GWh)" | elektrarna == "Termoelektrarne (GWh)" |
    elektrarna == "Jedrske elektrarne (GWh)" | elektrarna == "Sončne elektrarne (GWh)"| 
    elektrarna == "Vetrne elektrarne (GWh)"), aes(x="", y=`proizvodnja_(GWh)`, fill=elektrarna)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Dark2") + 
  geom_text(aes(label = paste(round(`proizvodnja_(GWh)` / sum(`proizvodnja_(GWh)`) * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  ggtitle('Tortni diagram za proizvodnjo obnovljive energije z odstotki za leto 2018') + theme(axis.text = element_blank(),
  axis.ticks = element_blank(), panel.grid  = element_blank(), axis.title.x = element_blank(),
  axis.title.y = element_blank())

zemljevid1 <- tm_shape(merge(zemljevid, TD_world_obnovljivi %>% group_by(drzave), by.x="SOVEREIGNT", by.y="drzave")) +
  tm_polygons("obnovljiva_energija_(GWh)", style = "kmeans", legend.hist = TRUE) + 
  tm_layout(legend.outside = TRUE, main.title = "Celotna pridelana obnovljiva energija")

zemljevid2 <- tm_shape(merge(zemljevid, TD_world_obnovljivi %>% group_by(drzave), by.x="SOVEREIGNT", by.y="drzave")) +
  tm_polygons("%_obnovljive_energije_iz_hidroelektrarn", style = "fixed", breaks = c(0, 20, 40, 60, 80, 100), palette = "Blues") + 
  tm_layout(legend.outside = TRUE, main.title = "Odstotek obnovljive energije iz hidroelektrarn")


