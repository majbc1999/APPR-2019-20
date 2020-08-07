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
TD_world_obnovljivi$`%_obnovljive_energije_iz_vetrnih elektrarn` <- as.numeric(gsub("%","",TD_world_obnovljivi$`%_obnovljive_energije_iz_vetrnih elektrarn`))
TD_world_obnovljivi$`%_obnovljive_energije_iz_biomase_in_odpadkov` <- as.numeric(gsub("%","",TD_world_obnovljivi$`%_obnovljive_energije_iz_biomase_in_odpadkov`))
TD_world_obnovljivi$`%_obnovljive_energije_iz_sončne energije` <- as.numeric(gsub("%","",TD_world_obnovljivi$`%_obnovljive_energije_iz_sončne energije`))
TD_world_obnovljivi$`%_obnovljive_energije_iz_geotermalne_energije` <- as.numeric(gsub("%","",TD_world_obnovljivi$`%_obnovljive_energije_iz_geotermalne_energije`))
TD_world_obnovljivi[is.na(TD_world_obnovljivi)] <- 0

# Ustvarjanje novih podatkov
samo_elektrika <- TD_cena_energentov %>% filter(energent == "Električna energija")
names(samo_elektrika) <- c("energent", "leto", "cena_elektrika")
samo_plin <- TD_cena_energentov %>% filter(energent == "Zemeljski plin")
names(samo_plin) <- c("energent", "leto", "cena_plin")
primerjava_cen <- merge(samo_elektrika,samo_plin,by="leto")
primerjava_cen[,c(2,4)] <- NULL
primerjava_cen$razlika <- with(primerjava_cen, cena_elektrika - cena_plin)

TD_world_obnovljivi[,"prevladujoči vir"] <- NA

for (vrstica in 1:nrow(TD_world_obnovljivi)) {
  he <- TD_world_obnovljivi[vrstica, "%_obnovljive_energije_iz_hidroelektrarn"]
  ve <- TD_world_obnovljivi[vrstica, "%_obnovljive_energije_iz_vetrnih elektrarn"]
  bm <- TD_world_obnovljivi[vrstica, "%_obnovljive_energije_iz_biomase_in_odpadkov"]
  se <- TD_world_obnovljivi[vrstica, "%_obnovljive_energije_iz_sončne energije"]
  ge <- TD_world_obnovljivi[vrstica, "%_obnovljive_energije_iz_geotermalne_energije"]
  if(he > ve & he > bm & he > se & he > ge){
    TD_world_obnovljivi[vrstica, "prevladujoči vir"] <- "Hidroenergija"
  }
  if(ve > he & ve > bm & ve > se & ve > ge){
    TD_world_obnovljivi[vrstica, "prevladujoči vir"] <- "Vetrna energija"
  }
  if(bm > ve & bm > he & bm > se & bm > ge){
    TD_world_obnovljivi[vrstica, "prevladujoči vir"] <- "Biomasa in odpadki"
  }
  if(se > ve & se > bm & se > he & se > ge){
    TD_world_obnovljivi[vrstica, "prevladujoči vir"] <- "Sončna energija"
  }
  if(ge > ve & ge > bm & ge > se & ge > he){
    TD_world_obnovljivi[vrstica, "prevladujoči vir"] <- "Geotermalna energija"
  }
}

TD_world_obnovljivi[is.na(TD_world_obnovljivi)] <- "Ni prevladujočega vira"

paleta1 = c("#5a1f1b","#3ee84c","#5285b9","#d5c7c7","#f9ff00","#f57822", "#d5c7c7")

# Tukaj so sedaj grafi

graf1 <- ggplot(data=TD_cena_energentov, aes(x=leto_in_cetrtletje, y=`cena_(EUR/kWh)`, col=energent)) + 
  geom_point() + geom_line() +xlab('leto') + ylab('cena (EUR/kWh)') + ggtitle('Gibanje cene električne energije in plina skozi čas')


graf2 <- ggplot(data=primerjava_cen, aes(x=leto, y=razlika)) + 
  geom_path(size=1.1, color="darkolivegreen") + xlab('leto') + ylab('cena elektrike - cena plina (EUR/kWh)') + ggtitle('Razlika cen elektrike in plina skozi čas')

#graf3 <- napredna analiza

graf4 <- ggplot(data=TD_poraba_gospodinjstev %>% filter(
  energetski_vir == "Utekočinjeni naftni plin (t)" | energetski_vir == "Premog (t)" | 
  energetski_vir == "Lesni sekanci, briketi in ostanki (t)" | energetski_vir == "Lesni peleti (t)" |
  energetski_vir == "Ekstra lahko kurilno olje (t)" | energetski_vir == "Drva - polena (t)"), aes(x=leto, y=poraba, fill=energetski_vir)) + 
  geom_bar(stat="identity") + xlab('leto') + ylab('letna poraba (t)') + ggtitle('Poraba energetskih virov skozi čas') +
  scale_fill_manual("Legenda", values = c("Utekočinjeni naftni plin (t)" = "#4C1D87", "Premog (t)" = "#FFBCFD", 
                                         "Lesni sekanci, briketi in ostanki (t)" = "#FFDC92", "Lesni peleti (t)"= "#DA9234",
                                         "Ekstra lahko kurilno olje (t)" = "#FF6000", "Drva - polena (t)" = "#926CB3"))

graf5 <- ggplot(data=TD_poraba_gospodinjstev %>% filter(
  energetski_vir == "Toplota iz okolice (TJ)" | energetski_vir == "Sončna energija (TJ)" | 
  energetski_vir == "Daljinska toplota (TJ)"), aes(x=leto, y=poraba, fill=energetski_vir)) + 
  geom_bar(stat="identity") + xlab('leto') + ylab('letna poraba (TJ)') + ggtitle('Poraba energetskih virov skozi čas') +
  scale_fill_manual("Legenda", values = c("Toplota iz okolice (TJ)" = "#173F5F", "Sončna energija (TJ)" = "#20639B", "Daljinska toplota (TJ)" = "#3CAEA3"))


#graf6 <- napredna analiza

graf7 <- ggplot(data = TD_slo_obnovljivi %>% filter(
  elektrarna == "Hidroelektrarne (GWh)" | elektrarna == "Termoelektrarne (GWh)" |
  elektrarna == "Jedrske elektrarne (GWh)" | elektrarna == "Sončne elektrarne (GWh)"| 
  elektrarna == "Vetrne elektrarne (GWh)"), aes(x=leto, y=`proizvodnja_(GWh)`, fill=elektrarna)) + 
  geom_area(aes(group=`elektrarna`)) + xlab('leto') + ylab('proizvodnja (GWh)') + ggtitle('Proizvodnja obnovljive energija po elektrarnah') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  scale_fill_manual("Legenda", values = c("Hidroelektrarne (GWh)" = "#588C7E", "Termoelektrarne (GWh)" = "#D96459", 
                                          "Jedrske elektrarne (GWh)" = "#F2AE72", "Sončne elektrarne (GWh)" = "#F2E394", 
                                          "Vetrne elektrarne (GWh)" = "#603E95"))

graf8 <- ggplot(data = TD_slo_obnovljivi %>% filter(
  leto == 2018, elektrarna == "Hidroelektrarne (GWh)" | elektrarna == "Termoelektrarne (GWh)" |
    elektrarna == "Jedrske elektrarne (GWh)" | elektrarna == "Sončne elektrarne (GWh)"| 
    elektrarna == "Vetrne elektrarne (GWh)"), aes(x="", y=`proizvodnja_(GWh)`, fill=elektrarna)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Dark2") + 
  geom_text(aes(label = paste(round(`proizvodnja_(GWh)` / sum(`proizvodnja_(GWh)`) * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  ggtitle('Tortni diagram za proizvodnjo obnovljive energije z odstotki za leto 2018') + theme(axis.text = element_blank(),
  axis.ticks = element_blank(), panel.grid  = element_blank(), axis.title.x = element_blank(),
  axis.title.y = element_blank())

zemljevid1 <- tm_shape(merge(zemljevid, TD_world_obnovljivi %>% group_by(drzave), by.x="SOVEREIGNT", by.y="drzave"), xlim=c(-170,200), ylim=c(-65,85)) +
  tm_polygons("obnovljiva_energija_(GWh)", 
              title = "obnovljiva energija (GWh)",
              style = "kmeans", legend.hist = TRUE) + 
  tm_layout(main.title = "Celotna pridelana obnovljiva energija") +
  tm_layout(legend.position = c("left","bottom"),
            legend.title.size = 1)

zemljevid2 <- tm_shape(merge(zemljevid, TD_world_obnovljivi %>% group_by(drzave), by.x="SOVEREIGNT", by.y="drzave"), xlim=c(-170,200), ylim=c(-65,85)) +
  tm_polygons("%_obnovljive_energije_iz_hidroelektrarn", 
              title = "% obnovljive energije \n iz hidroelektrarn", 
              style = "fixed", breaks = c(0, 20, 40, 60, 80, 100), palette = "Blues") + 
  tm_layout(main.title = "Odstotek obnovljive energije iz hidroelektrarn") +
  tm_layout(legend.position = c("left","bottom"),
            legend.title.size = 1)

zemljevid3 <- tm_shape(merge(zemljevid, TD_world_obnovljivi %>% group_by(drzave), by.x="SOVEREIGNT", by.y="drzave"), xlim=c(-25,60), ylim=c(-45,72)) +
  tm_polygons(col = "prevladujoči vir", palette = paleta1) + tm_layout(legend.outside = TRUE, legend.text.size = 0.54, main.title = "Prevladujoči vir obnovljive energije")


