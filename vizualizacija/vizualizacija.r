# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid(
  "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", "ne_50m_admin_0_countries", encoding="UTF-8")

graf1 <- ggplot(data=TD_cena_energentov, aes(x=leto_in_cetrtletje, y=`cena_(EUR/kWh)`, col=energent)) + 
  geom_point() + xlab('leto') + ylab('cena (EUR/kWh') + ggtitle('Gibanje cene nafte in plina skozi čas') + 
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))