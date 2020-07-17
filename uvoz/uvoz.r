#--------------------------------------------------------------------------------------------------------------------------------------------------------
# ANALIZA CENE ELEKTRIČNE ENERGIJE IN ZEMELJSKEGA PLINA

cene_elektrike <- read_csv('podatki/cena_elektricna_energija.csv',
                             col_names=c("energent", "drugi", "2012Q1","2012Q2","2012Q3","2012Q4",
                                         "2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2",
                                         "2014Q3","2014Q4","2015Q1","2015Q2","2015Q3","2015Q4","2016Q1",
                                         "2016Q2","2016Q3","2016Q4","2017Q1","2017Q2","2017Q3","2017Q4",
                                         "2018Q1","2018Q2","2018Q3","2018Q4","2019Q1","2019Q2","2019Q3",
                                         "2019Q4"),skip=1, locale=locale(encoding='Windows-1250'))

cene_elektrike$drugi <- NULL
cene_elektrike$energent[cene_elektrike$energent == "Slovenija"] <- "Elekrična energija (EUR/kWh)"

cene_plina <- read_csv('podatki/cena_zemeljski_plin.csv',
                       col_names=c("energent", "enota", "drugi", "2012Q1","2012Q2","2012Q3","2012Q4",
                                   "2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2",
                                   "2014Q3","2014Q4","2015Q1","2015Q2","2015Q3","2015Q4","2016Q1",
                                   "2016Q2","2016Q3","2016Q4","2017Q1","2017Q2","2017Q3","2017Q4",
                                   "2018Q1","2018Q2","2018Q3","2018Q4","2019Q1","2019Q2","2019Q3",
                                   "2019Q4"),skip=1, locale=locale(encoding='Windows-1250'))

cene_plina$drugi <- NULL
cene_plina$enota <- NULL
cene_plina$energent[cene_plina$energent == "Slovenija"] <- "Zemeljski plin (EUR/kWh)"

tabela_cen_energentov <- rbind(cene_elektrike, cene_plina)



#--------------------------------------------------------------------------------------------------------------------------------------------------------
# ANALIZA PRIMERJAVE PORABE PO GOSPODINJSTVIH

tabela_primerjav <- read_csv('podatki/primerjava_porabe_po_gospodinjstvih.csv',
                             col_names=c("energetski vir","2009","2010","2011","2012", "2013","2014","2015","2016","2017","2018"),
                             skip=1, locale=locale(encoding='Windows-1250'))

tabela_primerjav$`energetski vir`[tabela_primerjav$`energetski vir` == "Uteko?injeni naftni plin (t)"] <- "Utekočinjeni naftni plin (t)"
tabela_primerjav$`energetski vir`[tabela_primerjav$`energetski vir` == "Elektri?na energija (GWh)"] <- "Električna energija (GWh)"
tabela_primerjav$`energetski vir`[tabela_primerjav$`energetski vir` == "Son?na energija (TJ)"] <- "Sončna energija (TJ)"



#--------------------------------------------------------------------------------------------------------------------------------------------------------
# ANALIZA PROIZVODNJE OBNOVLJIVIH VIROV PO ELEKTARNAH IN LETIH

tabela_obnovljivih_slo <- read_csv('podatki/proizvodnja_v_elektrarnah.csv',
                                   col_names=c("elektrarna","2002","2003","2004","2005","2006","2007",
                                               "2008","2009","2010","2011","2012","2013",
                                               "2014","2015","2016","2017","2018"),skip=1, locale=locale(encoding='Windows-1250'))

tabela_obnovljivih_slo <- tabela_obnovljivih_slo[-c(3,4,5),]
tabela_obnovljivih_slo$elektrarna[tabela_obnovljivih_slo$elektrarna == "Proizvodnja na pragu-SKUPAJ(GWh)"] <- "Skupaj (GWh)"
tabela_obnovljivih_slo$elektrarna[tabela_obnovljivih_slo$elektrarna == "Proizvodnja na pragu-termoelektrarne (GWh)"] <- "Termoelektrarne (GWh)"
tabela_obnovljivih_slo$elektrarna[tabela_obnovljivih_slo$elektrarna == "Proizvodnja na pragu-hidroelektrarne-SKUPAJ (GWh)"] <- "Hidroelektrarne (GWh)"
tabela_obnovljivih_slo$elektrarna[tabela_obnovljivih_slo$elektrarna == "Proizvodnja na pragu-jedrska elektrarna (GWh)"] <- "Jedrske elektrarne (GWh)"
tabela_obnovljivih_slo$elektrarna[tabela_obnovljivih_slo$elektrarna == "Proizvodnja na pragu-son?ne elektrarne (GWh)"] <- "Sončne elektrarne (GWh)"
tabela_obnovljivih_slo$elektrarna[tabela_obnovljivih_slo$elektrarna == "Proizvodnja na pragu-vetrne elektrarne (GWh)"] <- "Vetrne elektrarne (GWh)"



#--------------------------------------------------------------------------------------------------------------------------------------------------------
# ANALIZA PORABE OBNOVLJIVIH VIROV ENERGIJE DRŽAV SVETA (html)

uvozen_html <- read_html('podatki/obnovljivi_viri_energije_svet.html')

tabela_porabe_drzav <- uvozen_html %>% html_nodes(xpath="//table[@class='wikitable sortable mw-datatable']") %>%
  .[[1]] %>% html_table(fill = TRUE)

tabela_porabe_drzav[,c(2,3,5,6,7,9,10,12,13,15,16,18,19,21)] <- NULL

names(tabela_porabe_drzav) <- c("države", "obnovljiva energija (GWh)", "% obnovljive energije iz hidroelektrarn",
                                "% obnovljive energije iz vetrnih elektrarn","% obnovljive energije iz biomase in odpadkov",
                                "% obnovljive energije iz sončne energije", "% obnovljive energije iz geotermalne energije")

tabela_porabe_drzav <- tabela_porabe_drzav[-1, ]

