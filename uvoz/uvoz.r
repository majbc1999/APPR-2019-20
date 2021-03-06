#--------------------------------------------------------------------------------------------------------------------------------------------------------
# ANALIZA CENE ELEKTRIČNE ENERGIJE IN ZEMELJSKEGA PLINA

cene_elektrike <- read_csv('podatki/cena_elektricna_energija.csv',
                             col_names=c("energent", "drugi", 2012,2012.25,2012.5,2012.75,
                                         2013,2013.25,2013.5,2013.75,2014,2014.25,
                                         2014.5,2014.75,2015,2015.25,2015.5,2015.75,2016,
                                         2016.25,2016.5,2016.75,2017,2017.25,2017.5,2017.75,
                                         2018,2018.25,2018.5,2018.75,2019,2019.25,2019.5,
                                         2019.75),skip=3, locale=locale(encoding='Windows-1250'))

cene_elektrike$drugi <- NULL
cene_elektrike$energent[cene_elektrike$energent == "Slovenija"] <- "Električna energija"

cene_plina <- read_csv('podatki/cena_zemeljski_plin.csv',
                       col_names=c("energent", "enota", "drugi", 2012,2012.25,2012.5,2012.75,
                                   2013,2013.25,2013.5,2013.75,2014,2014.25,
                                   2014.5,2014.75,2015,2015.25,2015.5,2015.75,2016,
                                   2016.25,2016.5,2016.75,2017,2017.25,2017.5,2017.75,
                                   2018,2018.25,2018.5,2018.75,2019,2019.25,2019.5,
                                   2019.75),skip=3, locale=locale(encoding='Windows-1250'))

cene_plina$drugi <- NULL
cene_plina$enota <- NULL
cene_plina$energent[cene_plina$energent == "Slovenija"] <- "Zemeljski plin"

tabela_cen_energentov <- rbind(cene_elektrike, cene_plina)

TD_cena_energentov <- tabela_cen_energentov %>% gather("leto_in_cetrtletje", "cena_(EUR/kWh)", 2:33)


elektrika_TDa <- TD_cena_energentov %>% filter(energent=="Električna energija", leto_in_cetrtletje == 2012| leto_in_cetrtletje == 2013| leto_in_cetrtletje == 2014| leto_in_cetrtletje == 2015|
                                               leto_in_cetrtletje == 2016| leto_in_cetrtletje == 2017| leto_in_cetrtletje == 2018| leto_in_cetrtletje == 2019)
elektrika_TDa$energent <- NULL

plin_TDa <- TD_cena_energentov %>% filter(energent=="Zemeljski plin", leto_in_cetrtletje == 2012| leto_in_cetrtletje == 2013| leto_in_cetrtletje == 2014| leto_in_cetrtletje == 2015|
                                            leto_in_cetrtletje == 2016| leto_in_cetrtletje == 2017| leto_in_cetrtletje == 2018| leto_in_cetrtletje == 2019)
plin_TDa$energent <- NULL
#--------------------------------------------------------------------------------------------------------------------------------------------------------
# ANALIZA PRIMERJAVE PORABE PO GOSPODINJSTVIH

tabela_primerjav <- read_csv('podatki/primerjava_porabe_po_gospodinjstvih.csv',
                             col_names=c("energetski_vir",2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),
                             skip=3, locale=locale(encoding='Windows-1250'))

tabela_primerjav$`energetski_vir`[tabela_primerjav$`energetski_vir` == "Uteko?injeni naftni plin (t)"] <- "Utekočinjeni naftni plin (t)"
tabela_primerjav$`energetski_vir`[tabela_primerjav$`energetski_vir` == "Elektri?na energija (GWh)"] <- "Električna energija (GWh)"
tabela_primerjav$`energetski_vir`[tabela_primerjav$`energetski_vir` == "Son?na energija (TJ)"] <- "Sončna energija (TJ)"

TD_poraba_gospodinjstev <- tabela_primerjav %>% gather("leto", "poraba", 2:11)

#--------------------------------------------------------------------------------------------------------------------------------------------------------
# ANALIZA PROIZVODNJE OBNOVLJIVIH VIROV PO ELEKTARNAH IN LETIH

tabela_obnovljivih_slo <- read_csv('podatki/proizvodnja_v_elektrarnah.csv',
                                   col_names=c("elektrarna",2002,2003,2004,2005,2006,2007,
                                               2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),skip=3, locale=locale(encoding='Windows-1250'))

tabela_obnovljivih_slo <- tabela_obnovljivih_slo[-c(3,4,5),]
tabela_obnovljivih_slo$elektrarna[tabela_obnovljivih_slo$elektrarna == "Proizvodnja na pragu-SKUPAJ(GWh)"] <- "Skupaj (GWh)"
tabela_obnovljivih_slo$elektrarna[tabela_obnovljivih_slo$elektrarna == "Proizvodnja na pragu-termoelektrarne (GWh)"] <- "Termoelektrarne (GWh)"
tabela_obnovljivih_slo$elektrarna[tabela_obnovljivih_slo$elektrarna == "Proizvodnja na pragu-hidroelektrarne-SKUPAJ (GWh)"] <- "Hidroelektrarne (GWh)"
tabela_obnovljivih_slo$elektrarna[tabela_obnovljivih_slo$elektrarna == "Proizvodnja na pragu-jedrska elektrarna (GWh)"] <- "Jedrske elektrarne (GWh)"
tabela_obnovljivih_slo$elektrarna[tabela_obnovljivih_slo$elektrarna == "Proizvodnja na pragu-sončne elektrarne (GWh)"] <- "Sončne elektrarne (GWh)"
tabela_obnovljivih_slo$elektrarna[tabela_obnovljivih_slo$elektrarna == "Proizvodnja na pragu-vetrne elektrarne (GWh)"] <- "Vetrne elektrarne (GWh)"

TD_slo_obnovljivi <- tabela_obnovljivih_slo %>% gather("leto", "proizvodnja_(GWh)", 2:18)

#--------------------------------------------------------------------------------------------------------------------------------------------------------
# ANALIZA PORABE OBNOVLJIVIH VIROV ENERGIJE DRŽAV SVETA (html)

uvozen_html <- read_html('podatki/obnovljivi_viri_energije_svet.html')

tabela_porabe_drzav <- uvozen_html %>% html_nodes(xpath="//table[@class='wikitable sortable mw-datatable']") %>%
  .[[1]] %>% html_table(fill = TRUE)

tabela_porabe_drzav[,c(2,3,5,6,7,9,10,12,13,15,16,18,19,21)] <- NULL

names(tabela_porabe_drzav) <- c("drzave", "obnovljiva_energija_(GWh)", "%_obnovljive_energije_iz_hidroelektrarn",
                                "%_obnovljive_energije_iz_vetrnih elektrarn","%_obnovljive_energije_iz_biomase_in_odpadkov",
                                "%_obnovljive_energije_iz_sončne energije", "%_obnovljive_energije_iz_geotermalne_energije")

tabela_porabe_drzav <- tabela_porabe_drzav[-1, ]


slovar <- c("Afghanistan" = "Afganistan",
            "Albania" = "Albanija",
            "Algeria" = "Alžirija",
            "Anguilla" = "Angvila",
            "Antigua and Barbuda" = "Antigva in Barbuda",
            "Armenia" = "Armenija",
            "Australia" = "Avstralija",
            "Austria" = "Avstrija",
            "Azerbaijan" = "Azerbajdžan",
            "Bahamas" = "Bahami",
            "Bahrain" = "Bahrajn",
            "Bangladesh" = "Bangladeš",
            "Belarus" = "Belorusija",
            "Belgium" = "Belgija",
            "BES Islands" = "Nizozemski Karibi",
            "Bhutan" = "Butan",
            "Bolivia" = "Bolivija",
            "Bosnia and Herzegovina" = "Bosna in Hercegovina",
            "Botswana" = "Bocvana",
            "British Virgin Islands" = "Britanski Deviški otoki",
            "Brazil" = "Brazilija",
            "Brunei" = "Brunej",
            "Bulgaria" = "Bolgarija",
            "Cabo Verde" = "Kapverdski otoki",
            "Cambodia" = "Kambodža",
            "Cameroon" = "Kamerun",
            "Canada" = "Kanada",
            "Central African Republic" = "Centralnoafriška republika",
            "Chad" = "Čad",
            "Chile" = "Čile",
            "Colombia" = "Kolumbija",
            "Comoros" = "Komori",
            "Republic of the Congo" = "Kongo",
            "Democratic Republic of the Congo" = "DR Kongo",
            "Cook Islands" = "Cookovi otoki",
            "Costa Rica" = "Kostarika",
            "Ivory Coast" = "Slonokoščena obala",
            "Croatia" = "Hrvaška",
            "Cuba" = "Kuba",
            "Cyprus" = "Ciper",
            "Czechia" = "Češka",
            "Denmark" = "Danska",
            "Djibouti" = "Džibuti",
            "Dominica" = "Dominika",
            "Dominican Republic" = "Dominikanska Republika",
            "Ecuador" = "Ekvador",
            "Egypt" = "Egipt",
            "El Salvador" = "Salvador",
            "Equatorial Guinea" = "Ekvatorialna Gvineja",
            "Eritrea" = "Eritreja",
            "Estonia" = "Estonija",
            "Eswatini (Swaziland)" = "Svazi",
            "Ethiopia" = "Etopija",
            "Falkland Islands" = "Falklandski otoki",
            "Faroe Islands" = "Falklandski otoki",
            "Fiji" = "Fidži",
            "Finland" = "Finska",
            "French Guiana" = "Francoska Gvajana",
            "French Polynesia" = "Francoska Polinezija",
            "France" = "Francija",
            "Gambia" = "Gambija",
            "Georgia" = "Gruzija",
            "Germany" = "Nemčija",
            "Ghana" = "Gana",
            "Greece" = "Grčija",
            "Greenland" = "Grenlandija",
            "Guadeloupe" = "Gvadelupe",
            "Guatemala" = "Gvatemala",
            "Guinea" = "Gvineja",
            "Guinea Bissau" = "Gvineja Bissau",
            "Azerbaijan" = "Azerbajdžan",
            "Guyana" = "Gvajana",
            "Hungary" = "Madžarska",
            "Iceland" = "Islandija",
            "India" = "Indija",
            "Indonesia" = "Indonezija",
            "Iraq" = "Irak",
            "Ireland" = "Irska",
            "Israel" = "Izrael",
            "Italy" = "Italija",
            "Jamaica" = "Jamajka",
            "Japan" = "Japonska",
            "Kazakhstan" = "Kazahstan",
            "Kenya" = "Kenija",
            "Korea DPR" = "Severna Koreja",
            "Korea Rep" = "Južna Koreja",
            "Kuwait" = "Kuvajt",
            "Kyrgyzstan" = "Kirgizistan",
            "Latvia" = "Latvija",
            "Lebanon" = "Libanon",
            "Lesotho" = "Lesoto",
            "Liberia" = "Liberija",
            "Libya" = "Libija",
            "Lithuania" = "Litva",
            "Luxembourg" = "Luksemburg",
            "Madagascar" = "Madagaskar",
            "Malawi" = "Malavi",
            "Malaysia" = "Malezija",
            "Maldives" = "Maldivi",
            "Marshall Islands" = "Marshallovi otoki",
            "Martinique" = "Martinik",
            "Mauritania" = "Mavretanija",
            "Mexico" = "Mehika",
            "Micronesia" = "Mikronezija",
            "Moldova" = "Moldavija",
            "Mongolia" = "Mongolija",
            "Montenegro" = "Črna Gora",
            "Morocco" = "Maroko",
            "Mozambique" = "Mozambik",
            "Myanmar" = "Mjanmar",
            "Namibia" = "Namibija",
            "Netherlands" = "Nizozemska",
            "New Caledonia" = "Nova Kaledonija",
            "New Zealand" = "Nova Zelandija",
            "Nicaragua" = "Nikaragva",
            "Nigeria" = "Nigerija",
            "North Macedonia" = "Severna Makedonija",
            "Norway" = "Norveška",
            "Palestine" = "Palestina",
            "Papua New Guinea" = "Papua Nova Gvineja",
            "Paraguay" = "Paragvaj",
            "Philippines" = "Filipini",
            "Poland" = "Poljska",
            "Portugal" = "Portugalska",
            "Puerto Rico" = "Portoriko",
            "Qatar" = "Katar",
            "Romania" = "Romunija",
            "Russia" = "Rusija",
            "Rwanda" = "Ruanda",
            "Sao Tome & Principe" = "Sao Tome in Principe",
            "Saudi Arabia" = "Savdska Arabija",
            "Serbia" = "Srbija",
            "Seychelles" = "Sejšeli",
            "Singapore" = "Singapur",
            "Slovakia" = "Slovaška",
            "Slovenia" = "Slovenija",
            "Solomon Islands" = "Salomonovi otoki",
            "Somalia" = "Somalija",
            "South Africa" = "JAR",
            "South Sudan" = "Južni Sudan",
            "Spain" = "Španija",
            "Sri Lanka" = "Šrilanka",
            "St Kitts and Nevis" = "St Kitts in Nevis",
            "St Martin" = "Sveti Martin",
            "St Vincent & the Grenadines" = "Saint Vincent in Grenadine",
            "Suriname" = "Surinam",
            "Sweden" = "Švedska",
            "Switzerland" = "Švica",
            "Syria" = "Sirija",
            "Taiwan" = "Tajvan",
            "Tajikistan" = "Tadžikistan",
            "Tanzania" = "Tanzanija",
            "Thailand" = "Tajska",
            "Trinidad and Tobago" = "Trinidad in Tobago",
            "Tunisia" = "Tunizija",
            "Turkey" = "Turčija",
            "Ukraine" = "Ukrajina",
            "United Arab Emirates" = "Združeni Arabski Emirati",
            "United Kingdom" = "Združeno Kraljestvo",
            "United States" = "ZDA",
            "US Virgin Islands" = "Deviški otoki",
            "Uruguay" = "Urugvaj",
            "Yemen" = "Jemen",
            "Zambia" = "Zambija",
            "Zimbabwe" = "Zimbabve",
            "Angola" = "Angola",
            "Argentina" = "Argentina",
            "Aruba" = "Aruba", 
            "Barbados" = "Barbados", 
            "Belize" = "Belize", 
            "Benin" = "Benin", 
            "Burkina Faso" = "Burkina Faso", 
            "Burundi" = "Burundi", 
            "China" = "Kitajska", 
            "Curacao" = "Curacao",
            "Gabon" = "Gabon", 
            "Grenada" = "Grenada", 
            "Guam" = "Guam", 
            "Haiti" = "Haiti", 
            "Honduras" = "Honduras", 
            "Iran" = "Iran", 
            "Jordan" = "Jordan", 
            "Kiribati" = "Kiribati", 
            "Kosovo" = "Kosovo", 
            "Laos" = "Laos", 
            "Mali" = "Mali", 
            "Malta" = "Malta", 
            "Mauritius" = "Mauritius", 
            "Mayotte" = "Mayotte", 
            "Nauru" = "Nauru", 
            "Nepal" = "Nepal", 
            "Niger" = "Niger",
            "Oman" = "Oman", 
            "Pakistan" = "Pakistan", 
            "Palau" = "Palau", 
            "Palestine" = "Palestine", 
            "Panama" = "Panama", 
            "Peru" = "Peru",
            "Reunion" = "Reunion", 
            "Samoa" = "Samoa", 
            "Senegal" = "Senegal", 
            "Sierra Leone" = "Sierra Leone", 
            "St Lucia" = "St Lucia", 
            "St Maarten" = "St Maarten", 
            "Sudan" = "Sudan",
            "Timor Leste" = "Timor Leste", 
            "Togo" = "Togo", 
            "Tokelau" = "Tokelau", 
            "Tonga" = "Tonga", 
            "Turkmenistan" = "Turkmenistan", 
            "Tuvalu" = "Tuvalu", 
            "Uganda" = "Uganda", 
            "Uzbekistan" = "Uzbekistan", 
            "Vanuatu" = "Vanuatu", 
            "Venezuela" = "Venezuela", 
            "Vietnam" = "Vietnam",
            "United States of America" = "ZDA",
            "United Republic of Tanzania" = "Tanzanija",
            "Republic of Serbia" = "Srbija",
            "Republic of North Macedonia" = "Severna Makedonija",
            "Republic of Djibouti" = "Džibuti",
            "Republic of Guinea-Bissau" = "Gvineja Bissau",
            "Swaziland" = "Svazi") 

TD_svet_obnovljivi <- tabela_porabe_drzav %>% mutate(drzave=slovar[drzave])

tabela_porabe_drzav$drzave[tabela_porabe_drzav$drzave == "United States"] <- "United States of America"
tabela_porabe_drzav$drzave[tabela_porabe_drzav$drzave == "Tanzania"] <- "United Republic of Tanzania"
tabela_porabe_drzav$drzave[tabela_porabe_drzav$drzave == "Serbia"] <- "Republic of Serbia"
tabela_porabe_drzav$drzave[tabela_porabe_drzav$drzave == "North Macedonia"] <- "Republic of North Macedonia"
tabela_porabe_drzav$drzave[tabela_porabe_drzav$drzave == "Djibouti"] <- "Republic of Djibouti"
tabela_porabe_drzav$drzave[tabela_porabe_drzav$drzave == "Guinea Bissau"] <- "Republic of Guinea-Bissau"
tabela_porabe_drzav$drzave[tabela_porabe_drzav$drzave == "Eswatini (Swaziland)"] <- "Swaziland"

TD_world_obnovljivi <- tabela_porabe_drzav