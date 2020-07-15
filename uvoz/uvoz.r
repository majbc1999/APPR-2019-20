# ANALIZA CENE ELEKTRIČNE ENERGIJE IN ZEMELJSKEGA PLINA

uvozi_cene_elektrike <- function(){
  cene_elektrike <- read_csv('podatki/cena_elektricna_energija.csv',col_names=TRUE,locale=locale(encoding='Windows-1250'))
  return(cene_elektrike)
}

uvozi_cene_plina <- function(){
  cene_plina <- read_csv('podatki/cena_zemeljski_plin.csv',col_names=TRUE,locale=locale(encoding='Windows-1250'))
  return(cene_plina)
}


# ANALIZA PRIMERJAVE PORABE PO GOSPODINJSTVIH

uvozi_primerjavo <- function(){
  tabela_primerjav <- read_csv('podatki/primerjava_porabe_po_gospodinjstvih.csv',col_names=TRUE,locale=locale(encoding='Windows-1250'))
  return(tabela_primerjav)
}


# ANALIZA