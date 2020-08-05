library(shiny)

shinyServer(function(input, output) {
  output$TD_world_obnovljivi <- DT::renderDataTable({
    TD_world_obnovljivi %>% mutate(drzave=slovar[drzave]) %>% 
      rename("Država"=drzave, "Delež energije iz hidroelektrarn (%)"= `%_obnovljive_energije_iz_hidroelektrarn`, 
             "Celotna obnovljiva energija (GWh)"=`obnovljiva_energija_(GWh)`, "Delež energije iz vetrnih elektrarn (%)" = `%_obnovljive_energije_iz_vetrnih elektrarn`, 
             "Delež energije iz biomase in odpadkov (%)"= `%_obnovljive_energije_iz_biomase_in_odpadkov`, 
             "Delež obnovljive energije iz sončne energije (%)"=`%_obnovljive_energije_iz_sončne energije`,
             "Delež obnovljive energije iz geotermalne energije (%)"=`%_obnovljive_energije_iz_geotermalne_energije`, 
             "Prevladujoči vir" = `prevladujoči vir`)
  })
})