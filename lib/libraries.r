library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(shiny)
library(readr)
library(dplyr)
library(rgdal)
library(markdown)
library(DT)
library(rgeos)
library(digest)
library(ggplot2)
library(mosaic)
library(maptools)
library(extrafont)
library(tmap)
library(scales)


options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")