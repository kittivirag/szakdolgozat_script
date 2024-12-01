library(readxl)
library(tidyverse)
library(stringr)
setwd(('C:/Users/viragk/Desktop/Tanulmányok/Repülési_árak/'))
getwd()
adat = read_excel('Clean_Dataset_FP_t.xlsx')


#adatisztítás
adat = 
  adat %>%
  rename('légitársaság' = airline, 
         #'járat' = flight, 
         'kiinduló_város' = source_city, 
         'indulási_napszak' = departure_time, 
         'megállások_száma' = stops, 
         'érkezési_napszak' = arrival_time, 
         'úticél' = destination_city, 
         'üzleti_osztály' = class, 
         'utazási_idő' = duration, 
         'hátralévő_napok_száma' = days_left, 
         'ár' = price) %>%
  mutate(légitársaság = 
           case_when(légitársaság == 'Air_India' ~ 'Air_India', 
                     légitársaság == 'AirAsia' ~ 'Air_Asia', 
                     légitársaság == 'GO_FIRST' ~ 'Go_First', 
                     légitársaság == 'Indigo' ~ 'Indigo', 
                     légitársaság == 'SpiceJet' ~ 'Spice_Jet', 
                     légitársaság == 'Vistara' ~ 'Vistara')) %>%
  mutate(indulási_napszak =
           case_when(indulási_napszak == 'Early_Morning' ~ 'kora_reggel', 
                     indulási_napszak == 'Morning' ~ 'délelőtt', 
                     indulási_napszak == 'Afternoon' ~ 'délután', 
                     indulási_napszak == 'Evening' ~ 'este', 
                     indulási_napszak == 'Night' ~ 'éjszaka', 
                     indulási_napszak == 'Late_Night' ~ 'késő_éjszaka')) %>%
  mutate(megállások_száma =
           case_when(megállások_száma == 'zero' ~ '0', 
                     megállások_száma == 'one' ~ '1', 
                     megállások_száma == 'two_or_more' ~ '2_vagy_több')) %>%
  mutate(érkezési_napszak =
           case_when(érkezési_napszak == 'Early_Morning' ~ 'kora_reggel', 
                     érkezési_napszak == 'Morning' ~ 'délelőtt', 
                     érkezési_napszak == 'Afternoon' ~ 'délután', 
                     érkezési_napszak == 'Evening' ~ 'este', 
                     érkezési_napszak == 'Night' ~ 'éjszaka', 
                     érkezési_napszak == 'Late_Night' ~ 'késő_éjszaka')) %>%
  mutate(üzleti_osztály =
           case_when(üzleti_osztály == 'Business' ~ 1,
                     üzleti_osztály == 'Economy' ~ 0)) %>%
  mutate(légitársaság = factor(légitársaság)) %>%
  mutate(kiinduló_város = factor(kiinduló_város)) %>%
  mutate(indulási_napszak = factor(indulási_napszak)) %>%
  mutate(megállások_száma = factor(megállások_száma,
                            levels = c('0', '1','2_vagy_több'), 
                            ordered = TRUE)) %>%
  mutate(érkezési_napszak = factor(érkezési_napszak)) %>%
  mutate(üzleti_osztály = factor(üzleti_osztály,
                                 levels = c(0,1), 
                                 ordered = TRUE)) %>%
  mutate(úticél = factor(úticél)) %>%
  mutate(utazási_idő = as.numeric(utazási_idő)) %>%
  mutate(hátralévő_napok_száma = as.numeric(hátralévő_napok_száma)) %>%
  mutate(ár = as.numeric(ár))


library(caret)
set.seed(12)
teszt_index <- 
  createDataPartition(adat$ár, 
                      p = 0.1, 
                      list = FALSE, 
                      times = 1)

teszt_halmaz <- adat[teszt_index, ]
teljes_tanító_halmaz <- adat[-teszt_index, ]
set.seed(23)
tanító_halmaz_1 = sample_frac(teljes_tanító_halmaz, size = 0.6)
set.seed(34)
tanító_halmaz_2 = sample_frac(teljes_tanító_halmaz, size = 0.7)
set.seed(45)
tanító_halmaz_3 = sample_frac(teljes_tanító_halmaz, size = 0.8)
set.seed(56)
tanító_halmaz_4 = sample_frac(teljes_tanító_halmaz, size = 0.9)
tanító_halmaz_5 = teljes_tanító_halmaz
