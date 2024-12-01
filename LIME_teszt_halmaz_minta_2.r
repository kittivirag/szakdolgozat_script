library(lime)
#vizsgált adatsorok minden esetben
teszt_halmaz_minta_2 = sample_n(anti_join(teszt_halmaz, teszt_halmaz_minta), size = 300)
teszt_halmaz_lokális_esetek_2 = teszt_halmaz_minta_2[, -10]
#GBM modelleknek speciális formátum
teszt_mátrix_lokális_esetek_2 <- model.matrix(~ . - 1, data = teszt_halmaz_minta_2[, -10])

#ellenőrzés, hogy van-e átfedés
nrow(semi_join(teszt_halmaz_minta, teszt_halmaz_minta_2))

#1.Lineáris regreszió LIME
lin_reg_1_lime_2 <- 
  explain(x = teszt_halmaz_lokális_esetek_2,
          explainer = lime(tanító_halmaz_1[,-10], model = lin_reg_1),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

lin_reg_1_lime_2 = data.frame(lin_reg_1_lime_2)

lin_reg_1_globál_lime_2 <- 
  lin_reg_1_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_1_változó = feature, 
         lin_reg_1_átlagos_súly = avg_weight)

lin_reg_1_absz_globál_lime_2 <- 
  lin_reg_1_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_1_változó = feature, 
         lin_reg_1_átlagos_súly = avg_weight)

#2.Lineáris regreszió LIME
lin_reg_2_lime_2 <- 
  explain(x = teszt_halmaz_lokális_esetek_2,
          explainer = lime(tanító_halmaz_2[,-10], model = lin_reg_2),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

lin_reg_2_lime_2 = data.frame(lin_reg_2_lime_2)

lin_reg_2_globál_lime_2 <- 
  lin_reg_2_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_2_változó = feature, 
         lin_reg_2_átlagos_súly = avg_weight)

lin_reg_2_absz_globál_lime_2 <- 
  lin_reg_2_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_2_változó = feature, 
         lin_reg_2_átlagos_súly = avg_weight)

#3.Lineáris regreszió LIME
lin_reg_3_lime_2 <- 
  explain(x = teszt_halmaz_lokális_esetek_2,
          explainer = lime(tanító_halmaz_3[,-10], model = lin_reg_3),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

lin_reg_3_lime_2 = data.frame(lin_reg_3_lime_2)

lin_reg_3_globál_lime_2 <- 
  lin_reg_3_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_3_változó = feature, 
         lin_reg_3_átlagos_súly = avg_weight)

lin_reg_3_absz_globál_lime_2 <- 
  lin_reg_3_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_3_változó = feature, 
         lin_reg_3_átlagos_súly = avg_weight)

#4.Lineáris regreszió LIME
lin_reg_4_lime_2 <- 
  explain(x = teszt_halmaz_lokális_esetek_2,
          explainer = lime(tanító_halmaz_4[,-10], model = lin_reg_4),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

lin_reg_4_lime_2 = data.frame(lin_reg_4_lime_2)

lin_reg_4_globál_lime_2 <- 
  lin_reg_4_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_4_változó = feature, 
         lin_reg_4_átlagos_súly = avg_weight)

lin_reg_4_absz_globál_lime_2 <- 
  lin_reg_4_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_4_változó = feature, 
         lin_reg_4_átlagos_súly = avg_weight)

#5.Lineáris regreszió LIME
lin_reg_5_lime_2 <- 
  explain(x = teszt_halmaz_lokális_esetek_2,
          explainer = lime(tanító_halmaz_5[,-10], model = lin_reg_5),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

lin_reg_5_lime_2 = data.frame(lin_reg_5_lime_2)

lin_reg_5_globál_lime_2 <- 
  lin_reg_5_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_5_változó = feature, 
         lin_reg_5_átlagos_súly = avg_weight)

lin_reg_5_absz_globál_lime_2 <- 
  lin_reg_5_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_5_változó = feature, 
         lin_reg_5_átlagos_súly = avg_weight)

lin_reg_globál_lime_2 <-
  cbind (lin_reg_1_globál_lime_2, lin_reg_2_globál_lime_2, lin_reg_3_globál_lime_2, lin_reg_4_globál_lime_2, lin_reg_5_globál_lime_2)

lin_reg_absz_globál_lime_2 <-
  cbind (lin_reg_1_absz_globál_lime_2, lin_reg_2_absz_globál_lime_2, lin_reg_3_absz_globál_lime_2, lin_reg_4_absz_globál_lime_2, lin_reg_5_absz_globál_lime_2)

#1.random forest LIME
rf_1_lime_2 <- 
  explain(x = teszt_halmaz_lokális_esetek_2,
          explainer = lime(tanító_halmaz_1[,-10], model = rf_1),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

rf_1_lime_2 = data.frame(rf_1_lime_2)

rf_1_globál_lime_2 <- 
  rf_1_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_1_változó = feature, 
         rf_1_átlagos_súly = avg_weight)

rf_1_absz_globál_lime_2 <- 
  rf_1_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_1_változó = feature, 
         rf_1_átlagos_súly = avg_weight)

#2.random forest LIME
rf_2_lime_2 <- 
  explain(x = teszt_halmaz_lokális_esetek_2,
          explainer = lime(tanító_halmaz_2[,-10], model = rf_2),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

rf_2_lime_2 = data.frame(rf_2_lime_2)

rf_2_globál_lime_2 <- 
  rf_2_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_2_változó = feature, 
         rf_2_átlagos_súly = avg_weight)

rf_2_absz_globál_lime_2 <- 
  rf_2_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_2_változó = feature, 
         rf_2_átlagos_súly = avg_weight)

#3.random forest LIME
rf_3_lime_2 <- 
  explain(x = teszt_halmaz_lokális_esetek_2,
          explainer = lime(tanító_halmaz_3[,-10], model = rf_3),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

rf_3_lime_2 = data.frame(rf_3_lime_2)

rf_3_globál_lime_2 <- 
  rf_3_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_3_változó = feature, 
         rf_3_átlagos_súly = avg_weight)

rf_3_absz_globál_lime_2 <- 
  rf_3_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_3_változó = feature, 
         rf_3_átlagos_súly = avg_weight)

#4.random forest LIME
rf_4_lime_2 <- 
  explain(x = teszt_halmaz_lokális_esetek_2,
          explainer = lime(tanító_halmaz_4[,-10], model = rf_4),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

rf_4_lime_2 = data.frame(rf_4_lime_2)

rf_4_globál_lime_2 <- 
  rf_4_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_4_változó = feature, 
         rf_4_átlagos_súly = avg_weight)

rf_4_absz_globál_lime_2 <- 
  rf_4_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_4_változó = feature, 
         rf_4_átlagos_súly = avg_weight)

#5.random forest LIME
rf_5_lime_2 <- 
  explain(x = teszt_halmaz_lokális_esetek_2,
          explainer = lime(tanító_halmaz_5[,-10], model = rf_5),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

rf_5_lime_2 = data.frame(rf_5_lime_2)

rf_5_globál_lime_2 <- 
  rf_5_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_5_változó = feature, 
         rf_5_átlagos_súly = avg_weight)

rf_5_absz_globál_lime_2 <- 
  rf_5_lime_2 %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_5_változó = feature, 
         rf_5_átlagos_súly = avg_weight)

rf_globál_lime_2 <-
  cbind (rf_1_globál_lime_2, rf_2_globál_lime_2, rf_3_globál_lime_2, rf_4_globál_lime_2, rf_5_globál_lime_2)
rf_absz_globál_lime_2 <-
  cbind (rf_1_absz_globál_lime_2, rf_2_absz_globál_lime_2, rf_3_absz_globál_lime_2, rf_4_absz_globál_lime_2, rf_5_absz_globál_lime_2)


#1.GBM LIME
gbm_1_lime_2 <- 
  explain(x = as.data.frame(teszt_mátrix_lokális_esetek_2),
          explainer = lime(as.data.frame(tanító_mátrix_1), model = gbm_1),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

gbm_1_lime_2 = data.frame(gbm_1_lime_2)

gbm_1_globál_lime_2 <- 
  gbm_1_lime_2 %>%
  mutate(feature = 
           case_when(
             grepl('légitársaság', feature) == TRUE ~ 'légitársaság', 
             grepl('kiinduló_város', feature) == TRUE ~ 'kiinduló_város', 
             grepl('indulási_napszak', feature) == TRUE ~ 'indulási_napszak', 
             grepl('megállások_száma', feature) == TRUE ~ 'megállások_száma', 
             grepl('érkezési_napszak', feature) == TRUE ~ 'érkezési_napszak', 
             grepl('úticél', feature) == TRUE ~ 'úticél', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály', 
             grepl('utazási_idő', feature) == TRUE ~ 'utazási_idő', 
             grepl('hátralévő_napok_száma', feature) == TRUE ~ 'hátralévő_napok_száma', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály'
           )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(gbm_1_változó = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_1_absz_globál_lime_2 <- 
  gbm_1_lime_2 %>%
  mutate(feature = 
           case_when(
             grepl('légitársaság', feature) == TRUE ~ 'légitársaság', 
             grepl('kiinduló_város', feature) == TRUE ~ 'kiinduló_város', 
             grepl('indulási_napszak', feature) == TRUE ~ 'indulási_napszak', 
             grepl('megállások_száma', feature) == TRUE ~ 'megállások_száma', 
             grepl('érkezési_napszak', feature) == TRUE ~ 'érkezési_napszak', 
             grepl('úticél', feature) == TRUE ~ 'úticél', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály', 
             grepl('utazási_idő', feature) == TRUE ~ 'utazási_idő', 
             grepl('hátralévő_napok_száma', feature) == TRUE ~ 'hátralévő_napok_száma', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály'
           )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(gbm_1_változó = feature, 
         gbm_1_átlagos_súly = avg_weight)

# 2.GBM LIME
gbm_2_lime_2 <- 
  explain(x = as.data.frame(teszt_mátrix_lokális_esetek_2),
          explainer = lime(as.data.frame(tanító_mátrix_2), model = gbm_2),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

gbm_2_lime_2 = data.frame(gbm_2_lime_2)

gbm_2_globál_lime_2 <- 
  gbm_2_lime_2 %>%
  mutate(feature = 
           case_when(
             grepl('légitársaság', feature) == TRUE ~ 'légitársaság', 
             grepl('kiinduló_város', feature) == TRUE ~ 'kiinduló_város', 
             grepl('indulási_napszak', feature) == TRUE ~ 'indulási_napszak', 
             grepl('megállások_száma', feature) == TRUE ~ 'megállások_száma', 
             grepl('érkezési_napszak', feature) == TRUE ~ 'érkezési_napszak', 
             grepl('úticél', feature) == TRUE ~ 'úticél', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály', 
             grepl('utazási_idő', feature) == TRUE ~ 'utazási_idő', 
             grepl('hátralévő_napok_száma', feature) == TRUE ~ 'hátralévő_napok_száma', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály'
           )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(gbm_2_változó = feature, 
         gbm_2_átlagos_súly = avg_weight)

gbm_2_absz_globál_lime_2 <- 
  gbm_2_lime_2 %>%
  mutate(feature = 
           case_when(
             grepl('légitársaság', feature) == TRUE ~ 'légitársaság', 
             grepl('kiinduló_város', feature) == TRUE ~ 'kiinduló_város', 
             grepl('indulási_napszak', feature) == TRUE ~ 'indulási_napszak', 
             grepl('megállások_száma', feature) == TRUE ~ 'megállások_száma', 
             grepl('érkezési_napszak', feature) == TRUE ~ 'érkezési_napszak', 
             grepl('úticél', feature) == TRUE ~ 'úticél', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály', 
             grepl('utazási_idő', feature) == TRUE ~ 'utazási_idő', 
             grepl('hátralévő_napok_száma', feature) == TRUE ~ 'hátralévő_napok_száma', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály'
           )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(gbm_2_változó = feature, 
         gbm_2_átlagos_súly = avg_weight)


#3.GBM LIME
gbm_3_lime_2 <- 
  explain(x = as.data.frame(teszt_mátrix_lokális_esetek_2),
          explainer = lime(as.data.frame(tanító_mátrix_3), model = gbm_3),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

gbm_3_lime_2 = data.frame(gbm_3_lime_2)

gbm_3_globál_lime_2 <- 
  gbm_3_lime_2 %>%
  mutate(feature = 
           case_when(
             grepl('légitársaság', feature) == TRUE ~ 'légitársaság', 
             grepl('kiinduló_város', feature) == TRUE ~ 'kiinduló_város', 
             grepl('indulási_napszak', feature) == TRUE ~ 'indulási_napszak', 
             grepl('megállások_száma', feature) == TRUE ~ 'megállások_száma', 
             grepl('érkezési_napszak', feature) == TRUE ~ 'érkezési_napszak', 
             grepl('úticél', feature) == TRUE ~ 'úticél', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály', 
             grepl('utazási_idő', feature) == TRUE ~ 'utazási_idő', 
             grepl('hátralévő_napok_száma', feature) == TRUE ~ 'hátralévő_napok_száma', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály'
           )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(gbm_3_változó = feature, 
         gbm_3_átlagos_súly = avg_weight)

gbm_3_absz_globál_lime_2 <- 
  gbm_3_lime_2 %>%
  mutate(feature = 
           case_when(
             grepl('légitársaság', feature) == TRUE ~ 'légitársaság', 
             grepl('kiinduló_város', feature) == TRUE ~ 'kiinduló_város', 
             grepl('indulási_napszak', feature) == TRUE ~ 'indulási_napszak', 
             grepl('megállások_száma', feature) == TRUE ~ 'megállások_száma', 
             grepl('érkezési_napszak', feature) == TRUE ~ 'érkezési_napszak', 
             grepl('úticél', feature) == TRUE ~ 'úticél', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály', 
             grepl('utazási_idő', feature) == TRUE ~ 'utazási_idő', 
             grepl('hátralévő_napok_száma', feature) == TRUE ~ 'hátralévő_napok_száma', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály'
           )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(gbm_3_változó = feature, 
         gbm_3_átlagos_súly = avg_weight)

#4.GBM LIME
gbm_4_lime_2 <- 
  explain(x = as.data.frame(teszt_mátrix_lokális_esetek_2),
          explainer = lime(as.data.frame(tanító_mátrix_4), model = gbm_4),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

gbm_4_lime_2 = data.frame(gbm_4_lime_2)

gbm_4_globál_lime_2 <- 
  gbm_4_lime_2 %>%
  mutate(feature = 
           case_when(
             grepl('légitársaság', feature) == TRUE ~ 'légitársaság', 
             grepl('kiinduló_város', feature) == TRUE ~ 'kiinduló_város', 
             grepl('indulási_napszak', feature) == TRUE ~ 'indulási_napszak', 
             grepl('megállások_száma', feature) == TRUE ~ 'megállások_száma', 
             grepl('érkezési_napszak', feature) == TRUE ~ 'érkezési_napszak', 
             grepl('úticél', feature) == TRUE ~ 'úticél', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály', 
             grepl('utazási_idő', feature) == TRUE ~ 'utazási_idő', 
             grepl('hátralévő_napok_száma', feature) == TRUE ~ 'hátralévő_napok_száma', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály'
           )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(gbm_4_változó = feature, 
         gbm_4_átlagos_súly = avg_weight)

gbm_4_absz_globál_lime_2 <- 
  gbm_4_lime_2 %>%
  mutate(feature = 
           case_when(
             grepl('légitársaság', feature) == TRUE ~ 'légitársaság', 
             grepl('kiinduló_város', feature) == TRUE ~ 'kiinduló_város', 
             grepl('indulási_napszak', feature) == TRUE ~ 'indulási_napszak', 
             grepl('megállások_száma', feature) == TRUE ~ 'megállások_száma', 
             grepl('érkezési_napszak', feature) == TRUE ~ 'érkezési_napszak', 
             grepl('úticél', feature) == TRUE ~ 'úticél', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály', 
             grepl('utazási_idő', feature) == TRUE ~ 'utazási_idő', 
             grepl('hátralévő_napok_száma', feature) == TRUE ~ 'hátralévő_napok_száma', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály'
           )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(gbm_4_változó = feature, 
         gbm_4_átlagos_súly = avg_weight)

#5.GBM LIME
gbm_5_lime_2 <- 
  explain(x = as.data.frame(teszt_mátrix_lokális_esetek_2),
          explainer = lime(as.data.frame(tanító_mátrix_5), model = gbm_5),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

gbm_5_lime_2 = data.frame(gbm_5_lime_2)

gbm_5_globál_lime_2 <- 
  gbm_5_lime_2 %>%
  mutate(feature = 
           case_when(
             grepl('légitársaság', feature) == TRUE ~ 'légitársaság', 
             grepl('kiinduló_város', feature) == TRUE ~ 'kiinduló_város', 
             grepl('indulási_napszak', feature) == TRUE ~ 'indulási_napszak', 
             grepl('megállások_száma', feature) == TRUE ~ 'megállások_száma', 
             grepl('érkezési_napszak', feature) == TRUE ~ 'érkezési_napszak', 
             grepl('úticél', feature) == TRUE ~ 'úticél', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály', 
             grepl('utazási_idő', feature) == TRUE ~ 'utazási_idő', 
             grepl('hátralévő_napok_száma', feature) == TRUE ~ 'hátralévő_napok_száma', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály'
           )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(gbm_5_változó = feature, 
         gbm_5_átlagos_súly = avg_weight)

gbm_5_absz_globál_lime_2 <- 
  gbm_5_lime_2 %>%
  mutate(feature = 
           case_when(
             grepl('légitársaság', feature) == TRUE ~ 'légitársaság', 
             grepl('kiinduló_város', feature) == TRUE ~ 'kiinduló_város', 
             grepl('indulási_napszak', feature) == TRUE ~ 'indulási_napszak', 
             grepl('megállások_száma', feature) == TRUE ~ 'megállások_száma', 
             grepl('érkezési_napszak', feature) == TRUE ~ 'érkezési_napszak', 
             grepl('úticél', feature) == TRUE ~ 'úticél', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály', 
             grepl('utazási_idő', feature) == TRUE ~ 'utazási_idő', 
             grepl('hátralévő_napok_száma', feature) == TRUE ~ 'hátralévő_napok_száma', 
             grepl('üzleti_osztály', feature) == TRUE ~ 'üzleti_osztály'
           )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(gbm_5_változó = feature, 
         gbm_5_átlagos_súly = avg_weight)

gbm_globál_lime_2 <-
  cbind (gbm_1_globál_lime_2, gbm_2_globál_lime_2, gbm_3_globál_lime_2, gbm_4_globál_lime_2, gbm_5_globál_lime_2)

gbm_absz_globál_lime_2 <-
  cbind (gbm_1_absz_globál_lime_2, gbm_2_absz_globál_lime_2, gbm_3_absz_globál_lime_2, gbm_4_absz_globál_lime_2, gbm_5_absz_globál_lime_2)