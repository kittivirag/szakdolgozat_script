library(lime)
#vizsgált adatsorok minden esetben
teszt_halmaz_minta = sample_n(teszt_halmaz, size = 300)
teszt_halmaz_lokális_esetek = teszt_halmaz_minta[, -10]
#GBM modelleknek speciális formátum
teszt_mátrix_lokális_esetek <- model.matrix(~ . - 1, data = teszt_halmaz_minta[, -10])

colnames(as.data.frame(teszt_mátrix_lokális_esetek))

#1.Lineáris regreszió LIME
lin_reg_1_lime <- 
  explain(x = teszt_halmaz_lokális_esetek,
          explainer = lime(tanító_halmaz_1[,-10], model = lin_reg_1),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

lin_reg_1_lime = data.frame(lin_reg_1_lime)

lin_reg_1_globál_lime <- 
  lin_reg_1_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_1_változó = feature, 
         lin_reg_1_átlagos_súly = avg_weight)

lin_reg_1_absz_globál_lime <- 
  lin_reg_1_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_1_változó = feature, 
         lin_reg_1_átlagos_súly = avg_weight)

#2.Lineáris regreszió LIME
lin_reg_2_lime <- 
  explain(x = teszt_halmaz_lokális_esetek,
          explainer = lime(tanító_halmaz_2[,-10], model = lin_reg_2),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

lin_reg_2_lime = data.frame(lin_reg_2_lime)

lin_reg_2_globál_lime <- 
  lin_reg_2_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_2_változó = feature, 
         lin_reg_2_átlagos_súly = avg_weight)

lin_reg_2_absz_globál_lime <- 
  lin_reg_2_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_2_változó = feature, 
         lin_reg_2_átlagos_súly = avg_weight)

#3.Lineáris regreszió LIME
lin_reg_3_lime <- 
  explain(x = teszt_halmaz_lokális_esetek,
          explainer = lime(tanító_halmaz_3[,-10], model = lin_reg_3),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

lin_reg_3_lime = data.frame(lin_reg_3_lime)

lin_reg_3_globál_lime <- 
  lin_reg_3_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_3_változó = feature, 
         lin_reg_3_átlagos_súly = avg_weight)

lin_reg_3_absz_globál_lime <- 
  lin_reg_3_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_3_változó = feature, 
         lin_reg_3_átlagos_súly = avg_weight)

#4.Lineáris regreszió LIME
lin_reg_4_lime <- 
  explain(x = teszt_halmaz_lokális_esetek,
          explainer = lime(tanító_halmaz_4[,-10], model = lin_reg_4),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

lin_reg_4_lime = data.frame(lin_reg_4_lime)

lin_reg_4_globál_lime <- 
  lin_reg_4_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_4_változó = feature, 
         lin_reg_4_átlagos_súly = avg_weight)

lin_reg_4_absz_globál_lime <- 
  lin_reg_4_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_4_változó = feature, 
         lin_reg_4_átlagos_súly = avg_weight)

#5.Lineáris regreszió LIME
lin_reg_5_lime <- 
  explain(x = teszt_halmaz_lokális_esetek,
          explainer = lime(tanító_halmaz_5[,-10], model = lin_reg_5),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

lin_reg_5_lime = data.frame(lin_reg_5_lime)

lin_reg_5_globál_lime <- 
  lin_reg_5_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_5_változó = feature, 
         lin_reg_5_átlagos_súly = avg_weight)

lin_reg_5_absz_globál_lime <- 
  lin_reg_5_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(lin_reg_5_változó = feature, 
         lin_reg_5_átlagos_súly = avg_weight)

lin_reg_globál_lime <-
  cbind (lin_reg_1_globál_lime, lin_reg_2_globál_lime, lin_reg_3_globál_lime, lin_reg_4_globál_lime, lin_reg_5_globál_lime)

lin_reg_absz_globál_lime <-
  cbind (lin_reg_1_absz_globál_lime, lin_reg_2_absz_globál_lime, lin_reg_3_absz_globál_lime, lin_reg_4_absz_globál_lime, lin_reg_5_absz_globál_lime)



#1.random forest LIME
rf_1_lime <- 
  explain(x = teszt_halmaz_lokális_esetek,
          explainer = lime(tanító_halmaz_1[,-10], model = rf_1),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

rf_1_lime = data.frame(rf_1_lime)

rf_1_globál_lime <- 
  rf_1_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_1_változó = feature, 
         rf_1_átlagos_súly = avg_weight)

rf_1_absz_globál_lime <- 
  rf_1_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_1_változó = feature, 
         rf_1_átlagos_súly = avg_weight)

#2.random forest LIME
rf_2_lime <- 
  explain(x = teszt_halmaz_lokális_esetek,
          explainer = lime(tanító_halmaz_2[,-10], model = rf_2),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

rf_2_lime = data.frame(rf_2_lime)

rf_2_globál_lime <- 
  rf_2_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_2_változó = feature, 
         rf_2_átlagos_súly = avg_weight)

rf_2_absz_globál_lime <- 
  rf_2_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_2_változó = feature, 
         rf_2_átlagos_súly = avg_weight)

#3.random forest LIME
rf_3_lime <- 
  explain(x = teszt_halmaz_lokális_esetek,
          explainer = lime(tanító_halmaz_3[,-10], model = rf_3),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

rf_3_lime = data.frame(rf_3_lime)

rf_3_globál_lime <- 
  rf_3_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_3_változó = feature, 
         rf_3_átlagos_súly = avg_weight)

rf_3_absz_globál_lime <- 
  rf_3_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_3_változó = feature, 
         rf_3_átlagos_súly = avg_weight)

#4.random forest LIME
rf_4_lime <- 
  explain(x = teszt_halmaz_lokális_esetek,
          explainer = lime(tanító_halmaz_4[,-10], model = rf_4),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

rf_4_lime = data.frame(rf_4_lime)

rf_4_globál_lime <- 
  rf_4_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_4_változó = feature, 
         rf_4_átlagos_súly = avg_weight)

rf_4_absz_globál_lime <- 
  rf_4_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_4_változó = feature, 
         rf_4_átlagos_súly = avg_weight)

#5.random forest LIME
rf_5_lime <- 
  explain(x = teszt_halmaz_lokális_esetek,
          explainer = lime(tanító_halmaz_5[,-10], model = rf_5),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

rf_5_lime = data.frame(rf_5_lime)

rf_5_globál_lime <- 
  rf_5_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_5_változó = feature, 
         rf_5_átlagos_súly = avg_weight)

rf_5_absz_globál_lime <- 
  rf_5_lime %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(abs(avg_weight))) %>%
  rename(rf_5_változó = feature, 
         rf_5_átlagos_súly = avg_weight)

rf_globál_lime <-
  cbind (rf_1_globál_lime, rf_2_globál_lime, rf_3_globál_lime, rf_4_globál_lime, rf_5_globál_lime)

rf_absz_globál_lime <-
  cbind (rf_1_absz_globál_lime, rf_2_absz_globál_lime, rf_3_absz_globál_lime, rf_4_absz_globál_lime, rf_5_absz_globál_lime)

#1.GBM LIME
gbm_1_lime <- 
  explain(x = as.data.frame(teszt_mátrix_lokális_esetek),
          explainer = lime(as.data.frame(tanító_mátrix_1), model = gbm_1),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

gbm_1_lime = data.frame(gbm_1_lime)

gbm_1_globál_lime <- 
  gbm_1_lime %>%
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

gbm_1_absz_globál_lime <- 
  gbm_1_lime %>%
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
gbm_2_lime <- 
  explain(x = as.data.frame(teszt_mátrix_lokális_esetek),
          explainer = lime(as.data.frame(tanító_mátrix_2), model = gbm_2),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

gbm_2_lime = data.frame(gbm_2_lime)

gbm_2_globál_lime <- 
  gbm_2_lime %>%
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

gbm_2_absz_globál_lime <- 
  gbm_2_lime %>%
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
gbm_3_lime <- 
  explain(x = as.data.frame(teszt_mátrix_lokális_esetek),
          explainer = lime(as.data.frame(tanító_mátrix_3), model = gbm_3),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

gbm_3_lime = data.frame(gbm_3_lime)

gbm_3_globál_lime <- 
  gbm_3_lime %>%
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

gbm_3_absz_globál_lime <- 
  gbm_3_lime %>%
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
gbm_4_lime <- 
  explain(x = as.data.frame(teszt_mátrix_lokális_esetek),
          explainer = lime(as.data.frame(tanító_mátrix_4), model = gbm_4),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

gbm_4_lime = data.frame(gbm_4_lime)

gbm_4_globál_lime <- 
  gbm_4_lime %>%
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

gbm_4_absz_globál_lime <- 
  gbm_4_lime %>%
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
gbm_5_lime <- 
  explain(x = as.data.frame(teszt_mátrix_lokális_esetek),
          explainer = lime(as.data.frame(tanító_mátrix_5), model = gbm_5),
          n_features = 9,        # Hány legfontosabb jellemzőt jelenítsen meg
          kernel_width = 0.5)     # Kernel szélesség, az adat környezetének meghatározására

gbm_5_lime = data.frame(gbm_5_lime)

gbm_5_globál_lime <- 
  gbm_5_lime %>%
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

gbm_5_absz_globál_lime <- 
  gbm_5_lime %>%
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

gbm_globál_lime <-
  cbind (gbm_1_globál_lime, gbm_2_globál_lime, gbm_3_globál_lime, gbm_4_globál_lime, gbm_5_globál_lime)

gbm_absz_globál_lime <-
  cbind (gbm_1_absz_globál_lime, gbm_2_absz_globál_lime, gbm_3_absz_globál_lime, gbm_4_absz_globál_lime, gbm_5_absz_globál_lime)
