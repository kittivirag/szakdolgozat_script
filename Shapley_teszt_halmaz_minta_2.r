library(iml)
#1.Lineáris regreszió Shapley 2
lin_reg_1_predictor_2 <- Predictor$new(
  model = lin_reg_1,
  data = teszt_halmaz_lokális_esetek_2,
  y = teszt_halmaz_minta_2$ár)

lin_reg_1_shap_2 <- lapply(1:nrow(teszt_halmaz_lokális_esetek_2), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek_2[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = lin_reg_1_predictor_2, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

lin_reg_1_shap_táblázat_2 <- do.call(rbind, lin_reg_1_shap_2)

lin_reg_1_globális_shapley_átlag_2 <- 
  lin_reg_1_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

lin_reg_1_globális_shapley_összeg_2 <- 
  lin_reg_1_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

lin_reg_1_globális_shapley_absz_átlag_2 <- 
  lin_reg_1_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

lin_reg_1_globális_shapley_absz_összeg_2 <- 
  lin_reg_1_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#2.Lineáris regreszió Shapley 2
lin_reg_2_predictor_2 <- Predictor$new(
  model = lin_reg_2,
  data = teszt_halmaz_lokális_esetek_2,
  y = teszt_halmaz_minta_2$ár)

lin_reg_2_shap_2 <- lapply(1:nrow(teszt_halmaz_lokális_esetek_2), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek_2[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = lin_reg_2_predictor_2, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

lin_reg_2_shap_táblázat_2 <- do.call(rbind, lin_reg_2_shap_2)

lin_reg_2_globális_shapley_átlag_2 <- 
  lin_reg_2_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

lin_reg_2_globális_shapley_összeg_2 <- 
  lin_reg_2_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

lin_reg_2_globális_shapley_absz_átlag_2 <- 
  lin_reg_2_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

lin_reg_2_globális_shapley_absz_összeg_2 <- 
  lin_reg_2_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#3.Lineáris regreszió Shapley 2
lin_reg_3_predictor_2 <- Predictor$new(
  model = lin_reg_3,
  data = teszt_halmaz_lokális_esetek_2,
  y = teszt_halmaz_minta_2$ár)

lin_reg_3_shap_2 <- lapply(1:nrow(teszt_halmaz_lokális_esetek_2), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek_2[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = lin_reg_3_predictor_2, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

lin_reg_3_shap_táblázat_2 <- do.call(rbind, lin_reg_3_shap_2)

lin_reg_3_globális_shapley_átlag_2 <- 
  lin_reg_3_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

lin_reg_3_globális_shapley_összeg_2 <- 
  lin_reg_3_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

lin_reg_3_globális_shapley_absz_átlag_2 <- 
  lin_reg_3_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

lin_reg_3_globális_shapley_absz_összeg_2 <- 
  lin_reg_3_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#4.Lineáris regreszió Shapley 2
lin_reg_4_predictor_2 <- Predictor$new(
  model = lin_reg_4,
  data = teszt_halmaz_lokális_esetek_2,
  y = teszt_halmaz_minta_2$ár)

lin_reg_4_shap_2 <- lapply(1:nrow(teszt_halmaz_lokális_esetek_2), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek_2[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = lin_reg_4_predictor_2, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

lin_reg_4_shap_táblázat_2 <- do.call(rbind, lin_reg_4_shap_2)

lin_reg_4_globális_shapley_átlag_2 <- 
  lin_reg_4_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

lin_reg_4_globális_shapley_összeg_2 <- 
  lin_reg_4_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

lin_reg_4_globális_shapley_absz_átlag_2 <- 
  lin_reg_4_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

lin_reg_4_globális_shapley_absz_összeg_2 <- 
  lin_reg_4_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#5.Lineáris regreszió Shapley 2
lin_reg_5_predictor_2 <- Predictor$new(
  model = lin_reg_5,
  data = teszt_halmaz_lokális_esetek_2,
  y = teszt_halmaz_minta_2$ár)

lin_reg_5_shap_2 <- lapply(1:nrow(teszt_halmaz_lokális_esetek_2), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek_2[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = lin_reg_5_predictor_2, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

lin_reg_5_shap_táblázat_2 <- do.call(rbind, lin_reg_5_shap_2)

lin_reg_5_globális_shapley_átlag_2 <- 
  lin_reg_5_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

lin_reg_5_globális_shapley_összeg_2 <- 
  lin_reg_5_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

lin_reg_5_globális_shapley_absz_átlag_2 <- 
  lin_reg_5_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

lin_reg_5_globális_shapley_absz_összeg_2 <- 
  lin_reg_5_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

lin_reg_globális_shapley_átlag_2 <-
  cbind(lin_reg_1_globális_shapley_átlag_2, lin_reg_2_globális_shapley_átlag_2, lin_reg_3_globális_shapley_átlag_2, lin_reg_4_globális_shapley_átlag_2, lin_reg_5_globális_shapley_átlag_2)

lin_reg_globális_shapley_összeg_2 <-
  cbind(lin_reg_1_globális_shapley_összeg_2, lin_reg_2_globális_shapley_összeg_2, lin_reg_3_globális_shapley_összeg_2, lin_reg_4_globális_shapley_összeg_2, lin_reg_5_globális_shapley_összeg_2)

lin_reg_globális_shapley_absz_átlag_2 <-
  cbind(lin_reg_1_globális_shapley_absz_átlag_2, lin_reg_2_globális_shapley_absz_átlag_2, lin_reg_3_globális_shapley_absz_átlag_2, lin_reg_4_globális_shapley_absz_átlag_2, lin_reg_5_globális_shapley_absz_átlag_2)

lin_reg_globális_shapley_absz_összeg_2 <-
  cbind(lin_reg_1_globális_shapley_absz_összeg_2, lin_reg_2_globális_shapley_absz_összeg_2, lin_reg_3_globális_shapley_absz_összeg_2, lin_reg_4_globális_shapley_absz_összeg_2, lin_reg_5_globális_shapley_absz_összeg_2)

#1. Random forest Shapley 2
rf_1_predictor_2 <- Predictor$new(
  model = rf_1,
  data = teszt_halmaz_lokális_esetek_2,
  y = teszt_halmaz_minta_2$ár)

rf_1_shap_2 <- lapply(1:nrow(teszt_halmaz_lokális_esetek_2), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek_2[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = rf_1_predictor_2, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

rf_1_shap_táblázat_2 <- do.call(rbind, rf_1_shap_2)

rf_1_globális_shapley_átlag_2 <- 
  rf_1_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

rf_1_globális_shapley_összeg_2 <- 
  rf_1_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

rf_1_globális_shapley_absz_átlag_2 <- 
  rf_1_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

rf_1_globális_shapley_absz_összeg_2 <- 
  rf_1_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#2. Random forest Shapley 2
rf_2_predictor_2 <- Predictor$new(
  model = rf_2,
  data = teszt_halmaz_lokális_esetek_2,
  y = teszt_halmaz_minta_2$ár)

rf_2_shap_2 <- lapply(1:nrow(teszt_halmaz_lokális_esetek_2), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek_2[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = rf_2_predictor_2, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

rf_2_shap_táblázat_2 <- do.call(rbind, rf_2_shap_2)

rf_2_globális_shapley_átlag_2 <- 
  rf_2_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

rf_2_globális_shapley_összeg_2 <- 
  rf_2_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

rf_2_globális_shapley_absz_átlag_2 <- 
  rf_2_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

rf_2_globális_shapley_absz_összeg_2 <- 
  rf_2_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#3. Random forest Shapley 2
rf_3_predictor_2 <- Predictor$new(
  model = rf_3,
  data = teszt_halmaz_lokális_esetek_2,
  y = teszt_halmaz_minta_2$ár)

rf_3_shap_2 <- lapply(1:nrow(teszt_halmaz_lokális_esetek_2), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek_2[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = rf_3_predictor_2, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

rf_3_shap_táblázat_2 <- do.call(rbind, rf_3_shap_2)

rf_3_globális_shapley_átlag_2 <- 
  rf_3_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

rf_3_globális_shapley_összeg_2 <- 
  rf_3_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

rf_3_globális_shapley_absz_átlag_2 <- 
  rf_3_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

rf_3_globális_shapley_absz_összeg_2 <- 
  rf_3_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#4. Random forest Shapley 2
rf_4_predictor_2 <- Predictor$new(
  model = rf_4,
  data = teszt_halmaz_lokális_esetek_2,
  y = teszt_halmaz_minta_2$ár)

rf_4_shap_2 <- lapply(1:nrow(teszt_halmaz_lokális_esetek_2), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek_2[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = rf_4_predictor_2, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

rf_4_shap_táblázat_2 <- do.call(rbind, rf_4_shap_2)

rf_4_globális_shapley_átlag_2 <- 
  rf_4_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

rf_4_globális_shapley_összeg_2 <- 
  rf_4_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

rf_4_globális_shapley_absz_átlag_2 <- 
  rf_4_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

rf_4_globális_shapley_absz_összeg_2 <- 
  rf_4_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#5. Random forest Shapley 2
rf_5_predictor_2 <- Predictor$new(
  model = rf_5,
  data = teszt_halmaz_lokális_esetek_2,
  y = teszt_halmaz_minta_2$ár)

rf_5_shap_2 <- lapply(1:nrow(teszt_halmaz_lokális_esetek_2), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek_2[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = rf_5_predictor_2, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

rf_5_shap_táblázat_2 <- do.call(rbind, rf_5_shap_2)

rf_5_globális_shapley_átlag_2 <- 
  rf_5_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

rf_5_globális_shapley_összeg_2 <- 
  rf_5_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

rf_5_globális_shapley_absz_átlag_2 <- 
  rf_5_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

rf_5_globális_shapley_absz_összeg_2 <- 
  rf_5_shap_táblázat_2 %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

rf_globális_shapley_átlag_2 <-
  cbind (rf_1_globális_shapley_átlag_2, rf_2_globális_shapley_átlag_2, rf_3_globális_shapley_átlag_2, rf_4_globális_shapley_átlag_2, rf_5_globális_shapley_átlag_2)

rf_globális_shapley_összeg_2  <-
  cbind (rf_1_globális_shapley_összeg_2, rf_2_globális_shapley_összeg_2, rf_3_globális_shapley_összeg_2, rf_4_globális_shapley_összeg_2, rf_5_globális_shapley_összeg_2)

rf_globális_shapley_absz_átlag_2  <-
  cbind(rf_1_globális_shapley_absz_átlag_2, rf_2_globális_shapley_absz_átlag_2, rf_3_globális_shapley_absz_átlag_2, rf_4_globális_shapley_absz_átlag_2, rf_5_globális_shapley_absz_átlag_2)

rf_globális_shapley_absz_összeg_2  <-
  cbind(rf_1_globális_shapley_absz_összeg_2, rf_2_globális_shapley_absz_összeg_2, rf_3_globális_shapley_absz_összeg_2, rf_4_globális_shapley_absz_összeg_2, rf_5_globális_shapley_absz_összeg_2)

#1. GBM Shapley 2
gbm_1_predictor_2 <- predict(gbm_1, as.matrix(teszt_mátrix_lokális_esetek_2), predcontrib = TRUE)

gbm_1_shap_2 <- as.data.frame(gbm_1_predictor_2)

gbm_1_shap_2 <- 
  gbm_1_shap_2 %>%
  select(-BIAS)

gbm_1_shap_táblázat_2 <- 
  gbm_1_shap_2 %>%
  pivot_longer(cols = everything(), 
               names_to = "feature", 
               values_to = "feature_weight")

gbm_1_globális_shapley_átlag_2 <- 
  gbm_1_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma'
  )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_1_globális_shapley_összeg_2 <- 
  gbm_1_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = sum(feature_weight)) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_1_globális_shapley_absz_átlag_2 <- 
  gbm_1_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_1_globális_shapley_absz_összeg_2 <- 
  gbm_1_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = sum(abs(feature_weight))) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

#2. GBM Shapley 2
gbm_2_predictor_2 <- predict(gbm_2, as.matrix(teszt_mátrix_lokális_esetek_2), predcontrib = TRUE)

gbm_2_shap_2 <- as.data.frame(gbm_2_predictor_2)

gbm_2_shap_2 <- 
  gbm_2_shap_2 %>%
  select(-BIAS)

gbm_2_shap_táblázat_2 <- 
  gbm_2_shap_2 %>%
  pivot_longer(cols = everything(), 
               names_to = "feature", 
               values_to = "feature_weight")

gbm_2_globális_shapley_átlag_2 <- 
  gbm_2_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma'
  )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_2_globális_shapley_összeg_2 <- 
  gbm_2_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = sum(feature_weight)) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_2_globális_shapley_absz_átlag_2 <- 
  gbm_2_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_2_globális_shapley_absz_összeg_2 <- 
  gbm_2_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = sum(abs(feature_weight))) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

#3. GBM Shapley 2
gbm_3_predictor_2 <- predict(gbm_3, as.matrix(teszt_mátrix_lokális_esetek_2), predcontrib = TRUE)

gbm_3_shap_2 <- as.data.frame(gbm_3_predictor_2)

gbm_3_shap_2 <- 
  gbm_3_shap_2 %>%
  select(-BIAS)

gbm_3_shap_táblázat_2 <- 
  gbm_3_shap_2 %>%
  pivot_longer(cols = everything(), 
               names_to = "feature", 
               values_to = "feature_weight")

gbm_3_globális_shapley_átlag_2 <- 
  gbm_3_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma'
  )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_3_globális_shapley_összeg_2 <- 
  gbm_3_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = sum(feature_weight)) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_3_globális_shapley_absz_átlag_2 <- 
  gbm_3_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_3_globális_shapley_absz_összeg_2 <- 
  gbm_3_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = sum(abs(feature_weight))) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)


#4. GBM Shapley 2
gbm_4_predictor_2 <- predict(gbm_1, as.matrix(teszt_mátrix_lokális_esetek_2), predcontrib = TRUE)

gbm_4_shap_2 <- as.data.frame(gbm_1_predictor_2)

gbm_4_shap_2 <- 
  gbm_4_shap_2 %>%
  select(-BIAS)

gbm_4_shap_táblázat_2 <- 
  gbm_4_shap_2 %>%
  pivot_longer(cols = everything(), 
               names_to = "feature", 
               values_to = "feature_weight")

gbm_4_globális_shapley_átlag_2 <- 
  gbm_4_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma'
  )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_4_globális_shapley_összeg_2 <- 
  gbm_4_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = sum(feature_weight)) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_4_globális_shapley_absz_átlag_2 <- 
  gbm_4_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_4_globális_shapley_absz_összeg_2 <- 
  gbm_4_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = sum(abs(feature_weight))) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

#5. GBM Shapley 2
gbm_5_predictor_2 <- predict(gbm_5, as.matrix(teszt_mátrix_lokális_esetek_2), predcontrib = TRUE)

gbm_5_shap_2 <- as.data.frame(gbm_5_predictor_2)

gbm_5_shap_2 <- 
  gbm_5_shap_2 %>%
  select(-BIAS)

gbm_5_shap_táblázat_2 <- 
  gbm_5_shap_2 %>%
  pivot_longer(cols = everything(), 
               names_to = "feature", 
               values_to = "feature_weight")

gbm_5_globális_shapley_átlag_2 <- 
  gbm_5_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma'
  )) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(feature_weight)) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_5_globális_shapley_összeg_2 <- 
  gbm_5_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = sum(feature_weight)) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_5_globális_shapley_absz_átlag_2 <- 
  gbm_5_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = mean(abs(feature_weight))) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)

gbm_5_globális_shapley_absz_összeg_2 <- 
  gbm_5_shap_táblázat_2 %>%
  mutate(feature = case_when(
    grepl('légitársaság', feature) ~ 'légitársaság', 
    grepl('kiinduló_város', feature) ~ 'kiinduló_város', 
    grepl('indulási_napszak', feature) ~ 'indulási_napszak', 
    grepl('megállások_száma', feature) ~ 'megállások_száma', 
    grepl('érkezési_napszak', feature) ~ 'érkezési_napszak', 
    grepl('úticél', feature) ~ 'úticél', 
    grepl('üzleti_osztály', feature) ~ 'üzleti_osztály', 
    grepl('utazási_idő', feature) ~ 'utazási_idő', 
    grepl('hátralévő_napok_száma', feature) ~ 'hátralévő_napok_száma')) %>%
  group_by(feature) %>%
  summarise(avg_weight = sum(abs(feature_weight))) %>%
  arrange(desc(avg_weight)) %>%
  rename(gbm_1 = feature, 
         gbm_1_átlagos_súly = avg_weight)


gbm_globális_shapley_átlag_2 <-
  cbind(gbm_1_globális_shapley_átlag_2, gbm_2_globális_shapley_átlag_2, gbm_3_globális_shapley_átlag_2, gbm_4_globális_shapley_átlag_2, gbm_5_globális_shapley_átlag_2)

gbm_globális_shapley_összeg_2 <-
  cbind(gbm_1_globális_shapley_összeg_2, gbm_2_globális_shapley_összeg_2, gbm_3_globális_shapley_összeg_2, gbm_4_globális_shapley_összeg_2, gbm_5_globális_shapley_összeg_2)

gbm_globális_shapley_absz_átlag_2 <-
  cbind(gbm_1_globális_shapley_absz_átlag_2, gbm_2_globális_shapley_absz_átlag_2, gbm_3_globális_shapley_absz_átlag_2, gbm_4_globális_shapley_absz_átlag_2, gbm_5_globális_shapley_absz_átlag_2)

gbm_globális_shapley_absz_összeg_2 <-
  cbind(gbm_1_globális_shapley_absz_összeg_2, gbm_2_globális_shapley_absz_összeg_2, gbm_3_globális_shapley_absz_összeg_2, gbm_4_globális_shapley_absz_összeg, gbm_5_globális_shapley_absz_összeg_2)
