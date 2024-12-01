library(iml)
#1.Lineáris regreszió Shapley
lin_reg_1_predictor <- Predictor$new(
  model = lin_reg_1,
  data = teszt_halmaz_lokális_esetek,
  y = teszt_halmaz_minta$ár)

lin_reg_1_shap <- lapply(1:nrow(teszt_halmaz_lokális_esetek), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = lin_reg_1_predictor, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

lin_reg_1_shap_táblázat <- do.call(rbind, lin_reg_1_shap)

lin_reg_1_globális_shapley_átlag <- 
  lin_reg_1_shap_táblázat %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

lin_reg_1_globális_shapley_összeg <- 
  lin_reg_1_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

lin_reg_1_globális_shapley_absz_átlag <- 
  lin_reg_1_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

lin_reg_1_globális_shapley_absz_összeg <- 
  lin_reg_1_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))
          
#2.Lineáris regreszió Shapley
lin_reg_2_predictor <- Predictor$new(
  model = lin_reg_2,
  data = teszt_halmaz_lokális_esetek,
  y = teszt_halmaz_minta$ár)

lin_reg_2_shap <- lapply(1:nrow(teszt_halmaz_lokális_esetek), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = lin_reg_2_predictor, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

lin_reg_2_shap_táblázat <- do.call(rbind, lin_reg_2_shap)

lin_reg_2_globális_shapley_átlag <- 
  lin_reg_2_shap_táblázat %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

lin_reg_2_globális_shapley_összeg <- 
  lin_reg_2_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

lin_reg_2_globális_shapley_absz_átlag <- 
  lin_reg_2_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

lin_reg_2_globális_shapley_absz_összeg <- 
  lin_reg_2_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#3.Lineáris regreszió Shapley
lin_reg_3_predictor <- Predictor$new(
  model = lin_reg_3,
  data = teszt_halmaz_lokális_esetek,
  y = teszt_halmaz_minta$ár)

lin_reg_3_shap <- lapply(1:nrow(teszt_halmaz_lokális_esetek), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = lin_reg_3_predictor, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

lin_reg_3_shap_táblázat <- do.call(rbind, lin_reg_3_shap)

lin_reg_3_globális_shapley_átlag <- 
  lin_reg_3_shap_táblázat %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

lin_reg_3_globális_shapley_összeg <- 
  lin_reg_3_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

lin_reg_3_globális_shapley_absz_átlag <- 
  lin_reg_3_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

lin_reg_3_globális_shapley_absz_összeg <- 
  lin_reg_3_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))
          
#4.Lineáris regreszió Shapley
lin_reg_4_predictor <- Predictor$new(
  model = lin_reg_4,
  data = teszt_halmaz_lokális_esetek,
  y = teszt_halmaz_minta$ár)

lin_reg_4_shap <- lapply(1:nrow(teszt_halmaz_lokális_esetek), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = lin_reg_4_predictor, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

lin_reg_4_shap_táblázat <- do.call(rbind, lin_reg_4_shap)

lin_reg_4_globális_shapley_átlag <- 
  lin_reg_4_shap_táblázat %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

lin_reg_4_globális_shapley_összeg <- 
  lin_reg_4_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

lin_reg_4_globális_shapley_absz_átlag <- 
  lin_reg_4_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

lin_reg_4_globális_shapley_absz_összeg <- 
  lin_reg_4_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#5.Lineáris regreszió Shapley
lin_reg_5_predictor <- Predictor$new(
  model = lin_reg_5,
  data = teszt_halmaz_lokális_esetek,
  y = teszt_halmaz_minta$ár)

lin_reg_5_shap <- lapply(1:nrow(teszt_halmaz_lokális_esetek), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = lin_reg_5_predictor, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

lin_reg_5_shap_táblázat <- do.call(rbind, lin_reg_4_shap)

lin_reg_5_globális_shapley_átlag <- 
  lin_reg_5_shap_táblázat %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

lin_reg_5_globális_shapley_összeg <- 
  lin_reg_5_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

lin_reg_5_globális_shapley_absz_átlag <- 
  lin_reg_5_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

lin_reg_5_globális_shapley_absz_összeg <- 
  lin_reg_5_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

lin_reg_globális_shapley_átlag <-
  cbind(lin_reg_1_globális_shapley_átlag, lin_reg_2_globális_shapley_átlag, lin_reg_3_globális_shapley_átlag, lin_reg_4_globális_shapley_átlag, lin_reg_5_globális_shapley_átlag)

lin_reg_globális_shapley_összeg <-
  cbind(lin_reg_1_globális_shapley_összeg, lin_reg_2_globális_shapley_összeg, lin_reg_3_globális_shapley_összeg, lin_reg_4_globális_shapley_összeg, lin_reg_5_globális_shapley_összeg)

lin_reg_globális_shapley_absz_átlag <-
  cbind(lin_reg_1_globális_shapley_absz_átlag, lin_reg_2_globális_shapley_absz_átlag, lin_reg_3_globális_shapley_absz_átlag, lin_reg_4_globális_shapley_absz_átlag, lin_reg_5_globális_shapley_absz_átlag)

lin_reg_globális_shapley_absz_összeg <-
  cbind(lin_reg_1_globális_shapley_absz_összeg, lin_reg_2_globális_shapley_absz_összeg, lin_reg_3_globális_shapley_absz_összeg, lin_reg_4_globális_shapley_absz_összeg, lin_reg_5_globális_shapley_absz_összeg)

#1. Random forest Shapley
rf_1_predictor <- Predictor$new(
  model = rf_1,
  data = teszt_halmaz_lokális_esetek,
  y = teszt_halmaz_minta$ár)

rf_1_shap <- lapply(1:nrow(teszt_halmaz_lokális_esetek), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = rf_1_predictor, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

rf_1_shap_táblázat <- do.call(rbind, rf_1_shap)

rf_1_globális_shapley_átlag <- 
  rf_1_shap_táblázat %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

rf_1_globális_shapley_összeg <- 
  rf_1_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

rf_1_globális_shapley_absz_átlag <- 
  rf_1_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

rf_1_globális_shapley_absz_összeg <- 
  rf_1_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#2. Random forest Shapley
rf_2_predictor <- Predictor$new(
  model = rf_2,
  data = teszt_halmaz_lokális_esetek,
  y = teszt_halmaz_minta$ár)

rf_2_shap <- lapply(1:nrow(teszt_halmaz_lokális_esetek), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = rf_2_predictor, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

rf_2_shap_táblázat <- do.call(rbind, rf_2_shap)

rf_2_globális_shapley_átlag <- 
  rf_2_shap_táblázat %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

rf_2_globális_shapley_összeg <- 
  rf_2_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

rf_2_globális_shapley_absz_átlag <- 
  rf_2_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

rf_2_globális_shapley_absz_összeg <- 
  rf_2_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#3. Random forest Shapley
rf_3_predictor <- Predictor$new(
  model = rf_3,
  data = teszt_halmaz_lokális_esetek,
  y = teszt_halmaz_minta$ár)

rf_3_shap <- lapply(1:nrow(teszt_halmaz_lokális_esetek), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = rf_3_predictor, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

rf_3_shap_táblázat <- do.call(rbind, rf_3_shap)

rf_3_globális_shapley_átlag <- 
  rf_3_shap_táblázat %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

rf_3_globális_shapley_összeg <- 
  rf_3_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

rf_3_globális_shapley_absz_átlag <- 
  rf_3_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

rf_3_globális_shapley_absz_összeg <- 
  rf_3_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#4. Random forest Shapley
rf_4_predictor <- Predictor$new(
  model = rf_4,
  data = teszt_halmaz_lokális_esetek,
  y = teszt_halmaz_minta$ár)

rf_4_shap <- lapply(1:nrow(teszt_halmaz_lokális_esetek), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = rf_4_predictor, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

rf_4_shap_táblázat <- do.call(rbind, rf_4_shap)

rf_4_globális_shapley_átlag <- 
  rf_4_shap_táblázat %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

rf_4_globális_shapley_összeg <- 
  rf_4_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

rf_4_globális_shapley_absz_átlag <- 
  rf_4_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

rf_4_globális_shapley_absz_összeg <- 
  rf_4_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))

#5. Random forest Shapley
rf_5_predictor <- Predictor$new(
  model = rf_5,
  data = teszt_halmaz_lokális_esetek,
  y = teszt_halmaz_minta$ár)

rf_5_shap <- lapply(1:nrow(teszt_halmaz_lokális_esetek), function(i) {
  egy_megfigyelés <- teszt_halmaz_lokális_esetek[i, , drop = FALSE]
  shapley <- Shapley$new(predictor = rf_5_predictor, x.interest = egy_megfigyelés)
  data.frame(sor_index = i, shapley$results)})

rf_5_shap_táblázat <- do.call(rbind, rf_5_shap)

rf_5_globális_shapley_átlag <- 
  rf_5_shap_táblázat %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(phi)) %>%
  arrange(desc(abs(mean_phi)))

rf_5_globális_shapley_összeg <- 
  rf_5_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(phi)) %>%
  arrange(desc(abs(phi)))

rf_5_globális_shapley_absz_átlag <- 
  rf_5_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = mean(abs(phi))) %>%
  arrange(desc(abs(phi)))

rf_5_globális_shapley_absz_összeg <- 
  rf_5_shap_táblázat %>%
  group_by(feature) %>%
  summarise(phi = sum(abs(phi))) %>%
  arrange(desc(phi))


rf_globális_shapley_átlag <-
  cbind(rf_1_globális_shapley_átlag, rf_2_globális_shapley_átlag, rf_3_globális_shapley_átlag, rf_4_globális_shapley_átlag, rf_5_globális_shapley_átlag)

rf_globális_shapley_összeg <-
  cbind(rf_1_globális_shapley_összeg, rf_2_globális_shapley_összeg, rf_3_globális_shapley_összeg, rf_4_globális_shapley_összeg, rf_5_globális_shapley_összeg)

rf_globális_shapley_absz_átlag <-
  cbind(rf_1_globális_shapley_absz_átlag, rf_2_globális_shapley_absz_átlag, rf_3_globális_shapley_absz_átlag, rf_4_globális_shapley_absz_átlag, rf_5_globális_shapley_absz_átlag)

rf_globális_shapley_absz_összeg <-
  cbind(rf_1_globális_shapley_absz_összeg, rf_2_globális_shapley_absz_összeg, rf_3_globális_shapley_absz_összeg, rf_4_globális_shapley_absz_összeg, rf_5_globális_shapley_absz_összeg)

#1. GBM Shapley
gbm_1_predictor <- predict(gbm_1, as.matrix(teszt_mátrix_lokális_esetek), predcontrib = TRUE)

gbm_1_shap <- as.data.frame(gbm_1_predictor)

gbm_1_shap <- 
  gbm_1_shap %>%
  select(-BIAS)

gbm_1_shap_táblázat <- 
  gbm_1_shap %>%
  pivot_longer(cols = everything(), 
               names_to = "feature", 
               values_to = "feature_weight")

gbm_1_globális_shapley_átlag <- 
  gbm_1_shap_táblázat %>%
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

gbm_1_globális_shapley_összeg <- 
  gbm_1_shap_táblázat %>%
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

gbm_1_globális_shapley_absz_átlag <- 
  gbm_1_shap_táblázat %>%
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

gbm_1_globális_shapley_absz_összeg <- 
  gbm_1_shap_táblázat %>%
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

#2. GBM Shapley
gbm_2_predictor <- predict(gbm_2, as.matrix(teszt_mátrix_lokális_esetek), predcontrib = TRUE)

gbm_2_shap <- as.data.frame(gbm_2_predictor)

gbm_2_shap <- 
  gbm_2_shap %>%
  select(-BIAS)

gbm_2_shap_táblázat <- 
  gbm_2_shap %>%
  pivot_longer(cols = everything(), 
               names_to = "feature", 
               values_to = "feature_weight")

gbm_2_globális_shapley_átlag <- 
  gbm_2_shap_táblázat %>%
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
  rename(gbm_2 = feature, 
         gbm_2_átlagos_súly = avg_weight)

gbm_2_globális_shapley_összeg <- 
  gbm_2_shap_táblázat %>%
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
  rename(gbm_2 = feature, 
         gbm_2_átlagos_súly = avg_weight)

gbm_2_globális_shapley_absz_átlag <- 
  gbm_2_shap_táblázat %>%
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
  rename(gbm_2 = feature, 
         gbm_2_átlagos_súly = avg_weight)

gbm_2_globális_shapley_absz_összeg <- 
  gbm_2_shap_táblázat %>%
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
  rename(gbm_2 = feature, 
         gbm_2_átlagos_súly = avg_weight)

#3. GBM Shapley
gbm_3_predictor <- predict(gbm_3, as.matrix(teszt_mátrix_lokális_esetek), predcontrib = TRUE)

gbm_3_shap <- as.data.frame(gbm_3_predictor)

gbm_3_shap <- 
  gbm_3_shap %>%
  select(-BIAS)

gbm_3_shap_táblázat <- 
  gbm_3_shap %>%
  pivot_longer(cols = everything(), 
               names_to = "feature", 
               values_to = "feature_weight")

gbm_3_globális_shapley_átlag <- 
  gbm_3_shap_táblázat %>%
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
  rename(gbm_3 = feature, 
         gbm_3_átlagos_súly = avg_weight)

gbm_3_globális_shapley_összeg <- 
  gbm_3_shap_táblázat %>%
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
  rename(gbm_3 = feature, 
         gbm_3_átlagos_súly = avg_weight)

gbm_3_globális_shapley_absz_átlag <- 
  gbm_3_shap_táblázat %>%
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
  rename(gbm_3 = feature, 
         gbm_3_átlagos_súly = avg_weight)

gbm_3_globális_shapley_absz_összeg <- 
  gbm_3_shap_táblázat %>%
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
  rename(gbm_3 = feature, 
         gbm_3_átlagos_súly = avg_weight)

#4. GBM Shapley
gbm_4_predictor <- predict(gbm_4, as.matrix(teszt_mátrix_lokális_esetek), predcontrib = TRUE)

gbm_4_shap <- as.data.frame(gbm_4_predictor)

gbm_4_shap <- 
  gbm_4_shap %>%
  select(-BIAS)

gbm_4_shap_táblázat <- 
  gbm_4_shap %>%
  pivot_longer(cols = everything(), 
               names_to = "feature", 
               values_to = "feature_weight")

gbm_4_globális_shapley_átlag <- 
  gbm_4_shap_táblázat %>%
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
  rename(gbm_4 = feature, 
         gbm_4_átlagos_súly = avg_weight)

gbm_4_globális_shapley_összeg <- 
  gbm_4_shap_táblázat %>%
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
  rename(gbm_4 = feature, 
         gbm_4_átlagos_súly = avg_weight)

gbm_4_globális_shapley_absz_átlag <- 
  gbm_4_shap_táblázat %>%
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
  rename(gbm_4 = feature, 
         gbm_4_átlagos_súly = avg_weight)

gbm_4_globális_shapley_absz_összeg <- 
  gbm_4_shap_táblázat %>%
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
  rename(gbm_4 = feature, 
         gbm_4_átlagos_súly = avg_weight)

#5. GBM Shapley
gbm_5_predictor <- predict(gbm_5, as.matrix(teszt_mátrix_lokális_esetek), predcontrib = TRUE)

gbm_5_shap <- as.data.frame(gbm_5_predictor)

gbm_5_shap <- 
  gbm_5_shap %>%
  select(-BIAS)

gbm_5_shap_táblázat <- 
  gbm_5_shap %>%
  pivot_longer(cols = everything(), 
               names_to = "feature", 
               values_to = "feature_weight")

gbm_5_globális_shapley_átlag <- 
  gbm_5_shap_táblázat %>%
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
  rename(gbm_5 = feature, 
         gbm_5_átlagos_súly = avg_weight)

gbm_5_globális_shapley_összeg <- 
  gbm_5_shap_táblázat %>%
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
  rename(gbm_5 = feature, 
         gbm_5_átlagos_súly = avg_weight)

gbm_5_globális_shapley_absz_átlag <- 
  gbm_5_shap_táblázat %>%
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
  rename(gbm_5 = feature, 
         gbm_5_átlagos_súly = avg_weight)

gbm_5_globális_shapley_absz_összeg <- 
  gbm_5_shap_táblázat %>%
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
  rename(gbm_5 = feature, 
         gbm_5_átlagos_súly = avg_weight)

gbm_globális_shapley_átlag <-
  cbind(gbm_1_globális_shapley_átlag, gbm_2_globális_shapley_átlag, gbm_3_globális_shapley_átlag, gbm_4_globális_shapley_átlag, gbm_5_globális_shapley_átlag)

gbm_globális_shapley_összeg <-
  cbind(gbm_1_globális_shapley_összeg, gbm_2_globális_shapley_összeg, gbm_3_globális_shapley_összeg, gbm_4_globális_shapley_összeg, gbm_5_globális_shapley_összeg)

gbm_globális_shapley_absz_átlag <-
  cbind(gbm_1_globális_shapley_absz_átlag, gbm_2_globális_shapley_absz_átlag, gbm_3_globális_shapley_absz_átlag, gbm_4_globális_shapley_absz_átlag, gbm_5_globális_shapley_absz_átlag)

gbm_globális_shapley_absz_összeg <-
  cbind(gbm_1_globális_shapley_absz_összeg, gbm_2_globális_shapley_absz_összeg, gbm_3_globális_shapley_absz_összeg, gbm_4_globális_shapley_absz_összeg, gbm_5_globális_shapley_absz_összeg)
