library(xgboost)
# 1. XGBoost
# Kombinált adathalmaz létrehozása a közös kategóriákért
gbm_kombinált_adatok_1 <- rbind(tanító_halmaz_1, teszt_halmaz)
gmb_kombinált_mátrix_1 <- model.matrix(ár ~ . - 1, data = gbm_kombinált_adatok_1)
tanító_mátrix_1 <- gmb_kombinált_mátrix_1[1:nrow(tanító_halmaz_1), ]
teszt_mátrix_1 <- gmb_kombinált_mátrix_1[(nrow(tanító_halmaz_1) + 1):nrow(gbm_kombinált_adatok_1), ]

gbm_1 <- xgboost(
  data = tanító_mátrix_1,
  label = tanító_halmaz_1$ár,
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.2,
  nthread = 2,
  nrounds = 1700,
  verbose = 1)

gbm_predikció_1 <- predict(gbm_1, teszt_mátrix_1)
gbm_rmse_1 <- sqrt(mean((teszt_halmaz$ár - gbm_predikció_1)^2))
gbm_mae_1 <- mean(abs(teszt_halmaz$ár - gbm_predikció_1))
gbm_mape_1 <- mean(abs((teszt_halmaz$ár - gbm_predikció_1) / teszt_halmaz$ár)) * 100

# 2. XGBoost
# Kombinált adathalmaz létrehozása a közös kategóriákért
gbm_kombinált_adatok_2 <- rbind(tanító_halmaz_2, teszt_halmaz)
gmb_kombinált_mátrix_2 <- model.matrix(ár ~ . - 1, data = gbm_kombinált_adatok_2)
tanító_mátrix_2 <- gmb_kombinált_mátrix_2[1:nrow(tanító_halmaz_2), ]
teszt_mátrix_2 <- gmb_kombinált_mátrix_2[(nrow(tanító_halmaz_2) + 1):nrow(gbm_kombinált_adatok_2), ]

gbm_2 <- xgboost(
  data = tanító_mátrix_2,
  label = tanító_halmaz_2$ár,
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.2,
  nthread = 2,
  nrounds = 1700,
  verbose = 1)

gbm_predikció_2 <- predict(gbm_2, teszt_mátrix_2)
gbm_rmse_2 <- sqrt(mean((teszt_halmaz$ár - gbm_predikció_2)^2))
gbm_mae_2 <- mean(abs(teszt_halmaz$ár - gbm_predikció_2))
gbm_mape_2<- mean(abs((teszt_halmaz$ár - gbm_predikció_2) / teszt_halmaz$ár)) * 100

#3. GBM
# Kombinált adathalmaz létrehozása a közös kategóriákért
gbm_kombinált_adatok_3 <- rbind(tanító_halmaz_3, teszt_halmaz)
gmb_kombinált_mátrix_3 <- model.matrix(ár ~ . - 1, data = gbm_kombinált_adatok_3)
tanító_mátrix_3 <- gmb_kombinált_mátrix_3[1:nrow(tanító_halmaz_3), ]
teszt_mátrix_3 <- gmb_kombinált_mátrix_3[(nrow(tanító_halmaz_3) + 1):nrow(gbm_kombinált_adatok_3), ]

gbm_3 <- xgboost(
  data = tanító_mátrix_3,
  label = tanító_halmaz_3$ár,
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.2,
  nthread = 2,
  nrounds = 1700,
  verbose = 1)

gbm_predikció_3 <- predict(gbm_3, teszt_mátrix_3)
gbm_rmse_3 <- sqrt(mean((teszt_halmaz$ár - gbm_predikció_3)^2))
gbm_mae_3 <- mean(abs(teszt_halmaz$ár - gbm_predikció_3))
gbm_mape_3<- mean(abs((teszt_halmaz$ár - gbm_predikció_3) / teszt_halmaz$ár)) * 100

#4. XGBoost
# Kombinált adathalmaz létrehozása a közös kategóriákért
gbm_kombinált_adatok_4 <- rbind(tanító_halmaz_4, teszt_halmaz)
gmb_kombinált_mátrix_4 <- model.matrix(ár ~ . - 1, data = gbm_kombinált_adatok_4)
tanító_mátrix_4 <- gmb_kombinált_mátrix_4[1:nrow(tanító_halmaz_4), ]
teszt_mátrix_4 <- gmb_kombinált_mátrix_4[(nrow(tanító_halmaz_4) + 1):nrow(gbm_kombinált_adatok_4), ]

gbm_4 <- xgboost(
  data = tanító_mátrix_4,
  label = tanító_halmaz_4$ár,
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.2,
  nthread = 2,
  nrounds = 1700,
  verbose = 1)

gbm_predikció_4 <- predict(gbm_4, teszt_mátrix_4)
gbm_rmse_4 <- sqrt(mean((teszt_halmaz$ár - gbm_predikció_4)^2))
gbm_mae_4 <- mean(abs(teszt_halmaz$ár - gbm_predikció_4))
gbm_mape_4<- mean(abs((teszt_halmaz$ár - gbm_predikció_4) / teszt_halmaz$ár)) * 100

#5. XGBoost
# Kombinált adathalmaz létrehozása a közös kategóriákért
gbm_kombinált_adatok_5 <- rbind(tanító_halmaz_5, teszt_halmaz)
gmb_kombinált_mátrix_5 <- model.matrix(ár ~ . - 1, data = gbm_kombinált_adatok_5)
tanító_mátrix_5 <- gmb_kombinált_mátrix_5[1:nrow(tanító_halmaz_5), ]
teszt_mátrix_5 <- gmb_kombinált_mátrix_5[(nrow(tanító_halmaz_5) + 1):nrow(gbm_kombinált_adatok_5), ]

gbm_5 <- xgboost(
  data = tanító_mátrix_5,
  label = tanító_halmaz_5$ár,
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.2,
  nthread = 2,
  nrounds = 1700,
  verbose = 1)

gbm_predikció_5 <- predict(gbm_5, teszt_mátrix_5)
gbm_rmse_5 <- sqrt(mean((teszt_halmaz$ár - gbm_predikció_5)^2))
gbm_mae_5 <- mean(abs(teszt_halmaz$ár - gbm_predikció_5))
gbm_mape_5<- mean(abs((teszt_halmaz$ár - gbm_predikció_5) / teszt_halmaz$ár)) * 100


#XGBoost mutatók összegyűjtése
gbm_modell_neve = c('gbm_1', 'gbm_2', 'gbm_3', 'gbm_4', 'gbm_5')
gbm_rmse = c(gbm_rmse_1, gbm_rmse_2, gbm_rmse_3, gbm_rmse_4, gbm_rmse_5)
gbm_mae = c(gbm_mae_1, gbm_mae_2, gbm_mae_3, gbm_mae_4, gbm_mae_5)
gbm_mape = c(gbm_mape_1, gbm_mape_2, gbm_mape_3, gbm_mape_4, gbm_mape_5)

gbm_modellek_mutató =
  data.frame(Modell = gbm_modell_neve,
             RMSE = gbm_rmse,
             MAE = gbm_mae,
             MAPE = gbm_mape)

összes_modell_mutató = 
  rbind(lin_reg_modellek_mutató, rf_modellek_mutató,gbm_modellek_mutató)