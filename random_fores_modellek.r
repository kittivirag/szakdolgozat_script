library(ranger)
#1. random forest
rf_1 <- ranger(ár ~ ., 
               data = tanító_halmaz_1, 
               num.trees = 1200,
               mtry = 6,
               min.node.size = 4)
rf_predikció_1 <- predict(rf_1, data = teszt_halmaz)$predictions
rf_rmse_1 <- sqrt(mean((teszt_halmaz$ár - rf_predikció_1)^2))
rf_mae_1 <- mean(abs(teszt_halmaz$ár - rf_predikció_1))
rf_mape_1 <- mean(abs((teszt_halmaz$ár - rf_predikció_1) / teszt_halmaz$ár)) * 100

#2. random forest
rf_2 <- ranger(ár ~ ., 
              data = tanító_halmaz_2, 
              num.trees = 1200,
              mtry = 6,
              min.node.size = 4)
rf_predikció_2 <-  predict(rf_2, data = teszt_halmaz)$predictions
rf_rmse_2 <- sqrt(mean((teszt_halmaz$ár - rf_predikció_2)^2))
rf_mae_2 <- mean(abs(teszt_halmaz$ár - rf_predikció_2))
rf_mape_2 <- mean(abs((teszt_halmaz$ár - rf_predikció_2) / teszt_halmaz$ár)) * 100


#3. random forest
rf_3 <- ranger(ár ~ ., 
               data = tanító_halmaz_1, 
               num.trees = 1200,
               mtry = 6,
               min.node.size = 4)
rf_predikció_3 <- predict(rf_3, teszt_halmaz)$predictions
rf_rmse_3 <- sqrt(mean((teszt_halmaz$ár - rf_predikció_3)^2))
rf_mae_3 <- mean(abs(teszt_halmaz$ár - rf_predikció_3))
rf_mape_3 <- mean(abs((teszt_halmaz$ár - rf_predikció_3) / teszt_halmaz$ár)) * 100

#4. random forest
rf_4 <- ranger(ár ~ ., 
               data = tanító_halmaz_1, 
               num.trees = 1200,
               mtry = 6,
               min.node.size = 4)
rf_predikció_4 <- predict(rf_4, teszt_halmaz)$predictions
rf_rmse_4 <- sqrt(mean((teszt_halmaz$ár - rf_predikció_4)^2))
rf_mae_4 <- mean(abs(teszt_halmaz$ár - rf_predikció_4))
rf_mape_4 <- mean(abs((teszt_halmaz$ár - rf_predikció_4) / teszt_halmaz$ár)) * 100

#5. random forest
rf_5 <- ranger(ár ~ ., 
               data = tanító_halmaz_1, 
               num.trees = 1200,
               mtry = 6,
               min.node.size = 4)
rf_predikció_5 <- predict(rf_5, teszt_halmaz)$predictions
rf_rmse_5 <- sqrt(mean((teszt_halmaz$ár - rf_predikció_5)^2))
rf_mae_5 <- mean(abs(teszt_halmaz$ár - rf_predikció_5))
rf_mape_5 <- mean(abs((teszt_halmaz$ár - rf_predikció_5) / teszt_halmaz$ár)) * 100


#random forest mutatók összegyűjtése
rf_modell_neve = c('random_forest_1', 'random_forest_2', 'random_forest_3', 'random_forest_4', 'random_forest_5')
rf_rmse = c(rf_rmse_1, rf_rmse_2, rf_rmse_3, rf_rmse_4, rf_rmse_5)
rf_mae = c(rf_mae_1, rf_mae_2, rf_mae_3, rf_mae_4, rf_mae_5)
rf_mape = c(rf_mape_1, rf_mape_2, rf_mape_3, rf_mape_4, rf_mape_5)

rf_modellek_mutató =
  data.frame(Modell = rf_modell_neve,
             RMSE = rf_rmse,
             MAE = rf_mae,
             MAPE = rf_mape)




