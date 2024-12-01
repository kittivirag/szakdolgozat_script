#1. lineáris regresszió 
lin_reg_1 <- train(ár ~ ., 
                   data = tanító_halmaz_1, 
                   method = "lm")

predikció_lin_reg_1 <- predict(lin_reg_1, teszt_halmaz)
lin_reg_mse_1 <- mean((teszt_halmaz$ár - predikció_lin_reg_1)^2)
lin_reg_rmse_1 <- sqrt(lin_reg_mse_1)
lin_reg_mae_1 <- mean(abs(teszt_halmaz$ár - predikció_lin_reg_1))
lin_reg_mape_1 <- mean(abs((teszt_halmaz$ár - predikció_lin_reg_1) / teszt_halmaz$ár)) * 100

#2. lineáris regresszió 
lin_reg_2 <- train(ár ~ ., 
                   data = tanító_halmaz_2, 
                   method = "lm")
predikció_lin_reg_2 <- predict(lin_reg_2, teszt_halmaz)
print(lin_reg_2)
lin_reg_rmse_2 <- sqrt(mean((teszt_halmaz$ár - predikció_lin_reg_2)^2))
lin_reg_mae_2 <- mean(abs(teszt_halmaz$ár - predikció_lin_reg_2))
lin_reg_mape_2 <- mean(abs((teszt_halmaz$ár - predikció_lin_reg_2) / teszt_halmaz$ár)) * 100


#3. lineáris regresszió 
lin_reg_3 <- train(ár ~ ., 
                   data = tanító_halmaz_3, 
                   method = "lm")
predikció_lin_reg_3 <- predict(lin_reg_3, teszt_halmaz)
lin_reg_rmse_3 <- sqrt(mean((teszt_halmaz$ár - predikció_lin_reg_3)^2))
lin_reg_mae_3 <- mean(abs(teszt_halmaz$ár - predikció_lin_reg_3))
lin_reg_mape_3 <- mean(abs((teszt_halmaz$ár - predikció_lin_reg_3) / teszt_halmaz$ár)) * 100

#4. lineáris regresszió 
lin_reg_4 <- train(ár ~ ., 
                   data = tanító_halmaz_4, 
                   method = "lm")
predikció_lin_reg_4 <- predict(lin_reg_4, teszt_halmaz)
lin_reg_rmse_4 <- sqrt(mean((teszt_halmaz$ár - predikció_lin_reg_4)^2))
lin_reg_mae_4 <- mean(abs(teszt_halmaz$ár - predikció_lin_reg_4))
lin_reg_mape_4 <- mean(abs((teszt_halmaz$ár - predikció_lin_reg_4) / teszt_halmaz$ár)) * 100

#5. lineáris regresszió 
lin_reg_5 <- train(ár ~ ., 
                   data = tanító_halmaz_5, 
                   method = "lm")
predikció_lin_reg_5 <- predict(lin_reg_5, teszt_halmaz)
lin_reg_rmse_5 <- sqrt(mean((teszt_halmaz$ár - predikció_lin_reg_5)^2))
lin_reg_mae_5 <- mean(abs(teszt_halmaz$ár - predikció_lin_reg_5))
lin_reg_mape_5 <- mean(abs((teszt_halmaz$ár - predikció_lin_reg_5) / teszt_halmaz$ár)) * 100

#lineáris regresszió modell mutatók összegyűjtése
lin_reg_modell_neve = c('lineáris_regresszió_1', 'lineáris_regresszió_2', 'lineáris_regresszió_3', 'lineáris_regresszió_4', 'lineáris_regresszió_5')
lin_reg_rmse = c(lin_reg_rmse_1, lin_reg_rmse_2, lin_reg_rmse_3, lin_reg_rmse_4, lin_reg_rmse_5)
lin_reg_mae = c(lin_reg_mae_1, lin_reg_mae_2, lin_reg_mae_3, lin_reg_mae_4, lin_reg_mae_5)
lin_reg_mape = c(lin_reg_mape_1, lin_reg_mape_2, lin_reg_mape_3, lin_reg_mape_4, lin_reg_mape_5)

lin_reg_modellek_mutató =
  data.frame(Modell = lin_reg_modell_neve,
             RMSE = lin_reg_rmse,
             MAE = lin_reg_mae,
             MAPE = lin_reg_mape)


