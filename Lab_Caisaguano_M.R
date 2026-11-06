#NOMBRE: MAURICIO CAISAGUANO 
#Para cada una de las series siguientes, haz un gráfico de los datos. 
#Si la transformación te parece adecuada, hazla y describe el efecto.

#Sacrificio de «Toros, bueyes y novillos» de Victoria en aus_livestock
#Filtrado y gráfica
z <- aus_livestock %>% filter( Animal== 'Bulls, bullocks and steers',State=='Victoria')
z %>% autoplot(Count)+labs(x = "Mes", y = "Number de animales", title = 'Sacrificio de «Toros, bueyes y novillos» de Victoria')

#Transformación de Box-Cox
lambda <- z %>% features(Count, features = guerrero) %>%pull(lambda_guerrero)
lambda
z %>%autoplot(box_cox(Count, lambda))+labs(x = "Mes", y = "Number de animales", title = 'Sacrificio de «Toros, bueyes y novillos» de Victoria')

# El lambda es muy pequeño, por lo que la variacion en la serie no es significativa

#Descomposición STL
z |>
  model(
    STL(
      Count ~ trend(window = 6) +
        season(window = "periodic"),
      robust = TRUE
    )
  ) |>
  components() |>
  autoplot() +
  labs(x = "Mes")

#Tomamos en la descomposióN STL con una ventana estacional 6 y mediante el método
#robusto, visualizandose una tendencia mas estable.