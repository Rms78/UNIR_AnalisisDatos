#******************************************************************************************************#
#DF sólo con los datos de casos y temperaturas
df_CT <- data.frame(PrDnTmTc20$TotalCasos,PrDnTmTc20$TempM)

#Grafico y valores de correlación
chart.Correlation(df_CT, method = "spearman", exact = FALSE) #Gráfico y datos básicos
cor.test(df_CT$PrDnTmTc20.TotalCasos, df_CT$PrDnTmTc20.TempM, method = "spearman", exact = FALSE) #esquema de calculos
#******************************************************************************************************#


#******************************************************************************************************#
#DF sólo con los datos de casos y densidades de poblacion
df_CD <- data.frame(PrDnTmTc20$TotalCasos,PrDnTmTc20$`Densidad (hab/km2)`)

#Grafico y valores de correlación
chart.Correlation(df_CD, method = "spearman", exact = FALSE)#Gráfico y datos básicos
cor.test(df_CD$PrDnTmTc20.TotalCasos, df_CD$PrDnTmTc20..Densidad..hab.km2.., method = "spearman", exact = FALSE)#esquema de calculos
#******************************************************************************************************#