##Datos

install.packages("writexl")

#Cargado de librerías
library(dplyr)
library(stringr)
library(sentimentr)
library("writexl")

#Lectura de datos
tweets_data <- read.csv(file = file.choose(), header = TRUE)

#Conversión de tipos de datos
tweets_data$date <- as.Date(tweets_data$date)
tweets_data$id <- as.character(tweets_data$id)

#Función para limpieza de texto
clear_text_of_tweets <- function(val_text) {
  val_text = gsub("&amp", "", val_text)
  val_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", val_text)
  val_text = gsub("@\\w+", "", val_text)
  val_text = gsub("[[:punct:]]", "", val_text)
  val_text = gsub("[[:digit:]]", "", val_text)
  val_text = gsub("http\\w+", "", val_text)
  val_text = gsub("[ \t]{2,}", "", val_text)
  val_text = gsub("^\\s+|\\s+$", "", val_text)
  val_text <- str_replace_all(val_text," "," ")
  # Take out retweet header, there is only one
  val_text <- str_replace(val_text,"RT @[a-z,A-Z]*: ","")
  # Get rid of hashtags
  val_text <- str_replace_all(val_text,"#[a-z,A-Z]*","")
  # Get rid of references to other screennames
  val_text <- str_replace_all(val_text,"@[a-z,A-Z]*","")
  val_text <- iconv(val_text, "latin1", "ASCII", sub="")
  return (val_text)
}

#Limpiar texto de los tweets
tweets_data['clean_text']=clear_text_of_tweets(tweets_data$text)

#Obtener valor de polaridad de los tweets
data_polarity=sentiment(tweets_data$clean_text) %>%
  mutate(category = case_when(
    sentiment < 0 ~ 'Negativo', 
    sentiment == 0 ~ 'Neutral', 
    sentiment > 0 ~ 'Positivo'
  )%>%
    factor(levels = c('Negativo', 'Neutral', 'Positivo'))
  )

#Fusionar datos de dataframes
tweets_data=cbind(tweets_data, data_polarity$word_count,data_polarity$sentiment, data_polarity$category)

#Renombrar columnas agregadas
colnames(tweets_data)[colnames(tweets_data) == "data_polarity$word_count"] ="word_count"
colnames(tweets_data)[colnames(tweets_data) == "data_polarity$sentiment"] ="sentiment"
colnames(tweets_data)[colnames(tweets_data) == "data_polarity$category"] ="category"

#Limpieza del campo user_location
tweets_data['clean_location']=iconv(str_trim(tweets_data$user_location, side="both"), "latin1", "ASCII", sub="")

#Guardar conjunto de datos
write.csv(tweets_data, "H:\\Mi unidad\\Maestría\\Tercer semestre\\Visualización de datos\\Desarrollo\\11. 27 Diciembre - 23 Enero 2023\\Datos\\data_tweets_vaccines.csv", row.names=FALSE)
write_xlsx(tweets_data,"H:\\Mi unidad\\Maestría\\Tercer semestre\\Visualización de datos\\Desarrollo\\11. 27 Diciembre - 23 Enero 2023\\Datos\\data_tweets_vaccines.xlsx")


#Leer conjunto de datos
global_polarity_tweets <- read.csv(file = file.choose(), header = TRUE)
write_xlsx(global_polarity_tweets,"H:\\Mi unidad\\Maestría\\Tercer semestre\\Visualización de datos\\Desarrollo\\11. 27 Diciembre - 23 Enero 2023\\Datos\\global_polarity_tweets.xlsx")


global_polarity_tweets_by_months <- read.csv(file = file.choose(), header = TRUE)
write_xlsx(global_polarity_tweets_by_months,"H:\\Mi unidad\\Maestría\\Tercer semestre\\Visualización de datos\\Desarrollo\\11. 27 Diciembre - 23 Enero 2023\\Datos\\global_polarity_tweets_by_months.xlsx")

tc_global_vaccines <- read.csv(file = file.choose(), header = TRUE)
write_xlsx(tc_global_vaccines,"H:\\Mi unidad\\Maestría\\Tercer semestre\\Visualización de datos\\Desarrollo\\11. 27 Diciembre - 23 Enero 2023\\Datos\\tc_global_vaccines.xlsx")


pol_months_global <- read.csv(file = file.choose(), header = TRUE)
write_xlsx(pol_months_global,"H:\\Mi unidad\\Maestría\\Tercer semestre\\Visualización de datos\\Desarrollo\\11. 27 Diciembre - 23 Enero 2023\\Datos\\pol_months_global.xlsx")



