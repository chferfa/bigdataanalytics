##################################################
##################################################
# 0. PREPARACIÓN
##################################################
##################################################


# 0.1 Instalación de paquetes necesarios para la ejecución (si no están ya instalados previamente)


list.of.packages <- c("qdap", "tm", "XML", "splitstackshape", "caret", "tidytext", "dplyr", "longurl", "randomForest", "e1071")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(qdap)
library(tm)
library(XML)
library(splitstackshape)
library(caret)
library(tidytext)
library(dplyr)
library(longurl)
library(randomForest)
library(e1071)


# 0.2 Configuratión de rutas del dataset


path_training <- "/.../pan-ap17-bigdata/training/"
path_test <- "/.../pan-ap17-bigdata/test/"


# 0.3.1 Parametrización global


VERBOSE <- TRUE

EXPANDURLS <- FALSE
LOWCASE <- TRUE
PUNCTUATIONS <- TRUE
NUMBERS <- TRUE
ACCENTS <- TRUE
SWLANG <- "es"
SWLIST <- c("d", "q", "x", "pq", "xq")
WHITESPACES <- TRUE


# 0.3.2 Parametrización para detección de género


EXPANDURLS_GENDER <- EXPANDURLS
LOWCASE_GENDER <- LOWCASE
PUNCTUATIONS_GENDER <- PUNCTUATIONS
NUMBERS_GENDER <- NUMBERS
ACCENTS_GENDER <- ACCENTS
SWLANG_GENDER <- SWLANG
SWLIST_GENDER <- SWLIST
WHITESPACES_GENDER <- WHITESPACES


# 0.3.3 Parametrización para detección de variedad


EXPANDURLS_VARIETY <- EXPANDURLS
LOWCASE_VARIETY <- LOWCASE
PUNCTUATIONS_VARIETY <- PUNCTUATIONS
NUMBERS_VARIETY <- NUMBERS
ACCENTS_VARIETY <- ACCENTS
SWLANG_VARIETY <- SWLANG
SWLIST_VARIETY <- SWLIST
WHITESPACES_VARIETY <- WHITESPACES


# 0.4 Definición de funciones auxiliares


# Detección y expansión de URLs

url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

# Recibe una cadena de texto y devuelve la misma cadena de texto con las URLs acortadas (si las hay) en su formato original
ExpandURLs <- function(sentence) {
  
  # Extrae las URLs de una cadena de texto
  urls <- unlist(regmatches(sentence, gregexpr(url_pattern, sentence)))
  
  # Para cada URL extraída, se expande y se reemplaza sobre la cadena de texto de entrada
  sentence_expanded <- sentence
  for (url in urls) {
    original_url <- paste("", ProcessURL(url), "")
    sentence_expanded <- gsub(url, original_url, sentence_expanded)
  }
  
  return (sentence_expanded)
  
}

# Recibe una URL y, si está en formado acortado, la devuelve en su formato original. Si no está en formato acortado devuelve la misma URL recibida
# Además, permite quitar el protocolo de la URL ("http://" o "https://") y reemplazar las barras por espacios
ProcessURL <- function(url, remove_protocol = TRUE, replace_slash = TRUE) {
  
  # Expande la URL
  expand_urls_result <- expand_urls(url, warn = FALSE, .progress = FALSE)
  original_url <- expand_urls_result$expanded_url
  
  if (!is.na(original_url)) {
  
    if (remove_protocol) { # Quita el protocolo de la URL
      if (startsWith(tolower(original_url), "https://")) {
        original_url <- substr(original_url, 9, nchar(original_url))
      } else if (startsWith(tolower(original_url), "http://")) {
        original_url <- substr(original_url, 8, nchar(original_url))
      }
    }
    
    if (replace_slash) { # Reemplaza las barras por espacios
      original_url <- gsub("/", " ", original_url)
    }
    
  } else {
    
    original_url <- url
    
  }
  
  return (original_url)
  
}

# Obtiene una lista de cadenas de texto con las palabras preprocesadas según la configuración establecida en los parámetros, filtradas por un valor concreto de una clase determinada
GetRawWords <- function(path, class = "gender", filter = "male", expandurls = FALSE, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, accents = TRUE, swlang = "", swlist = "", whitespaces = TRUE, verbose = FALSE) {
  
  # Obtensión de cadenas de texto filtradas por clase
  
  setwd(path)
  
  truth <- read.csv("truth.txt", sep = ":", header = FALSE)
  truth <- truth[, c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  files = list.files(pattern = "*.xml")
  
  corpus.raw <- NULL
  
  for (file in files) {
    
    author <- gsub(".xml", "", file)
    
    if (!is.null(class) && !is.null(filter)) {
      
      class_value <- NULL
      if (tolower(class) == "gender") {
        class_value <- truth[truth$author == author, "gender"]
      } else {
        class_value <- truth[truth$author == author, "variety"]
      }
        
      if (class_value == filter) {
        xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
        corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
      }
      
    } else {
      xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
      corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    }
    
  }
  
  # Preprocesado de cadenas de texto
  
  corpus.preprocessed <- corpus.raw
  
  if (expandurls) {
    if (verbose) print("Expanding URLs...")
    corpus.preprocessed <- unlist(lapply(corpus.preprocessed, ExpandURLs))
  }
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }	
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")		
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (swlang != "")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "...", sep = ""))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (!is.null(swlist) && length(swlist) > 0) {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  if (accents != "") {
    if (verbose) print("Removing accents...")
    corpus.preprocessed <- chartr("áéíóúàèìòùâêîôû", "aeiouaeiouaeiou", corpus.preprocessed)
  }

  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  return (corpus.preprocessed)
  
}

# Genera la bolsa de palabras para una clase determinada empleado el vocabulario proporcionado
# Además, se aplica el mismo preprocesado que se ha aplicado para la generación del vocabulario
GenerateBoW <- function(path, class = "gender", vocabulary, n = 1000, expandurls = FALSE, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, accents = TRUE, whitespaces = TRUE, verbose = FALSE) {
	
  setwd(path)

	truth <- read.csv("truth.txt", sep = ":", header = FALSE)
	truth <- truth[, c(1,4,7)]
	colnames(truth) <- c("author", "gender", "variety")

	files = list.files(pattern = "*.xml")
	
	i <- 0
	bow <- NULL
	
	for (file in files) {
	  
		author <- gsub(".xml", "", file)
		gender <- truth[truth$author == author, "gender"]
		variety <- truth[truth$author == author, "variety"]
		
		xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
		txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
		
		if (expandurls) {
		  txtdata <- ExpandURLs(txtdata)
		}

		if (lowcase) {
			txtdata <- tolower(txtdata)
		}

		if (punctuations) {
			txtdata <- removePunctuation(txtdata)
		}

		if (numbers) {
			txtdata <- removeNumbers(txtdata)
		}
		
		if (accents) {
		  txtdata <- chartr("áéíóúàèìòùâêîôû", "aeiouaeiouaeiou", txtdata)
		}
		
		if (whitespaces) {
			txtdata <- stripWhitespace(txtdata)
		}
	
		line <- author
		freq <- freq_terms(txtdata, n)
		
		for (word in vocabulary$WORD) {
		  
			thefreq <- 0
			if (length(freq[freq$WORD == word, "FREQ"]) > 0) {
				thefreq <- freq[freq$WORD == word, "FREQ"]
			}
			line <- paste(line, ",", thefreq, sep = "")
			
		}
		
		if (class == "gender") {
		  line <- paste(line, ",", gender, sep="")
		} else {
		  line <- paste(line, ",", variety, sep="")
		}

		bow <- rbind(bow, line)

		i <- i + 1

		if (verbose) {
			if (class == "gender") {
			  print(paste(i, author, gender))
			} else {
			  print(paste(i, author, variety))
			}
		}
		
	}

	return (bow)
	
}


##################################################
##################################################
# 1. DETECCIÓN DE GÉNERO
##################################################
##################################################


# 1.1: Obtención del vocabulario


# 1.1.1 Obtención de los tweets preprocesados y separados por género


# Hombres
start_time <- proc.time()
rawWordsMale <- GetRawWords(path = path_training, class = "gender", filter = "male", expandurls = EXPANDURLS_GENDER, lowcase = LOWCASE_GENDER, punctuations = PUNCTUATIONS_GENDER,
                            numbers = NUMBERS_GENDER, accents = ACCENTS_GENDER, swlang = SWLANG_GENDER, swlist = SWLIST_GENDER, whitespaces = WHITESPACES_GENDER, verbose = VERBOSE)
end_time <- proc.time()
spent_time_raw_words_gender_male <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en obtener y preprocesar los tweets de hombres:", spent_time_raw_words_gender_male["elapsed"], "segundos"))

# Mujeres
start_time <- proc.time()
rawWordsFemale <- GetRawWords(path = path_training, class = "gender", filter = "female", expandurls = EXPANDURLS_GENDER, lowcase = LOWCASE_GENDER, punctuations = PUNCTUATIONS_GENDER,
                            numbers = NUMBERS_GENDER, accents = ACCENTS_GENDER, swlang = SWLANG_GENDER, swlist = SWLIST_GENDER, whitespaces = WHITESPACES_GENDER, verbose = VERBOSE)
end_time <- proc.time()
spent_time_raw_words_gender_female <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en obtener y preprocesar los tweets de mujeres:", spent_time_raw_words_gender_female["elapsed"], "segundos"))

spent_time_raw_words_gender = (spent_time_raw_words_gender_male + spent_time_raw_words_gender_female) / 2
if (VERBOSE) print(paste("Tiempo medio empleado en obtener y preprocesar los tweets por género:", spent_time_raw_words_gender["elapsed"], "segundos"))


# 1.1.2 Obtención de las palabras más frecuentes para cada género


n_gender <- 1000

start_time <- proc.time()
frequentMale <- freq_terms(rawWordsMale, n_gender)
end_time <- proc.time()
spent_time_freq_gender_male <- end_time - start_time

frequentMale$gender <- "male"

if (VERBOSE) print(paste("Tiempo empleado en obtener las", n_gender, "palabras más frecuentes en los tweets de hombres:", spent_time_freq_gender_male["elapsed"], "segundos"))

if (VERBOSE) head(frequentMale)
if (VERBOSE) tail(frequentMale)

start_time <- proc.time()
frequentFemale <- freq_terms(rawWordsFemale, n_gender)
end_time <- proc.time()
spent_time_freq_gender_female <- end_time - start_time

frequentFemale$gender <- "female"

if (VERBOSE) print(paste("Tiempo empleado en obtener las", n_gender, "palabras más frecuentes en los tweets de mujeres:", spent_time_freq_gender_female["elapsed"], "segundos"))

if (VERBOSE) head(frequentFemale)
if (VERBOSE) tail(frequentFemale)

spent_time_freq_gender = (spent_time_freq_gender_male + spent_time_freq_gender_female) / 2
if (VERBOSE) print(paste("Tiempo medio empleado en obtener las", n_gender, "palabras más frecuentes en los tweets por género:", spent_time_freq_gender["elapsed"], "segundos"))


# 1.1.3 Obtención de los pesos tf-idf para las palabras más frecuentes con respecto a todos los géneros


freq_matrix_gender <- rbind(frequentMale, frequentFemale)

start_time <- proc.time()
tf_idf_gender <- bind_tf_idf(freq_matrix_gender, WORD, gender, FREQ)
end_time <- proc.time()
spent_time_tfidf_gender <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en obtener los pesos tf-idf de las palabras más frecuentes de todos los géneros:", spent_time_tfidf_gender["elapsed"], "segundos"))

if (VERBOSE) head(tf_idf_gender)

tf_idf_gender_ordered <- arrange(tf_idf_gender, desc(tf_idf))

if (VERBOSE) head(tf_idf_gender_ordered)


# 1.1.4 Obtención las palabras con mayor índice TF_IDF: vocabulario


m_gender <- 700

vocabulary_gender <- tf_idf_gender_ordered[1:m_gender, 1:2]

if (VERBOSE) head(vocabulary_gender)
if (VERBOSE) tail(vocabulary_gender)


# 1.2. Obtención de las bolsas de palabras


start_time <- proc.time()
bow_gender_training <- GenerateBoW(path = path_training, class = "gender", vocabulary = vocabulary_gender, n = m_gender, expandurls = EXPANDURLS_GENDER,
                                   lowcase = LOWCASE_GENDER, punctuations = PUNCTUATIONS_GENDER, numbers = NUMBERS_GENDER, accents = ACCENTS_GENDER, whitespaces = WHITESPACES_GENDER, verbose = VERBOSE)
end_time <- proc.time()
spent_time <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en generar la bolsa de palabras de training para el género:", spent_time["elapsed"], "segundos"))

start_time <- proc.time()
bow_gender_test <- GenerateBoW(path = path_test, class = "gender", vocabulary = vocabulary_gender, n = m_gender, expandurls = EXPANDURLS_GENDER,
                               lowcase = LOWCASE_GENDER, punctuations = PUNCTUATIONS_GENDER, numbers = NUMBERS_GENDER, accents = ACCENTS_GENDER, whitespaces = WHITESPACES_GENDER, verbose = VERBOSE)
end_time <- proc.time()
spent_time <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en generar la bolsa de palabras de test para el género:", spent_time["elapsed"], "segundos"))


# 1.3 -	Obtención del modelo de machine learning mediante Random Forest


# 1.3.1 Preparación de los datos de las bolsas de palabras para el entrenamiento y el test del modelo


training_gender <- cSplit(bow_gender_training, "V1", ",")
test_gender <- cSplit(bow_gender_test, "V1", ",")

number_columns_gender <- length(names(training_gender))

training_gender <- training_gender[, 2:number_columns_gender]
names(training_gender)[number_columns_gender - 1] <- "class"
truth_gender <- unlist(test_gender[, number_columns_gender:number_columns_gender])
test_gender <- test_gender[, 2:(number_columns_gender - 1)]


# 1.3.2 Entrenamiento del modelo


start_time <- proc.time()
fit_gender <- randomForest(class~., data = training_gender)
end_time <- proc.time()
spent_time <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en generar el modelo RandomForest con el conjunto de datos de training para el género:", spent_time["elapsed"], "segundos"))


# 1.3.3 Test del modelo


start_time <- proc.time()
pred_gender_RF <- predict(fit_gender, test_gender)
end_time <- proc.time()
spent_time <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en realizar la predicción con el conjunto de datos de test para el género:", spent_time["elapsed"], "segundos"))


# 1.3.4 Evaluación del modelo


start_time <- proc.time()
cm_gender = confusionMatrix(pred_gender_RF, truth_gender)
end_time <- proc.time()
spent_time <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en generar la matriz de confusión para el género:", spent_time["elapsed"], "segundos"))

if (VERBOSE) print(cm_gender)

print(paste("Accuracy detección de género:", cm_gender$overall["Accuracy"]))


##################################################
##################################################
# 2. DETECCIÓN DE VARIEDAD
##################################################
##################################################


# 2.1: Obtención del vocabulario


# 2.1.1 Obtención de los tweets preprocesados y separados por variedad


# México
start_time <- proc.time()
rawWordsMexico <- GetRawWords(path = path_training, class = "variety", filter = "mexico", expandurls = EXPANDURLS_VARIETY, lowcase = LOWCASE_VARIETY, punctuations = PUNCTUATIONS_VARIETY,
                            numbers = NUMBERS_VARIETY, accents = ACCENTS_VARIETY, swlang = SWLANG_VARIETY, swlist = SWLIST_VARIETY, whitespaces = WHITESPACES_VARIETY, verbose = VERBOSE)
end_time <- proc.time()
spent_time_raw_words_variety_mexico <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en obtener y preprocesar los tweets de México:", spent_time_raw_words_variety_mexico["elapsed"], "segundos"))

# Chile
start_time <- proc.time()
rawWordsChile <- GetRawWords(path = path_training, class = "variety", filter = "chile", expandurls = EXPANDURLS_VARIETY, lowcase = LOWCASE_VARIETY, punctuations = PUNCTUATIONS_VARIETY,
                              numbers = NUMBERS_VARIETY, accents = ACCENTS_VARIETY, swlang = SWLANG_VARIETY, swlist = SWLIST_VARIETY, whitespaces = WHITESPACES_VARIETY, verbose = VERBOSE)
end_time <- proc.time()
spent_time_raw_words_variety_chile <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en obtener y preprocesar los tweets de Chile:", spent_time_raw_words_variety_chile["elapsed"], "segundos"))

# Perú
start_time <- proc.time()
rawWordsPeru <- GetRawWords(path = path_training, class = "variety", filter = "peru", expandurls = EXPANDURLS_VARIETY, lowcase = LOWCASE_VARIETY, punctuations = PUNCTUATIONS_VARIETY,
                              numbers = NUMBERS_VARIETY, accents = ACCENTS_VARIETY, swlang = SWLANG_VARIETY, swlist = SWLIST_VARIETY, whitespaces = WHITESPACES_VARIETY, verbose = VERBOSE)
end_time <- proc.time()
spent_time_raw_words_variety_peru <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en obtener y preprocesar los tweets de Perú:", spent_time_raw_words_variety_peru["elapsed"], "segundos"))

# Venezuela
start_time <- proc.time()
rawWordsVenezuela <- GetRawWords(path = path_training, class = "variety", filter = "venezuela", expandurls = EXPANDURLS_VARIETY, lowcase = LOWCASE_VARIETY, punctuations = PUNCTUATIONS_VARIETY,
                              numbers = NUMBERS_VARIETY, accents = ACCENTS_VARIETY, swlang = SWLANG_VARIETY, swlist = SWLIST_VARIETY, whitespaces = WHITESPACES_VARIETY, verbose = VERBOSE)
end_time <- proc.time()
spent_time_raw_words_variety_venezuela <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en obtener y preprocesar los tweets de Venezuela:", spent_time_raw_words_variety_venezuela["elapsed"], "segundos"))

# España
start_time <- proc.time()
rawWordsSpain <- GetRawWords(path = path_training, class = "variety", filter = "spain", expandurls = EXPANDURLS_VARIETY, lowcase = LOWCASE_VARIETY, punctuations = PUNCTUATIONS_VARIETY,
                              numbers = NUMBERS_VARIETY, accents = ACCENTS_VARIETY, swlang = SWLANG_VARIETY, swlist = SWLIST_VARIETY, whitespaces = WHITESPACES_VARIETY, verbose = VERBOSE)
end_time <- proc.time()
spent_time_raw_words_variety_spain <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en obtener y preprocesar los tweets de España:", spent_time_raw_words_variety_spain["elapsed"], "segundos"))

# Argentina
start_time <- proc.time()
rawWordsArgentina <- GetRawWords(path = path_training, class = "variety", filter = "argentina", expandurls = EXPANDURLS_VARIETY, lowcase = LOWCASE_VARIETY, punctuations = PUNCTUATIONS_VARIETY,
                              numbers = NUMBERS_VARIETY, accents = ACCENTS_VARIETY, swlang = SWLANG_VARIETY, swlist = SWLIST_VARIETY, whitespaces = WHITESPACES_VARIETY, verbose = VERBOSE)
end_time <- proc.time()
spent_time_raw_words_variety_argentina <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en obtener y preprocesar los tweets de Argentina:", spent_time_raw_words_variety_argentina["elapsed"], "segundos"))

# Colombia
start_time <- proc.time()
rawWordsColombia <- GetRawWords(path = path_training, class = "variety", filter = "colombia", expandurls = EXPANDURLS_VARIETY, lowcase = LOWCASE_VARIETY, punctuations = PUNCTUATIONS_VARIETY,
                              numbers = NUMBERS_VARIETY, accents = ACCENTS_VARIETY, swlang = SWLANG_VARIETY, swlist = SWLIST_VARIETY, whitespaces = WHITESPACES_VARIETY, verbose = VERBOSE)
end_time <- proc.time()
spent_time_raw_words_variety_colombia <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en obtener y preprocesar los tweets de Colombia:", spent_time_raw_words_variety_colombia["elapsed"], "segundos"))

spent_time_raw_words_variety = (spent_time_raw_words_variety_mexico +
                                spent_time_raw_words_variety_chile +
                                spent_time_raw_words_variety_peru +
                                spent_time_raw_words_variety_venezuela +
                                spent_time_raw_words_variety_spain +
                                spent_time_raw_words_variety_argentina +
                                spent_time_raw_words_variety_colombia) / 7
if (VERBOSE) print(paste("Tiempo medio empleado en obtener y preprocesar los tweets por variedad:", spent_time_raw_words_variety["elapsed"], "segundos"))


# 2.1.2 Obtención de las palabras más frecuentes para cada género


n_variety <- 1000

# México
start_time <- proc.time()
frequentMexico <- freq_terms(rawWordsMexico, n_variety)
end_time <- proc.time()
spent_time_freq_variery_mexico <- end_time - start_time

frequentMexico$variety <- "mexico"

if (VERBOSE) print(paste("Tiempo empleado en obtener las", n_variety, "palabras más frecuentes en los tweets de México:", spent_time_freq_variery_mexico["elapsed"], "segundos"))

if (VERBOSE) head(frequentMexico)
if (VERBOSE) tail(frequentMexico)

# Chile
start_time <- proc.time()
frequentChile <- freq_terms(rawWordsChile, n_variety)
end_time <- proc.time()
spent_time_freq_variery_chile <- end_time - start_time

frequentChile$variety <- "chile"

if (VERBOSE) print(paste("Tiempo empleado en obtener las", n_variety, "palabras más frecuentes en los tweets de Chile:", spent_time_freq_variery_chile["elapsed"], "segundos"))

if (VERBOSE) head(frequentChile)
if (VERBOSE) tail(frequentChile)

# Peru
start_time <- proc.time()
frequentPeru <- freq_terms(rawWordsPeru, n_variety)
end_time <- proc.time()
spent_time_freq_variery_peru <- end_time - start_time

frequentPeru$variety <- "peru"

if (VERBOSE) print(paste("Tiempo empleado en obtener las", n_variety, "palabras más frecuentes en los tweets de Perú:", spent_time_freq_variery_peru["elapsed"], "segundos"))

if (VERBOSE) head(frequentPeru)
if (VERBOSE) tail(frequentPeru)

# Venezuela
start_time <- proc.time()
frequentVenezuela <- freq_terms(rawWordsVenezuela, n_variety)
end_time <- proc.time()
spent_time_freq_variery_venezuela <- end_time - start_time

frequentVenezuela$variety <- "venezuela"

if (VERBOSE) print(paste("Tiempo empleado en obtener las", n_variety, "palabras más frecuentes en los tweets de Venezuela:", spent_time_freq_variery_venezuela["elapsed"], "segundos"))

if (VERBOSE) head(frequentVenezuela)
if (VERBOSE) tail(frequentVenezuela)

# España
start_time <- proc.time()
frequentSpain <- freq_terms(rawWordsSpain, n_variety)
end_time <- proc.time()
spent_time_freq_variery_spain <- end_time - start_time

frequentSpain$variety <- "spain"

if (VERBOSE) print(paste("Tiempo empleado en obtener las", n_variety, "palabras más frecuentes en los tweets de España:", spent_time_freq_variery_spain["elapsed"], "segundos"))

if (VERBOSE) head(frequentSpain)
if (VERBOSE) tail(frequentSpain)

# Argentina
start_time <- proc.time()
frequentArgentina <- freq_terms(rawWordsArgentina, n_variety)
end_time <- proc.time()
spent_time_freq_variery_argentina <- end_time - start_time

frequentArgentina$variety <- "argentina"

if (VERBOSE) print(paste("Tiempo empleado en obtener las", n_variety, "palabras más frecuentes en los tweets de Argentina:", spent_time_freq_variery_argentina["elapsed"], "segundos"))

if (VERBOSE) head(frequentArgentina)
if (VERBOSE) tail(frequentArgentina)

# Colombia
start_time <- proc.time()
frequentColombia <- freq_terms(rawWordsColombia, n_variety)
end_time <- proc.time()
spent_time_freq_variery_colombia <- end_time - start_time

frequentColombia$variety <- "colombia"

if (VERBOSE) print(paste("Tiempo empleado en obtener las", n_variety, "palabras más frecuentes en los tweets de Colombia:", spent_time_freq_variery_colombia["elapsed"], "segundos"))

if (VERBOSE) head(frequentColombia)
if (VERBOSE) tail(frequentColombia)

spent_time_freq_variery = (spent_time_freq_variery_mexico +
                           spent_time_freq_variery_chile +
                           spent_time_freq_variery_peru +
                           spent_time_freq_variery_venezuela +
                           spent_time_freq_variery_spain +
                           spent_time_freq_variery_argentina +
                           spent_time_freq_variery_colombia) / 7
if (VERBOSE) print(paste("Tiempo medio empleado en obtener las", n_variety, "palabras más frecuentes en los tweets por variedad:", spent_time_freq_variery["elapsed"], "segundos"))


# 2.1.3 Obtención de los pesos tf-idf para las palabras más frecuentes con respecto a todas las variedades


freq_matrix_variety <- rbind(frequentMexico, frequentChile, frequentPeru, frequentVenezuela, frequentSpain, frequentArgentina, frequentColombia)

start_time <- proc.time()
tf_idf_variety <- bind_tf_idf(freq_matrix_variety, WORD, variety, FREQ)
end_time <- proc.time()
spent_time_tfidf_variety <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en obtener los pesos tf-idf de las palabras más frecuentes de todas las variedades:", spent_time_tfidf_variety["elapsed"], "segundos"))

if (VERBOSE) head(tf_idf_variety)

tf_idf_variety_ordered <- arrange(tf_idf_variety, desc(tf_idf))

if (VERBOSE) head(tf_idf_variety_ordered)


# 2.1.4 Obtención las palabras con mayor índice TF_IDF: vocabulario


m_variety <- 500

vocabulary_variety <- tf_idf_variety_ordered[1:m_variety, 1:2]

if (VERBOSE) head(vocabulary_variety)
if (VERBOSE) tail(vocabulary_variety)


# 2.2. Obtención de las bolsas de palabras


start_time <- proc.time()
bow_variety_training <- GenerateBoW(path = path_training, class = "variety", vocabulary = vocabulary_variety, n = m_variety, expandurls = EXPANDURLS_VARIETY,
                                   lowcase = LOWCASE_VARIETY, punctuations = PUNCTUATIONS_VARIETY, numbers = NUMBERS_VARIETY, accents = ACCENTS_VARIETY, whitespaces = WHITESPACES_VARIETY, verbose = VERBOSE)
end_time <- proc.time()
spent_time <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en generar la bolsa de palabras de training para la variedad:", spent_time["elapsed"], "segundos"))

start_time <- proc.time()
bow_variety_test <- GenerateBoW(path = path_test, class = "variety", vocabulary = vocabulary_variety, n = m_variety, expandurls = EXPANDURLS_VARIETY,
                               lowcase = LOWCASE_VARIETY, punctuations = PUNCTUATIONS_VARIETY, numbers = NUMBERS_VARIETY, accents = ACCENTS_VARIETY, whitespaces = WHITESPACES_VARIETY, verbose = VERBOSE)
end_time <- proc.time()
spent_time <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en generar la bolsa de palabras de test para la variedad:", spent_time["elapsed"], "segundos"))


# 2.3 -	Obtención del modelo de machine learning mediante Random Forest


# 2.3.1 Preparación de los datos de las bolsas de palabras para el entrenamiento y el test del modelo


training_variety <- cSplit(bow_variety_training, "V1", ",")
test_variety <- cSplit(bow_variety_test, "V1", ",")

number_columns_variety <- length(names(training_variety))

training_variety <- training_variety[, 2:number_columns_variety]
names(training_variety)[number_columns_variety - 1] <- "class"
truth_variety <- unlist(test_variety[, number_columns_variety:number_columns_variety])
test_variety <- test_variety[, 2:(number_columns_variety - 1)]


# 2.3.2 Entrenamiento del modelo


start_time <- proc.time()
fit_variety <- randomForest(class~., data = training_variety)
end_time <- proc.time()
spent_time <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en generar el modelo RandomForest con el conjunto de datos de training para la variedad:", spent_time["elapsed"], "segundos"))


# 2.3.3 Test del modelo


start_time <- proc.time()
pred_variety_RF <- predict(fit_variety, test_variety)
end_time <- proc.time()
spent_time <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en realizar la predicción con el conjunto de datos de test para la variedad:", spent_time["elapsed"], "segundos"))


# 2.3.4 Evaluación del modelo


start_time <- proc.time()
cm_variety = confusionMatrix(pred_variety_RF, truth_variety)
end_time <- proc.time()
spent_time <- end_time - start_time
if (VERBOSE) print(paste("Tiempo empleado en generar la matriz de confusión para la variedad:", spent_time["elapsed"], "segundos"))

if (VERBOSE) print(cm_variety)

print(paste("Accuracy detección de variedad:", cm_variety$overall["Accuracy"]))
