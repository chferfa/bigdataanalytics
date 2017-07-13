##################################################
##################################################
# 0. PREPARACIÓN
##################################################
##################################################


# 0.1 Instalación de paquetes necesarios para la ejecución (si no están ya instalados previamente)


list.of.packages <- c("ggplot2", "grid", "XML")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(grid)
library(XML)


# 0.2 Configuratión de rutas del dataset


path_training <- "/.../pan-ap17-bigdata/training/"
path_test <- "/.../pan-ap17-bigdata/test/"


##################################################
##################################################
# 1. GENERACIÓN DE DATASET DE METADATOS
##################################################
##################################################


# 1.1 Training


setwd(path_training)

author_features_training <- read.csv("truth.txt", sep=":", header=FALSE)
author_features_training <- author_features_training[, c(1, 4, 7)]
colnames(author_features_training) <- c("author", "gender", "variety")

files = list.files(pattern = "*.xml")

tweets <- NULL
total_words <- NULL
mean_words <- NULL

for (file in files) {
  
  author <- gsub(".xml", "", file)
  xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
  
  author_tweets <- xpathApply(xmlfile, "//document", xmlValue)
  author_tweets_words <- unlist(lapply(author_tweets, function(x) length(unlist(strsplit(x, " ")))))
  
  tweets <- c(tweets, length(author_tweets))
  total_words <- c(total_words, sum(author_tweets_words))
  mean_words <- c(mean_words, mean(author_tweets_words))
  
}

author_features_training <- cbind(author_features_training, tweets, total_words, mean_words)


# 1.2 Test


setwd(path_test)

author_features_test <- read.csv("truth.txt", sep=":", header=FALSE)
author_features_test <- author_features_test[, c(1, 4, 7)]
colnames(author_features_test) <- c("author", "gender", "variety")

files = list.files(pattern = "*.xml")

tweets <- NULL
total_words <- NULL
mean_words <- NULL

for (file in files) {
  
  author <- gsub(".xml", "", file)
  xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
  
  author_tweets <- xpathApply(xmlfile, "//document", xmlValue)
  author_tweets_words <- unlist(lapply(author_tweets, function(x) length(unlist(strsplit(x, " ")))))
  
  tweets <- c(tweets, length(author_tweets))
  total_words <- c(total_words, sum(author_tweets_words))
  mean_words <- c(mean_words, mean(author_tweets_words))
  
}

author_features_test <- cbind(author_features_test, tweets, total_words, mean_words)


##################################################
##################################################
# 2. ESTADÍSTICAS
##################################################
##################################################


# 2.1 Estadísticas básicas de training


summary(author_features_training)


# 2.2 Estadísticas básicas de test


summary(author_features_test)


# 2.3 Generación de gráficas


# 2.3.1 Palabras por clase en training


# Total por género
gender_total_words_training <- ggplot(author_features_training, aes(factor(gender), total_words, fill = variety)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer() +
  theme_bw() +
  xlab("Género") +
  ylab("Total palabras") +
  labs(fill = "Variedad")

# Total por variedad
variety_total_words_training <- ggplot(author_features_training, aes(factor(variety), total_words, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer() +
  theme_bw() +
  xlab("Variedad") +
  ylab("Total palabras") +
  labs(fill = "Género")

# Media por género
gender_mean_words_training <- ggplot(author_features_training, aes(factor(gender), mean_words, fill = variety)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer() +
  theme_bw() +
  xlab("Género") +
  ylab("Media palabras") +
  labs(fill = "Variedad")

# Media por variedad
variety_mean_words_training <- ggplot(author_features_training, aes(factor(variety), mean_words, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer() +
  theme_bw() +
  xlab("Variedad") +
  ylab("Media palabras") +
  labs(fill = "Género")

# Total de palabras en tweets por clase en training
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1, heights = unit(c(1, 4, 4), "null"))))
grid.text("Training - Total de palabras en tweets por clase", vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(gender_total_words_training, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(variety_total_words_training, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))

# Media de palabras por tweet por clase en training
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1, heights = unit(c(1, 4, 4), "null"))))
grid.text("Training - Media de palabras por tweet por clase", vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(gender_mean_words_training, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(variety_mean_words_training, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))


# 2.3.2 Palabras por clase en test


# Total por género
gender_total_words_test <- ggplot(author_features_test, aes(factor(gender), total_words, fill = variety)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer() +
  theme_bw() +
  xlab("Género") +
  ylab("Total palabras") +
  labs(fill = "Variedad")

# Total por variedad
variety_total_words_test <- ggplot(author_features_test, aes(factor(variety), total_words, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer() +
  theme_bw() +
  xlab("Variedad") +
  ylab("Total palabras") +
  labs(fill = "Género")

# Media por género
gender_mean_words_test <- ggplot(author_features_test, aes(factor(gender), mean_words, fill = variety)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer() +
  theme_bw() +
  xlab("Género") +
  ylab("Media palabras") +
  labs(fill = "Variedad")

# Media por variedad
variety_mean_words_test <- ggplot(author_features_test, aes(factor(variety), mean_words, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer() +
  theme_bw() +
  xlab("Variedad") +
  ylab("Media palabras") +
  labs(fill = "Género")

# Total de palabras en tweets por clase en test
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1, heights = unit(c(1, 4, 4), "null"))))
grid.text("Test - Total de palabras en tweets por clase", vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(gender_total_words_test, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(variety_total_words_test, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))

# Media de palabras en tweets por clase en test
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1, heights = unit(c(1, 4, 4), "null"))))
grid.text("Test - Media de palabras por tweet por clase", vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(gender_mean_words_test, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(variety_mean_words_test, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))


# 2.3.3 Número de muestras por la clase


# Training
ggplot(author_features_training, aes(factor(variety), fill = gender)) +
  stat_count(geom = "bar") +
  scale_fill_brewer() +
  theme_bw() +
  ggtitle("Training - Muestras por clase") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Variedad") +
  ylab("Total autores") +
  labs(fill = "Género")

# Test
ggplot(author_features_test, aes(factor(variety), fill = gender)) +
  stat_count(geom = "bar") +
  scale_fill_brewer() +
  theme_bw() +
  ggtitle("Test - Muestras por clase") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Variedad") +
  ylab("Total autores") +
  labs(fill = "Género")
