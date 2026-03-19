options(stringsAsFactors = FALSE)

library(cwbtools)
library(polmineR)
#library(quanteda) #DTM
#require(topicmodels) #LDA

REGISTRY_DIR <- path.expand("~/cwb/registry")
CORPUS_ID <- "GERMAPARL2"

Sys.setenv(CORPUS_REGISTRY = REGISTRY_DIR) #sagt polmine in welchem Odner es nach dem Korpora schauen soll

YEAR_MIN <- 2017 #Untersuchungszeitraum
YEAR_MAX <- 2023
WAHLJAHRE <- c(2017, 2021)

K <- 20 #Anzahl topics für LDA Modell (finde 20 Themen im Korpus)
GIBBS_ITER <- 500 #ANzahl der Iterationen 
SEED <- 1
ALPHA <- 0.02

TOPIC_TO_FILTER <- 6
TOPIC_THRESHOLD <- 0.10 #Schwelle, ab wann ein topic als relevant gilt 