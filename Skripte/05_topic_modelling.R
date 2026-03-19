options(stringsAsFactors = FALSE)

library(topicmodels)
library(quanteda)
library(tm)


if (!file.exists("Daten/aufbereitet/dtm_trimmed_2017_2023.rds")) {
  stop("Datei 'Daten/aufbereitet/dtm_trimmed_2017_2023.rds' nicht gefunden. Bitte zuerst 04_dtm.R ausführen.")
}

dir.create("Ergebnisse", recursive = TRUE, showWarnings = FALSE)

# DTM laden

dtm_raw <- readRDS("Daten/aufbereitet/dtm_trimmed_2017_2023.rds")

cat("Klasse von dtm_raw:\n")
print(class(dtm_raw))

# In DocumentTermMatrix für topicmodels umwandeln

if (inherits(dtm_raw, "DocumentTermMatrix")) {
  dtm <- dtm_raw
  
} else if (inherits(dtm_raw, "dfm")) {
  dtm <- quanteda::convert(dtm_raw, to = "topicmodels")
  
} else {
  stop("Unbekanntes DTM-Format. Bitte class(dtm_raw) prüfen.")
}

cat("Klasse von dtm nach Umwandlung:\n")
print(class(dtm))
print(dtm)


# Leere Dokumente/Begriffe entfernen


row_totals <- slam::row_sums(dtm)
col_totals <- slam::col_sums(dtm)

dtm <- dtm[row_totals > 0, col_totals > 0]

cat("DTM nach Bereinigung:\n")
print(dtm)

if (nrow(dtm) == 0) {
  stop("Die DTM enthält nach Bereinigung keine Dokumente mehr.")
}

if (ncol(dtm) == 0) {
  stop("Die DTM enthält nach Bereinigung keine Terme mehr.")
}

# Dokument-IDs speichern

doc_ids_trimmed <- rownames(dtm)

if (is.null(doc_ids_trimmed)) {
  stop("Die bereinigte DTM hat keine rownames. Dokument-IDs können nicht gespeichert werden.")
}

saveRDS(doc_ids_trimmed, "Daten/aufbereitet/doc_ids_trimmed_2017_2023.rds")

cat("Bereinigte doc_ids gespeichert:\n")
cat("- Daten/aufbereitet/doc_ids_trimmed_2017_2023.rds\n")

# Topic Model berechnen

k <- 20
set.seed(1234)

topicModel <- LDA(
  dtm,
  k = k,
  method = "Gibbs",
  control = list(
    seed = 1234,
    burnin = 1000,
    iter = 2000,
    thin = 100
  )
)


# Speichern

saveRDS(topicModel, "Ergebnisse/topic_model_2017_2023.rds")

cat("\n05_topic_modelling.R erfolgreich durchgelaufen.\n")
cat("Gespeichert wurde:\n")
cat("- Ergebnisse/topic_model_2017_2023.rds\n")