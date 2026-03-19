options(stringsAsFactors = FALSE)

library(quanteda)
library(readr)

dir.create("Daten/aufbereitet", recursive = TRUE, showWarnings = FALSE)




if (!file.exists("Daten/aufbereitet/tokens_2017_2023.rds")) {
  stop("Datei 'Daten/aufbereitet/tokens_2017_2023.rds' nicht gefunden. Bitte zuerst 02_segmentierung.R ausführen.")
}

if (!file.exists("Daten/aufbereitet/all_stopwords.rds")) {
  stop("Datei 'Daten/aufbereitet/all_stopwords.rds' nicht gefunden. Bitte zuerst 03_preprocessing.R ausführen.")
}

# Laden

tokens_raw <- readRDS("Daten/aufbereitet/tokens_2017_2023.rds")
all_stopwords <- readRDS("Daten/aufbereitet/all_stopwords.rds")

cat("Klasse von tokens_raw:\n")
print(class(tokens_raw))

if (!inherits(tokens_raw, "tokens")) {
  stop("Das geladene Objekt aus 'tokens_2017_2023.rds' ist kein quanteda-tokens-Objekt.")
}

# Stopwörter entfernen

tokens_clean <- quanteda::tokens_remove(
  tokens_raw,
  pattern = all_stopwords
)

# nur Tokens mit min. 3 Zeichen behalten
tokens_clean <- quanteda::tokens_keep(
  tokens_clean,
  min_nchar = 3
)

tokens_clean <- quanteda::tokens_ngrams(
  tokens_clean,
  n = 1:2
)

# DFM erstellen

dtm_reden <- quanteda::dfm(tokens_clean)

cat("dfm vor Trim:\n")
print(dtm_reden)

if (ndoc(dtm_reden) == 0) stop("dfm enthält 0 Dokumente.")
if (nfeat(dtm_reden) == 0) stop("dfm enthält 0 Features.")


# 
dtm_trimmed <- quanteda::dfm_trim(
  dtm_reden,
  min_termfreq = 5,
  min_docfreq = 3
)

cat("dfm nach Trim:\n")
print(dtm_trimmed)

if (ndoc(dtm_trimmed) == 0) stop("dfm_trimmed enthält 0 Dokumente.")
if (nfeat(dtm_trimmed) == 0) stop("dfm_trimmed enthält 0 Features.")

# speichernn

doc_ids <- quanteda::docnames(dtm_trimmed)

saveRDS(doc_ids, "Daten/aufbereitet/doc_ids.rds")
saveRDS(dtm_reden, "Daten/aufbereitet/dtm_reden_2017_2023.rds")
saveRDS(dtm_trimmed, "Daten/aufbereitet/dtm_trimmed_2017_2023.rds")

cat("\n04_dtm.R erfolgreich durchgelaufen.\n")
cat("Gespeichert wurden:\n")
cat("- Daten/aufbereitet/dtm_reden_2017_2023.rds\n")
cat("- Daten/aufbereitet/dtm_trimmed_2017_2023.rds\n")
cat("- Daten/aufbereitet/doc_ids.rds\n")
cat("Anzahl Dokumente:", ndoc(dtm_trimmed), "\n")
cat("Anzahl Features:", nfeat(dtm_trimmed), "\n")