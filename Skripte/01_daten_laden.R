# Korpus laden
g <- polmineR::corpus(CORPUS_ID)

# Zeitraum 2017–2023 filtern
g_17_23 <- subset(
  g,
  protocol_year >= YEAR_MIN & protocol_year <= YEAR_MAX
)

#Zwischenrufe entfernen
g_17_23 <- subset(
  g_17_23,
  ne_type != "interjection"
)


size(g)
size(g_17_23)

# Objekt speichern
saveRDS(
  g_17_23,
  file = "Daten/aufbereitet/germaparl2_2017_2023.rds"
)