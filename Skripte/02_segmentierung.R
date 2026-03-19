# Daten laden (also Ergebnis aus Skript 01)
g_17_23 <- readRDS("Daten/aufbereitet/germaparl2_2017_2023.rds")
size(g_17_23)

g_speech <- subset(g_17_23, p == "type=\"speech\"")
size(g_speech, s_attribute = "p")

saveRDS(
  g_speech,
  file = "Daten/aufbereitet/germaparl2_speech_2017_2023.rds"
)