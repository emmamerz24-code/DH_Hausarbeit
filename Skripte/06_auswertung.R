options(stringsAsFactors = FALSE)

library(topicmodels)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(ggplot2)
library(forcats)
library(stringr)


if (file.exists("Ergebnisse/topic_model_2017_2023.rds")) {
  topicModel <- readRDS("Ergebnisse/topic_model_2017_2023.rds")
} else if (!exists("topicModel")) {
  stop("Weder gespeicherte Datei 'Ergebnisse/topic_model_2017_2023.rds' noch Objekt 'topicModel' gefunden. Bitte zuerst 05_topic_modelling.R ausführen.")
}

if (!file.exists("Daten/aufbereitet/doc_ids_trimmed_2017_2023.rds")) {
  stop("Datei 'Daten/aufbereitet/doc_ids_trimmed_2017_2023.rds' nicht gefunden. Bitte zuerst 05_topic_modelling.R ausführen.")
}

if (!file.exists("Daten/aufbereitet/doc_party.csv")) {
  stop("Datei 'Daten/aufbereitet/doc_party.csv' nicht gefunden. Bitte zuerst 03b_doc_party.R ausführen.")
}

dir.create("Ergebnisse", recursive = TRUE, showWarnings = FALSE)
dir.create("Grafiken", recursive = TRUE, showWarnings = FALSE)
dir.create("Grafiken/parteien_einzeln", recursive = TRUE, showWarnings = FALSE)

# Top-Wörter je Topic


top_n <- 10
topic_terms_mat <- terms(topicModel, top_n)

topic_labels_df <- tibble(
  topic = 1:ncol(topic_terms_mat),
  top_terms = apply(topic_terms_mat, 2, paste, collapse = ", "),
  label_vorlaeufig = c(
    "Brexit / Großbritannien / CETA",        # 1
    "Innere Sicherheit / BAMF / Cannabis",   # 2
    "EZB / Bundesbank / Finanzpolitik",      # 3
    "Afghanistan / UN / Taliban",            # 4
    "Afrika / Saudi-Arabien / Jemen",        # 5
    "EU / IWF / internationale Wirtschaft",  # 6
    "Hartz / KfW / Sicherheit",              # 7
    "Bundeswehr / NATO / Auslandseinsätze",  # 8
    "Karlsruhe / Energie / Kontrolle",       # 9
    "Freie Demokraten / Krise / Verwaltung", # 10
    "Nahost / Israel / Iran",                # 11
    "Chemnitz / Regionalbezüge",             # 12
    "DDR / Ostdeutschland",                  # 13
    "Rechtsextremismus / Hanau / Lübcke",    # 14
    "Russland / Ukraine / Osteuropa",        # 15
    "China / Weltwirtschaft",                # 16
    "Green Deal / Paris / Lateinamerika",    # 17
    "Mittelmeer / Libyen",                   # 18
    "Sahel / MINUSMA / Auslandseinsatz",     # 19
    "NATO / USA / Osteuropa"                 # 20
  ),
  interpretation_kurz = NA_character_,
  brauchbar = c(
    TRUE,  TRUE,  FALSE, TRUE,  TRUE,
    FALSE, FALSE, TRUE,  FALSE, FALSE,
    TRUE,  FALSE, TRUE,  TRUE,  TRUE,
    TRUE,  TRUE,  FALSE, FALSE, TRUE
  ),
  topic_family = c(
    "eu_grossbritannien",   # 1
    "innenpolitik",         # 2
    "wirtschaft",           # 3
    "afghanistan",          # 4
    "afrika_nahost",        # 5
    "europa_misc",          # 6
    "sozialstaat_misc",     # 7
    "militaer",             # 8
    "kontrolle_misc",       # 9
    "parteien_misc",        # 10
    "nahost",               # 11
    "regional_misc",        # 12
    "ostdeutschland",       # 13
    "rechtsextremismus",    # 14
    "ukraine",              # 15
    "china_weltwirtschaft", # 16
    "klima_eu",             # 17
    "mittelmeer",           # 18
    "sahel",                # 19
    "militaer"              # 20
  )
)

print(topic_labels_df, n = 20)

# Theta extrahieren

theta <- posterior(topicModel)$topics
theta_df <- as.data.frame(theta)
colnames(theta_df) <- paste0("Topic_", 1:ncol(theta_df))


doc_ids <- readRDS("Daten/aufbereitet/doc_ids_trimmed_2017_2023.rds")

if (nrow(theta_df) != length(doc_ids)) {
  stop("Anzahl der Theta-Dokumente passt nicht zu doc_ids_trimmed_2017_2023.rds.")
}

theta_df$doc <- doc_ids

#  Metadaten aus docnames bauen

year_part <- sub(".*?(\\d{4}).*", "\\1", doc_ids)
date_part <- sub(".*?(\\d{4}-\\d{2}-\\d{2}).*", "\\1", doc_ids)

meta <- tibble(
  doc = doc_ids,
  date = date_part,
  year = as.numeric(year_part)
) %>%
  mutate(
    wahljahr = ifelse(year %in% c(2017, 2021), "Wahljahr", "Nichtwahljahr")
  )

print(table(meta$year, useNA = "ifany"))
print(table(meta$wahljahr, useNA = "ifany"))

# Theta + Metadaten verbinden

theta_meta <- left_join(meta, theta_df, by = "doc")

# Parteidaten anhängen

doc_party <- read_csv("Daten/aufbereitet/doc_party.csv", show_col_types = FALSE)

theta_meta_party <- theta_meta %>%
  left_join(doc_party, by = "doc") %>%
  filter(!is.na(party))

cat("\nDokumente mit Parteizuordnung (roh):\n")
print(table(theta_meta_party$party))

# Nur für micg wichtige Parteien nehmen


parteien_fest <- c("CDU/CSU", "SPD", "AfD", "FDP", "Grüne", "Linke")

cat("\nParteien in den Daten vor Filter:\n")
print(sort(unique(theta_meta_party$party)))

theta_meta_party <- theta_meta_party %>%
  filter(party %in% parteien_fest)

cat("\nDokumente mit Parteizuordnung (nach Filter auf 6 Parteien):\n")
print(table(theta_meta_party$party))

fehlende_parteien <- setdiff(parteien_fest, unique(theta_meta_party$party))
if (length(fehlende_parteien) > 0) {
  warning(
    paste(
      "Diese festgelegten Parteien wurden in den Daten nicht gefunden:",
      paste(fehlende_parteien, collapse = ", ")
    )
  )
}
#########################################

topic_by_year <- theta_meta %>%
  group_by(year) %>%
  summarise(across(starts_with("Topic_"), mean), .groups = "drop")

print(topic_by_year)


topic_compare <- theta_meta %>%
  group_by(wahljahr) %>%
  summarise(across(starts_with("Topic_"), mean), .groups = "drop")

print(topic_compare)


topic_compare_long <- topic_compare %>%
  pivot_longer(
    cols = starts_with("Topic_"),
    names_to = "topic_name",
    values_to = "mean_theta"
  ) %>%
  mutate(
    topic = as.numeric(gsub("Topic_", "", topic_name))
  ) %>%
  left_join(topic_labels_df, by = "topic")

print(topic_compare_long, n = 40)

#differenz berechnen

topic_diff <- topic_compare_long %>%
  select(wahljahr, topic, label_vorlaeufig, top_terms, mean_theta) %>%
  pivot_wider(
    names_from = wahljahr,
    values_from = mean_theta
  )

if (!all(c("Wahljahr", "Nichtwahljahr") %in% colnames(topic_diff))) {
  stop("Es wurden nicht beide Gruppen gefunden: 'Wahljahr' und 'Nichtwahljahr'.")
}

topic_diff <- topic_diff %>%
  mutate(
    diff_wahljahr_minus_nichtwahljahr = Wahljahr - Nichtwahljahr
  ) %>%
  arrange(desc(diff_wahljahr_minus_nichtwahljahr))

print(topic_diff, n = 20)


topic_by_year_long <- topic_by_year %>%
  pivot_longer(
    cols = starts_with("Topic_"),
    names_to = "topic_name",
    values_to = "mean_theta"
  ) %>%
  mutate(
    topic = as.numeric(gsub("Topic_", "", topic_name))
  ) %>%
  left_join(topic_labels_df, by = "topic")

print(topic_by_year_long, n = 40)

write_csv(topic_labels_df, "Ergebnisse/topic_labels.csv")
write_csv(theta_meta, "Ergebnisse/theta_meta.csv")
write_csv(topic_by_year, "Ergebnisse/topic_by_year.csv")
write_csv(topic_by_year_long, "Ergebnisse/topic_by_year_long.csv")
write_csv(topic_compare, "Ergebnisse/topic_compare_wahljahr.csv")
write_csv(topic_compare_long, "Ergebnisse/topic_compare_wahljahr_long.csv")
write_csv(topic_diff, "Ergebnisse/topic_diff_wahljahr.csv")
####################################################

plot_topics_unique <- topic_diff %>%
  left_join(
    topic_labels_df %>% select(topic, topic_family, brauchbar),
    by = "topic"
  ) %>%
  filter(brauchbar) %>%
  mutate(
    abs_diff = abs(diff_wahljahr_minus_nichtwahljahr)
  ) %>%
  group_by(topic_family) %>%
  slice_max(order_by = abs_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  slice_max(order_by = abs_diff, n = 5, with_ties = FALSE) %>%
  arrange(desc(diff_wahljahr_minus_nichtwahljahr))

write_csv(
  plot_topics_unique,
  "Ergebnisse/results_topic_diff_top5_unique.csv"
)

###################################################

plot_top5_unique <- ggplot(
  plot_topics_unique,
  aes(
    x = reorder(label_vorlaeufig, diff_wahljahr_minus_nichtwahljahr),
    y = diff_wahljahr_minus_nichtwahljahr,
    fill = diff_wahljahr_minus_nichtwahljahr > 0
  )
) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#2E86AB", "FALSE" = "#C0392B"), guide = "none") +
  labs(
    title = "Top 5 relevante, nicht-redundante Topics",
    subtitle = "Positive Werte = häufiger im Wahljahr",
    x = "",
    y = "Differenz der mittleren Topic-Prävalenz"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "Grafiken/plot_topic_diff_top5_unique.png",
  plot_top5_unique,
  width = 10,
  height = 7,
  dpi = 300
)


plot_topics_ids <- plot_topics_unique$topic

trend_top5_unique <- topic_by_year_long %>%
  filter(topic %in% plot_topics_ids)

write_csv(
  trend_top5_unique,
  "Ergebnisse/results_topic_trends_top5_unique.csv"
)

plot_trends_top5_unique <- ggplot(
  trend_top5_unique,
  aes(
    x = year,
    y = mean_theta,
    color = label_vorlaeufig,
    group = label_vorlaeufig
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Zeitverlauf der 5 relevanten, nicht-redundanten Topics (2017–2023)",
    x = "Jahr",
    y = "Mittlere Topic-Prävalenz",
    color = "Topic"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "Grafiken/plot_topic_trends_top5_unique.png",
  plot_trends_top5_unique,
  width = 11,
  height = 7,
  dpi = 300
)


theta_meta_party_long <- theta_meta_party %>%
  pivot_longer(
    cols = starts_with("Topic_"),
    names_to = "topic_name",
    values_to = "gamma"
  ) %>%
  mutate(
    topic = as.numeric(gsub("Topic_", "", topic_name))
  ) %>%
  left_join(topic_labels_df, by = "topic")


party_wahl <- theta_meta_party_long %>%
  group_by(party, topic, label_vorlaeufig, wahljahr) %>%
  summarise(
    mean_gamma = mean(gamma, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = wahljahr,
    values_from = mean_gamma
  )

if (!all(c("Wahljahr", "Nichtwahljahr") %in% colnames(party_wahl))) {
  stop("In der Parteiauswertung wurden nicht beide Gruppen gefunden: 'Wahljahr' und 'Nichtwahljahr'.")
}

party_wahl <- party_wahl %>%
  mutate(
    Wahljahr = coalesce(Wahljahr, 0),
    Nichtwahljahr = coalesce(Nichtwahljahr, 0),
    diff = Wahljahr - Nichtwahljahr,
    abs_diff = abs(diff),
    Wahljahr_pct = round(Wahljahr * 100, 2),
    Nichtwahljahr_pct = round(Nichtwahljahr * 100, 2),
    diff_pct = round(diff * 100, 2)
  ) %>%
  arrange(party, desc(abs_diff))

# Tabelle mit allen 20 Topics 

topic_table_all_topics_by_party <- party_wahl %>%
  transmute(
    topic,
    Partei = party,
    Topic = paste0("T", topic),
    Thema = label_vorlaeufig,
    `Nichtwahljahr (%)` = round(Nichtwahljahr_pct, 2),
    `Wahljahr (%)` = round(Wahljahr_pct, 2),
    `Differenz (%)` = round(diff_pct, 2)
  ) %>%
  arrange(topic, Partei) %>%
  select(-topic)



write_csv(
  topic_table_all_topics_by_party,
  "Ergebnisse/topic_table_all_topics_by_party.csv"
)



party_wahl_top5 <- party_wahl %>%
  group_by(party) %>%
  slice_max(order_by = abs_diff, n = 5, with_ties = FALSE) %>%
  ungroup()

write_csv(party_wahl_top5, "Ergebnisse/topic_diff_by_party_top5.csv")



plot_data_party_wahl <- party_wahl_top5 %>%
  select(party, topic, label_vorlaeufig, Wahljahr_pct, Nichtwahljahr_pct) %>%
  pivot_longer(
    cols = c(Wahljahr_pct, Nichtwahljahr_pct),
    names_to = "gruppe",
    values_to = "percent"
  ) %>%
  mutate(
    gruppe = recode(
      gruppe,
      "Wahljahr_pct" = "Wahljahr",
      "Nichtwahljahr_pct" = "Nichtwahljahr"
    )
  ) %>%
  group_by(party) %>%
  mutate(label_vorlaeufig = fct_reorder(label_vorlaeufig, percent, .desc = TRUE)) %>%
  ungroup()

plot_topics_by_party_wahljahr_top5 <- ggplot(
  plot_data_party_wahl,
  aes(x = label_vorlaeufig, y = percent, fill = gruppe)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~ party, scales = "free_y") +
  labs(
    title = "Top 5 Topics: Wahljahr vs. Nichtwahljahr nach Partei",
    x = "",
    y = "Mittlere Topic-Prävalenz (%)",
    fill = ""
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "Grafiken/plot_topics_by_party_wahljahr_top5.png",
  plot_topics_by_party_wahljahr_top5,
  width = 14,
  height = 10,
  dpi = 300
)
# eine grafik für jede partei einzelnd 

for (p in parteien_fest) {
  
  df_p <- party_wahl_top5 %>%
    filter(party == p)
  
  if (nrow(df_p) == 0) next
  
  plot_df_p <- df_p %>%
    select(party, topic, label_vorlaeufig, Wahljahr_pct, Nichtwahljahr_pct) %>%
    pivot_longer(
      cols = c(Wahljahr_pct, Nichtwahljahr_pct),
      names_to = "gruppe",
      values_to = "percent"
    ) %>%
    mutate(
      gruppe = recode(
        gruppe,
        "Wahljahr_pct" = "Wahljahr",
        "Nichtwahljahr_pct" = "Nichtwahljahr"
      )
    ) %>%
    group_by(party) %>%
    mutate(label_vorlaeufig = fct_reorder(label_vorlaeufig, percent, .desc = TRUE)) %>%
    ungroup()
  
  plot_single_party_wahl <- ggplot(
    plot_df_p,
    aes(x = label_vorlaeufig, y = percent, fill = gruppe)
  ) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(
      title = paste("Top 5 Topics:", p),
      subtitle = "Wahljahr vs. Nichtwahljahr",
      x = "",
      y = "Mittlere Topic-Prävalenz (%)",
      fill = ""
    ) +
    theme_minimal(base_size = 12)
  
  file_name_p <- str_replace_all(p, "[^[:alnum:]]+", "_")
  
  ggsave(
    paste0("Grafiken/parteien_einzeln/plot_topic_diff_", file_name_p, ".png"),
    plot_single_party_wahl,
    width = 10,
    height = 7,
    dpi = 300
  )
}
# Trends

party_year <- theta_meta_party_long %>%
  group_by(party, year, topic, label_vorlaeufig) %>%
  summarise(
    mean_gamma = mean(gamma, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent = round(mean_gamma * 100, 2)
  )

top5_topics_party_year <- party_year %>%
  group_by(party, topic, label_vorlaeufig) %>%
  summarise(
    mean_all_years = mean(percent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(party) %>%
  slice_max(order_by = mean_all_years, n = 5, with_ties = FALSE) %>%
  ungroup()

party_year_top5 <- party_year %>%
  semi_join(
    top5_topics_party_year,
    by = c("party", "topic", "label_vorlaeufig")
  )

write_csv(party_year_top5, "Ergebnisse/topic_trends_by_party_top5.csv")

# Diagramm Zeitverlauf

plot_topic_trends_by_party_top5 <- ggplot(
  party_year_top5,
  aes(
    x = year,
    y = percent,
    color = label_vorlaeufig,
    group = label_vorlaeufig
  )
) +
  geom_vline(
    xintercept = c(2017, 2021),
    color = "#2E86AB",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ party, scales = "free_y") +
  scale_x_continuous(
    breaks = 2017:2023,
    limits = c(2016.8, 2023.2)
  ) +
  labs(
    title = "Zeitverlauf der Top-5-Themen 2017–2023 nach Partei",
    subtitle = "Blau hinterlegt sind die Wahljahre 2017 und 2021",
    x = "Jahr",
    y = "Anteil der Redebeiträge zum Thema (%)",
    color = "Thema"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "Grafiken/plot_topic_trends_by_party_top5.png",
  plot_topic_trends_by_party_top5,
  width = 14,
  height = 10,
  dpi = 300
)

# Einzelne Diagrammme für jede Partei

for (p in parteien_fest) {
  
  df_p <- party_year_top5 %>%
    filter(party == p)
  
  if (nrow(df_p) == 0) next
  
  plot_single_party_trend <- ggplot(
    df_p,
    aes(
      x = year,
      y = percent,
      color = label_vorlaeufig,
      group = label_vorlaeufig
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 2017:2023) +
    labs(
      title = paste("Topic-Trends 2017–2023:", p),
      x = "Jahr",
      y = "Mittlere Topic-Prävalenz (%)",
      color = "Topic"
    ) +
    theme_minimal(base_size = 12)
  
  file_name_p <- str_replace_all(p, "[^[:alnum:]]+", "_")
  
  ggsave(
    paste0("Grafiken/parteien_einzeln/plot_topic_trend_", file_name_p, ".png"),
    plot_single_party_trend,
    width = 10,
    height = 7,
    dpi = 300
  )
}


cat("06_auswertung.R erfolgreich durchgelaufen.\n")
cat("Gespeichert wurden:\n")
cat("- Ergebnisse/topic_labels.csv\n")
cat("- Ergebnisse/theta_meta.csv\n")
cat("- Ergebnisse/topic_by_year.csv\n")
cat("- Ergebnisse/topic_by_year_long.csv\n")
cat("- Ergebnisse/topic_compare_wahljahr.csv\n")
cat("- Ergebnisse/topic_compare_wahljahr_long.csv\n")
cat("- Ergebnisse/topic_diff_wahljahr.csv\n")
cat("- Ergebnisse/results_topic_diff_top5_unique.csv\n")
cat("- Ergebnisse/results_topic_trends_top5_unique.csv\n")
cat("- Ergebnisse/topic_diff_by_party_top5.csv\n")
cat("- Ergebnisse/topic_trends_by_party_top5.csv\n")
cat("- Grafiken/plot_topic_diff_top5_unique.png\n")
cat("- Grafiken/plot_topic_trends_top5_unique.png\n")
cat("- Grafiken/plot_topics_by_party_wahljahr_top5.png\n")
cat("- Grafiken/plot_topic_trends_by_party_top5.png\n")
cat("- Grafiken/parteien_einzeln/plot_topic_diff_CDU_CSU_.png etc.\n")
cat("- Grafiken/parteien_einzeln/plot_topic_trend_CDU_CSU_.png etc.\n")