options(stringsAsFactors = FALSE)

library(stopwords)

dir.create("Daten/aufbereitet", recursive = TRUE, showWarnings = FALSE)

# Stopwörter

custom_stopwords <- c(
  # parlamentarischer Rahmen
  "damen", "herren", "herr", "frau",
  "präsident", "präsidentin", "praesident", "praesidentin",
  "bundestag", "bundestages", "bundestagsfraktion",
  "kolleginnen", "kollegen", "kollegin", "kollege",
  "beifall", "zuruf", "gegenruf",
  "fraktion", "fraktionen",
  "bundesregierung", "regierung",
  "minister", "ministerin", "bundesminister", "bundesministerin",
  "staatsminister", "staatsministerin",
  "bundesministerium", "bundesministeriums",
  "parlamentarische", "parlamentarischen",
  "deutschland", "deutsche", "deutschen", "deutscher", "deutschem", "deutsches", "deutschlands", "deutsch",
  
  # parteien
  "afd","cdu","csu","spd","fdp",
  "grüne","grünen","gruene","gruenen",
  "linke","linken",
  "bündnis","buendnis","union",
  "sozialdemokraten","linkspartei",
  
  # institutionen
  "institut","stiftung","gesetz","gesetzentwurf","gesetze",
  "bundesrat","bundesverfassungsgericht",
  "bundesrepublik",
  "parlament","parlaments","gerichtshof",
  "kommission","parlamentarier",
  
  # Müll
  "bmf","bmbf","bmz","bka","bafin","dgb","bdi","ktf","thw",
  "who","covid","corona",
  "facebook","twitter","google",
  "germany","guardian",
  "you","never","alone","walk",
  
  # länder / regionen
  "europa","europas","europäische","europäischen","europäischer","europäisch","europäisches","europäer",
  "brüssel",
  "deutschland","frankreich","franzosen","französisch","französische","französischen",
  "polen","russland","ukraine","china","usa","amerika","amerikanischen","amerikanische","amerikaner",
  "kanada","indien","japan","hongkong",
  "österreich","schweiz","ungarn","dänemark","italien","spanien","griechenland","griechischen",
  "türkei","türkische","türkischen",
  "iran","irak","irakischen","syrien","syrischen","israel","israels","libanon",
  "mali","niger","ägypten","somalia","sudan","südsudan","marokko",
  "bosnien","serbien","kosovo","westbalkan","balkan",
  "belarus","moldau","moskau",
  
  # bundesländer / städte
  "bayern","nordrhein","westfalen","baden","württemberg","niedersachsen","hessen",
  "schleswig","holstein","rheinland","pfalz","nrw",
  "berlin","berliner","hamburg","hamburger","bremen","bonn","köln","frankfurt","münchen","stuttgart",
  "leipzig","dresden","thüringen","sachsen","ostdeutschland","ostdeutschen","mecklenburg","vorpommern","anhalt",
  
  # generische sachwörter (zu oberflächig)
  "euro","cent","wirtschaft","arbeit","soziales",
  "bank","universität","gmbh","bahn",
  "haus","liebe","rot","gross","grosse","new"
)

# Namen entfernen

name_stopwords <- c(
  "jens","stephan","frank","katja","norbert","sabine","ulrich","jan",
  "sebastian","anja","katrin","susanne","beck","kerstin","uwe","bernd",
  "christoph","jürgen","juergen","olaf","scholz","merkel","spahn","lindner",
  "seehofer","faeser","habeck","scheuer","lauterbach","karliczek",
  "klöckner","kloeckner","thomae","birkwald","aggelidis","grigorios","horst",
  "hubertus","angela","michael","thomas","peter","martin","markus","stefan",
  "andreas","karl","klaus","volker","christian","lisa","klein","kühn","kuehn",
  "hoffmann","müller","mueller","ullrich","brehm","frei","thorsten","matthias",
  "alexander","johannes","wolfgang","carsten","dirk","schmidt","claudia","hans",
  "gabriele","joachim","marc","bernhard","helge",
  "maas","merz","fechner","binding","gremmels","scheer","timon",
  "erndl","kuffer","zeulner","lenz","staffler","brandner","altmaier",
  "marie","schmeink","gemmeke","göring","goering","asche","eckardt","gonther",
  "kappert","kramp","karrenbauer",
  "kuhn","kuehn","strengmann","dugnus","aschenberg","tübingen","tuebingen",
  "stark","watzinger","strack","plahr","brömer","broemer","helling","carl","berg",
  "adam","kohl","söder","soeder","macron","trump","putin","assad"
)

# Basis-Stopwörteer

all_stopwords <- unique(c(
  stopwords::stopwords("de"),
  custom_stopwords,
  name_stopwords
))

# Sprechernamen ergänzen

if (!file.exists("Daten/aufbereitet/doc_ids.rds")) {
  stop("Datei 'Daten/aufbereitet/doc_ids.rds' nicht gefunden. Bitte zuerst 02_segmentierung.R ausführen.")
}
docnames_all <- readRDS("Daten/aufbereitet/doc_ids.rds")

speaker_names <- sub("_.*", "", docnames_all)
name_parts <- unique(unlist(strsplit(speaker_names, " ")))
name_parts <- tolower(name_parts)
name_parts <- name_parts[nchar(name_parts) > 2]
name_parts <- setdiff(name_parts, c("von","der","den","und","des","zur","zum"))

all_stopwords <- unique(c(all_stopwords, name_parts))
all_stopwords <- sort(all_stopwords)

saveRDS(all_stopwords, "Daten/aufbereitet/all_stopwords.rds")

cat("03_preprocessing.R erfolgreich durchgelaufen\n")
cat("Anzahl Stopwörter:", length(all_stopwords), "\n")