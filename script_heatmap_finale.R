# ── Packages ──────────────────────────────────────────────────────────────────
library(tibble)
library(dplyr)
library(readr)
library(ComplexHeatmap)
library(circlize)

# ── Métadonnées ───────────────────────────────────────────────────────────────
metadata <- read_csv("list_samples_Cimex.csv")

metadata <- metadata %>%
  mutate(Host = ifelse(Host == "M", "B", Host)) %>%
  mutate(ID_clean = gsub(" ", "", ID),         # enlève espaces : "Cimex 113" -> "Cimex113"
         ID_clean = gsub("_", "-", ID_clean))  # remplace _ par - : "Cimex18_66" -> "Cimex18-66"

metadata %>% filter(grepl("18.66", ID))

# ── Table de correspondance ID -> Location ────────────────────────────────────
location_bat <- c(
  "Cimex451"=" - Beurnevésin - Suisse", "Cimex443"=" - Beurnevésin - Suisse", "Cimex437"=" - Beurnevésin - Suisse",
  "Cimex453"=" - Beurnevésin - Suisse", "Cimex435"=" - Beurnevésin - Suisse", "Cimex449"=" - Beurnevésin - Suisse",
  "Cimex450"=" - Beurnevésin - Suisse", "Cimex454"=" - Beurnevésin - Suisse", "Cimex444"=" - Beurnevésin - Suisse",
  "Cimex441"=" - Beurnevésin - Suisse", "Cimex447"=" - Beurnevésin - Suisse", "Cimex438"=" - Beurnevésin - Suisse",
  "Cimex446"=" - Beurnevésin - Suisse", "Cimex442"=" - Beurnevésin - Suisse", "Cimex452"=" - Beurnevésin - Suisse",
  "Cimex448"=" - Beurnevésin - Suisse", "Cimex436"=" - Beurnevésin - Suisse", "Cimex440"=" - Beurnevésin - Suisse",
  "Cimex445"=" - Beurnevésin - Suisse", "Cimex439"=" - Beurnevésin - Suisse",
  "Cimex327"=" - St-Ursanne - Suisse",  "Cimex332"=" - St-Ursanne - Suisse",  "Cimex328"=" - St-Ursanne - Suisse",
  "Cimex12"=" - St-Ursanne - Suisse",   "Cimex11"=" - St-Ursanne - Suisse",   "Cimex10"=" - St-Ursanne - Suisse",
  "Cimex29"=" - St-Ursanne - Suisse",   "Cimex418"=" - St-Ursanne - Suisse",  "Cimex28"=" - St-Ursanne - Suisse",
  "Cimex421"=" - St-Ursanne - Suisse",  "Cimex330"=" - St-Ursanne - Suisse",  "Cimex329"=" - St-Ursanne - Suisse",
  "Cimex429"=" - St-Ursanne - Suisse",  "Cimex416"=" - St-Ursanne - Suisse",  "Cimex410"=" - St-Ursanne - Suisse",
  "Cimex26"=" - St-Ursanne - Suisse",   "Cimex431"=" - St-Ursanne - Suisse",  "Cimex405"=" - St-Ursanne - Suisse",
  "Cimex420"=" - St-Ursanne - Suisse",  "Cimex331"=" - St-Ursanne - Suisse",  "Cimex434"=" - St-Ursanne - Suisse",
  "Cimex425"=" - St-Ursanne - Suisse",  "Cimex402"=" - St-Ursanne - Suisse",  "Cimex427"=" - St-Ursanne - Suisse",
  "Cimex32"=" - St-Ursanne - Suisse",   "Cimex419"=" - St-Ursanne - Suisse",  "Cimex403"=" - St-Ursanne - Suisse",
  "Cimex415"=" - St-Ursanne - Suisse",  "Cimex407"=" - St-Ursanne - Suisse",  "Cimex408"=" - St-Ursanne - Suisse",
  "Cimex31"=" - St-Ursanne - Suisse",   "Cimex426"=" - St-Ursanne - Suisse",  "Cimex422"=" - St-Ursanne - Suisse",
  "Cimex424"=" - St-Ursanne - Suisse",  "Cimex423"=" - St-Ursanne - Suisse",  "Cimex430"=" - St-Ursanne - Suisse",
  "Cimex417"=" - St-Ursanne - Suisse",  "Cimex8"=" - St-Ursanne - Suisse",    "Cimex406"=" - St-Ursanne - Suisse",
  "Cimex428"=" - St-Ursanne - Suisse",  "Cimex432"=" - St-Ursanne - Suisse",  "Cimex413"=" - St-Ursanne - Suisse",
  "Cimex30"=" - St-Ursanne - Suisse",   "Cimex412"=" - St-Ursanne - Suisse",  "Cimex409"=" - St-Ursanne - Suisse",
  "Cimex433"=" - St-Ursanne - Suisse",  "Cimex414"=" - St-Ursanne - Suisse",  "Cimex404"=" - St-Ursanne - Suisse",
  "Cimex411"=" - St-Ursanne - Suisse",  "Cimex33"=" - St-Ursanne - Suisse",   "Cimex34"=" - St-Ursanne - Suisse",
  "Cimex27"=" - St-Ursanne - Suisse",
  "Cimex292"="VD - Estavayer - Suisse", "Cimex293"="VD - Estavayer - Suisse", "Cimex294"="VD - Estavayer - Suisse",
  "Cimex295"="VD - Estavayer - Suisse", "Cimex296"="VD - Estavayer - Suisse",
  "Cimex37"="VS - Naters - Suisse", "Cimex35"="VS - Naters - Suisse", "Cimex36"="VS - Naters - Suisse",
  "Cimex18-44"="BE - Berne - Suisse",  "Cimex18-45"="BE - Berne - Suisse",  "Cimex18-46"="BE - Berne - Suisse",
  "Cimex18-47"="BE - Berne - Suisse",  "Cimex18-48"="BE - Berne - Suisse",
  "Cimex18-49"="BE - Wangen - Suisse", "Cimex18-50"="BE - Wangen - Suisse", "Cimex18-51"="BE - Wangen - Suisse",
  "Cimex18-52"="BE - Wangen - Suisse", "Cimex18-53"="BE - Wangen - Suisse", "Cimex18-54"="BE - Wangen - Suisse",
  "Cimex18-55"="BE - Wangen - Suisse", "Cimex18-56"="BE - Wangen - Suisse", "Cimex18-57"="BE - Wangen - Suisse",
  "SRR31096200"="CZ - Prague",
  "SRR31096201"="CZ - Ustek",
  "SRR31096202"="CZ - Ustek",
  "SRR31096203"="CZ - Ustek",
  "SRR31096204"="CZ - Ustek",
  "SRR31096205"="CZ - Prudka",
  "SRR31096206"="CZ - Prudka",
  "SRR31096207"="CZ - Prudka",
  "SRR31096208"="CZ - Havirov",
  "SRR31096209"="CZ - Havirov",
  "SRR31096210"="CZ - Havirov",
  "SRR31096211"="CZ - Havirov",
  "SRR31096212"="CZ - Prague",
  "SRR31096213"="CZ - Duba",
  "SRR31096214"="CZ - Duba",
  "SRR33823585"="CZ",
  "SRR33823587"="Hongrie"
)

# Ajoute Location dans metadata via ID_clean
metadata <- metadata %>%
  mutate(Location = case_when(
    ID_clean %in% names(location_bat) ~ location_bat[ID_clean],
    TRUE ~ "Suisse"
  )) %>%
  select(-ID_clean)


matrice_heatmap <- symbiotes_mapq_20 %>%
  column_to_rownames("sample") %>%
  as.matrix()

# 2. Enlève .markedDup des lignes
rownames(matrice_heatmap) <- gsub("\\.markedDup", "", rownames(matrice_heatmap))

# 3. Renomme les colonnes (une seule fois !)
colnames(matrice_heatmap)[colnames(matrice_heatmap) == "GCA_405_Wolbachia4_16S.fna"]            <- "Wolbachia_405"
colnames(matrice_heatmap)[colnames(matrice_heatmap) == "NC_012920.1_14747-15887_Homo_sapiens"]  <- "Homo_sapiens"
colnames(matrice_heatmap)[colnames(matrice_heatmap) == "NC_030043.1_Cimex_lectularius"]         <- "Cimex_lectularius"
colnames(matrice_heatmap)[colnames(matrice_heatmap) == "Wbev-like_16S_1"]                       <- "Symbiopectobacterium"
colnames(matrice_heatmap)[colnames(matrice_heatmap) == "Wmass_16S"]                             <- "Wolbachia_mass"
colnames(matrice_heatmap)[colnames(matrice_heatmap) == "GCA_505_serratia_16S.fna"]              <- "Serratia_505"
colnames(matrice_heatmap)[colnames(matrice_heatmap) == "Myotis_myotis"]                         <- "Myotis_myotis"
colnames(matrice_heatmap)[colnames(matrice_heatmap) == "NC_053523.1_13671-14813_Gallus_gallus"] <- "Gallus_gallus"

# Vérifie
colnames(matrice_heatmap)

# ── Tableau d'annotations ─────────────────────────────────────────────────────
annots <- data.frame(ID = rownames(matrice_heatmap)) %>%
  mutate(ID_clean = gsub("\\.markedDup", "", ID),
         ID_clean = gsub("_", "-", ID_clean),
         ID_clean = gsub("Cimex", "Cimex ", ID_clean)) %>%
  left_join(metadata %>% rename(ID_clean = ID), by = "ID_clean") %>%
  select(-ID_clean) %>%
  column_to_rownames("ID")

# ── Couleurs ──────────────────────────────────────────────────────────────────
col_hote <- c(H = "skyblue", B = "plum")

col_location <- c(
  " - Beurnevésin - Suisse" = "#1b7837",
  " - St-Ursanne - Suisse"  = "#a6d96a",
  "VD - Estavayer - Suisse" = "#4575b4",
  "VS - Naters - Suisse"    = "#d73027",
  "BE - Berne - Suisse"     = "#f46d43",
  "BE - Wangen - Suisse"    = "#fdae61",
  "CZ - Prague"             = "#8856a7",
  "CZ - Ustek"              = "#9ebcda",
  "CZ - Prudka"             = "#e0ecf4",
  "CZ - Havirov"            = "#810f7c",
  "CZ - Duba"               = "#4d004b",
  "CZ"                      = "#d4b9da",
  "Hongrie"                 = "sienna",
  "Suisse"                  = "lightgrey"
)

# ── Annotation ────────────────────────────────────────────────────────────────
ha <- rowAnnotation(
  Hote       = annots$Host,
  Provenance = annots$Location,
  col = list(
    Hote       = col_hote,
    Provenance = col_location
  )
)

# ── Heatmap ───────────────────────────────────────────────────────────────────
Heatmap(log10(matrice_heatmap_symbiotes + 1),
        name              = "log10(count+1)",
        left_annotation   = ha,
        cluster_rows      = TRUE,
        cluster_columns   = TRUE,
        column_names_side = "top",
        column_names_rot  = 45,
        row_names_gp      = gpar(fontsize = 5),
        column_names_gp   = gpar(fontsize = 8),
        col               = colorRamp2(c(0, 1, 3, 5), c("snow", "yellow1", "darkorange", "darkred")),
        column_title      = "Profil Symbiotique des Punaises de Lit",
        row_title         = "Échantillons (Clustering par profil)")



### Code de profondeur 
df_novo_espece %>%
  filter(grepl("Cimex lectularius", tax_name)) %>%
  mutate(groupe = ifelse(sample %in% c("Cimex355","Cimex341","Cimex443",
                                       "Cimex453","Cimex417","Cimex454",
                                       "Cimex450","Cimex437","Cimex449",
                                       "Cimex411","Cimex441","Cimex438","Cimex445"),
                         "Sans Wolbachia", "Avec Wolbachia")) %>%
  group_by(groupe) %>%
  summarise(mean_cimex = mean(mapped))







 ######### HEATMAP QUE SYMBIOTES ####### 
# Colonnes à enlever
a_enlever <- c("Cimex_lectularius", "Homo_sapiens", "Myotis_myotis", "Gallus_gallus")

matrice_heatmap_symbiotes <- matrice_heatmap[, !colnames(matrice_heatmap) %in% a_enlever]

#puis relancer heatmap avec "matrice_heatmap_symbiotes" à la place de heatmap_matrice 