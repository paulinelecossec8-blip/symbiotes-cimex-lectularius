#Sélection des 20 espèces les plus abondantes

#packages à charger 
library(readr)
library(dplyr)
library(tidyr)
library(ComplexHeatmap)
library(circlize)
library(tibble)


### provient d'autres scripts ####
df_novo <- read_tsv("tableau_novo.tsv",
                    col_names = c("sample", "reference", "mapped"))

df_novo1 <- df_novo %>% filter(reference != "*", mapped > 10)

### Filtrage : ne garder que les contigs prokaryotes ───
taxo <- read_tsv("~/Documents/Galaxy5-[NCBI FCS GX on dataset 2_ Taxonomy report].tabular", comment = "#",
                 col_names = c("seq_id","seq_len","lens","cvg","sep1","tax_name","tax_id",
                               "div1","cvg_div1","cvg_tax1","score1","sep2","tax_id2","div2",
                               "cvg_div2","cvg_tax2","score2","sep3","tax_id3","div3",
                               "cvg_div3","cvg_tax3","score3","sep4","tax_id4","div4",
                               "cvg_div4","cvg_tax4","score4","sep5","reserved","result","div","div_pct_cvg"))

# Filtre les contigs prokaryotes
prok <- taxo %>% filter(grepl("prok", div1))
# Extrait juste les IDs depuis prok (déjà filtré)
contigs_prok_ids <- prok %>% pull(seq_id)

df_novo_prok <- df_novo1 %>%
  filter(reference %in% contigs_prok_ids)

longueur_contigs <- taxo %>% select(seq_id, seq_len)

#### script 

df_novo_prok_norm <- df_novo_prok %>%
  left_join(longueur_contigs, by = c("reference" = "seq_id")) %>%
  # filtrer pour enlever les contigs avec trop peu de reads 
  filter(mapped >= 10) %>%
  # Normalise chaque contig par sa propre longueur en kb
  mutate(mapped_norm = mapped / (seq_len / 1000)) %>%
  select(-seq_len, -mapped) %>%
  rename(mapped = mapped_norm)

top20_especes <- df_novo_espece_norm %>%
  group_by(tax_name) %>%
  summarise(total = sum(mapped)) %>%
  slice_max(total, n = 20) %>%
  pull(tax_name)

#Garde les contigs appartenant à ces 20 espèces
contigs_top20 <- annot_contigs %>%
  filter(tax_name %in% top20_especes)

# Tableau : 1 ligne = échantillon,1 colonne = contig
tableau_contigs <- df_novo_prok_norm %>%
  filter(reference %in% contigs_top20$seq_id) %>%
  pivot_wider(names_from = reference, values_from = mapped, values_fill = 0)

# ── Matrice ───────────────────────────────────────────────────────────────────
matrice_contigs <- tableau_contigs %>%
  column_to_rownames("sample") %>%
  as.matrix()

rownames(matrice_contigs) <- gsub("\\.markedDup", "", rownames(matrice_contigs))

# ── Annotation des COLONNES par espèce ───────────────────────────────────────
# C'est ça la nouveauté — on colorie les colonnes selon l'espèce du contig
annot_col <- contigs_top20 %>%
  filter(seq_id %in% colnames(matrice_contigs)) %>%
  arrange(match(seq_id, colnames(matrice_contigs)))


# Ajoute une colonne genre extraite du nom d'espèce
annot_col <- annot_col %>%
  mutate(genre = case_when(
    grepl("Wolbachia", tax_name)              ~ "Wolbachia",
    grepl("Symbiopectobacterium", tax_name)   ~ "Symbiopectobacterium",
    grepl("Rickettsia", tax_name)             ~ "Rickettsia",
    grepl("Sodalis", tax_name)                ~ "Sodalis",
    grepl("Sphingobium", tax_name)            ~ "Sphingobium",
    grepl("Paenibacillus", tax_name)          ~ "Paenibacillus",
    TRUE                                       ~ "Autre"
  ))

# Couleurs par genre
couleurs_genres <- c(
  "Wolbachia"              = "pink",
  "Symbiopectobacterium"   = "#4575b4",
  "Rickettsia"             = "lightgreen",
  "Sodalis"                = "seagreen",
  "Sphingobium"            = "cyan",
  "Paenibacillus"          = "orchid",
  "Autre"                  = "lightgrey"
)

# Annotation des colonnes par genre
ha_col <- HeatmapAnnotation(
  Genre = annot_col$genre,
  col = list(Genre = couleurs_genres),
  show_annotation_name = TRUE
)

# ── Heatmap ───────────────────────────────────────────────────────────────────
Heatmap(log10(matrice_contigs + 1),
        name              = "log10(count+1)",
        left_annotation   = ha,           # annotation hôte + provenance sur les lignes
        top_annotation    = ha_col,       # annotation espèce sur les colonnes
        cluster_rows      = TRUE,
        cluster_columns   = TRUE,
        column_names_side = "bottom",
        column_names_rot  = 45,
        show_column_names = FALSE,        # trop de contigs pour afficher les noms
        row_names_gp      = gpar(fontsize = 6),
        col               = colorRamp2(c(0, 1, 3, 5),
                                       c("snow", "yellow1", "darkorange", "darkred")),
        column_title      = "Profil Symbiotique — Approche de novo (par contig)",
        row_title         = "Échantillons")

