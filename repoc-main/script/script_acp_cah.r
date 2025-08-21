#Script R markdown pour l'automatisation de l'export de l'ensemble des Classification Ascendente Hiérarchique (CAH) et 
#Analyse des Commposantes Principales (ACP) des 7 types de navires. 
#Le traitement en double sur les fichiers est long, ce qui est normal sur R. Comptez au moins 10min en fichier R et 30min en Rmarkdown avec un knit HTML. 

---
title: "Analyse CAH/ACP "
author: "Centralité portuaire dans la Caraïbe"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_float: 
      collapsed: true
      smooth_scroll: true
    toc_depth: 3
    theme: flatly
    code_folding: hide
    fig_width: 12
    fig_height: 8
    number_sections: true
params:
  type_navire:
    label: "Type de navire"
    value: "bulk"
    input: select
    choices:
      bulk: "Vraquiers"
      cont: "Porte-conteneurs"
      crui: "Navires de croisière"
      gcar: "General cargo"
      iisl: "Navires inter-insulaires"
      serv: "Navires de service"
      tank: "Pétroliers"
  scenario:
    label: "Inclure les ports américains ?"
    value: "Avec"
    input: select
    choices:
      avec: "Oui"
      sans: "Non"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 12, fig.height = 8)
```

```{r libraries, message=FALSE, include=FALSE}
# Chargement des bibliothèques
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)
library(ggrepel)
library(tibble)
library(corrplot)
library(sf)
library(mapsf)
library(RColorBrewer)
library(knitr)
library(kableExtra)
```

```{r setup_paths, include=FALSE}
# Définition des chemins, fichiers inclus dans le dossiers "donnees_script_R"
data_path <- "fichiers csv"
geo_path_admin <- "*/gadm_regroup.gpkg"
geo_path_ocean <- "*/mer_caraib.gpkg"
geo_path_ports <- "*centroide_ports.gpkg"
zone <- "*/sortie_de_zone.geojson"

# Chargement des données géographiques
admin_boundaries <- st_read(geo_path_admin, quiet = TRUE)
ocean <- st_read(geo_path_ocean, quiet = TRUE)
centroide_ports <- st_read(geo_path_ports, quiet = TRUE)
sortiezone <- st_read(zone, quiet = TRUE)

couleurs_clusters <- c("#1b7837", "#92c5de", "#d95f0e", "#fec44f", "#c994c7")

# Types de navires
types_navires <- c("bulk", "cont", "crui", "gcar", "iisl", "serv", "tank")
```

```{r functions, include=FALSE}
# Fonction pour analyser le fichier
analyser_fichier <- function(nom_fichier) {
  
  # Chargement des fichiers csv 
  chemin_fichier <- file.path(data_path, paste0(nom_fichier, ".csv"))
  
  if (!file.exists(chemin_fichier)) {
    cat("Fichier non trouvé:", chemin_fichier, "\n")
    return(NULL)
  }
  
  cah_indicateurs <- read.csv(chemin_fichier)
  
  # Sélection des variables de centralité
  donnees_brutes <- cah_indicateurs %>% 
    select(degree, betweenness, closeness, eigenvector, pagerank)
  
  # Matrice de corrélation
  matrice_corr <- cor(donnees_brutes)
  
  # Standardisation des variables avec noms des ports
  donnees_cah <- cah_indicateurs %>% 
    select(nbport, degree, betweenness, closeness, eigenvector, pagerank) %>% 
    column_to_rownames("nbport") %>%
    scale() %>% 
    as.data.frame()
  
  # Calcul de la CAH
  dist_matrix <- dist(donnees_cah, method = "euclidean")
  res_cah <- hclust(dist_matrix, method = "ward.D2")
  
  # Calcul de l'inertie totale
  inertie_totale <- sum(apply(donnees_cah, 2, var) * (nrow(donnees_cah) - 1))
  
  # Fonction pour calculer les inerties
  calcul_inerties <- function(k) {
    clusters <- cutree(res_cah, k)
    
    inertie_intra <- sum(
      tapply(1:nrow(donnees_cah), clusters, function(idx) {
        cluster_data <- donnees_cah[idx, , drop = FALSE]
        if(nrow(cluster_data) > 1) {
          sum(apply(cluster_data, 2, var) * (nrow(cluster_data) - 1))
        } else {
          0
        }
      })
    )
    
    inertie_inter <- inertie_totale - inertie_intra
    
    data.frame(
      k = k,
      inertie_inter = inertie_inter,
      inertie_intra = inertie_intra,
      ratio_inter = inertie_inter / inertie_totale
    )
  }
  
  # Calcul des inerties pour k = 2 à 10
  inerties <- lapply(2:10, calcul_inerties) %>% bind_rows()
  inerties$gain_inertie <- c(NA, diff(inerties$inertie_inter))
  
  # Détermination du nombre optimal de clusters
  diff_gain <- diff(inerties$gain_inertie[-1])
  k_optimal <- which.max(abs(diff_gain)) + 2
  
  if (is.na(k_optimal) || k_optimal < 2) {
    k_optimal <- 4
  }
  
  # Attribution des clusters
  clusters_final <- cutree(res_cah, k = k_optimal)
  names(clusters_final) <- rownames(donnees_cah)
  donnees_cah$cluster <- factor(clusters_final)
  
  # Ajout des clusters aux données originales
  data_cluster <- cah_indicateurs %>%
    mutate(cluster = factor(clusters_final))
  
  # ACP
  res_pca <- PCA(donnees_cah %>% select(-cluster), graph = FALSE)
  
  # Ports principaux pour l'étiquetage
  ports_principaux <- cah_indicateurs %>%
    arrange(desc(degree)) %>%
    slice_head(n = 15) %>%
    pull(nbport)
  
  return(list(
    nom_fichier = nom_fichier,
    donnees = cah_indicateurs,
    donnees_cah = donnees_cah,
    data_cluster = data_cluster,
    matrice_corr = matrice_corr,
    res_cah = res_cah,
    res_pca = res_pca,
    inerties = inerties,
    k_optimal = k_optimal,
    clusters_final = clusters_final,
    ports_principaux = ports_principaux
  ))
}

creer_carte <- function(resultats) {
  
  # Préparation des données pour la carte
  ports_avec_clusters <- resultats$data_cluster %>%
    select(nbport, cluster) %>%
    mutate(id = as.character(nbport))
  
  centroide_ports_clean <- centroide_ports %>%
    mutate(id = as.character(id))
  
  # Jointure avec les données géographiques
  ports_geo <- centroide_ports_clean %>%
    left_join(ports_avec_clusters, by = "id") %>%
    filter(!is.na(cluster))
  
  # Définition des tailles des points
  tailles_clusters <- c(0.5, 0.4, 0.3, 0.2, 0.1)
  
  # Ajout de la taille selon le cluster par le vecteur
  ports_geo <- ports_geo %>%
    mutate(taille = tailles_clusters[as.numeric(cluster)])
  
  # Créer un objet spatial pour définir l'étendue
  bbox_ports <- st_bbox(ports_geo)
  
  # Créer un polygone d'étendue
  extent_poly <- st_as_sfc(bbox_ports) %>%
    st_buffer(dist = 0.5) 
  
  mf_init(x = extent_poly)
  
  
  # Fond admin - limites
  mf_map(admin_boundaries, 
         col = "lightgray", 
         border = "white", 
         lwd = 0.5,
         lty = 2,
         add = TRUE)
  
  # Océan
  mf_map(ocean, 
         col = "#E6F3FF", 
         border = NA, 
         add = TRUE)
  
  
  # Points des ports avec taille proportionnelle ET couleur par cluster
  mf_map(ports_geo, 
         var = c("taille", "cluster"),
         type = "prop_typo",
         inches = 0.15,  # Taille réduite
         pal = couleurs_clusters[1:resultats$k_optimal],
         border = "white",
         lwd = 0.3,
         leg_pos = NA,
         add = TRUE)
  
    mf_map(sortiezone, 
         col = "red",
         border = NA,
         lwd = 0.5, 
         add = TRUE)
    
  mf_title(paste("Carte des clusters -", resultats$nom_fichier), 
           pos = "left", 
           tab = TRUE, 
           cex = 1.2)
  
  # Légende en haut à gauche 
  mf_legend(type = "typo", 
            pos = "topleft",
            val = paste("Cluster", 1:resultats$k_optimal),
            pal = couleurs_clusters[1:resultats$k_optimal],
            title = "Clusters",
            title_cex = 0.9,
            val_cex = 0.7,
            frame = TRUE,
            bg = "white")
  
  # Flèche du nord en haut à droite
  mf_arrow(pos = "topright")
  
  # Échelle en bas à gauche
  mf_scale(size = 500,  
           pos = "bottomleft")
  
  # Crédits en bas à droite
  credits_text <- paste(
    "Sources : Flanders Marine Institute (2021). Global Oceans and Seas, version 1.",
    "Administrative area GADM (2021), Port calls dataset (Iphar et al, 2023)",
    sep = "\n"
  )
  
  mf_credits(credits_text,
             pos = "bottomleft",
             cex = 0.6,
             col = "grey40")
  
  return(invisible(NULL))
}

generer_analyse <- function(nom_fichier) {
  
  resultats <- analyser_fichier(nom_fichier)
  
  if (is.null(resultats)) {
    cat("Erreur lors du traitement du fichier", nom_fichier, "\n\n")
    return(NULL)
  }
  
  cat("### Matrice de corrélation {.tabset .tabset-fade}\n\n")
  
  # Graphe de corrélation
  corrplot(resultats$matrice_corr, 
           method = "color",
           type = "upper",
           order = "hclust",
           tl.cex = 0.8,
           tl.col = "black",
           tl.srt = 45,
           addCoef.col = "black",
           number.cex = 0.7,
           title = paste("Matrice de corrélation -", nom_fichier),
           mar = c(0,0,1,0))
  
  cat("\n\n### Dendrogramme\n\n")
  
  # Dendrogramme
  dendro <- fviz_dend(resultats$res_cah, k = resultats$k_optimal, 
                      cex = 0.5,
                      k_colors = couleurs_clusters[1:resultats$k_optimal],
                      color_labels_by_k = TRUE,
                      rect = TRUE,
                      main = paste("Dendrogramme -", nom_fichier),
                      labels_track_height = 0.8,
                      repel = TRUE)
  print(dendro)
  
  cat("\n\n### Analyse des inerties\n\n")
  
  # Graphiques d'inertie
  g1 <- ggplot(resultats$inerties %>% filter(!is.na(gain_inertie)), aes(x = k, y = gain_inertie)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_hline(yintercept = mean(resultats$inerties$gain_inertie[-1], na.rm = TRUE), 
               color = "red", linetype = "dashed") +
    labs(title = paste("Gain d'inertie inter-classe -", nom_fichier),
         x = "Nombre de clusters",
         y = "Δ Inertie inter-classe") +
    theme_minimal()
  
  g2 <- ggplot(resultats$inerties, aes(x = k, y = ratio_inter)) +
    geom_line(color = "steelblue", size = 1.5) +
    geom_point(color = "steelblue", size = 3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = paste("Part d'inertie expliquée -", nom_fichier),
         x = "Nombre de clusters",
         y = "Ratio inertie inter/totale") +
    theme_minimal()
  
  g3 <- ggplot(resultats$inerties, aes(x = k, y = inertie_intra)) +
    geom_line(color = "firebrick", size = 1.5) +
    geom_point(color = "firebrick", size = 3) +
    labs(title = paste("Méthode du coude -", nom_fichier),
         x = "Nombre de clusters",
         y = "Inertie intra-classe") +
    theme_minimal()
  
  grid.arrange(g1, g2, g3, ncol = 2)
  
  cat("\n\n### Visualisation des clusters\n\n")
  
  # Visualisation des clusters
  p_clusters <- fviz_cluster(list(data = resultats$donnees_cah %>% select(-cluster), 
                                  cluster = resultats$clusters_final),
                             ellipse.type = "convex",
                             repel = TRUE,
                             palette = couleurs_clusters[1:resultats$k_optimal],
                             ggtheme = theme_classic(),
                             main = paste("Classification en", resultats$k_optimal, "clusters -", nom_fichier),
                             show.clust.cent = TRUE,
                             labelsize = 0,
                             pointsize = 2) +
    geom_text_repel(aes(label = ifelse(rownames(resultats$res_pca$ind$coord) %in% resultats$ports_principaux, 
                                       rownames(resultats$res_pca$ind$coord), "")),
                    size = 3,
                    max.overlaps = 20,
                    box.padding = 0.5)
  
  print(p_clusters)
  
  cat("\n\n### Analyse en Composantes Principales {.tabset .tabset-fade}\n\n")
  
  cat("#### Valeurs propres\n\n")
  
  # Graphique des valeurs propres
  scree_plot <- fviz_eig(resultats$res_pca, 
                         addlabels = TRUE,
                         title = paste("Graphique des valeurs propres -", nom_fichier),
                         xlab = "Dimensions",
                         ylab = "Pourcentage de variance expliquée") +
    geom_hline(yintercept = 100/5, 
               linetype = "dashed", color = "red", alpha = 0.7)
  
  print(scree_plot)
  
  cat("\n\n#### Projection des individus (axes 1-2)\n\n")
  
  # Projection des individus avec clusters
  p1 <- fviz_pca_ind(resultats$res_pca,
                     geom.ind = "point",
                     col.ind = resultats$donnees_cah$cluster,
                     palette = couleurs_clusters[1:resultats$k_optimal],
                     addEllipses = TRUE,
                     ellipse.type = "convex",
                     legend.title = "Clusters",
                     repel = TRUE,
                     labelsize = 0,
                     pointsize = 2,
                     title = paste("Projection des ports -", nom_fichier)) +
    geom_text_repel(aes(label = ifelse(rownames(resultats$res_pca$ind$coord) %in% resultats$ports_principaux, 
                                       rownames(resultats$res_pca$ind$coord), "")),
                    size = 3,
                    max.overlaps = 20,
                    box.padding = 0.5)
  
  print(p1)
  
  cat("\n\n#### Projection des individus (axes 2-3)\n\n")
  
  # Projection des individus sur axes 2 et 3
  p1_23 <- fviz_pca_ind(resultats$res_pca,
                        axes = c(2, 3),
                        geom.ind = "point",
                        col.ind = resultats$donnees_cah$cluster,
                        palette = couleurs_clusters[1:resultats$k_optimal],
                        addEllipses = TRUE,
                        ellipse.type = "convex",
                        legend.title = "Clusters",
                        repel = TRUE,
                        labelsize = 0,
                        pointsize = 2,
                        title = paste("Projection des ports (axes 2-3) -", nom_fichier)) +
    geom_text_repel(aes(label = ifelse(rownames(resultats$res_pca$ind$coord) %in% resultats$ports_principaux, 
                                       rownames(resultats$res_pca$ind$coord), "")),
                    size = 3,
                    max.overlaps = 20,
                    box.padding = 0.5)
  
  print(p1_23)
  
  cat("\n\n#### Contribution des variables\n\n")
  
  # Contribution des variables
  p2 <- fviz_pca_var(resultats$res_pca,
                     col.var = "contrib",
                     gradient.cols = c("lightblue", "blue", "darkblue"),
                     repel = TRUE,
                     labelsize = 4,
                     title = paste("Contribution des variables -", nom_fichier))
  print(p2)
  
  cat("\n\n#### Biplot\n\n")
  
  # Biplot
  p3 <- fviz_pca_biplot(resultats$res_pca,
                        geom.ind = "point",
                        col.ind = resultats$donnees_cah$cluster,
                        palette = couleurs_clusters[1:resultats$k_optimal],
                        addEllipses = TRUE,
                        ellipse.type = "convex",
                        repel = TRUE,
                        col.var = "black",
                        legend.title = "Clusters",
                        labelsize = 0,
                        pointsize = 2,
                        title = paste("Biplot PCA -", nom_fichier)) +
    geom_text_repel(data = data.frame(PC1 = resultats$res_pca$ind$coord[,1],
                                      PC2 = resultats$res_pca$ind$coord[,2],
                                      cluster = resultats$donnees_cah$cluster,
                                      name = rownames(resultats$res_pca$ind$coord)),
                    aes(x = PC1, y = PC2, 
                        label = ifelse(name %in% resultats$ports_principaux, name, ""),
                        color = cluster),
                    size = 3,
                    max.overlaps = 20,
                    box.padding = 0.5,
                    show.legend = FALSE)
  
  print(p3)
  
  cat("\n\n### Tableau des résultats\n\n")
  
  # Tableau final
  tableau_final <- resultats$data_cluster %>%
    select(nbport, pays, codepays, cluster) %>%
    arrange(cluster, nbport)
  
  print(kable(tableau_final, 
              caption = paste("Tableau des clusters -", nom_fichier),
              format = "html") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")))
  
  cat("\n\n### Cartographie\n\n")
  
  # Création et affichage de la carte
  creer_carte(resultats)
  
  # Sauvegarde du tableau
  write.csv(tableau_final, paste0("resultats_", nom_fichier, ".csv"), row.names = FALSE)
  
  return(resultats)
}
```

# Analyses par type de navire {.tabset .tabset-pills}

```{r analyses, echo=FALSE, results='asis'}
# Boucle pour traiter tous les types de navires
for (type in types_navires) {
  
  # Nom lisible du type de navire
  nom_type <- switch(type,
                     "bulk" = "Vraquiers",
                     "cont" = "Porte-conteneurs", 
                     "crui" = "Navires de croisière",
                     "gcar" = "General cargo",
                     "iisl" = "Navires inter-insulaires",
                     "serv" = "Navires de service",
                     "tank" = "Pétroliers")
  
  cat("\n\n## ", nom_type, " (", toupper(type), ") {.tabset .tabset-fade}\n\n")
  
  # Analyse avec USA
  cat("\n\n### Avec USA\n\n")
  nom_fichier_avec <- paste0("centralities_avec_usa_", type)
  
  cat("#### Résumé\n\n")
  cat("Analyse des centralités portuaires pour les navires de type **", nom_type, "** en incluant les ports américains.\n\n")
  
  resultats_avec <- generer_analyse(nom_fichier_avec)
  
  # Analyse sans USA  
  cat("\n\n### Sans USA\n\n")
  nom_fichier_sans <- paste0("centralities_sans_usa_", type)
  
  cat("#### Résumé\n\n")
  cat("Analyse des centralités portuaires pour les navires de type **", nom_type, "** en excluant les ports américains.\n\n")
  
  resultats_sans <- generer_analyse(nom_fichier_sans)
  
  # Comparaison (optionnel)
  if (!is.null(resultats_avec) && !is.null(resultats_sans)) {
    cat("\n\n### Comparaison Avec/Sans USA\n\n")
    
    cat("- **Nombre optimal de clusters avec USA** :", resultats_avec$k_optimal, "\n")
    cat("- **Nombre optimal de clusters sans USA** :", resultats_sans$k_optimal, "\n\n")
    
    if (length(resultats_avec$donnees$nbport) > 0 && length(resultats_sans$donnees$nbport) > 0) {
      cat("- **Nombre de ports analysés avec USA** :", nrow(resultats_avec$donnees), "\n")
      cat("- **Nombre de ports analysés sans USA** :", nrow(resultats_sans$donnees), "\n\n")
    }
  }
}
```

