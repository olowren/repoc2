# Répertoire de visualisations intégrant l'analyse des trajectoires maritimes des navires dans la zone Caraïbe en 2019 à partir des données AIS 

Ce répertoire contient un gitlab Pages pour l'accès aux Classifications Ascendantes Hiérarchiques (CAH) et Analyses en Composantes Principales (ACP) pour la cartographie en clusters des ports pour chaque type de navire. Ainsi que la visualisation sous formes de diagrammes de membrures (chord) des liens entre ports organisés géographiquement. Une visualisation des fréquences uniques et totales du nombre de navires de passage dans un port pour chaque type de navire. Enfin, une cartographie interactive issue du traitement des trajectoires pour une fouille de motifs fréquents successifs. 

Ces visualisations ont été développées par : Gabriel Campoy, étudiant au Master 1 GEOMatique et Analyse Spatiale (GEOMAS) de l'Université Grenoble Alpes. 
Ce stage a été réalisé sous la supervision de Clément Iphar, Iwan le Berre et Christophe Claramunt au sein de l'UMR 6554 LETG.

Il a bénéficié d'un financement au titre du projet [ISblue "Interdisciplinary graduate school for the blue planet"](https://www.isblue.fr/en/about-us/) sur les thématiques de recherche de durabilité des systèmes côtiers et des systèmes d'observation à long terme pour la connaissance de l'océan (thème 3 et 5).


## Organisation générale 

### Structure des fichiers

L'ensemble des fichiers est organisé comme suit :

### Dossier `public/`

Ce dossier contient toutes les données et visualisations utilisées dans le projet :

- **Fichiers HTML** (`x.html`, `x.html`, ...):  
  Contiennent les visualisations interactives.

- **Fichiers JSON** (`data*.json`) :  
  Données pour les diagrammes de membrures.

- **Fichiers GeoJSON** :  
  Données géographiques utilisées dans la visualisation Leaflet pour la fouille de motifs fréquents.

- **Fichiers CSV** :  
  Données pour la visualisation des fréquences.

---

### Dossier `script/`

Ce dossier regroupe les scripts d'automatisation, d'analyse et de génération de résultats :

- **Script R** :  
  Automatisation du rapport Markdown des analyses CAH/ACP et intégration des couches géographiques nécessaires aux cartographies.

- **Scripts Python (Pyvis)** :  
  Génération automatique des fichiers HTML de graphes pour les **5 indicateurs**, accompagnés :
  - des **fichiers HTML** produits,
  - des **fichiers PNG** représentant la répartition de la centralité par types de navires sur le réseau maritime.

-Scripts de fouille de motifs fréquents :
  - Motifs successifs,
  - Motifs avec alternatives,
  - Motifs d'association.

- **Script de production de CSV** :
  - Génération des fréquences **uniques** et **totales**.

---

Chaque fichier ou script a été conçu pour répondre à un besoin spécifique dans l’analyse ou la visualisation des données maritimes. Pour plus de détails sur l’utilisation, se référer aux commentaires du script ou à la documentation des librairies python et R utilisées. 