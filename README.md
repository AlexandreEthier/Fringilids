### PROJET FRINGILIDÉS
Présenté dans le cadre du cours *Modèles hiérarchiques et inférence bayésienne pour les sciences naturelles FOR-7044*\
Université Laval

Badet, Adrien - adrien.badet.1@ulaval.ca\
Ethier, Alexandre - alexandre.ethier.1@ulaval.ca\
Hounsovo, Bérince - berince-setchegnon-romeo.hounsouvo.1@ulaval.ca\
Poirier-Joanette , Maxence - maxence.poirier-joanette.1@ulaval.ca

--- 





### STRUCTURE
```bash
C:.
├───Data
│   └───OG
├───Documentation
│   ├───DOCU
│   └───PRES_FINALE
│       └───Presentation_final_files
├───gitignore
└───Scripts_R
```

##### Data

```bash
+---Data
|   |   .RData
|   |   .Rhistory
|   |   abond_clean.xlsx
|   |   bague_clean.xlsx
|   |
|   \---OG
|           Abondance.xlsx
|           Baguage.xlsx
```
Les fichiers `.xlsx` dans le dossier `OG` constituent les données originales, transmises par l'OOT. 

`Abondance.xlsx`

`Baguage.xlsx`

##### Documentation

```bash
+---Documentation
|   |   .DS_Store
|   |   REFERENCES
|   |
|   +---DOCU
|   |       Protocole_Baguage_Passereaux_Automne_Tadoussac_2025.pdf
|   |       Protocole_Visuel_Total_Automne_Tadoussac_2025.pdf
|   |       Questions_fringilids.pptx
|   |
|   \---PRES_FINALE
|       |   Presentation_final.html
|       |   Presentation_final.log
|       |   Presentation_final.qmd
|       |   Presentation_final.tex
|       |
|       \---Presentation_final_files
```
##### Scripts R (à changer)

```bash
\---Scripts_R
        .RData
        .Rhistory
        Bague_explo.r
        beta_binom.txt
        beta_binomial.R
        df_final.xlsx
        Explo.R
        model_jeune.txt
        Proportion_jeunes.r
```

### Les tâches à accomplir en date du 24 mars 2026 (1 mois avant la présentation)

1) Comment évoluent la population de fringilidés boréaux (4 espèces) dans le temps (1996-2025); tendances temporelles sur 30 ans de suivi
  a) Est-ce que la proportion de jeunes de l'année évolue dans le temps: On a réalisé des graphiques et des lm pour regarder les valeurs de p mais rien de plus.
  b) Comment évolue la condition corporelle des différentes classes au cours du temps: On a réalisé des graphiques pour regarder les tendances mais rien de plus

2) Qu'est-ce qui explique les irruptions des différentes espèces de fringillidés ?
  a) Est-ce que la proportion de jeunes de l'année (indice de succès reproducteur) est corrélée avec l'abondance anuelle:
  b) Est-ce que la condition corporelle est corrélée à l'abondance annuelle


#### GLOSSAIRE

- `abond`: Dataframe du jeu de données d'abondance original nettoyé, à partir d'un fichier Excel
- `bague`: Dataframe du jeu de données de baguage original nettoyé, à partir d'un fichier Excel

- `DUSA`: Données d'abondance pour le Durbec des sapins
- `TAPI`: Données d'abondance pour le Tarin des pins
- `SIFL`: Données d'abondance pour le Sizerin flammé
- `JABO`: Données d'abondance pour le Jaseur boréal

- `props_all`: Propotion de jeunes pour les quatre espèces en un seul dataframe
- `abond_joint`: Jointure de `abond` + `props_all` pour avoir l'abondance et la proportion bagué + le nombre d'oiseux bagués + condition_moyenne + sd + se
- `abond_joint`: Bague + jeu de données résumé par année des conditions
- `bague_moyenne`: Résumé par année de la condition des oiseaux avec moyenne, sd, se (à éviter)
- `bague_DUSA`: Données de baguage pour le Durbec des sapins
- `bague_TAPI`: Données de baguage pour le Tarin des pins
- `bague_SIFL`: Données de baguage pour le Sizerin flammé
- `bague_JABO`: Données de baguage pour le Jaseur boréal

- `DUSA_modif`: Données annuelles de baguages de 2007 à 2025 et le calcul d'irruption

- `abond_irruption`: Calcul pour définir les années irruptives pour chaque espèce


##### Contacts
Alexandre Terrigeol, direction OOT: <direction.oot@explosnature.ca>\
Jean-François Therrien, direction scientifique OOT: <direction.sci.oot@explosnature.ca>
   
  
 
