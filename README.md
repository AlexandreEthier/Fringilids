### PROJET FRINGILIDÉS
Présenté dans le cadre du cours *Modèles hiérarchiques et inférence bayésienne pour les sciences naturelles FOR-7044*\
Université Laval

Badet, Adrien - adrien.badet.1@ulaval.ca\
Ethier, Alexandre - alexandre.ethier.1@ulaval.ca\
Hounsovo, Bérince - berince-setchegnon-romeo.hounsouvo.1@ulaval.ca\
Poirier-Joanette , Maxence - maxence.poirier-joanette.1@ulaval.ca

--- 

Ce projet est réalisé dans le cadre du cours *Modèles hiérarchiques et inférence bayésienne pour les sciences naturelles FOR-7044*, en partenariat avec l'Observatoire d'Oiseaux de Tadoussac (OOT).\
Il a pour but de caractériser les phénomèmes "irruptifs" chez quatre (4) espèces ciblées, soit:

- Durbec des sapins (*Pinicola enucleator*) [BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/pingro/cur/introduction)
- Tarin des pins (*Spinus pinus*) [BIRDS OF THE WOELD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/pinsis/cur/introduction)
- Sizerin flammé (*Acanthis flammea spp.*) [BIRDS OF TH WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/redpol1/cur/introduction)
- Jaseur boréal (*Bombycilla garrulus*) [BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/bohwax/cur/introduction)

Les «irruptions» sont définies par le déplacement semi-périodique d'un grand nombre d'individus d'une population en dehors de leur aire habituelle. Ce phénomène est influencé par les conditions climatiques et environnementales et dépend des processus démographiques de la population et de la disponibilité en ressources sur les sites habituels (Widick et al., 2022).\

HYPOTHÈSES

L'Observatoire d'Oiseaux de Tadoussac, fondée en 1996, est un organisme qui étudie la migration des oiseaux 





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

`abond_clean.csv`

`bague_clean.csv` : CSV des données de baguage traitées.

`DUSA.csv`
`JABO.csv`
`SIFL.csv`
`TAPI.csv`


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

##### Scripts R (à changer avec folder "new")

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

##### [`data_clean.R`]

Script R qui traite et nettoie les jeux de données originaux `abond.xlsx` et `bague.xlsx`. Une fois traitées, les données ont été sauvegardées en objet `R` et `.csv`. Voici les transformations:

- Standardisé les noms de colonnes
- Définition des classes d'âge (HY vs AHY)
- Retrait des individus dont l'âge est inconnu
- Retrait des individus capturés avant 2007 (où l'effort de baguage est devenu plus constant)

Un jeu de données par espèce a été sauvegardé en objet R (`DUSA`, `JABO`, `SIFL` et `TAPI`) pour faciliter les analyses intraspécifiques. 

##### `irruption.R`

Script R qui défini les années irruptives pour les quatre espèces cibles. Les irruptions ont été définies selon l'équation suivante (Widick et al., 2023):

$$
D_{i}t = \frac{N_{it} - P_{it}}{\sigma_{i}}
$$

où $D_{ij}$ correspond à l'écart standardisé de l'espèce i à l'année t, $N_{it}$ correspond au décompte moyen de l'espèce i à l'année t, $P_{it}$ correspond à la valeur prédite de la tendance à long-terme de l'espèce i à l'année t et $\sigma_{i}$ correspond à l'écart-type de toutes les années sans tendance pour l'espèce i.

##### `tendance.R`

Script R utilisé pour évaluer les tendances à long-terme de l'abondance de fringilidés qui transitent aux Dunes de Tadoussac. Nous avons aussi évalué la tendance de la condition physique et la démographie des espèces d'intérêts.

##### `carac_irr.R`

Script R qui permet de caractériser les années irruptives et non-irruptives. On s'intéresse entre autres à l'âge des individus bagués et inférer à la population globale. On modélise, avec `JAGS`, un modèle mixte suivant une distribution beta binomiale. 




1- Script R pour clean DF -> output csv avec le data_clean/
2- Définition irruption dans les scripts + années/
3- Tendance long-terme sur abondance, proportion jeunes et condition (+ randomisation)/
4- JAGS -> On cherche à caractériser les années d'irruption

`df_final` <- abond_irruption
`abond_joint` <- Même chose qu'`abond_irruption`





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
   
  
 
