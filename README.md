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
Les fichiers `.xlsx` dans le dossier `OG` constituent les données originales, transmises par l'OOT. Voir [data_clean.R](#data-clean)

`abond_clean.csv` : csv des données d'abondances de 30 ans, pour les 4 espèces.

`bague_clean.csv` : csv des données de baguage traitées.

`DUSA.csv` : 
`JABO.csv`
`SIFL.csv`
`TAPI.csv`

`DUSA_irr.R`
`JABO_irr.R`
`SIFL_irr.R`
`TAPI_irr.R`

`DUSA_output.R`
`JABO_output.R`
`SIFL_output.R`
`TAPI_output.R`


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
<a name="data-clean"></a>
##### `data_clean.R`

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

Output d'un objet R pour chaque espèce : Vecteur binaire où 1 est `TRUE` pour une irruption


##### `tendance.R`

Script R utilisé pour évaluer les tendances à long-terme de l'abondance de fringilidés qui transitent aux Dunes de Tadoussac. Nous avons aussi évalué la tendance de la condition physique et la démographie des espèces d'intérêts.

OUTPUT DE LA RANDOMISATION (Voir data.R)

##### `carac_irr.R`

Script R qui permet de caractériser les années irruptives et non-irruptives. On s'intéresse entre autres à l'âge des individus bagués et inférer à la population globale. On modélise, avec `JAGS`, un modèle mixte suivant une distribution beta binomiale. 




1- Script R pour clean DF -> output csv avec le data_clean/
2- Définition irruption dans les scripts + années/
3- Tendance long-terme sur abondance, proportion jeunes et condition (+ randomisation)/
4- JAGS -> On cherche à caractériser les années d'irruption

À checker (15/04/2026):

- Condition des espèces (Explo.R)
- Checker si, pour tendance temporelle, le lm est le LOG ou LOG + 1 de l'abondance standardisée....
- Sauvegarde output de la randomisation en objet.R?



##### Contacts
Alexandre Terrigeol, direction OOT: <direction.oot@explosnature.ca>\
Jean-François Therrien, direction scientifique OOT: <direction.sci.oot@explosnature.ca>
   
  
 
