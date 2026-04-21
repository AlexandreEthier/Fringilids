### PROJET FRINGILIDÉS
Présenté dans le cadre du cours *Modèles hiérarchiques et inférence bayésienne pour les sciences naturelles FOR-7044*\
Université Laval\
Avril 2026

###### CONTACTS
Badet, Adrien - adrien.badet.1@ulaval.ca\
Ethier, Alexandre - alexandre.ethier.1@ulaval.ca\
Hounsovo, Bérince - berince-setchegnon-romeo.hounsouvo.1@ulaval.ca\
Poirier-Joanette , Maxence - maxence.poirier-joanette.1@ulaval.ca

Alexandre Terrigeol, direction OOT: <direction.oot@explosnature.ca>\
Jean-François Therrien, direction scientifique OOT: <direction.sci.oot@explosnature.ca>

--- 

Ce projet est réalisé dans le cadre du cours *Modèles hiérarchiques et inférence bayésienne pour les sciences naturelles FOR-7044*, en partenariat avec l'Observatoire d'Oiseaux de Tadoussac (OOT).\
Il a pour but de caractériser les phénomèmes "irruptifs" chez quatre (4) espèces ciblées, soit:

- Durbec des sapins (*Pinicola enucleator*) [BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/pingro/cur/introduction)
- Tarin des pins (*Spinus pinus*) [BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/pinsis/cur/introduction)
- Sizerin flammé (*Acanthis flammea spp.*) [BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/redpol1/cur/introduction)
- Jaseur boréal (*Bombycilla garrulus*) [BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/bohwax/cur/introduction)

##### QUESTIONS DE RECHERCHE

1. **Comment évoluent les populations de fringillidés boréaux (4 espèces) dans le temps (1996-2025) ; tendances temporelles sur 30 ans de suivi ?**

    a) Est-ce que la proportion de jeunes de l’année (indice de succès reproducteur) évolue dans le temps ?\
    b) Comment évolue la condition corporelle des différentes classes au cours du temps ?

2. **Qu'est-ce qui explique les irruptions des différentes espèces de fringillidés ?**

    a) Est-ce que la proportion de jeunes de l’année (indice de succès reproducteur) est corrélée avec l’abondance annuelle?\
    b) Est-ce que la condition corporelle est corrélée à l'abondance annuelle ?


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
Les fichiers `Abondance.xlsx` et `Baguage.xlsx` dans le dossier `OG` constituent les données originales, transmises par l'OOT.

Voir [data_clean.R](#data-clean)

`abond_clean.csv` : .csv des données d'abondances de 30 ans, pour les quatre espèces d'intétrêt. \
`bague_clean.csv` : .csv des données de baguage depuis 2007 pour les quatre espèces d'intérêt, traitées. 

`DUSA.csv` : .csv qui regroupe toutes les variables d'intérêt pour le Durbec des sapins (1996-2025) \
`JABO.csv` : .csv qui regroupe toutes les variables d'intérêt pour le Jaseur boréal (1996-2025) \
`SIFL.csv` : .csv qui regroupe toutes les variables d'intérêt pour le Sizerin flammé (1996-2025) \
`TAPI.csv` : .csv qui regroupe toutes les variables d'intérêt pour le Tarin des pins (1996-2025) 

Voir [irruption.R](#irruption)

`DUSA_irr.R` : Vecteur R binaire qui représente les années d'irruption pour le Durbec des sapins (0 = non-irruption, 1 = irruption) (2007-2025) \
`JABO_irr.R` : Vecteur R binaire qui représente les années d'irruption pour le Jaseur boréal (0 = non-irruption, 1 = irruption) (2007-2025) \
`SIFL_irr.R` : Vecteur R binaire qui représente les années d'irruption pour le Sizerin flammé (0 = non-irruption, 1 = irruption) (2007-2025) \
`TAPI_irr.R` : Vecteur R binaire qui représente les années d'irruption pour le Tarin des pins (0 = non-irruption, 1 = irruption) (2007-2025) 

Voir [tendance.R](#tendance)

`DUSA_output.R` : Matrice R de l'output d'une boucle de randomisation du log de l'abondance standardisée selon l'année pour le Durbec des sapins (1996-2025) \
`JABO_output.R` : Matrice R de l'output d'une boucle de randomisation du log de l'abondance standardisée selon l'année pour le Jaseur boréal (1996-2025) \
`SIFL_output.R` : Matrice R de l'output d'une boucle de randomisation du log de l'abondance standardisée selon l'année pour le Sizerin flammé (1996-2025) \
`TAPI_output.R` : Matrice R de l'output d'une boucle de randomisation du log de l'abondance standardisée selon l'année pour le Tarin des pins (1996-2025) 

Voir [carac_irr.R](#carac)

MODEL_jags.txt pour chaque espèce

`mod_HY_DUSA.txt` : Fichier `.txt` du modèle `JAGS` pour la proportion des jeunes Durbec des sapins (2007-2025)\
`mod_HY_JABO.txt` : Fichier `.txt` du modèle `JAGS` pour la proportion des jeunes Jaseur boréal (2007-2025)\
`mod_HY_SIFL.txt` : Fichier `.txt` du modèle `JAGS` pour la proportion des jeunes Sizerin flammé (2007-2025)\
`mod_HY_TAPI.txt` : Fichier `.txt` du modèle `JAGS` pour la proportion des jeunes Tarin des pins (2007-2025)

`DUSA_out_jags.R` : Output de la simulation JAGS pour la proportion des jeunes Durbec des sapins (2007-2025)\
`JABO_out_jags.R` : Output de la simulation JAGS pour la proportion des jeunes Jaseur boréal (2007-2025)\
`SIFL_out_jags.R` : Output de la simulation JAGS pour la proportion des jeunes Sizerin flammé (2007-2025)\
`TAPI_out_jags.R` : Output de la simulation JAGS pour la proportion des jeunes Tarin des pins (2007-2025)\



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

<a name="irruption"></a>
##### `irruption.R`

Script R qui défini les années irruptives pour les quatre espèces cibles. Les irruptions ont été définies selon l'équation suivante (Widick et al., 2023):

$$
D_{it} = \frac{N_{it} - P_{it}}{\sigma_{i}}
$$

où $D_{ij}$ correspond à l'écart standardisé de l'espèce i à l'année t, $N_{it}$ correspond au décompte moyen de l'espèce i à l'année t, $P_{it}$ correspond à la valeur prédite de la tendance à long-terme de l'espèce i à l'année t et $\sigma_{i}$ correspond à l'écart-type de toutes les années sans tendance pour l'espèce i.

Output d'un objet R pour chaque espèce : Vecteur binaire où 1 est `TRUE` pour une irruption

<a name="tendance"></a>
##### `tendance.R`

Script R utilisé pour évaluer les tendances à long-terme de l'abondance de fringilidés qui transitent aux Dunes de Tadoussac. Nous avons aussi évalué la tendance de la condition physique et la démographie des espèces d'intérêts.

OUTPUT DE LA RANDOMISATION (Voir data.R)


<a name="carac"></a>
##### `carac_irr.R`

Script R qui permet de caractériser les années irruptives et non-irruptives. On s'intéresse entre autres à l'âge des individus bagués et inférer à la population globale. On modélise, avec `JAGS`, un modèle mixte suivant une distribution beta binomiale. 





###### Équation JAGS proportion


$log(\mu_{abondance_i}) = \alpha_{annee} + X_i\beta_{irruption} + \epsilon_i$

$\beta_{irruption} \sim N(0, 0.1)$

$\sigma_{irruption} \sim unif(0, 100)$

$\alpha_{{annee}} \sim N(\sigma_{annee}, \tau_{annee})$

$\sigma_{annee} \sim unif(0, 0.1)$

$\tau_{annee} = \frac{1} {\sigma^2_{annee}}$










   
  
 
