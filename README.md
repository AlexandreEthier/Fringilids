### PROJET FRINGILIDÉS
Présenté dans le cadre du cours *Modèles hiérarchiques et inférence bayésienne pour les sciences naturelles FOR-7046*\
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

Ce projet est réalisé dans le cadre du cours *Modèles hiérarchiques et inférence bayésienne pour les sciences naturelles FOR-7046*, en partenariat avec l'Observatoire d'Oiseaux de Tadoussac (OOT).\
Il a pour but de caractériser les phénomèmes "irruptifs" chez quatre (4) espèces ciblées, soit:

- Durbec des sapins (*Pinicola enucleator*) [BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/pingro/cur/introduction)
- Jaseur boréal (*Bombycilla garrulus*) [BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/bohwax/cur/introduction)
- Sizerin flammé (*Acanthis flammea spp.*) [BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/redpol1/cur/introduction)
- Tarin des pins (*Spinus pinus*) [BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/pinsis/cur/introduction)

##### QUESTIONS DE RECHERCHE

1. **Comment évoluent les populations de fringillidés boréaux (4 espèces) dans le temps (1996-2025) ; tendances temporelles sur 30 ans de suivi ?**

    a) Est-ce que la proportion de jeunes de l’année (indice de succès reproducteur) évolue dans le temps ?\
    b) Comment évolue la condition corporelle des différentes classes au cours du temps ?

2. **Qu'est-ce qui explique les irruptions des différentes espèces de fringillidés ?**

    a) Est-ce que la proportion de jeunes de l’année (indice de succès reproducteur) est corrélée au statut irruptif de l'année?\
    b) Est-ce que la condition corporelle est corrélée au statut irruptif de l'année ?


### STRUCTURE
```bash
C:.
├───Data
│   ├───Irruption
│   ├───Mod_JAGS
│   ├───OG
│   ├───Output_JAGS
│   └───Output_rand
├───Documentation
│   ├───DOCU
│   └───PRES_FINALE
│       ├───IMAGES
│       └───Presentation_final_files
├───gitignore
└───Scripts
```

##### Data

```bash
C:.
│   .RData
│   .Rhistory
│   abond_clean.csv
│   bague_clean.csv
│   DUSA.csv
│   DUSA_bague.csv
│   JABO.csv
│   JABO_bague.csv
│   SIFL.csv
│   SIFL_bague.csv
│   TAPI.csv
│   TAPI_bague.csv
│
├───Irruption
│       DUSA_irr.R
│       JABO_irr.R
│       SIFL_irr.R
│       TAPI_irr.R
│
├───Mod_JAGS
│       mod_HY_DUSA.txt
│       mod_HY_JABO.txt
│       mod_HY_SIFL.txt
│       mod_HY_TAPI.txt
│
├───OG
│       Abondance.xlsx
│       Baguage.xlsx
│
├───Output_JAGS
│       DUSA_out_jags.txt
│       JABO_out_jags.txt
│       SIFL_out_jags.txt
│       TAPI_out_jags.txt
│
└───Output_rand
        DUSA_output.R
        DUSA_output_cond.R
        DUSA_output_prop.R
        JABO_output.R
        JABO_output_cond.R
        JABO_output_prop.R
        SIFL_output.R
        SIFL_output_cond.R
        SIFL_output_prop.R
        TAPI_output.R
        TAPI_output_cond.R
        TAPI_output_prop.R
```

Voir [data_clean.R](#data-clean)

`abond_clean.csv` : .csv des données d'abondance de 30 ans, pour les quatre espèces d'intérêt. \
`bague_clean.csv` : .csv des données de baguage depuis 2007 pour les quatre espèces d'intérêt, traitées. 

`DUSA.csv` : .csv qui regroupe toutes les variables d'intérêts pour le Durbec des sapins par année (1996-2025) \
`JABO.csv` : .csv qui regroupe toutes les variables d'intérêts pour le Jaseur boréal par année (1996-2025) \
`SIFL.csv` : .csv qui regroupe toutes les variables d'intérêts pour le Sizerin flammé par année (1996-2025) \
`TAPI.csv` : .csv qui regroupe toutes les variables d'intérêts pour le Tarin des pins par année (1996-2025) 

`DUSA_bague.csv` : .csv qui regroupe les variables d'intérêts pour chaque DUSA bagué (2007-2025) \
`JABO_bague.csv` : .csv qui regroupe les variables d'intérêts pour chaque JABO bagué (2007-2025) \
`SIFL_bague.csv` : .csv qui regroupe les variables d'intérêts pour chaque SIFL bagué (2007-2025) \
`TAPI_bague.csv` : .csv qui regroupe les variables d'intérêts pour chaque TAPI bagué (2007-2025)

###### Irruption (Voir [irruption.R](#irruption))

`DUSA_irr.R` : Vecteur R binaire qui représente les années d'irruption pour le Durbec des sapins (0 = non-irruption, 1 = irruption) (2007-2025) \
`JABO_irr.R` : Vecteur R binaire qui représente les années d'irruption pour le Jaseur boréal (0 = non-irruption, 1 = irruption) (2007-2025) \
`SIFL_irr.R` : Vecteur R binaire qui représente les années d'irruption pour le Sizerin flammé (0 = non-irruption, 1 = irruption) (2007-2025) \
`TAPI_irr.R` : Vecteur R binaire qui représente les années d'irruption pour le Tarin des pins (0 = non-irruption, 1 = irruption) (2007-2025) 

###### Mod_JAGS (Voir [carac_irr.R](#carac))

`mod_HY_DUSA.txt` : Fichier `.txt` du modèle `JAGS` pour la proportion des jeunes Durbec des sapins (2007-2025)\
`mod_HY_JABO.txt` : Fichier `.txt` du modèle `JAGS` pour la proportion des jeunes Jaseur boréal (2007-2025)\
`mod_HY_SIFL.txt` : Fichier `.txt` du modèle `JAGS` pour la proportion des jeunes Sizerin flammé (2007-2025)\
`mod_HY_TAPI.txt` : Fichier `.txt` du modèle `JAGS` pour la proportion des jeunes Tarin des pins (2007-2025)

###### OG

`Abondance.xlsx`: Jeu de données de l'abondance original fourni par l'OOT
`Baguage.xlsx`: Jeu de données de baguage original fourni par l'OOT

###### Output_JAGS (Voir [carac_irr.R](#carac))

`DUSA_out_jags.txt` : Output de la simulation JAGS pour la proportion des jeunes Durbec des sapins (2007-2025)\
`JABO_out_jags.txt` : Output de la simulation JAGS pour la proportion des jeunes Jaseur boréal (2007-2025)\
`SIFL_out_jags.txt` : Output de la simulation JAGS pour la proportion des jeunes Sizerin flammé (2007-2025)\
`TAPI_out_jags.txt` : Output de la simulation JAGS pour la proportion des jeunes Tarin des pins (2007-2025)

###### Output_rand (Voir [tendance.R](#tendance))

`DUSA_output.R` : Output d'une boucle de randomisation du log de l'abondance standardisée selon l'année pour le Durbec des sapins (1996-2025) \
`JABO_output.R` : Output d'une boucle de randomisation du log de l'abondance standardisée selon l'année pour le Jaseur boréal (1996-2025) \
`SIFL_output.R` : Output d'une boucle de randomisation du log de l'abondance standardisée selon l'année pour le Sizerin flammé (1996-2025) \
`TAPI_output.R` : Output d'une boucle de randomisation du log de l'abondance standardisée selon l'année pour le Tarin des pins (1996-2025)

`DUSA_output_cond.R` : Output d'une boucle de randomisation de la condition selon le statut irruptif de l'année pour le Durbec des sapins (2007-2025) \
`JABO_output_cond.R` : Output d'une boucle de randomisation de la condition selon le statut irruptif de l'année pour le Jaseur boréal (2007-2025) \
`SIFL_output_cond.R` : Output d'une boucle de randomisation de la condition selon le statut irruptif de l'année pour le Sizerin flammé (2007-2025) \
`TAPI_output_cond.R` : Output d'une boucle de randomisation de la condition selon le statut irruptif de l'année pour le Tarin des pins (2007-2025) 

`DUSA_output_prop.R` : Output d'une boucle de randomisation de la proportion de jeunes selon l'année pour le Durbec des sapins (2007-2025) \
`JABO_output_prop.R` : Output d'une boucle de randomisation de la proportion de jeunes selon l'année pour le Jaseur boréal (2007-2025) \
`SIFL_output_prop.R` : Output d'une boucle de randomisation de la proportion de jeunes selon l'année pour le Sizerin flammé (2007-2025) \
`TAPI_output_prop.R` : Output d'une boucle de randomisation de la proportion de jeunes selon l'année pour le Tarin des pins (2007-2025)

##### Scripts

```bash
C:
    data_clean.R
    irruption.R
    tendance.R
    carac_irr.R
```
<a name="data-clean"></a>
##### `data_clean.R`

Script R qui traite et nettoie les jeux de données originaux `abond.xlsx` et `bague.xlsx`. Une fois traitées, les données ont été sauvegardées en objet `R` et `.csv` pour chaque espèce d'intérêt. Voici les transformations:

**fORMATTAGE DE DONNÉES**\
- Standardisé les noms de colonnes\
- Définition des classes d'âge (HY vs AHY)\
- Nettoyage des variables d'intérêt\
- Ajout de la condition (Ratio aile/masse)\
- Ajout du vecteur `_irr.R`    
    

<a name="irruption"></a>
##### `irruption.R`

Script R qui défini les années irruptives pour les quatre quatre espèces d'intérêt de fringilidés qui transitent aux Dunes de Tadoussac. Les irruptions ont été définies selon l'équation suivante (Widick et al., 2023):

$$
D_{it} = \frac{N_{it} - P_{it}}{\sigma_{i}}
$$

où $D_{ij}$ correspond à l'écart standardisé de l'espèce i à l'année t, $N_{it}$ correspond au décompte moyen de l'espèce i à l'année t, $P_{it}$ correspond à la valeur prédite de la tendance à long-terme de l'espèce i à l'année t et $\sigma_{i}$ correspond à l'écart-type de toutes les années sans tendance pour l'espèce i.

**DÉFINITION DE L'IRRUPTION** 
 * Calculs de la déviation standard
     *  Représentation graphique 

<a name="tendance"></a>
##### `tendance.R`

Script R utilisé pour évaluer les tendances à long-terme des quatre espèces d'intérêt de fringilidés qui transitent aux Dunes de Tadoussac.

**TENDANCE TEMPORELLE DE L'ABONDANCE**
 * Représentation graphique
   * Modèle linéaire
     * Randomisation

**TENDANCE TEMPORELLE DE LA PROPORTION DES JEUNES**
 * Représentation graphique
   * Modèle linéaire
     * Randomisation

**TENDANCE TEMPORELLE DE LA CONDITION**
 * Représentation graphique

<a name="carac"></a>
##### `carac_irr.R`

Script R qui rassemble les analyses liées à la caractérisation du statut irruptif des quatre espèces d'intérêt de fringilidés qui transitent aux Dunes de Tadoussac.

**ÉVALUATION DE LA PROPORTION DES JEUNES**
 * Approche fréquentiste\
    * Approche bayésienne avec `JAGS`
        * Modèle mixte linéaire

**ÉVALUATION DE LA CONDITION DES JEUNES**
 * Représentation graphique «boîte à moustaches»
    * Test de comparaison de moyennes Wilcox-test
        * Randomisation

##### Documentation

```bash
C:.
│   REFERENCES
│
├───DOCU
│       Protocole_Baguage_Passereaux_Automne_Tadoussac_2025.pdf
│       Protocole_Visuel_Total_Automne_Tadoussac_2025.pdf
│       Questions_fringilids.pptx
│
└───PRES_FINALE
    │   Presentation_final.html
    │   Presentation_final.log
    │   Presentation_final.qmd
    │   Presentation_final.tex
```
