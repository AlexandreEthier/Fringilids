### RÉPOSITOIRE GIT *FRINGILIDS*
<u>Contacts</u>
Alexandre Terrigeol, direction OOT: <direction.oot@explosnature.ca>\
Jean-François Therrien, direction scientifique OOT: <direction.sci.oot@explosnature.ca>

- - - 
#### MARCHE À SUIVRE
*Ne pas oublier d'avoir fait le lien entre R et Git. Vous devez travailler sur le projet Fringilids.Rproj*\

*S'assurer de changer les virgules (",") en point (".") dans les fichiers Excel car R ne les reconnaît pas*

*Formatage des bases de données dans le "header" du script R - IMPORTANT de faire rouler ces lignes de code pour chaque personne*

##### Actions à réaliser dans le terminal

 Pour changer de directory, il faut utiliser la commande `cd "/path"`\
 Utiliser la fonction `ls` pour lister l'ensemble des fichiers dans le directory

 Ces étapes sont réalisées dans le repo clôné\
 Cloner le répo Git actuel dans un dossier voulu -> `git clone "https://github.com/AlexandreEthier/Fringilids"`
 
  1- Mise-à-jour de la version commune -> `git pull`\
  2- Pour commencer à suivre un fichier -> `git add Nom_du_fichier.ext`\
  3- Voir l'état du directory -> `git status`\
  4- Faire un commit -> `git commit -m "Un message de commit"`\
  5- Envoyer le commit en ligne -> `git push`

 Les modifications devraient apparaître en ligne
- - - 
#### DOCUMENTATION
Ressources oiseaux
- Pyle, P. 1997. Identification Guide to North American Birds, Part 1, Slate Creek Press, Bolinas, California, 732 p.
- Tarin des pins - TAPI (*Spinus pinus*) > [LIEN BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/pinsis/cur/introduction)
- Durbec des sapins - DUSA (*Pinicola enucleator*) > [LIEN BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/pingro/cur/introduction)
- Sizerin flammé - SIFL (*Acanthis flammea*) > [LIEN BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/redpol1/cur/introduction)
- Jaseur boréal - JABO (*Bombycilla garrulus*) > [LIEN BIRDS OF THE WORLD](https://birdsoftheworld-org.acces.bibl.ulaval.ca/bow/species/bohwax/cur/introduction)

Ressources Git
- HappyGitwithR -> Lien utile pour faire le pont entre Git et R + toutes les commandes Git
[HappyGitwithR](https://happygitwithr.com/git-commands)
- Git help -> Documentation git
[Git help](https://git-scm.com/book/en/v2/Getting-Started-Getting-Help)


### 12/02

Maxence : Tarin\
Adrien : Durbec\
Bérince : Jaseur\
Alex : Sizerin

  standardiser les abondances par l'effort d'observation
  checker abondance en fonction des classes ou proportion des classes
  checker si l'effort de baguage est constant dans le temps

  condition physique = Longueur Aile / masse # plus le chiffre est faible, meilleur est la condition et inversement

  code sur EXPLO.R

  Sizerin : Faire graphique abondance annuelle dans le même graphique










