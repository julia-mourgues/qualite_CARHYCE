# **Mise en qualité des données CARHYCE**

Ce dépôt contient les fonctions et scripts R Markdown utilisés par l'OFB Normandie (Service Régional Connaissance) 
pour la mise en qualité des données **CARHYCE**.


Il permet d'aboutir à la création de fiches par station générées à partir des 
données brutes téléchargées sur la base de données CARHYCE (https://carhyce.eaufrance.fr/). 
Ces fiches permettent de mettre en évidence les éventuelles erreurs de 
saisie, données aberrantes et points de vigilance.

Pour l'utiliser, il faut cloner le dépot GitHub sur votre ordinateur et ouvrir le ficher .Rproj.
Ensuite, il faut installer l'ensemble des packages utilisés pour le projet en utilisant la fonction suivante du package `renv`.

```r
# Pour restaurer l'installation des packages
renv::restore()
```

## 1. Créer les tables de travail
La procédure d'export et de stockage des données CARHYCE est présentée en première 
partie du script **2_Preparation_donnees.html**.

⚠ Si les bassins étudiés ne sont pas Seine-Normandie et Loire-Bretagne, 
il faut penser à adapter la procédure.  
⚠ S'il y a plus de deux bassins, il faut compléter le script en important les 
bassins supplémentaires et ajouter le nom des tables dans la boucle permettant 
la jointure des données.

Ce script permet ensuite de construire les tables qui seront utilisées pour la 
création des fiches station. 

Pour ce faire, il est nécessaire d'avoir un fichier contenant au moins les codes 
des stations suivies dans la région.

Une fois créées, ces tables sont enregistrées dans 
le dossier **Data/clean_data/** .

## 2. Générer les fiches de mise en qualité
Pour générer les fiches de mise en qualité, il faut se rendre dans le dossier 
**fiches_qualite_stations** et faire tourner le script **script_generer_fiches.R**.

Ce script fonctionne à partir des tables créées dans la partie précédente.  

⚠ Penser à modifier le début du script où sont définis les objets contenant la région, les auteurs et la structure. 

⚠ Penser à adapter le chemin d'accès du dossier dans lequel seront générées les fiches.

Dans ces fiches, nous retrouvons un ensembles de cartes, tableaux et graphiques 
interactifs qui permettent d'identifier les problèmes présents dans les données 
brutes.

