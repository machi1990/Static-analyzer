EXPLANATION DE NOS OBSERVATION, RESULTATS ET IMPLEMENTATION.
============================================================

Le dossier "tests/observation" contient quelques tests que nous avons écrit au
fur et mésur de notre implementation.

3.1.1. OBSERVATION
=================
    1) L'instruction rand(l,h) genere une suite de d'entiers entre l et h si l
    est inferieur ou égal à h. L'interpreteur nous envoie bien la liste d'entiers
    dans le cas où  l est inferieur à h. Dans le cas contraire , il nous envoie
    "bottom" signalant un problème dans l'interpretation.

    2) Le program s'arrete dans deux conditions.
        (a). Le cas trivial, l'exécution du programme est terminé en arrivant à la
        fin du programem. Dans ce cas le résultat énvoyé sera l'evaluation du
        programme dans en suivant le semantique de langage: résulat attendu.

        (b). Le deuxième cas, c'est quand on evalue l'instruction "halt".
        Dans ce cas nous aurons "bottom" comme résulat de l'interpretation.

    3) Oui, si un programme comporte une boucle infinie l'interprete peut se
    términer. Cela arrive quand nous avons reussi à trouver un point fixe existe.


3.1.2: Assertion
===============
L'implementation est faite dans le fichier interpreter.ml.

3.1.3: Enrichement
==================
Nous avons choisi d'implementer le modulo. Pour ainsi faire, Nous nous sommes
servi de la function "erem" du package Zarith. Nous avons aussi écrit des fichiers
du test préfixé par "modulo", pour ensuite tester notre implementation.

3.2. Domaine des constantes.
===========================
Cette partie constistait à ajouter une certaine dégrée de précision dans le domaine
de constante. Par example, pour une multiplication par zero envoie toujours zero.
La refinement de la division pour obetnir l'operande de gauche constiste à une
multiplication du resultat de la division et l'operande de droite suivant par une
addition de la reste de la division, etc.

3.3. Domaine des intervalles
============================
Nous avons implementé cette partie en se basant sur le module "Q" du package Zarith.
En effet cet module nous offre déjà une implementation d'entiers plus le moins/plus
infinie. (Z inclus dans Q). Et donc, nous avons representé nos bornes d'intervalle
comme un nombre dans Q. Avec cette implementation nous avons réussi à avoir les meme
résulats attendu par l'analyseur. Nous offrons l'option "-interval" pour tester
notre implementation. En terme de compairision, cette peut nous envoyer un résulat
pas assez précis vue qu'il ya des cas qui s'est revelé presque impossible de traiter.
Par exemple, la non-égalité de deux intervalles où le premier intervalle est
totalement sous-ensemble de l'autres. i.e soit i1 = [a,b] et i2 = [c,d], si c < a et
b < d, alors i1 est totalement inclus dans i2, et dans ce cas notre implementation
se comportera comme l'identité. Nous avons aussi une petite imprécision pour le module
d'intervalle surtout quand l'intervalle contient des bornes négatives etc.

3.4: Analyse des boucles.
=========================


3.5: Produit reduit
===================
Cette implementation est réalisé de sorte qu'elle soit un functeur générique.
L'on peut en ainsi fait des produit des autres domaines abstraites que le
produit de parité et intervalle dont son implementation concrete est dans le fichier
"parity_interval_reduction.ml".

PS: Nous avons du ajouter une signature "value: t -> interval_value", qui étant
donné une domaine abstraite de type t, nous créera une interval d'entier: On peut
voir cette signature comme étant un accesseur des bornes d'intervalles.

Pour tester cette implementation, nous avons ajouté l'option "-parity-interval-reduction".

 
