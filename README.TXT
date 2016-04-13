EXPLANATION DE NOS OBSERVATION, RESULTATS ET IMPLEMENTATION.
============================================================

Le dossier "tests/observation" contient quelques tests que nous avons écrit au
fur et à mesure de notre implementation.

Pour génerer le fichier résultat de tout le banc de tests, il faut exécuter le script
"run.sh". Pour ce faire, il faut d'abord vérifier que toutes les dependances spécifiées dans le fichier INSTALL.txt soient satisfaites.

Sous terminal d'UNIX, la commande "bash run.sh -h", vous proposera une guide
d'utilisation.

Les résultats de notre analyse se trouve dans le dossier "chitimbo-larbi-results".

3.1.1. OBSERVATION
=================
    1) L'instruction rand(l,h) génère une suite d'entiers entre l et h si l
    est inferieur ou égal à h. L'interpreteur nous envoie bien la liste d'entiers
    dans le cas où  l est inferieur à h. Dans le cas contraire , il nous envoie
    "bottom" signalant un problème dans l'interpretation.

    2) Le programme s'arrête sous deux conditions :
        (a). Le cas trivial, l'exécution du programme est terminé en arrivant à la
        fin du programme. Dans ce cas, le résultat envoyé sera l'évaluation du
        programme en suivant la sémantique du langage: le résulat attendu.

        (b). Le second cas, càd lorsqu'on evalue l'instruction "halt".
        Dans ce cas nous aurons "bottom" comme résulat de l'interpretation.

    3) Oui, si un programme comporte une boucle infinie l'interprète peut se
    términer. Cela arrive quand nous avons reussi à trouver qu'un point fixe existe.


3.1.2: Assertion
===============
L'implementation est faite dans le fichier interpreter.ml.

3.1.3: Enrichement
==================
Nous avons choisi d'implementer le modulo. Pour ce faire, Nous nous sommes
servi de la function "erem" du package Zarith. Nous avons aussi écrit des fichiers
du test préfixé par "modulo", pour ensuite tester notre implementation.

3.2. Domaine des constantes.
===========================
Cette partie constiste à ajouter un certain degré de précision dans le domaine
des constantes. Par example, renvoyer toujours zero lors d'une multiplication par zero.
La raffinement de la division pour obetnir l'operande de gauche constiste en une
multiplication du resultat de la division et l'operande de droite par une
addition du reste de la division, etc.

3.3. Domaine des intervalles
============================
Nous avons implementé cette partie en se basant sur le module "Q" du package Zarith.
En effet ce module nous offre déjà une implementation d'entiers plus le moins/plus
infinie. (Z inclus dans Q). Et donc, nous avons representé nos bornes d'intervalle
comme un nombre dans Q. Avec cette implémentation nous avons réussi à avoir les mêmes
résulats attendus par l'analyseur. Nous offrons l'option "-interval" pour tester
notre implementation. En terme de comparaison, cette analyse peut nous envoyer un résulat
pas assez précis vu qu'il y a des cas qui se sont revelés presque impossible à traiter.
Par exemple, la non-égalité de deux intervalles où le premier intervalle est
totalement un sous-ensemble de l'autre i.e soit i1 = [a,b] et i2 = [c,d], si c < a et
b < d, alors i1 est totalement inclus dans i2, et dans ce cas notre implementation
se comportera comme l'identité. Nous avons aussi une petite imprécision pour le module
d'intervalle surtout quand l'intervalle contient des bornes négatives.

3.4: Analyse des boucles.
=========================
Le traitement des boucles dans intepreter.ml tel qu'il était, risquait de prendre
beaucoup du temps avant qu'on trouve un point fixe: Un risque d'une convergence
assez précis mais pas assez rapide.

Pour palier à cela, et ainsi avoir une convergence dans un temps raisonnable, nous
avons utilisé l'operateur de "widening". Le problème avec cet operateur, est qu'il
apporte une perte de précision et ainsi d'autres option comme "-delay N" pour
l'utilisation tardive de "widening" normalement aprés "N" iteration, et "unroll N"
pour l'evaluation de la boucle normale "sans faire de joining de widen" pour les "N"
premier iterations. Ce dernier nous apporte une bonne précision par rapport à delay,
vu qu'il s'agit d'une "simulation" d'evaluation de la boucle dans le domaine, et ainsi
la boucle est exécutée avec les valeur précises. 

Par example, pour le fichier "tests/observation/loop_3.c", nous avons pour unroll=3
et delay= 0 le résultat suivant:

tests/observation/loop_3.c:7.8-17: [ x in [0;0] ]
tests/observation/loop_3.c:7.8-17: [ x in [1;1] ]
tests/observation/loop_3.c:7.8-17: [ x in [2;2] ]
tests/observation/loop_3.c:7.8-17: [ x in [3;3] ]
tests/observation/loop_3.c:7.8-17: [ x in [3;64] ]
tests/observation/loop_3.c:11.4-13: [ x in [65;+inf] ]

Par contre pour delay=3 et unroll=0, nous avons:
tests/observation/loop_3.c:7.8-17: [ x in [0;0] ]
tests/observation/loop_3.c:7.8-17: [ x in [1;1] ]
tests/observation/loop_3.c:7.8-17: [ x in [1;2] ]
tests/observation/loop_3.c:7.8-17: [ x in [1;3] ]
tests/observation/loop_3.c:7.8-17: [ x in [1;4] ]
tests/observation/loop_3.c:7.8-17: [ x in [1;64] ]
tests/observation/loop_3.c:11.4-13: [ x in [65;+inf] ]

Dans cette partie aussi, il s'agissait d'implementer le narrowing de la boucle pour
gagner en précision (raffiner le point fixe par un opérateur de rétrécissement permet 
de forcer la convergence en temps fini) après l'utilisation de widening. Pour cela, nous
avons ajouté l'option "-narrow N". Cette option fera n iteration de la boucle en
appliquant l'operation de narrowing à chaque tour de boucle.

Pour ce même exemple, nous avons le résultat suivant en appliquant l'option de narrowing,
avec un unroll de 3 et delay de 0.
tests/observation/loop_3.c:7.8-17: [ x in [0;0] ]
tests/observation/loop_3.c:7.8-17: [ x in [1;1] ]
tests/observation/loop_3.c:7.8-17: [ x in [2;2] ]
tests/observation/loop_3.c:7.8-17: [ x in [3;3] ]
tests/observation/loop_3.c:7.8-17: [ x in [3;64] ]
tests/observation/loop_3.c:7.8-17: [ x in [3;64] ]
tests/observation/loop_3.c:11.4-13: [ x in [65;65] ]

NB: 
Etant donné quelques imprécisions remarquées lors de nos tests, cette partie est encore sous une revue
totale. Par example, à la fin d'analyse du programme "tests/observation/loop_5.c",nous avons,
toujours avec unroll 3 et narrowing 1, nous avons en sortie de la boucle la valeur de x dans
[0;47]. Ce résulat est assez imprécis comparé au vrai résulat qui est [0;0]. Nous estimons 
que cette imprecision peut être lié à notre application de narrowing.   
	

3.5: Produit reduit
===================
Cette implementation est réalisée de sorte qu'elle soit un foncteur générique.
On peut ainsi faire des produits des autres domaines abstraits tels que le
produit de parité et intervalle. Son implémentation concrète se trouve dans le fichier
"parity_interval_reduction.ml".

PS: Nous avons du ajouter une signature "value: t -> interval_value", qui étant
donné une domaine abstraite de type t, nous créera une intervalle d'entiers: On peut
voir cette signature comme étant un accesseur des bornes d'intervalles.

Pour tester cette implémentation, nous avons ajouté l'option "-parity-interval-reduction".

4: Extensions (Analyse disjonctive).
===================================
Comme extension pour notre analyseur nous avons choisi d'implémenter l'anaylse disjonctive par 
partitionnement de trace. Cette implémentation se comporte comme un autre module pour l'inteprétation. 
Tout les details de l'implémentation se trouvent dans le fichier "trace_interpreter.ml", et nous avons ajouté 
une option "-partition-interval" et "partition-constant" pour tester cette implémentation pour 
les domaine d'intervalles et constantes respectivement.


