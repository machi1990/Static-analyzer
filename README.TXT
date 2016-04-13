EXPLANATION DE NOS OBSERVATION, RESULTATS ET IMPLEMENTATION.
============================================================

Le dossier "tests/observation" contient quelques tests que nous avons écrit au
fur et mésur de notre implementation.

Pour génerer le fichier résultat de tout le banc de tests exécuter le script
"run.sh". Pour ce faire, il faut d'abord vérifier que toutes dependances comme
indiquées dans le fichie INSTALL.txt soient satisfaites.

Sous terminal d'UNIX, la commande "bash run.sh -h", vous proposera une guide
d'utilisation.

Les résultats de notre analyse se trouve dans le dossir "chitimbo-larbi-results".

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
notre implementation. En terme de comparaison, cette peut nous envoyer un résulat
pas assez précis vue qu'il y a des cas qui s'est revelé presque impossible de traiter.
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

Pour pariér à cela, et ainsi avoir une convergence dans un temps raisonnable, nous
avons utilé l'operateur de "widening". Le probleme avec cet operateur, est qu'il
apporte une perte de précision et ainsi d'autres option comme "-delay N" pour
l'utilisation tardive de "widening" normallement aprés "N" iteration, et "unroll N"
pour l'evaluation de la boucle normale "sans faire de joining de widen" pour les "N"
premier iterations. Ce deuxieme nous apporte une bonne précision par rapport à delay,
vue qu'il s'agit d'une "simulation" d'evaluation la boucle dans le domaine, et ainsi
la boucle est exécuté avec les valeur précises. 

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
gagner en précision (raffiné le point fixe par un opérateur de rétréecissement permet 
de forcer la convergence en temps fini) après l'utilisation de widening. Pour cela, nous
avons ajouté l'option "-narrow N". Cette option fera n iteration de la boucle en
appliquant l'operation de narrowing à chaque tour de boucle.

Pour ce meme exemple, nous avons le résultat suivant en appliquant l'option de narrowing,
avec un unroll de 3 et delay de 0.
tests/observation/loop_3.c:7.8-17: [ x in [0;0] ]
tests/observation/loop_3.c:7.8-17: [ x in [1;1] ]
tests/observation/loop_3.c:7.8-17: [ x in [2;2] ]
tests/observation/loop_3.c:7.8-17: [ x in [3;3] ]
tests/observation/loop_3.c:7.8-17: [ x in [3;64] ]
tests/observation/loop_3.c:7.8-17: [ x in [3;64] ]
tests/observation/loop_3.c:11.4-13: [ x in [65;65] ]

NB: 
Vue quelques imprécesion remarquées lors de nos tests, cette partie est encore sous une revue
totale. Par example, à la fin d'analyse du programme "tests/observation/loop_5.c",nous avons,
toujours avec unroll 3 et narrowing 1, nous avons en sortie de la boucle la valeur de x dans
[0;47]. Cette résulat est assez imprécis comparant au vrai résulat qui est [0;0]. Nous estimons 
que cette imprecision  peut être lié à notre application de narrowing.   
	

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

4: Extensions (Analyse disjonctive).
===================================
Comme extension pour notre analyseur nous avons choisi d'implementer l'anaylse disjonctive par 
partitionement de trace. Cette implementation se porte comme une autre module pour l'intepretation. 
Toute details d'implementation se trouve dans le fichier "trace_interpreter.ml", et nous avons ajouté 
une option "-partition-interval" et "partition-constant" pour tester cette implementation pour 
les domaine d'intervalle et constantes respectivement.


