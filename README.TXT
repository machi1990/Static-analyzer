EXPLANATION DE NOS OBSERVATION, RESULTATS ET IMPLEMENTATION.
============================================================

Le dossier "tests/observation" contient quelques tests que nous avons écrit au
fur et mésur de notre implementation.

3.1.1. OBSERVATION
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
L'implementation est faite dans le fichier interpreter.ml.

3.1.3: Enrichement
Nous avons choisi d'implementer le modulo. Pour ainsi faire, Nous nous sommes
servi de la function "erem" du package Zarith. Nous avons aussi écrit des fichiers
du test préfixé par "modulo", pour ensuite tester notre implementation.

3.2. Domaine des constantes.
