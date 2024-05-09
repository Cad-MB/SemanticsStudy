## Installation

Suivez les étapes ci-dessous pour installer le projet :

    git clone https://stl.algo-prog.info/21319982/APS-tme
    cd APS-tme/
    cd APSx/
    make

## Scripts et Exécution

Utilisez les commandes suivantes pour exécuter divers scripts et tâches :

### Exécuter le script `test_samples.sh`

    bash test_samples.sh

les résultats seront sauvegardés dans 2 fichiers CSV, output_typage.csv et output_evaluation.csv 

### Afficher la requête Prolog avec `prologTerm`

    ./prologTerm Samples/Sample.aps

### Vérifier le type avec `typer` (swipl)

    swipl typer.pl
    ?- trace.
    [trace] ?- g0(L),type_prog(L, ---sortie prolog--- , G).

### Évaluer avec `evaluateur`

    ./evaluateur Samples/Sample.aps
