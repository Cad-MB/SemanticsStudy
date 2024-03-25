# APS

## Tester un fichier avec prologTerm :

make

./prologTerm Samples/prog8.aps

## Tester le typeur tout seul :

swipl typer.pl

## trace in swipl

first, give the result of ./prologTerm to swipl as:
g0(L),type_prog(L, ---sortie prolog--- , G).
if false:

    - trace.

    - g0(L),type_prog(L, ---sortie prolog--- , G). --> and press enter
