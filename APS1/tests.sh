#!/bin/bash

# ! Bash pour tester la sortie du prologTerm.
# ----------------------------------------

# Fonction pour imprimer en couleur
print_color() {
    echo -e "\e[$1m$2\e[0m"
}

# Nom du fichier de sortie CSV
output_file_prologTerm="Sortie-prologTerm.csv"
output_file_typeur="verification-typage.csv"

# Effacer le contenu du fichier de sortie s'il existe déjà et écrire l'en-tête CSV
echo "sample path, correct (yes/no), result" > "$output_file_prologTerm"
echo "sample path, correct (yes/no), result" > "$output_file_typeur"

# Itérer sur tous les fichiers .aps dans le répertoire Samples et ses sous-répertoires
find Samples -type f -name '*.aps' | while read -r file; do
    # Exécuter prologTerm pour le fichier en cours
    resultPrologTerm=$(./prologTerm "$file" 2>&1)
    resultTyper=$(./prologTerm "$file" | swipl -s typer.pl -g main_stdin 2>&1)

    # Extraire la sortie standard et la sortie d'erreur
    error_output=$(echo "$resultPrologTerm" | grep "Erreur")
    standard_output=$(echo "$resultPrologTerm" | grep -v "Erreur")

    # Vérifier si la sortie d'erreur contient des messages d'erreur
    if [[ ! -z "$error_output" ]]; then
        # Écrire le résultat en tant que ligne CSV avec "no" pour correct
        echo "$file, no, \"$error_output\"" >> "$output_file_prologTerm"
    else
        # Écrire le résultat en tant que ligne CSV avec "yes" pour correct
        echo "$file, yes, \"$standard_output\"" >> "$output_file_prologTerm"
    fi
    # ! Typeur
    # Vérifier si la sortie standard contient "void"
    if [[ $resultTyper = *"void"* ]]; then
        # Écrire le résultat en tant que ligne CSV avec "yes" pour correct
        echo "$file, yes, Bien typé." >> "$output_file_typeur"
    else
        # Écrire le résultat en tant que ligne CSV avec "no" pour correct
        echo "$file, no, Erreur de typage !!!" >> "$output_file_typeur"
    fi
done


echo "Le résultat a été enregistré dans le fichier $output_file_prologTerm et $output_file_typeur et au format CSV."
