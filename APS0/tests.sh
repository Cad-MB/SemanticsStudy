#!/bin/bash

# Fonction pour imprimer en couleur
print_color() {
    echo -e "\e[$1m$2\e[0m"
}

# Nom du fichier de sortie CSV
output_file="output.csv"

# Effacer le contenu du fichier de sortie s'il existe déjà et écrire l'en-tête CSV
echo "sample path, correct (yes/no), result" > "$output_file"

# Itérer sur tous les fichiers .aps dans le répertoire Samples et ses sous-répertoires
find Samples -type f -name '*.aps' | while read -r file; do
    # Exécuter prologTerm pour le fichier en cours
    result=$(./prologTerm "$file" 2>&1)

    # Extraire la sortie standard et la sortie d'erreur
    error_output=$(echo "$result" | grep "Erreur")
    standard_output=$(echo "$result" | grep -v "Erreur")

    # Vérifier si la sortie d'erreur contient des messages d'erreur
    if [[ ! -z "$error_output" ]]; then
        # Écrire le résultat en tant que ligne CSV avec "no" pour correct
        echo "$file, no, \"$error_output\"" >> "$output_file"
    else
        # Écrire le résultat en tant que ligne CSV avec "yes" pour correct
        echo "$file, yes, \"$standard_output\"" >> "$output_file"
    fi
done

echo "Le résultat a été enregistré dans le fichier $output_file au format CSV."
