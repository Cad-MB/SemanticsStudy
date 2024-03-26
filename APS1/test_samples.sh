output_file_typage="output_typage.csv"
# Nom du fichier de sortie CSV pour evaluation
output_file_evaluation="output_evaluation.csv"

# Effacer le contenu du fichier de sortie s'il existe déjà et écrire l'en-tête CSV
echo "sample path, correct (yes/no), result, typage" > "$output_file_typage"
echo "sample path, correct (yes/no), result" > "$output_file_evaluation"

# Fusionner les fichiers expected_evaluation_results.csv et output_evaluation.csv basés sur la colonne 'path'
# Utiliser awk pour joindre les fichiers sur la colonne 'path'
# Utiliser left join pour garder toutes les entrées de output_evaluation.csv
# même s'il n'y a pas de correspondance dans expected_evaluation_results.csv
awk -F ',' 'BEGIN {OFS = ", "} NR == FNR {expected_evaluation_results[$1] = $2; next} {print $0, (expected_evaluation_results[$1] ? expected_evaluation_results[$1] : "N/A")}' expected_evaluation_results.csv output_evaluation.csv > temp_output.csv

# Renommer temp_output.csv en output_evaluation.csv
mv temp_output.csv output_evaluation.csv

# Itérer sur tous les fichiers .aps dans le répertoire Samples et ses sous-répertoires
find Samples -type f -name '*.aps' | while read -r file; do
    # Exécuter prologTerm pour le fichier en cours
    result_typage=$(./prologTerm "$file" | swipl -s typer.pl -g main_stdin 2>&1)
    resultPrologTerm=$(./prologTerm "$file" 2>&1)

    # Extraire la sortie standard et la sortie d'erreur
    error_output_typage=$(echo "$result_typage" | grep "Erreur")
    standard_output_typage=$(echo "$result_typage" | grep -v "Erreur")

    # Vérifier si la sortie standard contient "void"
    if [[ $result_typage = *"void"* ]]; then
        # Écrire le résultat en tant que ligne CSV avec "yes" pour correct
        echo "$file, yes, Bien typé., \"$resultPrologTerm\"" >> "$output_file_typage"
    else
        # Écrire le résultat en tant que ligne CSV avec "no" pour correct
        echo "$file, no, Erreur de typage !!!, \"$resultPrologTerm\"" >> "$output_file_typage"
    fi

    # Exécuter evaluateur pour le fichier en cours
    result_evaluation=$(./evaluateur "$file" 2>&1)

    # Vérifier s'il y a une erreur lors de l'exécution de l'évaluateur
    if [[ $? -eq 0 ]]; then
        # Pas d'erreur, écrire le résultat de l'évaluateur en tant que ligne CSV
        echo "$file, yes, $(echo "$result_evaluation" | head -n 1), $(awk -F ',' -v file_path="$file" '$1 == file_path {print $2}' expected_evaluation_results.csv)" >> "$output_file_evaluation"
    else
        # Erreur lors de l'exécution, écrire l'erreur dans la troisième colonne
        echo "$file, no, $result_evaluation, $(awk -F ',' -v file_path="$file" '$1 == file_path {print $2}' expected_evaluation_results.csv)" >> "$output_file_evaluation"
    fi
done


# Check if the result and expected columns match and update the correct column accordingly
awk -F ', ' 'NR==1 {print $0} NR>1 {if ($3 == $4) $2="yes"; else $2="no"; print $0}' OFS=', ' output_evaluation.csv > temp_output.csv
mv temp_output.csv output_evaluation.csv


echo "Le résultat du typage a été enregistré dans le fichier $output_file_typage au format CSV."
echo "Le résultat de l'évaluation a été enregistré dans le fichier $output_file_evaluation au format CSV."