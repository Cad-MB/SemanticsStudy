# output_file_typage="output_typage.csv"
output_file_evaluation="output_evaluation.csv"


# echo "sample path, correct (yes/no), result, prologTerm" > "$output_file_typage"
echo "sample path, correct (yes/no), result" > "$output_file_evaluation"

awk -F ',' 'BEGIN {OFS = ", "} NR == FNR {expected_evaluation_results[$1] = $2; next} {print $0, (expected_evaluation_results[$1] ? expected_evaluation_results[$1] : "N/A")}' expected_evaluation_results.csv output_evaluation.csv > temp_output.csv


mv temp_output.csv output_evaluation.csv


find Samples -type f -name '*.aps' | while read -r file; do
   
    # result_typage=$(./prologTerm "$file" | swipl -s typer.pl -g main_stdin 2>&1)
    # resultPrologTerm=$(./prologTerm "$file" 2>&1)

    
    # error_output_typage=$(echo "$result_typage" | grep "Erreur")
    # standard_output_typage=$(echo "$result_typage" | grep -v "Erreur")


    # if [[ $result_typage = *"void"* ]]; then
       
    #     echo "$file, yes, Bien typé., \"$resultPrologTerm\"" >> "$output_file_typage"
    # else
       
    #     echo "$file, no, Erreur de typage !!!, \"$resultPrologTerm\"" >> "$output_file_typage"
    # fi

  
    result_evaluation=$(./evaluateur "$file" 2>&1)

 
    if [[ $? -eq 0 ]]; then
       
        echo "$file, yes, $(echo "$result_evaluation" | head -n 1), $(awk -F ',' -v file_path="$file" '$1 == file_path {print $2}' expected_evaluation_results.csv)" >> "$output_file_evaluation"
    else
     
        echo "$file, no, $result_evaluation, $(awk -F ',' -v file_path="$file" '$1 == file_path {print $2}' expected_evaluation_results.csv)" >> "$output_file_evaluation"
    fi
done

awk -F ', ' 'NR==1 {print $0} NR>1 {if ($3 == $4) $2="yes"; else $2="no"; print $0}' OFS=', ' output_evaluation.csv > temp_output.csv
mv temp_output.csv output_evaluation.csv


# echo "Le résultat du typage a été enregistré dans le fichier $output_file_typage au format CSV."
echo "Le résultat de l'évaluation a été enregistré dans le fichier $output_file_evaluation au format CSV."