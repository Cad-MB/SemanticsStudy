typing_perf_file="output_typing_perf.csv"
evaluation_perf_file="output_evaluation_perf.csv"

# Fonction pour mesurer les performances du processus de typage, y compris l'utilisation de la mémoire
measure_typing_performance() {
    local file_path=$1
    local output_file=$2

    local start_time=$(date +%s%3N)
    valgrind --tool=massif --massif-out-file=massif.out ./prologTerm "$file_path" | swipl -s typer.pl -g main_stdin 2>&1 >/dev/null
    local mem_peak=$(grep mem_heap_B massif.out | sed -e 's/mem_heap_B=\(.*\)/\1/' | sort -g | tail -1)
    local end_time=$(date +%s%3N)
    local elapsed_time=$((end_time - start_time))

    echo "$file_path, $elapsed_time, $mem_peak" >> "$output_file"
    rm massif.out
}

# Fonction pour mesurer les performances du processus d'évaluation, y compris l'utilisation de la mémoire
measure_evaluation_performance() {
    local file_path=$1
    local output_file=$2

    local start_time=$(date +%s%3N)
    valgrind --tool=massif --massif-out-file=massif.out ./evaluateur "$file_path" 2>&1 >/dev/null
    local mem_peak=$(grep mem_heap_B massif.out | sed -e 's/mem_heap_B=\(.*\)/\1/' | sort -g | tail -1)
    local end_time=$(date +%s%3N)
    local elapsed_time=$((end_time - start_time))

    echo "$file_path, $elapsed_time, $mem_peak" >> "$output_file"
    rm massif.out
}

# Fonction principale pour orchestrer la mesure des performances
main() {
    echo "sample path, elapsed time (ms), memory usage (KB)" > "$typing_perf_file"
    echo "sample path, elapsed time (ms), memory usage (KB)" > "$evaluation_perf_file"

    find Samples -type f -name '*.aps' | while read -r file; do
        measure_typing_performance "$file" "$typing_perf_file"
        measure_evaluation_performance "$file" "$evaluation_perf_file"
    done

    echo "Résultats des performances du typage avec utilisation de la mémoire enregistrée dans $typing_perf_file"
    echo "Résultats des performances d'évaluation avec utilisation de la mémoire enregistrée dans $evaluation_perf_file"
}

main