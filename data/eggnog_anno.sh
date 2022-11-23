cat specieslist1 | while read i;
do
echo "$i"
emapper.py --target_orthologs all --go_evidence non-electronic -m diamond --seed_ortholog_evalue 0.001 --seed_ortholog_score 60 --query-cover 20 --subject-cover 0 --override -i /home/yichun/huilab/snail/longest_protO/$i.fa -o $i.eggnog --output_dir /home/yichun/huilab/snail/eggnog --temp_dir /home/yichun/huilab/snail/eggnog_tmp --database none --cpu 40
done
