python2 /tools/CAFE5/python_scripts/cafetutorial_prep_r8s.py -i SpeciesTree_rooted_node_labels.txt -o r8s_ctl_file.txt -s 1380750 -p 'Octopus_sinensis,Pomacea_canaliculata' -c '594'
/tools/r8s/r8s1.81/src/r8s -b -f r8s_ctl_file.txt > r8s_tmp.txt
tail -n 1 r8s_tmp.txt | cut -c 16- > r8s.ultrametric.tre
/tools/CAFE5/bin/cafe5 -i cafe.input.filter.tsv -t r8s.ultrametric.tre -o r8sr_lambda1 -c 35