for file in *.MSS *.mss *.MSM; do  awk '{ sub("\r$", ""); print }' $file > temp; mv temp $file; done
