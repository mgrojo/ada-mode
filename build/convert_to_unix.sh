# 'dos2unix' is installed in Cygwin but not Debian (it could be
# installed in Debian, but we don't need to fix what isn't broken).
for file in *.MSS *.mss *.MSM; do  awk '{ sub("\r$", ""); print }' $file > temp; mv temp $file; done
