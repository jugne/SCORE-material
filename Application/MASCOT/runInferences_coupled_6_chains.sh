files=h3n2*.xml
for f in $files
do
bsub -J mascot_150 -n 6 -W 200:00 -R "rusage[mem=8000]"  java -Xmx42g -jar mascot.jar $f
done
