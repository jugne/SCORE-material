files=h3n2*.xml
for f in $files
do
bsub -J inf -n 6 -W 200:00 -R "rusage[mem=8000]"  java -Xmx42g -jar /cluster/home/jugne/SCORE/score_with_logging.jar $f
done
