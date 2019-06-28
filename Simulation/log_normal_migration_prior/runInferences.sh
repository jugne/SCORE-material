files=inf*.xml
for f in $files
do
bsub -J inf_score -W 48:00 -R "rusage[mem=8000]" java -jar /cluster/home/jugne/SCORE/score_with_logging.jar $f
done
