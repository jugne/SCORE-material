files=inf*.xml
for f in $files
do
bsub -J inf_fixed_net -W 4:00 -R "rusage[mem=8000]" java -jar ~/SCORE/score_with_logging.jar $f
done
