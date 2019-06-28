files=sim*.xml
for f in $files
do
bsub -J sim -W 4:00 java -jar /cluster/home/jugne/SCORE/score_with_logging.jar $f
done
