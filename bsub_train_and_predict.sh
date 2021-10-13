N_TRAIN=$1 # number of med sessions to train on

PATIENTS=$(find Data -name "patient_*.rds")
SESSIONS=$(python Code/combi.py $N_TRAIN)
SMOOTH=("TRUE" "FALSE")

for patient in $PATIENTS
do
    for ses in $SESSIONS
    do
	for smooth in ${SMOOTH[*]}
	do 
	   bsub -o logs -e logs Rscript Code/train_movelet_and_predict.R $patient $ses $smooth
	 
	done
    done
done
