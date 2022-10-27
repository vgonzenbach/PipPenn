# Wrapper to run training on all participants

for N_TRAIN in 1 2 3; do
	for patient_data in $(find Data -name "patient_*.rds"); do
	    for ses in $(python Code/combi.py $N_TRAIN); do
		 
			# run both hands
			for npc in 3 4 5; do
				bsub -o logs/ntrain${N_TRAIN}.log -e logs/ntrain${N_TRAIN}.log Rscript Code/train_movelet_and_predict.R --ses $ses --npc $npc $patient_data
				bsub -o logs/ntrain${N_TRAIN}.log -e logs/ntrain${N_TRAIN}.log Rscript Code/train_movelet_and_predict.R --ses $ses --npc $npc --smooth $patient_data
			done
			exit
			# run dominant Code/train_movelet_and_predict.R
			bsub -o logs/ntrain${N_TRAIN}.log -e logs/ntrain${N_TRAIN}.log Rscript Code/train_movelet_and_predict.R --ses $ses --npc 6 --smooth --dominant  $patient_data
			bsub -o logs/ntrain${N_TRAIN}.log -e logs/ntrain${N_TRAIN}.log Rscript Code/train_movelet_and_predict.R --ses $ses --npc 6 --dominant  $patient_data
	    done
	done
done