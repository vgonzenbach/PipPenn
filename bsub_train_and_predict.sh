# Wrapper to run training on all participants

for N_TRAIN in 1 2 3; do
	for patient_data in $(find Data -name "patient_*.rds"); do
		# run all combinations of the sessions for N_TRAIN number of session
	    for ses in $(python Code/combi.py $N_TRAIN); do 
			# run both hands
			for npc in 6 7 8 9 10; do
				
				log_path=logs/bothhands/npc-${npc}_smooth-TRUE.log
				bsub -o $log_path -e $log_path Rscript Code/train_movelet_and_predict.R --hand 'bothhands' --ses $ses --npc $npc --smooth $patient_data

				log_path=logs/bothhands/npc-${npc}_smooth-FALSE.log
				bsub -o $log_path -e $log_path Rscript Code/train_movelet_and_predict.R --hand 'bothhands' --ses $ses --npc $npc $patient_data
				
			done
			
			# run dominant Code/train_movelet_and_predict.R
			#if [[ $N_TRAIN != 1 ]]; then
				# run on dominant hand w/ and w/o smoothing
				log_path=logs/dominanthand_smooth-TRUE.log
				bsub -o $log_path -e $log_path Rscript Code/train_movelet_and_predict.R --hand 'dominanthand' --ses $ses --npc 6 --smooth $patient_data
				#
				log_path=logs/dominanthand_smooth-FALSE.log
				bsub -o $log_path -e $log_path Rscript Code/train_movelet_and_predict.R --hand 'dominanthand' --ses $ses --npc 6 $patient_data
				# run on nondominant hand w/ and w/o smoothing
				log_path=logs/nondominanthand_smooth-TRUE.log
				bsub -o $log_path -e $log_path Rscript Code/train_movelet_and_predict.R --hand 'nondominanthand' --ses $ses --npc 6 --smooth $patient_data
				
				log_path=logs/nondominanthand_smooth-FALSE.log
				bsub -o $log_path -e $log_path Rscript Code/train_movelet_and_predict.R --hand 'nondominanthand' --ses $ses --npc 6 $patient_data

			#fi
	    done
	done
done

