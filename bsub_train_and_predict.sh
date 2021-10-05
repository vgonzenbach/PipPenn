for f in $(find Data -name "*.rds")
do
	#bsub Rscript Code/train_movelet_and_predict.R $f "1_ses_train_walk"
	bsub Rscript Code/train_movelet_and_predict.R $f "2_ses_train"
	#bsub Rscript Code/train_movelet_and_predict.R $f "3_ses_train"
	#bsub Rscript Code/train_movelet_and_predict.R $f "5_ses_train"
	#bsub Rscript Code/train_movelet_and_predict.R $f "4_ses_train"
	#bsub Rscript Code/train_movelet_and_predict.R $f "1_ses_train"
done
