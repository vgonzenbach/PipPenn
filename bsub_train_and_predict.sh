for patient in $(find Data -name "patient_*.rds")
do 
    for ses in 5 #6 8 9 11 12
    do
        bsub -o logs -e logs Rscript Code/train_movelet_and_predict.R $patient $ses
    done
done