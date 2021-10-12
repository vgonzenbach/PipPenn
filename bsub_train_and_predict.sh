n_train=$1
for patient in $(find Data -name "patient_*.rds")
do
    for ses in $(python Code/combi.py $n_train)
    do
      	bsub -o logs -e logs Rscript Code/train_movelet_and_predict.R $patient $ses
    done
done
