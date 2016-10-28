## CRISPRpred: Efficient in silico on-target activity prediction of sgRNAs in CRISPR/Cas9 ##

CRISPRpred works in four main steps.

Step 1: It constructs features using featurization.py python program and FC_plus_RESwithPredictions.csv file.

Step 2: Then it runs a wrapper feature selection algorithms of Boruta R package using featureselection.R

Step 3: In this step,  it performs anova test to discard irrelevant features.

Step 4: It performs machine learning algorithms.

Detail instructions are coming soon...

If you need any help, please contact: khaled.cse.07@gmail.com
