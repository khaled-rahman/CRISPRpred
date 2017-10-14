## CRISPRPRED: A CRISPR/CAS9 sgRNA activity prediction tool ##

This is a prediction tool named 'crisprpred' which is being developed to predict CRISPR/Cas9 sgRNA activity. This tool is being developed in R version 3.2.2. Here is the first small working version.

How To Run:

* Ensure that you have e1071,DAAG,h2o,roxygen2 R packages installed in your PC.

    * You can install it by typing 'install.packages('e1071')' in your R console.

* Ensure that you have installed viennaRNA tool in your PC. 

    * You can download and install it from the following link: http://www.tbi.univie.ac.at/RNA/#source_code_packages

* Clone the repository in your PC and do the followings.

* Go to crisprpred directory from terminal and run './link.sh'

* Then run './roxygenize.sh' to generate documentation.

* Then you can run './check.sh' optionally to check whether everything is ok or not.

* Then install the package by running './install.sh' in the terminal.

'data-raw' folder contains one CSV file. sample.csv is the main benchmark data. You can also check the crisprpred_main.R file to check the structures. More instructions will be added soon.

We hope to update this tool in every week until we deploy the full working version.
