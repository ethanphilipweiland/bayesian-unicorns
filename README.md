# Bayesian Unicorns
Privately held companies worth $1 billion are of special interest in business and finance because their success has been achieved without the need to raise capital on public markets. Additionally, if they do plan to go public, their initial public offering (IPO) will be worth hundreds of millions of dollars. This report explores the adequacy of the “unicorn” label by modeling the data generating process producing unicorn company valuations. If this underlying process can best be modeled by a single distribution, with one set of parameters, the label “unicorn” is adequate. If this underlying process is instead better modeled as a mixture of different distributions, each with different parameters, then it would be better to break unicorns down into different subspecies (so to speak). To answer this, I implement a Bayesian mixture model using the `gibbsnorm()` function in the `bayess` package in `R`. I then determine the optimal number of component distributions (one, two, three, or four) using model selection criteria and visualization techniques.

The analysis concludes that the three component mixture model is best. This model’s predicted density of logged valuation outperforms the predicted density of the two component and four component models. The predicted density of the one component model, which implies one set of parameters to describe all unicorns, performs poorly. The “unicorn” label is inadequate, and business and finance professionals should disaggregate unicorns into three groups to better understand valuations. The below report broadly describes these three groups, but future researchers should explore them in more detail. The chain for all the parameters converges and this analysis is robust to prior specification. 

This project was independently completed for STAT-S 626: Bayesian Theory and Data Analysis taught by Professor Manrique-Vallier at Indiana University Bloomington in Fall 2023. 

## Analytical Report
The files "Bayesian Unicorns Report.docx" and "Bayesian Unicorns Report.pdf" contain a write-up of the motivation, methodology, analysis, results, and implications of this project. 

## Necessary Software
You will need the following software and packages installed to run the code file and reproduce the analysis.

Necessary software: `R`

Necessary `R` packages: `tidyverse`, `bayess`, `gridExtra`, `stargazer`, `coda`

## File Descriptions
    1. /Tables/ : Folder containing all of the tables in the analytical report in HTML format
    2. Bayesian Unicorns Report.docx : Write-up of the motivation, methodology, analysis, results, and implications of this project in .docx format
    3. Bayesian Unicorns Report.pdf : Write-up of the motivation, methodology, analysis, results, and implications of this project in PDF format
    4. Code.R : R file that contains all data import, data cleaning, and statistical analyses.
    5. Final_Project_Guidelines.pdf : Instructions for this project provided by Professor Manrique-Vallier
    6. unicorn_companies.csv : Data on unicorn company valuations in late 2021 downloaded from DataCamp Workspace (https://www.datacamp.com/workspace/).

## Installation and File Execution
To begin, download Code.R and unicorn_companies.csv into a folder. Open `R` and set this folder as your working directory using  `setwd()`. `R` script files are executable once a working directoy to the folder containing the data is set. Running Code.R will reproduce all data import, data cleaning, and statistical analysis. 

## Acknowledgements
Marin, J. M. and Robert, C. (2013), Bayesian Essentials with R. New York: Springer. ISBN 978-1-4614-8686-2.

## License
See LICENSE for licensing details for this repository. 

