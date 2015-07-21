**ejanalysis package** 

This [environmental justice analysis tools package](https://ejanalysis.github.io/ejanalysis/) provides tools for R that simplify some basic tasks in exploring and analyzing a dataset in a matrix or data.frame that contains data on demographics (e.g., counts of residents in poverty) and local environmental indicators (e.g., an air quality index), with one row per spatial location (e.g., Census block group).  

Key functions help to find relative risk or similar ratios of means in demographic groups, etc.

This and related packages, once each is made available as a public repository on GitHub, until available on cran, can be installed using the devtools package: 

```r
if (!require('devtools')) install.packages('devtools')
devtools::install_github("ejanalysis/analyze.stuff")  
devtools::install_github("ejanalysis/countyhealthrankings")  
devtools::install_github("ejanalysis/UScensus2010blocks")  
devtools::install_github("ejanalysis/ACSdownload")  
devtools::install_github(c("ejanalysis/proxistat", "ejanalysis/ejanalysis"))
devtools::install_github("ejanalysis/ejscreen")
```
