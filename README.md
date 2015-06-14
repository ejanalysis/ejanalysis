# ejanalysis

This [environmental justice analysis tools package](https://ejanalysis.github.io/ejanalysis/) provides tools for R that simplify some basic tasks in exploring and analyzing a dataset in a matrix or data.frame that contains data on demographics (e.g., counts of residents in poverty) and local environmental indicators (e.g., an air quality index), with one row per spatial location (e.g., Census block group).  

Key functions help to find relative risk or similar ratios of means in demographic groups, etc.

The analyze.stuff, proxistat, ejanalysis, and countyhealthrankings packages, once made public can be installed from github using the devtools package:  

devtools::install_github(c("ejanalysis/analyze.stuff", "ejanalysis/proxistat", "ejanalysis/ejanalysis", "ejanalysis/countyhealthrankings"))
