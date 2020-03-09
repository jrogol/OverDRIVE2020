![img](https://github.com/jrogol/OverDRIVE2020/blob/master/Assets/Title.png)


This repository hosts the materials for the "Thinking Reproducibly" presentation and corresponding workshop, offered at APRA's [OverDRIVE/ conference](https://www.aprahome.org/page/overdrive-2020).
To download the materials, click the green "Clone or Download" button in the upper right of the repository, and selecting [Download ZIP](https://github.com/jrogol/OverDRIVE2020/archive/master.zip). 

## Organization

Files are organized into following folders:

* [Assets](https://github.com/jrogol/OverDRIVE2020/tree/master/Assets): `.pdf` versions of the [presentation](https://github.com/jrogol/OverDRIVE2020/blob/master/Assets/ThinkingReproducibly.pdf) and [workshop]()
* [Data](https://github.com/jrogol/OverDRIVE2020/tree/master/Data): Six `csv` files used in the workshop
  - [data-raw](https://github.com/jrogol/OverDRIVE2020/tree/master/Data/data-raw): Raw files from which the workshop files were created. For more, see below.
* [R](https://github.com/jrogol/OverDRIVE2020/tree/master/R/): `.R` files containing exercises for the workshop.
  - [`Solutions`](https://github.com/jrogol/OverDRIVE2020/tree/master/R/Solutions): Files with possible ways of solving the exercises.

## The Data

The files in the [`data/data-raw`]() folder were obtained from the  [Kaggle Fundraising Data set](https://www.kaggle.com/michaelpawlus/fundraising-data), curated by [Michael Pawlus](https://github.com/michaelpawlus).

* `data_science_for_fundraising_contact_reports.csv` provides data from 196 anonymized contact reports. Originally included with the [Data Science for Fundraising](http://nandeshwar.info/ds4fundraising/) book by Asutosh Nandeshwar and Rodger Devine.
* `data_science_for_fundraising_donor_data.csv` contains data for 34,000 anonymized constituents. Originally included with the [Data Science for Fundraising](http://nandeshwar.info/ds4fundraising/).
* lasala_das_2018.xlsx` was created by Michael Lasala for his presentation __"You Ain't Seen Nothing Yet": Intro to Data Analysis and Visualization in Excel__ at the 2018 Data Analytics Symposium.

To better reflect data seen in the wild, the files in the [`data` folder](https://github.com/jrogol/OverDRIVE2020/tree/master/Data/) were adapted from the above using the [`createData.R` script](https://github.com/jrogol/OverDRIVE2020/blob/master/Data/data-raw/createData.R).

## Exercises

The second part of the workshop moves from the conceptual to the practical, featuring examples of non-reproducible workflows. Possible solutions (no peeking!) are also included.

## Dependencies

The workshop makes use of many [`tidyverse`](https://www.tidyverse.org/) packages, particularly `dplyr`.

The [`here`](https://here.r-lib.org/) package can aid in heuristically locating files during an analysis.
Jenny Bryan's [__Ode to the here package__ ](https://github.com/jennybc/here_here) gives an excellent overview of how to use it.
Additionally, the [`keyring`](https://github.com/r-lib/keyring) package offers a way to utilize password protected resources without embedding the password in scripts.
Lastly, [`janitor`](http://sfirke.github.io/janitor/) provided functions for preprocessing the data, primarily when standardizing column names.

These packages can be installed using the following code:
```r
install.packages("tidyverse")
install.packages("here")
install.packages("keyring")
install.packages("janitor")
```
