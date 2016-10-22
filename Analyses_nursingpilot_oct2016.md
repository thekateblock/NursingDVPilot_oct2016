Analyses for Nursing Pilot Oct 2016
================
Kate Block
2016-10-22

``` r
library (tidyverse)
library (ggplot2)
library(knitr)
```

reading file
------------

``` r
CLEAN_hetero <- read_csv("/Users/KatharinaBlock/Desktop/Git_REPOS/NursingDVPilot_oct2016/Nursing2pilot_clean_hetero.csv")
```

Making a mean overview

| vars          |      Female|        Male|
|:--------------|-----------:|-----------:|
| participants  |  93.0000000|  99.0000000|
| M\_communal   |   7.3620072|   7.1447811|
| M\_agentic    |   6.9754224|   7.2640693|
| M\_rankNurse  |   6.0000000|   7.8585859|
| M\_fit        |   4.7491039|   4.1868687|
| M\_positive   |   3.9677419|   3.8787879|
| M\_SNoverlap  |   3.9247312|   3.1010101|
| M\_interest   |   3.7956989|   3.0404040|
| M\_rate       |   3.6559140|   3.7878788|
| Sd\_rankNurse |   3.5934723|   4.0025760|
| Sd\_SNoverlap |   1.6301278|   1.5875469|
| Sd\_interest  |   1.3918606|   1.4531791|
| Sd\_agentic   |   1.2665276|   1.3848053|
| Sd\_communal  |   1.2483379|   0.9959232|
| Sd\_fit       |   1.2244055|   1.2565639|
| Sd\_rate      |   1.0053620|   1.1452896|
| Sd\_positive  |   0.8654179|   1.0129038|
