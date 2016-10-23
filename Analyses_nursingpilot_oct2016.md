Analyses for Nursing Pilot Oct 2016
================
Kate Block
2016-10-22

``` r
library (tidyverse)
library (ggplot2)
library(knitr)
library(Hmisc)
```

SUMMARY
=======

-   Sample of 192 men and women after selecting only heterosexual people
-   GOAL: select DVS, refine predictions
-   In this pilot, we tried out 4 measures of "interest"
    -   A self-report fit measure we made up (6 items)
    -   A 3-item interest measure we have use previously
    -   A measure of ranking nursing against other jobs
    -   measure of self-nursing overlap
-   The New FIT measure shows great intercorrelations, so all items seem reliable
-   Fit, interest, overlap and Rank of nursing show predicted gender differences
-   All 4 relate to communal values
-   For men, Fit, interest, overlap, but not rank relate significantly to rate of men moving into nursing, maybe we should not select rank as a measure then!!!
-   Ran interactions for men predicting interest and fit from "rate of men moving into nursing" \* "communal values"
    -   Seems like both independently have an effect
    -   interactions are small-ish and ns. They are in the wrong direction for FIT?
    -   SOOOOO... maybe we should expect an additive effect rather than an interaction????

reading file
============

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

Correlations
============

Correlations for FIT measure:
-----------------------------

LOOKING GREAT!

``` r
fitmeasure <- CLEAN_hetero[, c("fit1","fit2","fit3", "fit4", "fit5", "fit6")]
corrs1 <- rcorr (as.matrix(fitmeasure))
corrs1$r %>% kable ()
```

|      |       fit1|       fit2|       fit3|       fit4|       fit5|       fit6|
|------|----------:|----------:|----------:|----------:|----------:|----------:|
| fit1 |  1.0000000|  0.6850787|  0.6136234|  0.5844160|  0.5662511|  0.5389736|
| fit2 |  0.6850787|  1.0000000|  0.7904147|  0.5637980|  0.6772018|  0.5523527|
| fit3 |  0.6136234|  0.7904147|  1.0000000|  0.5766708|  0.6395782|  0.4754802|
| fit4 |  0.5844160|  0.5637980|  0.5766708|  1.0000000|  0.6375064|  0.6727729|
| fit5 |  0.5662511|  0.6772018|  0.6395782|  0.6375064|  1.0000000|  0.7020859|
| fit6 |  0.5389736|  0.5523527|  0.4754802|  0.6727729|  0.7020859|  1.0000000|

``` r
corrs1$P %>% kable ()
```

|      |  fit1|  fit2|  fit3|  fit4|  fit5|  fit6|
|------|-----:|-----:|-----:|-----:|-----:|-----:|
| fit1 |    NA|     0|     0|     0|     0|     0|
| fit2 |     0|    NA|     0|     0|     0|     0|
| fit3 |     0|     0|    NA|     0|     0|     0|
| fit4 |     0|     0|     0|    NA|     0|     0|
| fit5 |     0|     0|     0|     0|    NA|     0|
| fit6 |     0|     0|     0|     0|     0|    NA|

Correlations for INTEREST Measure:
----------------------------------

LOOKING GREAT!

``` r
interestmeasure <- CLEAN_hetero[, c("interest_nursing","enjoy_nursing","imagine_nursing")]
corrs2 <- rcorr (as.matrix(fitmeasure))
corrs2$r %>% kable ()
```

|      |       fit1|       fit2|       fit3|       fit4|       fit5|       fit6|
|------|----------:|----------:|----------:|----------:|----------:|----------:|
| fit1 |  1.0000000|  0.6850787|  0.6136234|  0.5844160|  0.5662511|  0.5389736|
| fit2 |  0.6850787|  1.0000000|  0.7904147|  0.5637980|  0.6772018|  0.5523527|
| fit3 |  0.6136234|  0.7904147|  1.0000000|  0.5766708|  0.6395782|  0.4754802|
| fit4 |  0.5844160|  0.5637980|  0.5766708|  1.0000000|  0.6375064|  0.6727729|
| fit5 |  0.5662511|  0.6772018|  0.6395782|  0.6375064|  1.0000000|  0.7020859|
| fit6 |  0.5389736|  0.5523527|  0.4754802|  0.6727729|  0.7020859|  1.0000000|

``` r
corrs2$P %>% kable ()
```

|      |  fit1|  fit2|  fit3|  fit4|  fit5|  fit6|
|------|-----:|-----:|-----:|-----:|-----:|-----:|
| fit1 |    NA|     0|     0|     0|     0|     0|
| fit2 |     0|    NA|     0|     0|     0|     0|
| fit3 |     0|     0|    NA|     0|     0|     0|
| fit4 |     0|     0|     0|    NA|     0|     0|
| fit5 |     0|     0|     0|     0|    NA|     0|
| fit6 |     0|     0|     0|     0|     0|    NA|

Overall correlation (MEN AND WOMEN)
-----------------------------------

I display these in different ways NOTES:

-   Communal values are related to all FIT, interest and rank. But MORE to fit and rank than interest.
-   We get gender differences in all three FIT, interest and rank
-   interestingly we don't get gender differences in communal values???

    |                    gen| der\_num rat | e\_men\_nursing mal | e\_nurse\_positive com | munal\_values age | ntic\_values ran | k\_nurse fit | \_nursing int | erest\_comp nur | sing\_overlap |
    |----------------------:|--------------|---------------------|------------------------|-------------------|------------------|--------------|---------------|-----------------|:--------------|
    |            gender\_num| 1.0000000    | -0.0612814          | 0.0472682              | 0.0965309         | -0.1084806       | -0.2380275   | 0.2219109     | 0.2575104       | 0.2491856     |
    |     rate\_men\_nursing| -0.0612814   | 1.0000000           | 0.2205480              | 0.1395007         | 0.1081959        | -0.0399454   | 0.1624604     | 0.1250158       | 0.1450112     |
    |  male\_nurse\_positive| 0.0472682    | 0.2205480           | 1.0000000              | 0.1727595         | 0.0791551        | -0.0831982   | 0.1671681     | 0.0872488       | 0.0888332     |
    |       communal\_values| 0.0965309    | 0.1395007           | 0.1727595              | 1.0000000         | 0.3701255        | -0.3568348   | 0.4482102     | 0.2664495       | 0.2499670     |
    |        agentic\_values| -0.1084806   | 0.1081959           | 0.0791551              | 0.3701255         | 1.0000000        | 0.1101166    | 0.0097131     | -0.0649606      | -0.0282813    |
    |            rank\_nurse| -0.2380275   | -0.0399454          | -0.0831982             | -0.3568348        | 0.1101166        | 1.0000000    | -0.6184070    | -0.6514257      | -0.5873606    |
    |           fit\_nursing| 0.2219109    | 0.1624604           | 0.1671681              | 0.4482102         | 0.0097131        | -0.6184070   | 1.0000000     | 0.6490603       | 0.6056745     |
    |         interest\_comp| 0.2575104    | 0.1250158           | 0.0872488              | 0.2664495         | -0.0649606       | -0.6514257   | 0.6490603     | 1.0000000       | 0.6395105     |
    |       nursing\_overlap| 0.2491856    | 0.1450112           | 0.0888332              | 0.2499670         | -0.0282813       | -0.5873606   | 0.6056745     | 0.6395105       | 1.0000000     |

    |                    gen| der\_num rat | e\_men\_nursing mal | e\_nurse\_positive com | munal\_values age | ntic\_values ran | k\_nurse fit | \_nursing int | erest\_comp nur | sing\_overlap |
    |----------------------:|--------------|---------------------|------------------------|-------------------|------------------|--------------|---------------|-----------------|:--------------|
    |            gender\_num| NA           | 0.3984520           | 0.5150110              | 0.1828769         | 0.1341957        | 0.0008855    | 0.0019783     | 0.0003111       | 0.0004914     |
    |     rate\_men\_nursing| 0.3984520    | NA                  | 0.0021122              | 0.0536313         | 0.1352265        | 0.5822477    | 0.0243605     | 0.0840343       | 0.0447674     |
    |  male\_nurse\_positive| 0.5150110    | 0.0021122           | NA                     | 0.0165643         | 0.2751163        | 0.2512614    | 0.0204735     | 0.2288384       | 0.2204640     |
    |       communal\_values| 0.1828769    | 0.0536313           | 0.0165643              | NA                | 0.0000001        | 0.0000004    | 0.0000000     | 0.0001872       | 0.0004710     |
    |        agentic\_values| 0.1341957    | 0.1352265           | 0.2751163              | 0.0000001         | NA               | 0.1283894    | 0.8936291     | 0.3706866       | 0.6969829     |
    |            rank\_nurse| 0.0008855    | 0.5822477           | 0.2512614              | 0.0000004         | 0.1283894        | NA           | 0.0000000     | 0.0000000       | 0.0000000     |
    |           fit\_nursing| 0.0019783    | 0.0243605           | 0.0204735              | 0.0000000         | 0.8936291        | 0.0000000    | NA            | 0.0000000       | 0.0000000     |
    |         interest\_comp| 0.0003111    | 0.0840343           | 0.2288384              | 0.0001872         | 0.3706866        | 0.0000000    | 0.0000000     | NA              | 0.0000000     |
    |       nursing\_overlap| 0.0004914    | 0.0447674           | 0.2204640              | 0.0004710         | 0.6969829        | 0.0000000    | 0.0000000     | 0.0000000       | NA            |

![](Analyses_nursingpilot_oct2016_files/figure-markdown_github/unnamed-chunk-6-1.png)

| row                   | column                |         cor|          p|
|:----------------------|:----------------------|-----------:|----------:|
| gender\_num           | rate\_men\_nursing    |  -0.0612814|  0.3984520|
| gender\_num           | male\_nurse\_positive |   0.0472682|  0.5150110|
| rate\_men\_nursing    | male\_nurse\_positive |   0.2205480|  0.0021122|
| gender\_num           | communal\_values      |   0.0965309|  0.1828769|
| rate\_men\_nursing    | communal\_values      |   0.1395007|  0.0536313|
| male\_nurse\_positive | communal\_values      |   0.1727595|  0.0165643|
| gender\_num           | agentic\_values       |  -0.1084806|  0.1341957|
| rate\_men\_nursing    | agentic\_values       |   0.1081959|  0.1352265|
| male\_nurse\_positive | agentic\_values       |   0.0791551|  0.2751163|
| communal\_values      | agentic\_values       |   0.3701255|  0.0000001|
| gender\_num           | rank\_nurse           |  -0.2380275|  0.0008855|
| rate\_men\_nursing    | rank\_nurse           |  -0.0399454|  0.5822477|
| male\_nurse\_positive | rank\_nurse           |  -0.0831982|  0.2512614|
| communal\_values      | rank\_nurse           |  -0.3568348|  0.0000004|
| agentic\_values       | rank\_nurse           |   0.1101166|  0.1283894|
| gender\_num           | fit\_nursing          |   0.2219109|  0.0019783|
| rate\_men\_nursing    | fit\_nursing          |   0.1624604|  0.0243605|
| male\_nurse\_positive | fit\_nursing          |   0.1671681|  0.0204735|
| communal\_values      | fit\_nursing          |   0.4482102|  0.0000000|
| agentic\_values       | fit\_nursing          |   0.0097131|  0.8936291|
| rank\_nurse           | fit\_nursing          |  -0.6184070|  0.0000000|
| gender\_num           | interest\_comp        |   0.2575104|  0.0003111|
| rate\_men\_nursing    | interest\_comp        |   0.1250158|  0.0840343|
| male\_nurse\_positive | interest\_comp        |   0.0872488|  0.2288384|
| communal\_values      | interest\_comp        |   0.2664495|  0.0001872|
| agentic\_values       | interest\_comp        |  -0.0649606|  0.3706866|
| rank\_nurse           | interest\_comp        |  -0.6514257|  0.0000000|
| fit\_nursing          | interest\_comp        |   0.6490603|  0.0000000|
| gender\_num           | nursing\_overlap      |   0.2491856|  0.0004914|
| rate\_men\_nursing    | nursing\_overlap      |   0.1450112|  0.0447674|
| male\_nurse\_positive | nursing\_overlap      |   0.0888332|  0.2204640|
| communal\_values      | nursing\_overlap      |   0.2499670|  0.0004710|
| agentic\_values       | nursing\_overlap      |  -0.0282813|  0.6969829|
| rank\_nurse           | nursing\_overlap      |  -0.5873606|  0.0000000|
| fit\_nursing          | nursing\_overlap      |   0.6056745|  0.0000000|
| interest\_comp        | nursing\_overlap      |   0.6395105|  0.0000000|

Correlations JUST FOR MEN!
==========================

-   For men, all FIT, interest and rankare correlate with communal
-   But only FIT and interest are related to rate
-   Seems like RANK won't work well
-   How positive male nurses are seen is probably not a great measure, it doens't really correlate with things!

    |                    rat| e\_men\_nursing mal | e\_nurse\_positive com | munal\_values age | ntic\_values ran | k\_nurse fit | \_nursing int | erest\_comp nur | sing\_overlap |
    |----------------------:|---------------------|------------------------|-------------------|------------------|--------------|---------------|-----------------|:--------------|
    |     rate\_men\_nursing| 1.0000000           | 0.2326970              | 0.2478697         | 0.0687668        | -0.1379426   | 0.3303507     | 0.2565778       | 0.2532286     |
    |  male\_nurse\_positive| 0.2326970           | 1.0000000              | 0.1659322         | -0.0382632       | -0.0420246   | 0.1302183     | 0.0819289       | 0.0076918     |
    |       communal\_values| 0.2478697           | 0.1659322              | 1.0000000         | 0.3276650        | -0.3220410   | 0.3577680     | 0.3182880       | 0.1961048     |
    |        agentic\_values| 0.0687668           | -0.0382632             | 0.3276650         | 1.0000000        | 0.1006952    | -0.0170592    | -0.0099438      | -0.0626502    |
    |            rank\_nurse| -0.1379426          | -0.0420246             | -0.3220410        | 0.1006952        | 1.0000000    | -0.6682702    | -0.6533786      | -0.7139428    |
    |           fit\_nursing| 0.3303507           | 0.1302183              | 0.3577680         | -0.0170592       | -0.6682702   | 1.0000000     | 0.6856524       | 0.6758783     |
    |         interest\_comp| 0.2565778           | 0.0819289              | 0.3182880         | -0.0099438       | -0.6533786   | 0.6856524     | 1.0000000       | 0.7147575     |
    |       nursing\_overlap| 0.2532286           | 0.0076918              | 0.1961048         | -0.0626502       | -0.7139428   | 0.6758783     | 0.7147575       | 1.0000000     |

    |                    rat| e\_men\_nursing mal | e\_nurse\_positive com | munal\_values age | ntic\_values ran | k\_nurse fit | \_nursing int | erest\_comp nur | sing\_overlap |
    |----------------------:|---------------------|------------------------|-------------------|------------------|--------------|---------------|-----------------|:--------------|
    |     rate\_men\_nursing| NA                  | 0.0204594              | 0.0133729         | 0.4988295        | 0.1733244    | 0.0008394     | 0.0103604       | 0.0114402     |
    |  male\_nurse\_positive| 0.0204594           | NA                     | 0.1007067         | 0.7069044        | 0.6795980    | 0.1989047     | 0.4201356       | 0.9397683     |
    |       communal\_values| 0.0133729           | 0.1007067              | NA                | 0.0009306        | 0.0011515    | 0.0002773     | 0.0013244       | 0.0517304     |
    |        agentic\_values| 0.4988295           | 0.7069044              | 0.0009306         | NA               | 0.3213420    | 0.8669034     | 0.9221821       | 0.5378610     |
    |            rank\_nurse| 0.1733244           | 0.6795980              | 0.0011515         | 0.3213420        | NA           | 0.0000000     | 0.0000000       | 0.0000000     |
    |           fit\_nursing| 0.0008394           | 0.1989047              | 0.0002773         | 0.8669034        | 0.0000000    | NA            | 0.0000000       | 0.0000000     |
    |         interest\_comp| 0.0103604           | 0.4201356              | 0.0013244         | 0.9221821        | 0.0000000    | 0.0000000     | NA              | 0.0000000     |
    |       nursing\_overlap| 0.0114402           | 0.9397683              | 0.0517304         | 0.5378610        | 0.0000000    | 0.0000000     | 0.0000000       | NA            |

![](Analyses_nursingpilot_oct2016_files/figure-markdown_github/unnamed-chunk-7-1.png)

| row                   | column                |         cor|          p|
|:----------------------|:----------------------|-----------:|----------:|
| rate\_men\_nursing    | male\_nurse\_positive |   0.2326970|  0.0204594|
| rate\_men\_nursing    | communal\_values      |   0.2478697|  0.0133729|
| male\_nurse\_positive | communal\_values      |   0.1659322|  0.1007067|
| rate\_men\_nursing    | agentic\_values       |   0.0687668|  0.4988295|
| male\_nurse\_positive | agentic\_values       |  -0.0382632|  0.7069044|
| communal\_values      | agentic\_values       |   0.3276650|  0.0009306|
| rate\_men\_nursing    | rank\_nurse           |  -0.1379426|  0.1733244|
| male\_nurse\_positive | rank\_nurse           |  -0.0420246|  0.6795980|
| communal\_values      | rank\_nurse           |  -0.3220410|  0.0011515|
| agentic\_values       | rank\_nurse           |   0.1006952|  0.3213420|
| rate\_men\_nursing    | fit\_nursing          |   0.3303507|  0.0008394|
| male\_nurse\_positive | fit\_nursing          |   0.1302183|  0.1989047|
| communal\_values      | fit\_nursing          |   0.3577680|  0.0002773|
| agentic\_values       | fit\_nursing          |  -0.0170592|  0.8669034|
| rank\_nurse           | fit\_nursing          |  -0.6682702|  0.0000000|
| rate\_men\_nursing    | interest\_comp        |   0.2565778|  0.0103604|
| male\_nurse\_positive | interest\_comp        |   0.0819289|  0.4201356|
| communal\_values      | interest\_comp        |   0.3182880|  0.0013244|
| agentic\_values       | interest\_comp        |  -0.0099438|  0.9221821|
| rank\_nurse           | interest\_comp        |  -0.6533786|  0.0000000|
| fit\_nursing          | interest\_comp        |   0.6856524|  0.0000000|
| rate\_men\_nursing    | nursing\_overlap      |   0.2532286|  0.0114402|
| male\_nurse\_positive | nursing\_overlap      |   0.0076918|  0.9397683|
| communal\_values      | nursing\_overlap      |   0.1961048|  0.0517304|
| agentic\_values       | nursing\_overlap      |  -0.0626502|  0.5378610|
| rank\_nurse           | nursing\_overlap      |  -0.7139428|  0.0000000|
| fit\_nursing          | nursing\_overlap      |   0.6758783|  0.0000000|
| interest\_comp        | nursing\_overlap      |   0.7147575|  0.0000000|

Linear Models
=============

-   We don't really have the power here but I am trying to see if there is an interaction between values and norms?
-   There is a small interaction, but it is not significant AND IN THE WRONG DIRECTION for fit & rank, in the right for interest.
-   Seems like there is a strong additive effect?? Might have to adjust predictions?

predicting Fit
--------------

``` r
regression <- lm (fit_nursing ~ scale(rate_men_nursing)*scale(communal_values), data=MENONLY)
coef(summary(regression)) 
```

    ##                                                   Estimate Std. Error
    ## (Intercept)                                     4.21136108  0.1182968
    ## scale(rate_men_nursing)                         0.34917562  0.1227859
    ## scale(communal_values)                          0.34724951  0.1218968
    ## scale(rate_men_nursing):scale(communal_values) -0.09981986  0.1137060
    ##                                                   t value     Pr(>|t|)
    ## (Intercept)                                    35.5999709 9.779902e-57
    ## scale(rate_men_nursing)                         2.8437773 5.458942e-03
    ## scale(communal_values)                          2.8487164 5.381579e-03
    ## scale(rate_men_nursing):scale(communal_values) -0.8778767 3.822254e-01

``` r
MENONLY$rate_men_nursingLOW <- scale(MENONLY$rate_men_nursing) + 1
MENONLY$rate_men_nursingHIGH <- scale(MENONLY$rate_men_nursing) - 1

regressionLOW <- lm (fit_nursing ~ rate_men_nursingLOW*scale(communal_values), data=MENONLY)
coef(summary(regressionLOW)) 
```

    ##                                               Estimate Std. Error
    ## (Intercept)                                 3.86218546  0.1656561
    ## rate_men_nursingLOW                         0.34917562  0.1227859
    ## scale(communal_values)                      0.44706937  0.1485276
    ## rate_men_nursingLOW:scale(communal_values) -0.09981986  0.1137060
    ##                                               t value     Pr(>|t|)
    ## (Intercept)                                23.3144843 4.372762e-41
    ## rate_men_nursingLOW                         2.8437773 5.458942e-03
    ## scale(communal_values)                      3.0100088 3.345643e-03
    ## rate_men_nursingLOW:scale(communal_values) -0.8778767 3.822254e-01

``` r
regressionHIGH <- lm (fit_nursing ~ rate_men_nursingHIGH*scale(communal_values), data=MENONLY)
coef(summary(regressionHIGH)) 
```

    ##                                                Estimate Std. Error
    ## (Intercept)                                  4.56053670  0.1752114
    ## rate_men_nursingHIGH                         0.34917562  0.1227859
    ## scale(communal_values)                       0.24742965  0.1830720
    ## rate_men_nursingHIGH:scale(communal_values) -0.09981986  0.1137060
    ##                                                t value     Pr(>|t|)
    ## (Intercept)                                 26.0287615 5.088539e-45
    ## rate_men_nursingHIGH                         2.8437773 5.458942e-03
    ## scale(communal_values)                       1.3515431 1.797310e-01
    ## rate_men_nursingHIGH:scale(communal_values) -0.8778767 3.822254e-01

predicting interest
===================

``` r
Bregression <- lm (interest_comp ~ scale(rate_men_nursing)*scale(communal_values), data=MENONLY)
coef(summary(Bregression))
```

    ##                                                  Estimate Std. Error
    ## (Intercept)                                    3.01962661  0.1416836
    ## scale(rate_men_nursing)                        0.25337357  0.1470602
    ## scale(communal_values)                         0.41309436  0.1459954
    ## scale(rate_men_nursing):scale(communal_values) 0.08467938  0.1361853
    ##                                                   t value     Pr(>|t|)
    ## (Intercept)                                    21.3124572 5.703382e-38
    ## scale(rate_men_nursing)                         1.7229240 8.815594e-02
    ## scale(communal_values)                          2.8295017 5.688318e-03
    ## scale(rate_men_nursing):scale(communal_values)  0.6217952 5.355659e-01

``` r
BregressionLOW <- lm (interest_comp ~ rate_men_nursingLOW*scale(communal_values), data=MENONLY)
coef(summary(BregressionLOW)) 
```

    ##                                              Estimate Std. Error
    ## (Intercept)                                2.76625303  0.1984057
    ## rate_men_nursingLOW                        0.25337357  0.1470602
    ## scale(communal_values)                     0.32841498  0.1778910
    ## rate_men_nursingLOW:scale(communal_values) 0.08467938  0.1361853
    ##                                               t value     Pr(>|t|)
    ## (Intercept)                                13.9424055 1.041200e-24
    ## rate_men_nursingLOW                         1.7229240 8.815594e-02
    ## scale(communal_values)                      1.8461583 6.798308e-02
    ## rate_men_nursingLOW:scale(communal_values)  0.6217952 5.355659e-01

``` r
BregressionHIGH <- lm (interest_comp ~ rate_men_nursingHIGH*scale(communal_values), data=MENONLY)
coef(summary(BregressionHIGH))
```

    ##                                               Estimate Std. Error
    ## (Intercept)                                 3.27300018  0.2098502
    ## rate_men_nursingHIGH                        0.25337357  0.1470602
    ## scale(communal_values)                      0.49777374  0.2192647
    ## rate_men_nursingHIGH:scale(communal_values) 0.08467938  0.1361853
    ##                                                t value     Pr(>|t|)
    ## (Intercept)                                 15.5968428 6.083691e-28
    ## rate_men_nursingHIGH                         1.7229240 8.815594e-02
    ## scale(communal_values)                       2.2701955 2.545564e-02
    ## rate_men_nursingHIGH:scale(communal_values)  0.6217952 5.355659e-01

self-nursing overlap
====================

``` r
Dregression <- lm (nursing_overlap ~ scale(rate_men_nursing)*scale(communal_values), data=MENONLY)


DregressionLOW <- lm (nursing_overlap ~ rate_men_nursingLOW*scale(communal_values), data=MENONLY)


DregressionHIGH <- lm (nursing_overlap ~ rate_men_nursingHIGH*scale(communal_values), data=MENONLY)

coef(summary(Dregression)) 
```

    ##                                                  Estimate Std. Error
    ## (Intercept)                                     3.1465125  0.1584504
    ## scale(rate_men_nursing)                         0.3937095  0.1644632
    ## scale(communal_values)                          0.1844600  0.1632724
    ## scale(rate_men_nursing):scale(communal_values) -0.1854473  0.1523014
    ##                                                  t value     Pr(>|t|)
    ## (Intercept)                                    19.858030 1.390076e-35
    ## scale(rate_men_nursing)                         2.393906 1.863423e-02
    ## scale(communal_values)                          1.129768 2.614185e-01
    ## scale(rate_men_nursing):scale(communal_values) -1.217633 2.263792e-01

``` r
coef(summary(DregressionLOW))
```

    ##                                              Estimate Std. Error   t value
    ## (Intercept)                                 2.7528030  0.2218849 12.406445
    ## rate_men_nursingLOW                         0.3937095  0.1644632  2.393906
    ## scale(communal_values)                      0.3699072  0.1989425  1.859367
    ## rate_men_nursingLOW:scale(communal_values) -0.1854473  0.1523014 -1.217633
    ##                                                Pr(>|t|)
    ## (Intercept)                                1.388794e-21
    ## rate_men_nursingLOW                        1.863423e-02
    ## scale(communal_values)                     6.607008e-02
    ## rate_men_nursingLOW:scale(communal_values) 2.263792e-01

``` r
coef(summary(DregressionHIGH)) 
```

    ##                                                  Estimate Std. Error
    ## (Intercept)                                  3.5402220749  0.2346837
    ## rate_men_nursingHIGH                         0.3937095377  0.1644632
    ## scale(communal_values)                      -0.0009872734  0.2452123
    ## rate_men_nursingHIGH:scale(communal_values) -0.1854472581  0.1523014
    ##                                                  t value     Pr(>|t|)
    ## (Intercept)                                 15.085078709 5.871384e-27
    ## rate_men_nursingHIGH                         2.393906234 1.863423e-02
    ## scale(communal_values)                      -0.004026198 9.967960e-01
    ## rate_men_nursingHIGH:scale(communal_values) -1.217633309 2.263792e-01

predicting rank
===============

``` r
Cregression <- lm (rank_nurse ~ scale(rate_men_nursing)*scale(communal_values), data=MENONLY)


CregressionLOW <- lm (rank_nurse ~ rate_men_nursingLOW*scale(communal_values), data=MENONLY)


CregressionHIGH <- lm (rank_nurse ~ rate_men_nursingHIGH*scale(communal_values), data=MENONLY)
```

``` r
coef(summary(Cregression)) 
```

    ##                                                  Estimate Std. Error
    ## (Intercept)                                     7.8000535  0.3964212
    ## scale(rate_men_nursing)                        -0.3090826  0.4114645
    ## scale(communal_values)                         -1.1747209  0.4084853
    ## scale(rate_men_nursing):scale(communal_values)  0.2385514  0.3810372
    ##                                                   t value     Pr(>|t|)
    ## (Intercept)                                    19.6761783 2.812727e-35
    ## scale(rate_men_nursing)                        -0.7511769 4.544030e-01
    ## scale(communal_values)                         -2.8757974 4.974981e-03
    ## scale(rate_men_nursing):scale(communal_values)  0.6260580 5.327783e-01

``` r
coef(summary(CregressionLOW))
```

    ##                                              Estimate Std. Error
    ## (Intercept)                                 8.1091360  0.5551257
    ## rate_men_nursingLOW                        -0.3090826  0.4114645
    ## scale(communal_values)                     -1.4132723  0.4977269
    ## rate_men_nursingLOW:scale(communal_values)  0.2385514  0.3810372
    ##                                               t value     Pr(>|t|)
    ## (Intercept)                                14.6077484 5.009362e-26
    ## rate_men_nursingLOW                        -0.7511769 4.544030e-01
    ## scale(communal_values)                     -2.8394532 5.527510e-03
    ## rate_men_nursingLOW:scale(communal_values)  0.6260580 5.327783e-01

``` r
coef(summary(CregressionHIGH)) 
```

    ##                                               Estimate Std. Error
    ## (Intercept)                                  7.4909709  0.5871465
    ## rate_men_nursingHIGH                        -0.3090826  0.4114645
    ## scale(communal_values)                      -0.9361695  0.6134877
    ## rate_men_nursingHIGH:scale(communal_values)  0.2385514  0.3810372
    ##                                                t value     Pr(>|t|)
    ## (Intercept)                                 12.7582663 2.613676e-22
    ## rate_men_nursingHIGH                        -0.7511769 4.544030e-01
    ## scale(communal_values)                      -1.5259794 1.303375e-01
    ## rate_men_nursingHIGH:scale(communal_values)  0.6260580 5.327783e-01
