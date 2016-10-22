Cleaning for Nursing Pilot Oct 2016 data
================
Kate Block
2016-10-22

``` r
library (tidyverse)
library (ggplot2)
library(knitr)
```

0.1) renaming variables so they make sense and making composite variables
=========================================================================

-   First, I read in the data

``` r
rawdata <- read_csv ("/Users/KatharinaBlock/Desktop/Git_REPOS/NursingDVPilot_oct2016/Nursing2pilot.csv")

str (rawdata)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    225 obs. of  63 variables:
    ##  $ V1         : chr  "R_xu49biNxFqKA2NH" "R_3COXnCGhZL6cNxg" "R_2EGm44v2wnZrAhT" "R_3kgDFxiJYEMGIui" ...
    ##  $ V8         : chr  "2016-10-06 8:54" "2016-10-06 9:16" "2016-10-06 9:37" "2016-10-06 9:54" ...
    ##  $ V9         : chr  "2016-10-06 9:15" "2016-10-06 9:30" "2016-10-06 9:44" "2016-10-06 10:00" ...
    ##  $ V10        : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q2         : chr  NA NA NA NA ...
    ##  $ Q93        : chr  "A" "A" "A" "A" ...
    ##  $ Q80        : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q77_1      : int  7 9 9 9 8 9 7 7 7 9 ...
    ##  $ Q77_2      : int  6 9 6 7 6 5 5 7 6 6 ...
    ##  $ Q77_3      : int  8 9 7 9 5 8 7 5 9 7 ...
    ##  $ Q77_4      : int  7 9 8 9 6 9 6 6 9 7 ...
    ##  $ Q77_5      : int  6 9 5 9 5 7 6 6 6 7 ...
    ##  $ Q77_6      : int  5 9 6 7 4 6 4 7 6 6 ...
    ##  $ Q77_7      : int  9 9 9 9 8 8 9 5 9 8 ...
    ##  $ Q77_8      : int  6 9 5 4 2 5 6 5 5 4 ...
    ##  $ Q77_9      : int  7 8 7 5 4 8 7 5 5 6 ...
    ##  $ Q77_10     : int  7 9 8 9 6 8 8 7 9 7 ...
    ##  $ Q77_11     : int  9 9 8 9 8 9 6 7 5 7 ...
    ##  $ Q77_12     : int  8 7 6 9 5 9 6 6 8 8 ...
    ##  $ Q77_13     : int  7 7 3 4 2 7 5 6 7 4 ...
    ##  $ Q77_14     : int  8 7 5 6 4 6 6 7 8 5 ...
    ##  $ Q75        : int  3 6 3 5 5 4 4 4 3 5 ...
    ##  $ Q77        : int  5 6 4 3 4 3 6 4 2 4 ...
    ##  $ Q76        : int  2 7 2 5 3 4 3 5 3 6 ...
    ##  $ Q74        : int  2 7 2 5 7 3 2 5 3 5 ...
    ##  $ Q70        : int  4 7 4 5 3 3 4 5 2 5 ...
    ##  $ Q78_1      : int  12 15 8 7 14 6 5 8 7 8 ...
    ##  $ Q78_2      : int  6 10 14 6 13 5 14 9 8 7 ...
    ##  $ Q78_3      : int  1 13 15 5 15 1 9 10 10 12 ...
    ##  $ Q78_4      : int  9 2 5 3 6 9 6 4 15 3 ...
    ##  $ Q78_5      : int  5 1 6 1 1 8 2 11 1 1 ...
    ##  $ Q78_6      : int  8 9 10 10 10 14 15 7 5 5 ...
    ##  $ Q78_7      : int  13 7 12 11 8 2 10 12 3 10 ...
    ##  $ Q78_8      : int  14 11 11 9 12 11 11 13 6 13 ...
    ##  $ Q78_9      : int  15 12 13 13 7 15 13 14 11 9 ...
    ##  $ Q78_10     : int  4 14 4 4 2 7 1 2 2 2 ...
    ##  $ Q78_11     : int  3 8 7 2 9 13 12 3 14 6 ...
    ##  $ Q78_12     : int  2 6 2 8 3 4 7 1 12 11 ...
    ##  $ Q78_13     : int  7 4 3 12 5 10 8 5 4 4 ...
    ##  $ Q78_14     : int  10 5 9 14 11 3 3 6 9 14 ...
    ##  $ Q78_15     : int  11 3 1 15 4 12 4 15 13 15 ...
    ##  $ Q95        : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q94        : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q96_4      : int  5 7 6 5 5 5 5 5 7 7 ...
    ##  $ Q96_8      : int  4 7 5 4 5 5 3 5 3 5 ...
    ##  $ Q96_9      : int  5 7 4 3 6 5 4 2 7 6 ...
    ##  $ Q96_10     : int  5 7 6 5 6 5 5 4 7 6 ...
    ##  $ Q96_11     : int  5 7 5 6 6 5 3 4 3 6 ...
    ##  $ Q96_12     : int  5 7 6 6 6 6 4 4 4 6 ...
    ##  $ Q98        : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q101       : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Q102       : int  5 7 2 5 3 5 5 2 1 6 ...
    ##  $ Q17        : chr  "Female" "Female" "Female" "Female" ...
    ##  $ Q11        : chr  "Before age 4." "Between 5 - 10 years of age." "Before age 4." "Before age 4." ...
    ##  $ Q13        : chr  "1st year" "3rd year" "3rd year" "4th year +" ...
    ##  $ Q15        : chr  "East Asian or Pacific Islander" "East Asian or Pacific Islander" "East Asian or Pacific Islander" "South Asian (e.g. India)" ...
    ##  $ Q15_TEXT   : chr  NA NA NA NA ...
    ##  $ Q19        : chr  "Science" "Human Kinetics" "Arts - not psychology" "Land and Food Systems" ...
    ##  $ Q84        : chr  "Computer Science" "Kinesiology health science" "Visual arts" "Nutritional sciences" ...
    ##  $ Q85        : chr  "Mostly Heterosexual" "Mostly Heterosexual" "Mostly Heterosexual" "Mostly Heterosexual" ...
    ##  $ Q85_TEXT   : chr  NA NA NA NA ...
    ##  $ Q18        : chr  "The probability of people going into nursing" "Public interest and opinion in nursing today" "How the average student feels towards nursing." "To determine what types of individuals are interested in nursing" ...
    ##  $ DO-BR-FL_11: chr  "Explicit Nursing Interest|Nursing Experiences" "Nursing Experiences|Explicit Nursing Interest" "Explicit Nursing Interest|Nursing Experiences" "Nursing Experiences|Explicit Nursing Interest" ...
    ##  - attr(*, "spec")=List of 2
    ##   ..$ cols   :List of 63
    ##   .. ..$ V1         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ V8         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ V9         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ V10        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q2         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Q93        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Q80        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_1      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_2      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_3      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_4      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_5      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_6      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_7      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_8      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_9      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_10     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_11     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_12     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_13     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77_14     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q75        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q77        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q76        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q74        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q70        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_1      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_2      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_3      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_4      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_5      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_6      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_7      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_8      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_9      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_10     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_11     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_12     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_13     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_14     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q78_15     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q95        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q94        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q96_4      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q96_8      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q96_9      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q96_10     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q96_11     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q96_12     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q98        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q101       : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q102       : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Q17        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Q11        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Q13        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Q15        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Q15_TEXT   : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Q19        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Q84        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Q85        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Q85_TEXT   : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Q18        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ DO-BR-FL_11: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   ..$ default: list()
    ##   .. ..- attr(*, "class")= chr  "collector_guess" "collector"
    ##   ..- attr(*, "class")= chr "col_spec"

-   Then I ranmed the variables so they make more intuitive sense, having things named V2 does not make sense

``` r
raw_renamed <- rawdata %>% rename(ID= V1, start= V8, Ra_ID=Q2,
                   end= V9, log= Q93,
                   helping = Q77_1, status=Q77_2, working_people=Q77_3,
                   achievement=Q77_4, attenting= Q77_5, self_promo= Q77_6, connection=Q77_7,
                   power=Q77_8, intimacy= Q77_9, caring=Q77_10,
                   independence=Q77_11, serving_humanity=Q77_12, competition=Q77_13,
                   recognition=Q77_14, rate_men_nursing=Q75,male_nurse_positive=Q77,
                   interest_nursing=Q76, enjoy_nursing=Q74, imagine_nursing=Q70, 
                   rank_lawclerk= Q78_1, rank_ITassistant=Q78_2, rank_softwareENG=Q78_3,
                   rank_nurse=Q78_4,rank_teacher=Q78_5,rank_immigrationofficer=Q78_6,
                   rank_businessanalyst=Q78_7, rank_accountant=Q78_8,rank_insurance_agent=Q78_9,
                   rank_counselor=Q78_10, rank_pharmacist=Q78_11, rank_graphicdesigner=Q78_12,
                   rank_HRmanager=Q78_13,rank_copywriter=Q78_14,rank_curator=Q78_15,
                   fit1=Q96_4,fit2=Q96_8, fit3=Q96_9,fit4=Q96_10, fit5=Q96_11,fit6=Q96_12,
                   nursing_overlap=Q102, gender=Q17,
                   when_learn_english=Q11,year_standing=Q13,
                   ethnicity=Q15,ethnicity_other=Q15_TEXT,faculty=Q19,
                   major=Q84,sexual_orientation=Q85,sexual_other=Q85_TEXT,
                   what_about=Q18)
                   
# Drop Q80, Q95, Q94, =Q98, Q101, which are not really variables at all
raw_renamed <- raw_renamed %>% select(-Q80, -Q95, -Q94, -Q98, -Q101)
```

\*then i make composite variables

``` r
raw_renamed <- 
  raw_renamed %>% 
  mutate(communal_values= (helping+working_people+attenting+connection+caring+serving_humanity)/6,
         agentic_values= (status+achievement+self_promo+power+intimacy+independence+competition+recognition)/7, fit_nursing= (fit1+fit2+fit3+fit4+fit5+fit6)/6, interest_comp = (interest_nursing+enjoy_nursing+imagine_nursing)/3)
```

-   Dropping some more variables: I drop the individual communal and agentic values here bc we can be quite confident in those, as we have previously used them.

``` r
CLEANdata <- raw_renamed %>% select (-V10, -start, -helping, -working_people, -attenting, -connection, -caring,-serving_humanity, - status, -achievement, -self_promo, -power, -intimacy, -independence, -competition, -recognition)

# looks better now
str (CLEANdata)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    225 obs. of  46 variables:
    ##  $ ID                     : chr  "R_xu49biNxFqKA2NH" "R_3COXnCGhZL6cNxg" "R_2EGm44v2wnZrAhT" "R_3kgDFxiJYEMGIui" ...
    ##  $ end                    : chr  "2016-10-06 9:15" "2016-10-06 9:30" "2016-10-06 9:44" "2016-10-06 10:00" ...
    ##  $ Ra_ID                  : chr  NA NA NA NA ...
    ##  $ log                    : chr  "A" "A" "A" "A" ...
    ##  $ rate_men_nursing       : int  3 6 3 5 5 4 4 4 3 5 ...
    ##  $ male_nurse_positive    : int  5 6 4 3 4 3 6 4 2 4 ...
    ##  $ interest_nursing       : int  2 7 2 5 3 4 3 5 3 6 ...
    ##  $ enjoy_nursing          : int  2 7 2 5 7 3 2 5 3 5 ...
    ##  $ imagine_nursing        : int  4 7 4 5 3 3 4 5 2 5 ...
    ##  $ rank_lawclerk          : int  12 15 8 7 14 6 5 8 7 8 ...
    ##  $ rank_ITassistant       : int  6 10 14 6 13 5 14 9 8 7 ...
    ##  $ rank_softwareENG       : int  1 13 15 5 15 1 9 10 10 12 ...
    ##  $ rank_nurse             : int  9 2 5 3 6 9 6 4 15 3 ...
    ##  $ rank_teacher           : int  5 1 6 1 1 8 2 11 1 1 ...
    ##  $ rank_immigrationofficer: int  8 9 10 10 10 14 15 7 5 5 ...
    ##  $ rank_businessanalyst   : int  13 7 12 11 8 2 10 12 3 10 ...
    ##  $ rank_accountant        : int  14 11 11 9 12 11 11 13 6 13 ...
    ##  $ rank_insurance_agent   : int  15 12 13 13 7 15 13 14 11 9 ...
    ##  $ rank_counselor         : int  4 14 4 4 2 7 1 2 2 2 ...
    ##  $ rank_pharmacist        : int  3 8 7 2 9 13 12 3 14 6 ...
    ##  $ rank_graphicdesigner   : int  2 6 2 8 3 4 7 1 12 11 ...
    ##  $ rank_HRmanager         : int  7 4 3 12 5 10 8 5 4 4 ...
    ##  $ rank_copywriter        : int  10 5 9 14 11 3 3 6 9 14 ...
    ##  $ rank_curator           : int  11 3 1 15 4 12 4 15 13 15 ...
    ##  $ fit1                   : int  5 7 6 5 5 5 5 5 7 7 ...
    ##  $ fit2                   : int  4 7 5 4 5 5 3 5 3 5 ...
    ##  $ fit3                   : int  5 7 4 3 6 5 4 2 7 6 ...
    ##  $ fit4                   : int  5 7 6 5 6 5 5 4 7 6 ...
    ##  $ fit5                   : int  5 7 5 6 6 5 3 4 3 6 ...
    ##  $ fit6                   : int  5 7 6 6 6 6 4 4 4 6 ...
    ##  $ nursing_overlap        : int  5 7 2 5 3 5 5 2 1 6 ...
    ##  $ gender                 : chr  "Female" "Female" "Female" "Female" ...
    ##  $ when_learn_english     : chr  "Before age 4." "Between 5 - 10 years of age." "Before age 4." "Before age 4." ...
    ##  $ year_standing          : chr  "1st year" "3rd year" "3rd year" "4th year +" ...
    ##  $ ethnicity              : chr  "East Asian or Pacific Islander" "East Asian or Pacific Islander" "East Asian or Pacific Islander" "South Asian (e.g. India)" ...
    ##  $ ethnicity_other        : chr  NA NA NA NA ...
    ##  $ faculty                : chr  "Science" "Human Kinetics" "Arts - not psychology" "Land and Food Systems" ...
    ##  $ major                  : chr  "Computer Science" "Kinesiology health science" "Visual arts" "Nutritional sciences" ...
    ##  $ sexual_orientation     : chr  "Mostly Heterosexual" "Mostly Heterosexual" "Mostly Heterosexual" "Mostly Heterosexual" ...
    ##  $ sexual_other           : chr  NA NA NA NA ...
    ##  $ what_about             : chr  "The probability of people going into nursing" "Public interest and opinion in nursing today" "How the average student feels towards nursing." "To determine what types of individuals are interested in nursing" ...
    ##  $ DO-BR-FL_11            : chr  "Explicit Nursing Interest|Nursing Experiences" "Nursing Experiences|Explicit Nursing Interest" "Explicit Nursing Interest|Nursing Experiences" "Nursing Experiences|Explicit Nursing Interest" ...
    ##  $ communal_values        : num  7.5 8.67 7.33 9 6.17 ...
    ##  $ agentic_values         : num  7.86 9.57 6.86 7.29 5.14 ...
    ##  $ fit_nursing            : num  4.83 7 5.33 4.83 5.67 ...
    ##  $ interest_comp          : num  2.67 7 2.67 5 4.33 ...

0.2) Revaluing ethnicity to get bigger groups.
==============================================

-   I choose to revalue my ethinicity factor here, I have to many groups,

``` r
# so here you can see there is a lot of ethnicity groups
CLEANdata %>% select (ethnicity) %>% 
                      group_by(ethnicity) %>% summarize(particicipants=n()) %>% kable
```

| ethnicity                                  |  particicipants|
|:-------------------------------------------|---------------:|
| Black/African American/Canadian            |               2|
| East Asian or Pacific Islander             |             108|
| Hispanic/Latino                            |               2|
| Middle Eastern                             |               4|
| More than one                              |              14|
| Native Canadian/Aboriginal/Native American |               3|
| Other (please specify)                     |              13|
| South Asian (e.g. India)                   |              20|
| White/Caucasian                            |              58|
| NA                                         |               1|

``` r
CLEANdata$ethcode <- plyr::revalue(CLEANdata$ethnicity, c("East Asian or Pacific Islander"="Asian","South Asian (e.g. India)"="Asian", "White/Caucasian"="White", "Black/African American/Canadian"="Other", "Hispanic/Latino"="Other","Middle Eastern"="Other","More than one"="Other","Native Canadian/Aboriginal/Native American"="Other", "Other (please specify)"="Other"))

CLEANdata %>% 
  group_by(ethcode) %>% select (ethnicity) %>% 
  summarize(particicipants=n()) %>% kable
```

    ## Adding missing grouping variables: `ethcode`

| ethcode |  particicipants|
|:--------|---------------:|
| Asian   |             128|
| Other   |              38|
| White   |              58|
| NA      |               1|

``` r
CLEANdata$gender_num <- plyr::revalue(CLEANdata$gender, c("Male"=0, "Female"=1, "Neither reflects my gender identity"=3))
CLEANdata$gender_num <- as.numeric(CLEANdata$gender_num)
str(CLEANdata$gender_num)
```

    ##  num [1:225] 1 1 1 1 1 1 1 1 1 1 ...

1) Exclusions
=============

I decided to drop those who identify as **anything other than heterosexual** and\* **didn't identify as male or female**

``` r
#make this a factor instead of a character
CLEANdata$sexual_orientation <- as.factor(CLEANdata$sexual_orientation)

str(CLEANdata$sexual_orientation)
```

    ##  Factor w/ 4 levels "Bisexual","Mostly Heterosexual",..: 2 2 2 2 2 2 3 2 2 2 ...

``` r
CLEAN_hetero <- CLEANdata %>% 
  filter(sexual_orientation != "Mostly Homosexual/Gay/Lesbian" & 
           sexual_orientation != "Bisexual" & gender != "Neither reflects my gender identity")

# this dropped a total of 31 participants!
```

2) Write Clean data
===================

I then write the clean data to file so I can use that one for my next script

``` r
write_csv(CLEAN_hetero,"/Users/KatharinaBlock/Desktop/Git_REPOS/NursingDVPilot_oct2016/Nursing2pilot_clean_hetero.csv")
```
