P8105\_hw2
================
yz4188
2021-10-08

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(dplyr)
```

### Problem 1

## Read and clean the Mr. Trash Wheel sheet

``` r
TrashWheel_df = read_excel("data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx",
  sheet = "Mr. Trash Wheel", range = cell_cols("A:N")) %>% 
janitor::clean_names() %>% 
drop_na(dumpster) %>% 
mutate(sports_balls = round(sports_balls))
```

## Read and clean precipitation data for 2018 and 2019

``` r
precipitation2018_df = read_excel("data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", 
  sheet = "2018 Precipitation", skip = 1) %>% 
janitor::clean_names() %>% 
drop_na(month) %>% 
mutate(year = 2018, month = month.name) 

precipitation2019_df = read_excel("data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", 
  sheet = "2019 Precipitation", skip = 1) %>% 
janitor::clean_names() %>% 
drop_na(month) %>% 
mutate(year = 2019, month = month.name) 
```

\#\#\#\#Conclusions for this problem 1 For “TrashWheel\_df” observations
is the “nrow(TrashWheel\_df)” For “precipitation2018\_df” observations
is“nrow(precipitation2018\_df)” For “precipitation2019\_df” observations
is “nrow(precipitation2019\_df)” For “total precipitation in 2018” is
“sum(pull(precipitation2018\_df, total))”  
For “median number of sports balls in a dumpster in 2019” is
“median(pull(TrashWheel\_df,sports\_balls))”

### Problem 2

``` r
pols_data = read_csv("data/pols-month.csv") %>% 
separate(col = mon, into = c("year", "month", "day")) %>% 
mutate(year = as.character(year), 
       month = as.integer(month),
       month = month.name[month],
president = ifelse(prez_dem =="1","dem","gop")) %>% 
select(-prez_dem, -prez_gop, -day) 
```

    ## Rows: 822 Columns: 9

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp_data = read_csv("data/snp.csv") %>% 
mutate(date = lubridate::mdy(date)) %>%
separate(col = date, into = c("year", "month", "day")) %>% 
mutate(year = as.numeric(year),
       year = ifelse(year > 2021, year - 100, year)) %>% 
       arrange(year,month) %>% 
mutate(year = as.character(year),
       month = as.integer(month),
       month = month.name[month]) %>%
select(-day)
```

    ## Rows: 787 Columns: 2

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemployment_data = read_csv("data/unemployment.csv") %>% 
     pivot_longer(Jan:Dec,
                 names_to = "month",
                 values_to = "unemployment_percentage") %>%
    mutate(Year = as.character(Year),
           month = match(month,month.abb),
           month = month.name[month]) %>%
    rename(year = Year) %>%
    relocate(year, month) %>% 
    arrange(year,month)
```

    ## Rows: 68 Columns: 13

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
result_data= left_join(left_join(pols_data,snp_data),unemployment_data)
```

    ## Joining, by = c("year", "month")

    ## Joining, by = c("year", "month")

\#\#**pols\_data** The dimension is “nrow(pols\_data)” x "
ncol(pols\_data)“. The range of this data
is”min(range(pull(pols\_data,year))):
max(range(pull(pols\_data,year)))“. The names of key variables:”
names(pols\_data)".

\#\#**snp\_data**

The dimension is “nrow(snp\_data)” “ncol(snp\_data)”. Then range of this
data is “min(range(pull(snp\_data ,year))) : max(range(pull(snp\_data
,year)))” The names of key variables: " names(snp\_data)".

**unemployment\_data** The dimension is “nrow(unemployment\_data)”
“ncol(unemployment\_data)”. Then range of this data is
“min(range(pull(unemployment\_data ,year))) :
max(range(pull(unemployment\_data ,year)))” The names of key variables:
" names(unemployment\_data)".

**result\_data** The dimension is “nrow(result\_data)”
“ncol(result\_data)”. Then range of this data is
“min(range(pull(unemployment\_data ,year))) :
max(range(pull(unemployment\_data ,year)))” The names of key variables:
" names(unemployment\_data)".

## Problem 3

``` r
babynames_data = read_csv("data/Popular_Baby_Names.csv") 
```

    ## Rows: 19418 Columns: 6

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Gender, Ethnicity, Child's First Name
    ## dbl (3): Year of Birth, Count, Rank

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
babynames_data %>% 
distinct(.keep_all = TRUE)
```

    ## # A tibble: 12,181 × 6
    ##    `Year of Birth` Gender Ethnicity              `Child's First Nam… Count  Rank
    ##              <dbl> <chr>  <chr>                  <chr>               <dbl> <dbl>
    ##  1            2016 FEMALE ASIAN AND PACIFIC ISL… Olivia                172     1
    ##  2            2016 FEMALE ASIAN AND PACIFIC ISL… Chloe                 112     2
    ##  3            2016 FEMALE ASIAN AND PACIFIC ISL… Sophia                104     3
    ##  4            2016 FEMALE ASIAN AND PACIFIC ISL… Emily                  99     4
    ##  5            2016 FEMALE ASIAN AND PACIFIC ISL… Emma                   99     4
    ##  6            2016 FEMALE ASIAN AND PACIFIC ISL… Mia                    79     5
    ##  7            2016 FEMALE ASIAN AND PACIFIC ISL… Charlotte              59     6
    ##  8            2016 FEMALE ASIAN AND PACIFIC ISL… Sarah                  57     7
    ##  9            2016 FEMALE ASIAN AND PACIFIC ISL… Isabella               56     8
    ## 10            2016 FEMALE ASIAN AND PACIFIC ISL… Hannah                 56     8
    ## # … with 12,171 more rows
