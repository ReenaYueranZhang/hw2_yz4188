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
library(ggplot2)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

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
name = read.csv("data/Popular_Baby_Names.csv") %>%
distinct()%>%
select(Year.of.Birth, Gender, Ethnicity, Child.s.First.Name, Rank) %>%
filter(Gender == "FEMALE", Child.s.First.Name == "Olivia")
name
```

    ##    Year.of.Birth Gender                  Ethnicity Child.s.First.Name Rank
    ## 1           2016 FEMALE ASIAN AND PACIFIC ISLANDER             Olivia    1
    ## 2           2016 FEMALE         BLACK NON HISPANIC             Olivia    8
    ## 3           2016 FEMALE                   HISPANIC             Olivia   13
    ## 4           2016 FEMALE         WHITE NON HISPANIC             Olivia    1
    ## 5           2015 FEMALE ASIAN AND PACIFIC ISLANDER             Olivia    1
    ## 6           2015 FEMALE         BLACK NON HISPANIC             Olivia    4
    ## 7           2015 FEMALE                   HISPANIC             Olivia   16
    ## 8           2015 FEMALE         WHITE NON HISPANIC             Olivia    1
    ## 9           2014 FEMALE ASIAN AND PACIFIC ISLANDER             Olivia    1
    ## 10          2014 FEMALE         BLACK NON HISPANIC             Olivia    8
    ## 11          2014 FEMALE                   HISPANIC             Olivia   16
    ## 12          2014 FEMALE         WHITE NON HISPANIC             Olivia    1
    ## 13          2013 FEMALE ASIAN AND PACIFIC ISLANDER             Olivia    3
    ## 14          2013 FEMALE         BLACK NON HISPANIC             Olivia    6
    ## 15          2013 FEMALE                   HISPANIC             Olivia   22
    ## 16          2013 FEMALE         WHITE NON HISPANIC             Olivia    1

``` r
name = read.csv("data/Popular_Baby_Names.csv") %>%
distinct()%>%
select(Year.of.Birth, Gender, Ethnicity, Child.s.First.Name, Rank) %>%
filter(Gender == "MALE", Rank == 1) 
name
```

    ##    Year.of.Birth Gender                  Ethnicity Child.s.First.Name Rank
    ## 1           2016   MALE ASIAN AND PACIFIC ISLANDER              Ethan    1
    ## 2           2016   MALE         BLACK NON HISPANIC               Noah    1
    ## 3           2016   MALE                   HISPANIC               Liam    1
    ## 4           2016   MALE         WHITE NON HISPANIC             Joseph    1
    ## 5           2015   MALE ASIAN AND PACIFIC ISLANDER             Jayden    1
    ## 6           2015   MALE         BLACK NON HISPANIC               Noah    1
    ## 7           2015   MALE                   HISPANIC               Liam    1
    ## 8           2015   MALE         WHITE NON HISPANIC              David    1
    ## 9           2014   MALE ASIAN AND PACIFIC ISLANDER             Jayden    1
    ## 10          2014   MALE         BLACK NON HISPANIC              Ethan    1
    ## 11          2014   MALE                   HISPANIC               Liam    1
    ## 12          2014   MALE         WHITE NON HISPANIC             Joseph    1
    ## 13          2013   MALE ASIAN AND PACIFIC ISLANDER             Jayden    1
    ## 14          2013   MALE         BLACK NON HISPANIC              Ethan    1
    ## 15          2013   MALE                   HISPANIC             Jayden    1
    ## 16          2013   MALE         WHITE NON HISPANIC              David    1
    ## 17          2012   MALE             ASIAN AND PACI               RYAN    1
    ## 18          2012   MALE             BLACK NON HISP             JAYDEN    1
    ## 19          2012   MALE                   HISPANIC             JAYDEN    1
    ## 20          2012   MALE             WHITE NON HISP             JOSEPH    1
    ## 21          2011   MALE ASIAN AND PACIFIC ISLANDER              ETHAN    1
    ## 22          2011   MALE         BLACK NON HISPANIC             JAYDEN    1
    ## 23          2011   MALE                   HISPANIC             JAYDEN    1
    ## 24          2011   MALE         WHITE NON HISPANIC            MICHAEL    1

``` r
name = read.csv("data/Popular_Baby_Names.csv") %>%
distinct()%>%
select(Year.of.Birth, Gender, Ethnicity, Child.s.First.Name, Rank, Count) %>%
filter(Gender == "MALE", Ethnicity == "WHITE NON HISPANIC", Year.of.Birth == 2016) 
name
```

    ##     Year.of.Birth Gender          Ethnicity Child.s.First.Name Rank Count
    ## 1            2016   MALE WHITE NON HISPANIC             Joseph    1   261
    ## 2            2016   MALE WHITE NON HISPANIC            Michael    2   260
    ## 3            2016   MALE WHITE NON HISPANIC              David    3   255
    ## 4            2016   MALE WHITE NON HISPANIC              Moshe    4   239
    ## 5            2016   MALE WHITE NON HISPANIC              Jacob    5   236
    ## 6            2016   MALE WHITE NON HISPANIC              James    6   231
    ## 7            2016   MALE WHITE NON HISPANIC           Benjamin    7   219
    ## 8            2016   MALE WHITE NON HISPANIC          Alexander    8   211
    ## 9            2016   MALE WHITE NON HISPANIC             Daniel    9   196
    ## 10           2016   MALE WHITE NON HISPANIC              Henry    9   196
    ## 11           2016   MALE WHITE NON HISPANIC               Adam   10   178
    ## 12           2016   MALE WHITE NON HISPANIC               Jack   10   178
    ## 13           2016   MALE WHITE NON HISPANIC            William   11   169
    ## 14           2016   MALE WHITE NON HISPANIC            Abraham   12   163
    ## 15           2016   MALE WHITE NON HISPANIC             Samuel   13   156
    ## 16           2016   MALE WHITE NON HISPANIC               Noah   14   147
    ## 17           2016   MALE WHITE NON HISPANIC               John   15   146
    ## 18           2016   MALE WHITE NON HISPANIC              Isaac   16   143
    ## 19           2016   MALE WHITE NON HISPANIC             Oliver   17   142
    ## 20           2016   MALE WHITE NON HISPANIC              Chaim   18   140
    ## 21           2016   MALE WHITE NON HISPANIC           Nicholas   19   136
    ## 22           2016   MALE WHITE NON HISPANIC              Ethan   20   133
    ## 23           2016   MALE WHITE NON HISPANIC               Ryan   21   129
    ## 24           2016   MALE WHITE NON HISPANIC               Liam   22   128
    ## 25           2016   MALE WHITE NON HISPANIC            Matthew   23   127
    ## 26           2016   MALE WHITE NON HISPANIC            Gabriel   24   124
    ## 27           2016   MALE WHITE NON HISPANIC                Leo   25   121
    ## 28           2016   MALE WHITE NON HISPANIC           Theodore   26   117
    ## 29           2016   MALE WHITE NON HISPANIC                Eli   26   117
    ## 30           2016   MALE WHITE NON HISPANIC              Lucas   27   115
    ## 31           2016   MALE WHITE NON HISPANIC              Yosef   28   114
    ## 32           2016   MALE WHITE NON HISPANIC             Thomas   29   112
    ## 33           2016   MALE WHITE NON HISPANIC            Charles   30   109
    ## 34           2016   MALE WHITE NON HISPANIC             Andrew   31   102
    ## 35           2016   MALE WHITE NON HISPANIC              Dylan   32   101
    ## 36           2016   MALE WHITE NON HISPANIC            Anthony   33   100
    ## 37           2016   MALE WHITE NON HISPANIC             Shimon   33   100
    ## 38           2016   MALE WHITE NON HISPANIC                Max   34    99
    ## 39           2016   MALE WHITE NON HISPANIC            Yisroel   35    98
    ## 40           2016   MALE WHITE NON HISPANIC               Luke   36    97
    ## 41           2016   MALE WHITE NON HISPANIC          Mordechai   37    91
    ## 42           2016   MALE WHITE NON HISPANIC             Yehuda   38    88
    ## 43           2016   MALE WHITE NON HISPANIC           Menachem   39    87
    ## 44           2016   MALE WHITE NON HISPANIC               Luca   40    85
    ## 45           2016   MALE WHITE NON HISPANIC             Julian   40    85
    ## 46           2016   MALE WHITE NON HISPANIC          Sebastian   41    84
    ## 47           2016   MALE WHITE NON HISPANIC            Zachary   42    80
    ## 48           2016   MALE WHITE NON HISPANIC            Jackson   43    77
    ## 49           2016   MALE WHITE NON HISPANIC             George   44    76
    ## 50           2016   MALE WHITE NON HISPANIC              Mason   45    73
    ## 51           2016   MALE WHITE NON HISPANIC               Owen   46    71
    ## 52           2016   MALE WHITE NON HISPANIC        Christopher   46    71
    ## 53           2016   MALE WHITE NON HISPANIC           Jonathan   47    70
    ## 54           2016   MALE WHITE NON HISPANIC              Asher   48    68
    ## 55           2016   MALE WHITE NON HISPANIC             Robert   48    68
    ## 56           2016   MALE WHITE NON HISPANIC              Aiden   49    67
    ## 57           2016   MALE WHITE NON HISPANIC             Nathan   49    67
    ## 58           2016   MALE WHITE NON HISPANIC           Yitzchok   49    67
    ## 59           2016   MALE WHITE NON HISPANIC             Shlomo   49    67
    ## 60           2016   MALE WHITE NON HISPANIC               Omar   50    64
    ## 61           2016   MALE WHITE NON HISPANIC             Hudson   51    63
    ## 62           2016   MALE WHITE NON HISPANIC             Joshua   52    62
    ## 63           2016   MALE WHITE NON HISPANIC              Miles   52    62
    ## 64           2016   MALE WHITE NON HISPANIC               Mark   53    61
    ## 65           2016   MALE WHITE NON HISPANIC               Aron   53    61
    ## 66           2016   MALE WHITE NON HISPANIC            Maxwell   54    60
    ## 67           2016   MALE WHITE NON HISPANIC             Shmuel   54    60
    ## 68           2016   MALE WHITE NON HISPANIC          Nathaniel   55    58
    ## 69           2016   MALE WHITE NON HISPANIC              Logan   56    57
    ## 70           2016   MALE WHITE NON HISPANIC              Yakov   56    57
    ## 71           2016   MALE WHITE NON HISPANIC               Meir   56    57
    ## 72           2016   MALE WHITE NON HISPANIC             Connor   56    57
    ## 73           2016   MALE WHITE NON HISPANIC               Levi   57    56
    ## 74           2016   MALE WHITE NON HISPANIC              Aaron   58    55
    ## 75           2016   MALE WHITE NON HISPANIC               Ezra   59    54
    ## 76           2016   MALE WHITE NON HISPANIC              Shaya   60    53
    ## 77           2016   MALE WHITE NON HISPANIC                Ali   60    53
    ## 78           2016   MALE WHITE NON HISPANIC                Dov   61    51
    ## 79           2016   MALE WHITE NON HISPANIC              Simon   61    51
    ## 80           2016   MALE WHITE NON HISPANIC               Tzvi   62    49
    ## 81           2016   MALE WHITE NON HISPANIC                Ari   62    49
    ## 82           2016   MALE WHITE NON HISPANIC              Oscar   63    48
    ## 83           2016   MALE WHITE NON HISPANIC             Edward   64    47
    ## 84           2016   MALE WHITE NON HISPANIC            Vincent   64    47
    ## 85           2016   MALE WHITE NON HISPANIC              Jonah   64    47
    ## 86           2016   MALE WHITE NON HISPANIC             Adrian   64    47
    ## 87           2016   MALE WHITE NON HISPANIC             Hunter   64    47
    ## 88           2016   MALE WHITE NON HISPANIC            Avraham   64    47
    ## 89           2016   MALE WHITE NON HISPANIC           Harrison   65    46
    ## 90           2016   MALE WHITE NON HISPANIC                Zev   66    45
    ## 91           2016   MALE WHITE NON HISPANIC             Yaakov   67    43
    ## 92           2016   MALE WHITE NON HISPANIC            Solomon   67    43
    ## 93           2016   MALE WHITE NON HISPANIC          Christian   68    42
    ## 94           2016   MALE WHITE NON HISPANIC            Eliezer   69    41
    ## 95           2016   MALE WHITE NON HISPANIC               Leon   69    41
    ## 96           2016   MALE WHITE NON HISPANIC              Peter   69    41
    ## 97           2016   MALE WHITE NON HISPANIC           Leonardo   69    41
    ## 98           2016   MALE WHITE NON HISPANIC              Wyatt   70    39
    ## 99           2016   MALE WHITE NON HISPANIC               Jake   70    39
    ## 100          2016   MALE WHITE NON HISPANIC               Cole   71    38
    ## 101          2016   MALE WHITE NON HISPANIC               Evan   71    38
    ## 102          2016   MALE WHITE NON HISPANIC              Aidan   71    38
    ## 103          2016   MALE WHITE NON HISPANIC              Dovid   71    38
    ## 104          2016   MALE WHITE NON HISPANIC            Mohamed   72    37
    ## 105          2016   MALE WHITE NON HISPANIC              Nolan   72    37
    ## 106          2016   MALE WHITE NON HISPANIC             Mendel   72    37
    ## 107          2016   MALE WHITE NON HISPANIC              Moses   72    37
    ## 108          2016   MALE WHITE NON HISPANIC            Grayson   72    37
    ## 109          2016   MALE WHITE NON HISPANIC                Sam   72    37
    ## 110          2016   MALE WHITE NON HISPANIC            Patrick   73    36
    ## 111          2016   MALE WHITE NON HISPANIC              Judah   74    35
    ## 112          2016   MALE WHITE NON HISPANIC              Gavin   74    35
    ## 113          2016   MALE WHITE NON HISPANIC               Finn   75    34
    ## 114          2016   MALE WHITE NON HISPANIC             Israel   75    34
    ## 115          2016   MALE WHITE NON HISPANIC             Arthur   75    34
    ## 116          2016   MALE WHITE NON HISPANIC               Dean   75    34
    ## 117          2016   MALE WHITE NON HISPANIC             Elijah   76    33
    ## 118          2016   MALE WHITE NON HISPANIC              Ahmed   76    33
    ## 119          2016   MALE WHITE NON HISPANIC             Shulem   76    33
    ## 120          2016   MALE WHITE NON HISPANIC            Nicolas   77    32
    ## 121          2016   MALE WHITE NON HISPANIC              Elias   77    32
    ## 122          2016   MALE WHITE NON HISPANIC            Raphael   78    31
    ## 123          2016   MALE WHITE NON HISPANIC              Chase   78    31
    ## 124          2016   MALE WHITE NON HISPANIC               Paul   78    31
    ## 125          2016   MALE WHITE NON HISPANIC             Elliot   78    31
    ## 126          2016   MALE WHITE NON HISPANIC             Shmiel   78    31
    ## 127          2016   MALE WHITE NON HISPANIC              Rocco   79    30
    ## 128          2016   MALE WHITE NON HISPANIC            Antonio   79    30
    ## 129          2016   MALE WHITE NON HISPANIC              Aryeh   79    30
    ## 130          2016   MALE WHITE NON HISPANIC              Colin   79    30
    ## 131          2016   MALE WHITE NON HISPANIC               Lipa   80    29
    ## 132          2016   MALE WHITE NON HISPANIC                Ben   80    29
    ## 133          2016   MALE WHITE NON HISPANIC              Frank   80    29
    ## 134          2016   MALE WHITE NON HISPANIC             Simcha   80    29
    ## 135          2016   MALE WHITE NON HISPANIC            Eliyahu   80    29
    ## 136          2016   MALE WHITE NON HISPANIC             Wesley   80    29
    ## 137          2016   MALE WHITE NON HISPANIC              Tyler   80    29
    ## 138          2016   MALE WHITE NON HISPANIC                Kai   80    29
    ## 139          2016   MALE WHITE NON HISPANIC             Cooper   80    29
    ## 140          2016   MALE WHITE NON HISPANIC               Amir   80    29
    ## 141          2016   MALE WHITE NON HISPANIC             Matteo   81    28
    ## 142          2016   MALE WHITE NON HISPANIC              Roman   81    28
    ## 143          2016   MALE WHITE NON HISPANIC             Hershy   81    28
    ## 144          2016   MALE WHITE NON HISPANIC              Mayer   81    28
    ## 145          2016   MALE WHITE NON HISPANIC            Yechiel   81    28
    ## 146          2016   MALE WHITE NON HISPANIC              Lukas   82    27
    ## 147          2016   MALE WHITE NON HISPANIC            Richard   82    27
    ## 148          2016   MALE WHITE NON HISPANIC            Naftali   82    27
    ## 149          2016   MALE WHITE NON HISPANIC            Naftuli   82    27
    ## 150          2016   MALE WHITE NON HISPANIC             Philip   82    27
    ## 151          2016   MALE WHITE NON HISPANIC             Parker   82    27
    ## 152          2016   MALE WHITE NON HISPANIC              Lazer   82    27
    ## 153          2016   MALE WHITE NON HISPANIC               Milo   82    27
    ## 154          2016   MALE WHITE NON HISPANIC              Rayan   82    27
    ## 155          2016   MALE WHITE NON HISPANIC               Alan   82    27
    ## 156          2016   MALE WHITE NON HISPANIC             Rafael   82    27
    ## 157          2016   MALE WHITE NON HISPANIC               Luka   82    27
    ## 158          2016   MALE WHITE NON HISPANIC             Declan   83    26
    ## 159          2016   MALE WHITE NON HISPANIC             Graham   83    26
    ## 160          2016   MALE WHITE NON HISPANIC               Alex   83    26
    ## 161          2016   MALE WHITE NON HISPANIC               Jude   83    26
    ## 162          2016   MALE WHITE NON HISPANIC            Charlie   83    26
    ## 163          2016   MALE WHITE NON HISPANIC             Yousef   83    26
    ## 164          2016   MALE WHITE NON HISPANIC             August   83    26
    ## 165          2016   MALE WHITE NON HISPANIC             Jordan   84    25
    ## 166          2016   MALE WHITE NON HISPANIC              Yusuf   84    25
    ## 167          2016   MALE WHITE NON HISPANIC            Elliott   84    25
    ## 168          2016   MALE WHITE NON HISPANIC           Giovanni   84    25
    ## 169          2016   MALE WHITE NON HISPANIC               Theo   84    25
    ## 170          2016   MALE WHITE NON HISPANIC             Justin   84    25
    ## 171          2016   MALE WHITE NON HISPANIC              Hamza   84    25
    ## 172          2016   MALE WHITE NON HISPANIC             Austin   85    24
    ## 173          2016   MALE WHITE NON HISPANIC             Victor   85    24
    ## 174          2016   MALE WHITE NON HISPANIC             Carter   85    24
    ## 175          2016   MALE WHITE NON HISPANIC              Maxim   85    24
    ## 176          2016   MALE WHITE NON HISPANIC         Maximilian   85    24
    ## 177          2016   MALE WHITE NON HISPANIC              Jason   85    24
    ## 178          2016   MALE WHITE NON HISPANIC              Conor   85    24
    ## 179          2016   MALE WHITE NON HISPANIC            Hershel   85    24
    ## 180          2016   MALE WHITE NON HISPANIC             Emmett   85    24
    ## 181          2016   MALE WHITE NON HISPANIC              Ariel   85    24
    ## 182          2016   MALE WHITE NON HISPANIC              Quinn   85    24
    ## 183          2016   MALE WHITE NON HISPANIC            Cameron   85    24
    ## 184          2016   MALE WHITE NON HISPANIC               Shia   85    24
    ## 185          2016   MALE WHITE NON HISPANIC            Maximus   86    23
    ## 186          2016   MALE WHITE NON HISPANIC             Martin   86    23
    ## 187          2016   MALE WHITE NON HISPANIC              Felix   86    23
    ## 188          2016   MALE WHITE NON HISPANIC              Marco   86    23
    ## 189          2016   MALE WHITE NON HISPANIC            Lorenzo   86    23
    ## 190          2016   MALE WHITE NON HISPANIC                Lev   87    22
    ## 191          2016   MALE WHITE NON HISPANIC              Louis   87    22
    ## 192          2016   MALE WHITE NON HISPANIC              Rowan   87    22
    ## 193          2016   MALE WHITE NON HISPANIC            Timothy   87    22
    ## 194          2016   MALE WHITE NON HISPANIC                Ian   87    22
    ## 195          2016   MALE WHITE NON HISPANIC              Caleb   87    22
    ## 196          2016   MALE WHITE NON HISPANIC               Sean   87    22
    ## 197          2016   MALE WHITE NON HISPANIC            Spencer   87    22
    ## 198          2016   MALE WHITE NON HISPANIC              Grant   87    22
    ## 199          2016   MALE WHITE NON HISPANIC             Landon   87    22
    ## 200          2016   MALE WHITE NON HISPANIC            Avrohom   87    22
    ## 201          2016   MALE WHITE NON HISPANIC            Brandon   88    21
    ## 202          2016   MALE WHITE NON HISPANIC               Noam   88    21
    ## 203          2016   MALE WHITE NON HISPANIC              Ellis   88    21
    ## 204          2016   MALE WHITE NON HISPANIC               Reid   88    21
    ## 205          2016   MALE WHITE NON HISPANIC              Usher   88    21
    ## 206          2016   MALE WHITE NON HISPANIC            Youssef   88    21
    ## 207          2016   MALE WHITE NON HISPANIC           Yehoshua   88    21
    ## 208          2016   MALE WHITE NON HISPANIC              Akiva   88    21
    ## 209          2016   MALE WHITE NON HISPANIC            Dominic   88    21
    ## 210          2016   MALE WHITE NON HISPANIC            Cheskel   89    20
    ## 211          2016   MALE WHITE NON HISPANIC               Eric   89    20
    ## 212          2016   MALE WHITE NON HISPANIC             Shlome   89    20
    ## 213          2016   MALE WHITE NON HISPANIC            Gavriel   89    20
    ## 214          2016   MALE WHITE NON HISPANIC             Marcus   89    20
    ## 215          2016   MALE WHITE NON HISPANIC             Baruch   89    20
    ## 216          2016   MALE WHITE NON HISPANIC             Albert   89    20
    ## 217          2016   MALE WHITE NON HISPANIC           Meshilem   89    20
    ## 218          2016   MALE WHITE NON HISPANIC              Eitan   89    20
    ## 219          2016   MALE WHITE NON HISPANIC               Arlo   89    20
    ## 220          2016   MALE WHITE NON HISPANIC               Nico   90    19
    ## 221          2016   MALE WHITE NON HISPANIC                Avi   90    19
    ## 222          2016   MALE WHITE NON HISPANIC           Abdullah   90    19
    ## 223          2016   MALE WHITE NON HISPANIC             Jasper   90    19
    ## 224          2016   MALE WHITE NON HISPANIC              Jaxon   90    19
    ## 225          2016   MALE WHITE NON HISPANIC            Ibrahim   90    19
    ## 226          2016   MALE WHITE NON HISPANIC              Mendy   90    19
    ## 227          2016   MALE WHITE NON HISPANIC            Everett   90    19
    ## 228          2016   MALE WHITE NON HISPANIC            Bennett   90    19
    ## 229          2016   MALE WHITE NON HISPANIC            Shloimy   90    19
    ## 230          2016   MALE WHITE NON HISPANIC            Shloime   90    19
    ## 231          2016   MALE WHITE NON HISPANIC          Salvatore   90    19
    ## 232          2016   MALE WHITE NON HISPANIC               Hugo   91    18
    ## 233          2016   MALE WHITE NON HISPANIC             Efraim   91    18
    ## 234          2016   MALE WHITE NON HISPANIC          Elimelech   91    18
    ## 235          2016   MALE WHITE NON HISPANIC             Jayden   91    18
    ## 236          2016   MALE WHITE NON HISPANIC             Morris   91    18
    ## 237          2016   MALE WHITE NON HISPANIC             Aharon   92    17
    ## 238          2016   MALE WHITE NON HISPANIC           Muhammad   92    17
    ## 239          2016   MALE WHITE NON HISPANIC              Kevin   92    17
    ## 240          2016   MALE WHITE NON HISPANIC              Hersh   92    17
    ## 241          2016   MALE WHITE NON HISPANIC               Kyle   92    17
    ## 242          2016   MALE WHITE NON HISPANIC              Yossi   92    17
    ## 243          2016   MALE WHITE NON HISPANIC              Samir   92    17
    ## 244          2016   MALE WHITE NON HISPANIC              Shane   92    17
    ## 245          2016   MALE WHITE NON HISPANIC               Anas   93    16
    ## 246          2016   MALE WHITE NON HISPANIC               Joel   93    16
    ## 247          2016   MALE WHITE NON HISPANIC             Angelo   93    16
    ## 248          2016   MALE WHITE NON HISPANIC             Moishe   93    16
    ## 249          2016   MALE WHITE NON HISPANIC            Pinchas   93    16
    ## 250          2016   MALE WHITE NON HISPANIC          Francesco   93    16
    ## 251          2016   MALE WHITE NON HISPANIC              Mateo   93    16
    ## 252          2016   MALE WHITE NON HISPANIC              Brian   93    16
    ## 253          2016   MALE WHITE NON HISPANIC           Mohammed   93    16
    ## 254          2016   MALE WHITE NON HISPANIC           Vincenzo   94    15
    ## 255          2016   MALE WHITE NON HISPANIC           Binyamin   94    15
    ## 256          2016   MALE WHITE NON HISPANIC               Axel   94    15
    ## 257          2016   MALE WHITE NON HISPANIC            Donovan   94    15
    ## 258          2016   MALE WHITE NON HISPANIC             Chesky   94    15
    ## 259          2016   MALE WHITE NON HISPANIC              Blake   94    15
    ## 260          2016   MALE WHITE NON HISPANIC             Damian   94    15
    ## 261          2016   MALE WHITE NON HISPANIC             Calvin   94    15
    ## 262          2016   MALE WHITE NON HISPANIC            Raymond   94    15
    ## 263          2016   MALE WHITE NON HISPANIC              Ronan   94    15
    ## 264          2016   MALE WHITE NON HISPANIC             Zalmen   94    15
    ## 265          2016   MALE WHITE NON HISPANIC              Ryder   94    15
    ## 266          2016   MALE WHITE NON HISPANIC            Benzion   94    15
    ## 267          2016   MALE WHITE NON HISPANIC              Bodhi   94    15
    ## 268          2016   MALE WHITE NON HISPANIC              Brody   95    14
    ## 269          2016   MALE WHITE NON HISPANIC            Beckett   95    14
    ## 270          2016   MALE WHITE NON HISPANIC          Frederick   95    14
    ## 271          2016   MALE WHITE NON HISPANIC           Mohammad   95    14
    ## 272          2016   MALE WHITE NON HISPANIC            Yisrael   95    14
    ## 273          2016   MALE WHITE NON HISPANIC            Pinchus   95    14
    ## 274          2016   MALE WHITE NON HISPANIC              Avery   95    14
    ## 275          2016   MALE WHITE NON HISPANIC              Harry   95    14
    ## 276          2016   MALE WHITE NON HISPANIC               Yoel   95    14
    ## 277          2016   MALE WHITE NON HISPANIC              Silas   95    14
    ## 278          2016   MALE WHITE NON HISPANIC             Reuven   95    14
    ## 279          2016   MALE WHITE NON HISPANIC            Griffin   95    14
    ## 280          2016   MALE WHITE NON HISPANIC              Timur   95    14
    ## 281          2016   MALE WHITE NON HISPANIC            Leonard   95    14
    ## 282          2016   MALE WHITE NON HISPANIC            Greyson   96    13
    ## 283          2016   MALE WHITE NON HISPANIC               Jace   96    13
    ## 284          2016   MALE WHITE NON HISPANIC             Boruch   96    13
    ## 285          2016   MALE WHITE NON HISPANIC              River   96    13
    ## 286          2016   MALE WHITE NON HISPANIC            Brayden   96    13
    ## 287          2016   MALE WHITE NON HISPANIC             Steven   96    13
    ## 288          2016   MALE WHITE NON HISPANIC           Dominick   96    13
    ## 289          2016   MALE WHITE NON HISPANIC               Elya   96    13
    ## 290          2016   MALE WHITE NON HISPANIC             Jaxson   96    13
    ## 291          2016   MALE WHITE NON HISPANIC            Tristan   96    13
    ## 292          2016   MALE WHITE NON HISPANIC            Francis   96    13
    ## 293          2016   MALE WHITE NON HISPANIC            Stephen   96    13
    ## 294          2016   MALE WHITE NON HISPANIC             Pierce   96    13
    ## 295          2016   MALE WHITE NON HISPANIC             Xavier   96    13
    ## 296          2016   MALE WHITE NON HISPANIC           Yitzchak   96    13
    ## 297          2016   MALE WHITE NON HISPANIC              Malik   96    13
    ## 298          2016   MALE WHITE NON HISPANIC                Jad   96    13
    ## 299          2016   MALE WHITE NON HISPANIC              Micah   96    13
    ## 300          2016   MALE WHITE NON HISPANIC               Otto   97    12
    ## 301          2016   MALE WHITE NON HISPANIC               Ivan   97    12
    ## 302          2016   MALE WHITE NON HISPANIC               Zayn   97    12
    ## 303          2016   MALE WHITE NON HISPANIC             Sholom   97    12
    ## 304          2016   MALE WHITE NON HISPANIC              Selim   97    12
    ## 305          2016   MALE WHITE NON HISPANIC            Brendan   97    12
    ## 306          2016   MALE WHITE NON HISPANIC             Fishel   97    12
    ## 307          2016   MALE WHITE NON HISPANIC               Enzo   97    12
    ## 308          2016   MALE WHITE NON HISPANIC              Ameer   97    12
    ## 309          2016   MALE WHITE NON HISPANIC            Desmond   97    12
    ## 310          2016   MALE WHITE NON HISPANIC             Lucian   97    12
    ## 311          2016   MALE WHITE NON HISPANIC              Imran   97    12
    ## 312          2016   MALE WHITE NON HISPANIC           Giuseppe   97    12
    ## 313          2016   MALE WHITE NON HISPANIC            Yonatan   97    12
    ## 314          2016   MALE WHITE NON HISPANIC              Avrum   97    12
    ## 315          2016   MALE WHITE NON HISPANIC             Julien   97    12
    ## 316          2016   MALE WHITE NON HISPANIC               Sami   97    12
    ## 317          2016   MALE WHITE NON HISPANIC              Cyrus   97    12
    ## 318          2016   MALE WHITE NON HISPANIC                Jay   97    12
    ## 319          2016   MALE WHITE NON HISPANIC              Ayden   97    12
    ## 320          2016   MALE WHITE NON HISPANIC             Shalom   98    11
    ## 321          2016   MALE WHITE NON HISPANIC             Jeremy   98    11
    ## 322          2016   MALE WHITE NON HISPANIC              Ahron   98    11
    ## 323          2016   MALE WHITE NON HISPANIC              Motty   98    11
    ## 324          2016   MALE WHITE NON HISPANIC            Russell   98    11
    ## 325          2016   MALE WHITE NON HISPANIC           Gianluca   98    11
    ## 326          2016   MALE WHITE NON HISPANIC             Yassin   98    11
    ## 327          2016   MALE WHITE NON HISPANIC             Walter   98    11
    ## 328          2016   MALE WHITE NON HISPANIC              Ahmad   98    11
    ## 329          2016   MALE WHITE NON HISPANIC            Gregory   98    11
    ## 330          2016   MALE WHITE NON HISPANIC            Nikolai   98    11
    ## 331          2016   MALE WHITE NON HISPANIC               Zain   98    11
    ## 332          2016   MALE WHITE NON HISPANIC              Jesse   98    11
    ## 333          2016   MALE WHITE NON HISPANIC             Marcel   98    11
    ## 334          2016   MALE WHITE NON HISPANIC            Santino   98    11
    ## 335          2016   MALE WHITE NON HISPANIC              Josef   98    11
    ## 336          2016   MALE WHITE NON HISPANIC             Isaiah   98    11
    ## 337          2016   MALE WHITE NON HISPANIC             Shraga   98    11
    ## 338          2016   MALE WHITE NON HISPANIC              Milan   98    11
    ## 339          2016   MALE WHITE NON HISPANIC               Wolf   98    11
    ## 340          2016   MALE WHITE NON HISPANIC             Peretz   98    11
    ## 341          2016   MALE WHITE NON HISPANIC             Dillon   99    10
    ## 342          2016   MALE WHITE NON HISPANIC              Malek   99    10
    ## 343          2016   MALE WHITE NON HISPANIC            Joaquin   99    10
    ## 344          2016   MALE WHITE NON HISPANIC               Umar   99    10
    ## 345          2016   MALE WHITE NON HISPANIC            Lincoln   99    10
    ## 346          2016   MALE WHITE NON HISPANIC               Eyad   99    10
    ## 347          2016   MALE WHITE NON HISPANIC              Meyer   99    10
    ## 348          2016   MALE WHITE NON HISPANIC             Nosson   99    10
    ## 349          2016   MALE WHITE NON HISPANIC            Bradley   99    10
    ## 350          2016   MALE WHITE NON HISPANIC            Meilech   99    10
    ## 351          2016   MALE WHITE NON HISPANIC            Winston   99    10
    ## 352          2016   MALE WHITE NON HISPANIC             Walker   99    10
    ## 353          2016   MALE WHITE NON HISPANIC           Augustus   99    10
    ## 354          2016   MALE WHITE NON HISPANIC           Leonidas   99    10
    ## 355          2016   MALE WHITE NON HISPANIC             Camden   99    10
    ## 356          2016   MALE WHITE NON HISPANIC             Herman   99    10
    ## 357          2016   MALE WHITE NON HISPANIC              Noach   99    10
    ## 358          2016   MALE WHITE NON HISPANIC             Colton   99    10
    ## 359          2016   MALE WHITE NON HISPANIC            Nikolas   99    10
    ## 360          2016   MALE WHITE NON HISPANIC              Aviel   99    10
    ## 361          2016   MALE WHITE NON HISPANIC               Berl   99    10
    ## 362          2016   MALE WHITE NON HISPANIC            Mordche   99    10
    ## 363          2016   MALE WHITE NON HISPANIC               Elan   99    10
    ## 364          2016   MALE WHITE NON HISPANIC            Leibish   99    10

``` r
ggplot(name, aes(x = Count, y = Rank)) + geom_point()
```

![](rmd_basic_plots_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
