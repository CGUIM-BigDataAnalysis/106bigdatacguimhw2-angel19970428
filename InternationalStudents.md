106-2 大數據分析方法 作業二
================
Yi-Ju Tseng

作業完整說明[連結](https://docs.google.com/document/d/1aLGSsGXhgOVgwzSg9JdaNz2qGPQJSoupDAQownkGf_I/edit?usp=sharing)

學習再也不限定在自己出生的國家，台灣每年有許多學生選擇就讀國外的大專院校，同時也有人多國外的學生來台灣就讀，透過分析[大專校院境外學生人數統計](https://data.gov.tw/dataset/6289)、[大專校院本國學生出國進修交流數](https://data.gov.tw/dataset/24730)、[世界各主要國家之我國留學生人數統計表](https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv)可以了解103年以後各大專院校國際交流的情形。請同學分析以下議題，並以視覺化的方式呈現分析結果，呈現103年以後大專院校國際交流的情形。

來台境外生分析
--------------

### 資料匯入與處理

``` r
library(readr)
library(readODS)
c103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv")
c104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv")
c105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv")
c106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv")
s103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv")
s104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv")
s105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv")
s106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv")
ods_result<-read_csv("Student_RPT_07.csv")
head(ods_result)
```

    ## # A tibble: 6 x 15
    ##   學年度  學期 設立別 學校類別 學校代碼 學校名稱  系所代碼 系所名稱  學制 
    ##    <int> <int> <chr>  <chr>    <chr>    <chr>        <int> <chr>     <chr>
    ## 1    101     1 公立   一般大學 0001     國立政治大學~   220326 土耳其語文學系~ 學士班(~
    ## 2    101     1 公立   一般大學 0001     國立政治大學~   220326 土耳其語文學系~ 學士班(~
    ## 3    101     1 公立   一般大學 0001     國立政治大學~   220326 土耳其語文學系~ 學士班(~
    ## 4    101     1 公立   一般大學 0001     國立政治大學~   220326 土耳其語文學系~ 學士班(~
    ## 5    101     1 公立   一般大學 0001     國立政治大學~   220326 土耳其語文學系~ 學士班(~
    ## 6    101     1 公立   一般大學 0001     國立政治大學~   220326 土耳其語文學系~ 學士班(~
    ## # ... with 6 more variables: `對方學校(機構)國別(地區)` <chr>,
    ## #   `對方學校(機構)中文名稱` <chr>, `對方學校(機構)英文名稱` <chr>,
    ## #   小計 <int>, 男 <int>, 女 <int>

### 哪些國家來台灣唸書的學生最多呢？

``` r
#這是R Code Chunk
```

### 哪間大學的境外生最多呢？

``` r
#這是R Code Chunk
```

### 各個國家來台灣唸書的學生人數條狀圖

``` r
#這是R Code Chunk
```

### 各個國家來台灣唸書的學生人數面量圖

``` r
#這是R Code Chunk
```

台灣學生國際交流分析
--------------------

### 資料匯入與處理

``` r
#這是R Code Chunk
```

### 台灣大專院校的學生最喜歡去哪些國家進修交流呢？

``` r
#這是R Code Chunk
```

### 哪間大學的出國交流學生數最多呢？

``` r
#這是R Code Chunk
```

### 台灣大專院校的學生最喜歡去哪些國家進修交流條狀圖

``` r
#這是R Code Chunk
```

### 台灣大專院校的學生最喜歡去哪些國家進修交流面量圖

``` r
#這是R Code Chunk
```

台灣學生出國留學分析
--------------------

### 資料匯入與處理

``` r
#這是R Code Chunk
```

### 台灣學生最喜歡去哪些國家留學呢？

``` r
#這是R Code Chunk
```

### 台灣學生最喜歡去哪些國家留學面量圖

``` r
#這是R Code Chunk
```

綜合分析
--------

請問來台讀書與離台讀書的來源國與留學國趨勢是否相同(5分)？想來台灣唸書的境外生，他們的母國也有很多台籍生嗎？請圖文並茂說明你的觀察(10分)。
