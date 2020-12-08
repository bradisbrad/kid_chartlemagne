---
title: 'Advent of Code: Day 4'
author: Brad Hill
date: '2020-12-08 17:00:00'
slug: adventofcode_day4
categories:
  - Data
tags:
  - Advent of Code
  - Christmas
  - coding puzzles
  - rstats
keywords:
  - tech
  - Advent
  - Advent of Code
  - AoC
  - Christmas
  - code
  - coding
  - coding puzzles
  - r
  - R
  - rstats
  - AoC 2020
  - AoC 2019
  - AoC 2018
  - AoC 2017
  - AoC 2016
  - AoC 2015
  - Day 3
---

Generally, you'd think fraud, breaking and entering, and hacking would land you on the naughty list, but it's the only way to save Christmas on Day 4 of Advent of Code.
<!--more-->

As I feared, this post is a mess. It may even be the template for the rest of these days as I try to write about and solve these challenges at the same time, but it just means that, once this is done on the 24th, this Christmas will be a very special Christmas to me, ba-da-bum ba-dum ba-dum-bum ba-da-bum.




# [2020](https://adventofcode.com/2020/day/4)

Time for us to commit travel fraud! Is that a thing? We've made our way to the airport, but now we've got the wrong credentials. We've mistakenly grabbed our North Pole Credentials instead of our passport. Evidently, the only difference is a missing Country ID, as the North Pole is more of an idea, and it lives in our hearts. 

So now, we're trying to ignore a missing Country ID, but require all other data fields. Our data comes as a series of batch files with key:value pairs.


```r
data <- read_lines(here('data/advent_of_code/2020/day4_input.txt')) %>% 
  paste(collapse = ' ') %>% 
  str_split('  ') %>% 
  unlist()
```


```
## [1] "iyr:2013 hcl:#ceb3a1 hgt:151cm eyr:2030 byr:1943 ecl:grn"             
## [2] "eyr:1988 iyr:2015 ecl:gry hgt:153in pid:173cm hcl:0c6261 byr:1966"    
## [3] "hcl:#733820 hgt:166cm eyr:2025 pid:79215921 byr:1952 iyr:2014 ecl:blu"
```

I think python is probably better at handling this, because it's probably technically a dictionary, but look, I don't like python because I don't know python that well, so I'm not going to use python.

We're going to start by splitting every line by the space.


```r
data <- str_split(data, ' ')
```


```
## [[1]]
## [1] "iyr:2013"    "hcl:#ceb3a1" "hgt:151cm"   "eyr:2030"    "byr:1943"   
## [6] "ecl:grn"    
## 
## [[2]]
## [1] "eyr:1988"   "iyr:2015"   "ecl:gry"    "hgt:153in"  "pid:173cm" 
## [6] "hcl:0c6261" "byr:1966"  
## 
## [[3]]
## [1] "hcl:#733820"  "hgt:166cm"    "eyr:2025"     "pid:79215921" "byr:1952"    
## [6] "iyr:2014"     "ecl:blu"     
## 
## [[4]]
## [1] "eyr:2022"      "hgt:165cm"     "hcl:#733820"   "iyr:2013"     
## [5] "pid:073015801" "ecl:oth"       "cid:101"      
## 
## [[5]]
## [1] "iyr:2013"      "ecl:brn"       "hcl:#623a2f"   "cid:246"      
## [5] "byr:1948"      "pid:122719649" "hgt:160cm"     "eyr:2026"
```

Then, we can turn this into a table with row ids.


```r
data <- data %>% 
  as_tibble_col(column_name = 'field') %>% 
  mutate(id = row_number()) %>% 
  unnest(cols = field)
```


```
## # A tibble: 2,038 x 2
##   field          id
##   <chr>       <int>
## 1 iyr:2013        1
## 2 hcl:#ceb3a1     1
## 3 hgt:151cm       1
## 4 eyr:2030        1
## 5 byr:1943        1
## # ... with 2,033 more rows
```

Now, let's split that field column up into field name and field value.


```r
data <- data %>% 
  separate(field, c('field_name', 'field_value'), sep = ':') %>% 
  mutate(field_name = na_if(field_name, ''))
```


```
## # A tibble: 2,038 x 3
##   field_name field_value    id
##   <chr>      <chr>       <int>
## 1 iyr        2013            1
## 2 hcl        #ceb3a1         1
## 3 hgt        151cm           1
## 4 eyr        2030            1
## 5 byr        1943            1
## # ... with 2,033 more rows
```

We want each id to contain each `field_name`, with `field_value` populated with `NA` if they don't exist. We can use the `complete()` function for that, given that we know all the field names. We're also going to spread this out so each field is a column and each observation is a row.


```r
data <- data %>% 
  group_by(id) %>% 
  complete(field_name = c('byr', 'iyr', 'eyr',
                          'hgt', 'hcl', 'ecl', 
                          'pid', 'cid')) %>% 
  filter(!is.na(field_name)) %>% 
  pivot_wider(names_from = field_name, 
              values_from = field_value) %>% 
  ungroup()
```


```
## # A tibble: 285 x 9
##      id byr   cid   ecl   eyr   hcl     hgt   iyr   pid      
##   <int> <chr> <chr> <chr> <chr> <chr>   <chr> <chr> <chr>    
## 1     1 1943  <NA>  grn   2030  #ceb3a1 151cm 2013  <NA>     
## 2     2 1966  <NA>  gry   1988  0c6261  153in 2015  173cm    
## 3     3 1952  <NA>  blu   2025  #733820 166cm 2014  79215921 
## 4     4 <NA>  101   oth   2022  #733820 165cm 2013  073015801
## 5     5 1948  246   brn   2026  #623a2f 160cm 2013  122719649
## # ... with 280 more rows
```

Now we can just look for rows that are either complete, or only have the `cid` column missing.


```r
valid_passports <- data %>% 
  filter(across(-cid, ~!is.na(.x)))
```


```
## # A tibble: 208 x 9
##      id byr   cid   ecl   eyr   hcl     hgt   iyr   pid      
##   <int> <chr> <chr> <chr> <chr> <chr>   <chr> <chr> <chr>    
## 1     2 1966  <NA>  gry   1988  0c6261  153in 2015  173cm    
## 2     3 1952  <NA>  blu   2025  #733820 166cm 2014  79215921 
## 3     5 1948  246   brn   2026  #623a2f 160cm 2013  122719649
## 4     6 2000  <NA>  hzl   2028  #ceb3a1 154cm 2017  229371724
## 5     7 1980  <NA>  amb   2029  #623a2f 177cm 2013  914628384
## # ... with 203 more rows
```

The remaining rows are the valid passports, which gives us our first answer: the number of valid passports is 208.

The second part of this question adds some data checks, which are easy to add to our wide table format.


```r
valid_passports <- data %>% 
  filter(between(as.numeric(byr), 1920, 2002),
         between(as.numeric(iyr), 2010, 2020),
         between(as.numeric(eyr), 2020, 2030),
         case_when(str_detect(hgt, 'cm') ~ between(as.numeric(str_extract(hgt, '[0-9]+')), 150, 193),
                   str_detect(hgt, 'in') ~ between(as.numeric(str_extract(hgt, '[0-9]+')), 59, 76)),
         str_detect(hcl, '^#[A-z0-9]{6}$'),
         ecl %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'),
         str_detect(pid, '^[0-9]{9}$'))
```


```
## # A tibble: 167 x 9
##      id byr   cid   ecl   eyr   hcl     hgt   iyr   pid      
##   <int> <chr> <chr> <chr> <chr> <chr>   <chr> <chr> <chr>    
## 1     5 1948  246   brn   2026  #623a2f 160cm 2013  122719649
## 2     6 2000  <NA>  hzl   2028  #ceb3a1 154cm 2017  229371724
## 3     7 1980  <NA>  amb   2029  #623a2f 177cm 2013  914628384
## 4     8 1945  <NA>  amb   2026  #efcc98 76in  2019  475316185
## 5    11 1920  212   oth   2029  #b6652a 160cm 2012  223041037
## # ... with 162 more rows
```

Kind of cumbersome, but data checks are cumbersome. Valid passports after the added checks total to 167.

# [2019](https://adventofcode.com/2019/day/4)

Well, we've made it to Venus, but it's password protected. The elves threw out the sticky note that had the password, but that's _good,_ and I don't think we're mentioning that enough. Don't leave passwords out, folks!   
  
They remember some of the password requirements though, and our input is a range of numbers in which the password could be. Our first challenge is to find the number of possible passwords in our input range. 


```r
data <- 284639:748759
```

The factsheet indicates that the password:    

- Is a six digit number    
- Is within our puzzle range    
- Has two matching adjacent digits  
- Never decreases from left to right  

So the first part is filtering for adjacent digits. I'm turning this into a table, because frankly I like to work in tables. There are a million better ways to do this, but I'm already trying to speed run these, so I'm going to use what I like.


```r
data <- data %>% 
  tibble(pass = .) %>% 
  mutate(pass = as.character(pass)) %>% 
  filter(str_detect(pass, '[0]{2}|[1]{2}|[2]{2}|[3]{2}|[4]{2}|[5]{2}|[6]{2}|[7]{2}|[8]{2}|[9]{2}')) 
```


```
## # A tibble: 186,191 x 1
##   pass  
##   <chr> 
## 1 284644
## 2 284655
## 3 284660
## 4 284661
## 5 284662
## # ... with 186,186 more rows
```

From here, we need to check whether each consecutive digit is greater than the last. We could do a loop here, but the solution I went with was to split each password, sort the characters, and put it back together. That means if the new reconstructed password is the same as the old one, it's valid. If not, that means there was a decrease at some point in the string.


```r
data <- data %>% 
  mutate(pass_splt = str_split(pass, ''),
         pass_srt = map(pass_splt, str_sort),
         pass_chk = unlist(map(pass_srt, paste0, collapse = '')),
         valid = pass == pass_chk)
```


```
## # A tibble: 186,191 x 5
##   pass   pass_splt pass_srt  pass_chk valid
##   <chr>  <list>    <list>    <chr>    <lgl>
## 1 284644 <chr [6]> <chr [6]> 244468   FALSE
## 2 284655 <chr [6]> <chr [6]> 245568   FALSE
## 3 284660 <chr [6]> <chr [6]> 024668   FALSE
## 4 284661 <chr [6]> <chr [6]> 124668   FALSE
## 5 284662 <chr [6]> <chr [6]> 224668   FALSE
## # ... with 186,186 more rows
```

Now the answer to our question is just the sum of valid passwords, or 895.

The second part of this question now requires that the two adjacent digits not be part of a larger group of matching digits. So something like `122345` would be valid, while `122234` would be invalid. The extra tricky part here is that we can't just filter out 2+ adjacent, because something like `111122` is _still_ valid because it contains the `22` outside of a larger group. Since we're not going to be adding any new valid passwords, only removing some, we don't really need to start this from scratch. We also already have a column where the password is split into characters, so we can use the `rle()` function to look for the number of consecutive characters. Then, we just do a quick `any()` call to see if any of the consecutive lengths is 2 and we get our new valid column. 


```r
data <- data %>% 
  filter(valid) %>% 
  mutate(cons = map(pass_splt, rle),
         cons = map(cons, `[[`, 1),
         valid2 = unlist(map(cons, ~any(.x == 2))))
```


```
## # A tibble: 895 x 7
##   pass   pass_splt pass_srt  pass_chk valid cons      valid2
##   <chr>  <list>    <list>    <chr>    <lgl> <list>    <lgl> 
## 1 288888 <chr [6]> <chr [6]> 288888   TRUE  <int [2]> FALSE 
## 2 288889 <chr [6]> <chr [6]> 288889   TRUE  <int [3]> FALSE 
## 3 288899 <chr [6]> <chr [6]> 288899   TRUE  <int [3]> TRUE  
## 4 288999 <chr [6]> <chr [6]> 288999   TRUE  <int [3]> TRUE  
## 5 289999 <chr [6]> <chr [6]> 289999   TRUE  <int [3]> FALSE 
## # ... with 890 more rows
```

Again, the answer to this part is the sum of valid passwords, `valid2` this time around, which comes out to 591.

# [2018](https://adventofcode.com/2018/day/4)

More spy work! This time, we're seeing when guards fall asleep so we can sneak in and fix the issues with Santa's suit. Again, you'd think these elves would be more up to the task here, but I guess it's 1518, so they're still curing people with leeches. Meanwhile, I'm hiding out in a _second_ supply closet for fear of ripping a hole in the space-time continuum, even though I'm fairly certain that concept wasn't around in 1518. I bet time travelers were just running rampant back in the 1500s, not having to worry about time travel rules. I assume you don't have to follow time travel rules that haven't been invented yet. That's why no one hangs out here in 2020, too many time travel rules.

Our first puzzle gives us an unsorted list of times when various guards start their shifts, as well as when they fall asleep during their shifts, and asks us to find the minute that the guard that sleeps the most is most often asleep. What a mess of a prompt summary.


```r
data <- read_tsv(here('data/advent_of_code/2018/day4_input.txt'), col_names = F)
```

We need to split this up into an actual table of useful things.


```r
data <- data %>% 
  separate(X1, into = c('date', 'time', 'desc'), sep = ' ', extra = 'merge') %>% 
  mutate(date = lubridate::ymd(str_remove(date, '\\[')),
         time = str_remove(time, '\\]')) %>% 
  arrange(date, time)
```


```
## # A tibble: 1,134 x 3
##   date       time  desc                    
##   <date>     <chr> <chr>                   
## 1 1518-02-17 00:00 Guard #1621 begins shift
## 2 1518-02-17 00:14 falls asleep            
## 3 1518-02-17 00:32 wakes up                
## 4 1518-02-18 00:00 Guard #2741 begins shift
## 5 1518-02-18 00:18 falls asleep            
## 6 1518-02-18 00:34 wakes up                
## # ... with 1,128 more rows
```

The time column isn't quite useful yet, but I wanted to stop here and explain the next step. We can create a column that contains the guard ID if there is one, and returns `NA` if not. We know when the guard starts their shift they have to be awake, so we're going to set that description, after we grab the guard_id, as awake. From there, we're going to dust off `complete()` because we want to know the entire sleeping schedule in the midnight hour.


```r
data <- data %>% 
  mutate(guard_id = case_when(str_detect(desc, '#') ~ str_extract(desc, '[0-9]+'),
                              T ~ 'NA'),
         guard_id = na_if(guard_id, 'NA'),
         desc = case_when(str_detect(desc, '#') ~ 'awake',
                          str_detect(desc, 'wakes') ~ 'awake',
                          str_detect(desc, 'asleep') ~ 'asleep'),
         date = case_when(str_sub(time, 1, 2) != '00' ~ date + 1,
                          T ~ date),
         time = case_when(str_sub(time, 1, 2) != '00' ~ '00:00',
                          T ~ time)) %>% 
  separate(time, into = c('hour', 'minute'), sep = ':') %>% 
  select(-hour) %>% 
  mutate(minute = as.numeric(minute)) %>% 
  group_by(date) %>% 
  complete(minute = 0:60) %>% 
  fill(guard_id, desc, .direction = 'down')
```


```
## # A tibble: 17,083 x 4
## # Groups:   date [280]
##   date       minute desc  guard_id
##   <date>      <dbl> <chr> <chr>   
## 1 1518-02-17      0 awake 1621    
## 2 1518-02-17      1 awake 1621    
## 3 1518-02-17      2 awake 1621    
## 4 1518-02-17      3 awake 1621    
## 5 1518-02-17      4 awake 1621    
## # ... with 17,078 more rows
```

So now we're asked which guard sleeps the most, and then which minute is that guard most often asleep.


```r
data <- data %>% 
  group_by(guard_id) %>% 
  mutate(asleep = sum(desc == 'asleep')) %>% 
  count(guard_id, asleep, minute, desc) %>% 
  filter(desc == 'asleep') %>% 
  arrange(desc(asleep), desc(n))
```


```
## # A tibble: 1,113 x 5
## # Groups:   guard_id [20]
##   guard_id asleep minute desc       n
##   <chr>     <int>  <dbl> <chr>  <int>
## 1 521         517     24 asleep    15
## # ... with 1,112 more rows
```

We find that guard 521 sleeps the most, and is most often asleep at minute 24. Technically, the question asks for the product of those numbers, so the answer is 12504, but that doesn't really mean anything new. 

The second part of the question asks instead which guard sleeps more at a specific minute than any other guard. Luckily, we've already got all that we need to answer this one. Let's just change up the sorting a bit.


```r
data <- data %>% 
  ungroup() %>% 
  arrange(desc(n))
```


```
## # A tibble: 1,113 x 5
##   guard_id asleep minute desc       n
##   <chr>     <int>  <dbl> <chr>  <int>
## 1 2969        470     47 asleep    19
## # ... with 1,112 more rows
```

Guard 2969 is asleep at minute 47 more often than any other guard is asleep at any other time, for a total of 19 instances! Consistency at its finest. (Also, technically need the product here as well, but I'll leave that one to viewers like you, if you're interested in a quick multiplication problem.)

# [2017](https://adventofcode.com/2017/day/4)

I think we're back in the computer for this one, but I'm having trouble keeping track. Probably because every neuron I have is now pixelated and running on Windows 95. There's a new system in place that requires pass phrases instead of passwords, and we've got to determine how many valid passphrases we've got.


```r
data <- read_lines(here('data/advent_of_code/2017/day4_input.txt'))
```


```
## [1] "sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp"
## [2] "xmuf znkhaes pggrlp zia znkhaes znkhaes"          
## [3] "nti rxr bogebb zdwrin"
```

A valid pass phrase contains no duplicate phrases. Just call me Dora the Explorer, because I am absolutely abusing `map()`. Split the strings, find duplicates, and return the sum of values without dupes.


```r
str_split(data, ' ') %>% 
  map(duplicated) %>% 
  map(any) %>% 
  unlist() %>% 
  `!` %>% 
  sum()
```

```
## [1] 383
```

The second part of the question uses the same data, but also prohibits anagrams. This adds maybe 3 functions to our weird little pipe concotion, a split for each word, a sort for each split up word, and a concatenation back into a single word.


```
## [1] 265
```


# [2016](https://adventofcode.com/2016/day/4)

Back in the Easter Bunny's lair, I'm looking for the room where they're hiding my Lucky Charms. Luckily, they keep a list of rooms out in the open. Unfortunately, the encrypt them. Fortunately, they have the instructions to decrypt them right next to the list. Unfortunately, there's a ton of decoy data in the list. Really, a roller coaster of emotions here in EBHQ. 

We're provided with a list, each item consists of an encrypted room name, a sector ID, and a checksum. The first question asks that we find the sum of the sector IDs of the valid rooms. A room is valid if the checksum is the 5 most common letters in the encrypted name in order, with ties broken alphabetically. Day 4 has been saturated with `str_split()` calls that I hope to never see again.


```r
data <- read_lines(here('data/advent_of_code/2016/day4_input.txt'))
```


```
## [1] "aczupnetwp-dnlgpyrpc-sfye-dstaatyr-561[patyc]"    
## [2] "jsehsyafy-vqw-ljsafafy-866[nymla]"                
## [3] "tyepcyletzylw-ncjzrpytn-prr-opawzjxpye-743[cnrdl]"
```

The first order of business is going to be getting this in a format that lets us look at everything separately.


```r
data <- tibble(data = data) %>% 
  transmute(checksum = str_remove_all(str_extract(data, '\\[[a-z]+\\]'), '\\[|\\]'),
            sector_id = str_extract(data, '[0-9]+'),
            enc = str_remove_all(data, '-[0-9]+\\[[a-z]+\\]'))
```


```
## # A tibble: 991 x 3
##   checksum sector_id enc                                   
##   <chr>    <chr>     <chr>                                 
## 1 patyc    561       aczupnetwp-dnlgpyrpc-sfye-dstaatyr    
## 2 nymla    866       jsehsyafy-vqw-ljsafafy                
## 3 cnrdl    743       tyepcyletzylw-ncjzrpytn-prr-opawzjxpye
## # ... with 988 more rows
```

From here, I wrote a small function that will take the encrypted name, count each character with `table()`, sort by character frequency, and return the first 5 letters as a string. The valid rooms will have a checksum that matches the output of the function.


```r
check_enc <- function(name){
  str_split(str_remove_all(name, '-'), '')[[1]] %>% 
    table() %>% 
    sort(decreasing = T) %>% 
    names() %>% 
    paste0(collapse = '') %>% 
    str_sub(1, 5)
}

data %>% 
  mutate(check_enc = map_chr(enc, check_enc)) %>% 
  filter(checksum == check_enc)
```

```
## # A tibble: 528 x 4
##    checksum sector_id enc                                  check_enc
##    <chr>    <chr>     <chr>                                <chr>    
##  1 patyc    561       aczupnetwp-dnlgpyrpc-sfye-dstaatyr   patyc    
##  2 krtue    436       ckgvutofkj-pkrrehkgt-zkinturume      krtue    
##  3 lzhvy    253       ipvohghykvbz-ihzrla-jbzavtly-zlycpjl lzhvy    
##  4 tveif    425       tipfxvezt-sleep-tljkfdvi-jvimztv     tveif    
##  5 aiqko    616       ktiaaqnqml-xtiabqk-oziaa-xczkpiaqvo  aiqko    
##  6 hvgmx    111       ikhcxvmbex-vtgwr-vhtmbgz-mxvaghehzr  hvgmx    
##  7 fstdh    805       dpssptjwf-fhh-tfswjdft               fstdh    
##  8 qadou    534       oaddaeuhq-otaoaxmfq-qzsuzqqduzs      qadou    
##  9 erdvi    685       dzczkrip-xiruv-sleep-drerxvdvek      erdvi    
## 10 sjnfy    853       nsyjwsfyntsfq-gfxpjy-jslnsjjwnsl     sjnfy    
## # ... with 518 more rows
```

The answer is then the sum of the sector IDs from the valid rooms which comes out to 278221.

The next part informs us that the sector ID actually tells us how many letters to the right we need to shift each character in the encrypted name to decrypt the room name. I built another function, this time using that same wrap around method from [day 1 2017](https://bradisblogging.com/2020/12/adventofcode_day1/) and [day 3 2020](https://bradisblogging.com/2020/12/adventofcode_day3/). I got kind of tired of messing with `map()` so I just threw this one together as a for loop.


```r
decrypter <- function(name, sector_id){
  real_name <- c()
  for(i in 1:str_length(name)){
    if(str_sub(name, i, i) == '-'){
      real_name[i] <- '-'
    } else {
      let_index <- which(letters == str_sub(name, i, i))
      new_index <- ifelse((let_index + as.numeric(sector_id))%%26 == 0, let_index, (let_index + as.numeric(sector_id))%%26)
      real_name[i] <- letters[new_index]
    }
  }
  res <- paste0(real_name, collapse = '')
  return(res)
}

data %>% 
  mutate(real_name = map2_chr(enc, sector_id, decrypter)) %>% 
  filter(str_detect(real_name, 'north'))
```

```
## # A tibble: 1 x 4
##   checksum sector_id enc                      real_name               
##   <chr>    <chr>     <chr>                    <chr>                   
## 1 hmxka    267       ghkmaihex-hucxvm-lmhktzx northpole-object-storage
```

So glad that they named their North Pole contraband room "North Pole Object Storage."


# [2015](https://adventofcode.com/2015/day/4)

Santa is mining AdventCoin, which is evidently a new alt-coin that someone is going to make a billion dollars in a day on. To mine these, he needs to find MD5 hashes that start with a certain number of 0s. We're given a puzzle input, `bgvyzdsv`, and must find the lower number to append to it that gives us the MD5 hash with the leading 0s.  
  

I'm going to be honest here, I know nearly nothing about MD5 hashes, so I just use the `openssl` library to hash a bunch of numbers in a while loop until I find the ones that start with five and six 0s respectively. This is not efficient, and probably not even the right way to go about this, but it got the job, and the post, done.


```r
library(openssl)
data <- 'bgvyzdsv'

i <- 1
md5_check <- md5(paste0(data, i))
while(str_sub(md5_check, 1, 5) != '00000'){
  i <- i+1
  md5_check <- md5(paste0(data, i))
}
i
```


```
## [1] 254575
```



```r
i <- 1
md5_check <- md5_check <- md5(paste0(data, i))
while(str_sub(md5_check, 1, 6) != '000000'){
  i <- i+1
  md5_check <- md5(paste0(data, i))
}
i
```


```
## [1] 1038736
```

My initial estimate of being caught up by Wednesday may be off, but, hey, what's life without the threat of missing a self-imposed deadline?
