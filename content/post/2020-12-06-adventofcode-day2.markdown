---
title: 'Advent of Code: Day 2'
author: Brad Hill
date: '2020-12-07 16:30:00'
slug: adventofcode_day2
categories:
  - Data
tags:
  - Advent of Code
  - Christmas
  - coding puzzles
  - R
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
  - Day 2
---

This Christmas, I'm asking for a treadmill, because I already feel like I'm running in place trying to catch up on these posts. The second day of Advent of Code brings us toboggan rental passwords, an introduction to intcode, and Santa suit construction.

<!--more-->

The days start coming and they don't stop coming. Every day I'm further behind, but I keep doing it for you, my one fan. 



# [2020](https://adventofcode.com/2020/day/2)  

Evidently, before I can go on vacation, I have to do more IT work. While this is probably par for the course, what would have happened if I _hadn't_ tried to rent a toboggan. Also, I feel like Mr. Toboggan gave his entire database of passwords to me a bit too easily. Have we met before? Do I do this work often? Have I just gained access to Santa's nuclear armament? What's my motivation?  

The first puzzle gives us a list of passwords and the policy in place when it was created. The goal here is to determine the number of valid passwords.


```r
data <-   read_tsv(here('data/advent_of_code/2020/day2_input.txt'), col_names = 'passwords') 
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> passwords </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 4-6 b: bbbdbtbbbj </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1-6 g: ggvggbgggstg </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1-4 s: lssss </td>
  </tr>
</tbody>
</table>

For a password to be valid, it must contain between the first digit and the second digit of the provided letter. So, for the passwords above, the only valid password is the 3rd, because the first and second have too many of the provided letter (7 B's and 8 G's respectively.)

The easiest way to look into these is separate the policy from the password, and then further break the policy into pieces, first digit, second digit, and letter. From there, we can really check whatever we want.


```r
data <- data %>% 
  separate(passwords, c('policy', 'pass'), sep = ": ") %>% 
  mutate(num1 = as.numeric(str_extract(policy, '[0-9]+')),
         num2 = as.numeric(str_remove(str_extract(policy, '-[0-9]+'), '-')),
         let = str_extract(policy, '[a-z]'))
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> policy </th>
   <th style="text-align:center;"> pass </th>
   <th style="text-align:center;"> num1 </th>
   <th style="text-align:center;"> num2 </th>
   <th style="text-align:center;"> let </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 4-6 b </td>
   <td style="text-align:center;"> bbbdbtbbbj </td>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> b </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1-6 g </td>
   <td style="text-align:center;"> ggvggbgggstg </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> g </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1-4 s </td>
   <td style="text-align:center;"> lssss </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> s </td>
  </tr>
</tbody>
</table>

Now, the only steps left are to count the given letter and compare it to the given numbers. 


```r
data %>% 
  mutate(pass_cnt = str_count(pass, let),
         valid = pass_cnt <= num2 & pass_cnt >= num1)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> policy </th>
   <th style="text-align:center;"> pass </th>
   <th style="text-align:center;"> num1 </th>
   <th style="text-align:center;"> num2 </th>
   <th style="text-align:center;"> let </th>
   <th style="text-align:center;"> pass_cnt </th>
   <th style="text-align:center;"> valid </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 4-6 b </td>
   <td style="text-align:center;"> bbbdbtbbbj </td>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> b </td>
   <td style="text-align:center;"> 7 </td>
   <td style="text-align:center;"> FALSE </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1-6 g </td>
   <td style="text-align:center;"> ggvggbgggstg </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> g </td>
   <td style="text-align:center;"> 8 </td>
   <td style="text-align:center;"> FALSE </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1-4 s </td>
   <td style="text-align:center;"> lssss </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> s </td>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> TRUE </td>
  </tr>
</tbody>
</table>

The count of all valid passwords comes out to 454.  

The second puzzle changes what the policy means. Instead of being limits on the number of the given letter, it's looking for exactly one of the positions to match the letter, but not both. In our top 3, we're looking at the second and third entries being valid.


```r
data %>% 
  mutate(pass_pos1 = str_sub(pass, num1, num1),
         pass_pos2 = str_sub(pass, num2, num2),
         valid = (pass_pos1 == let | pass_pos2 == let) & 
           !(pass_pos1 == let & pass_pos2 == let))
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> policy </th>
   <th style="text-align:center;"> pass </th>
   <th style="text-align:center;"> let </th>
   <th style="text-align:center;"> pass_pos1 </th>
   <th style="text-align:center;"> pass_pos2 </th>
   <th style="text-align:center;"> valid </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 4-6 b </td>
   <td style="text-align:center;"> bbbdbtbbbj </td>
   <td style="text-align:center;"> b </td>
   <td style="text-align:center;"> d </td>
   <td style="text-align:center;"> t </td>
   <td style="text-align:center;"> FALSE </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1-6 g </td>
   <td style="text-align:center;"> ggvggbgggstg </td>
   <td style="text-align:center;"> g </td>
   <td style="text-align:center;"> g </td>
   <td style="text-align:center;"> b </td>
   <td style="text-align:center;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1-4 s </td>
   <td style="text-align:center;"> lssss </td>
   <td style="text-align:center;"> s </td>
   <td style="text-align:center;"> l </td>
   <td style="text-align:center;"> s </td>
   <td style="text-align:center;"> TRUE </td>
  </tr>
</tbody>
</table>

The count of the valid passwords in _this_ case is 649.

# [2019](https://adventofcode.com/2019/day/2) 

This puzzle is using something that I've read comes up a lot in the 2019 Advent of Code challenges, so I'm kind of future proofing this by building this into functions. Part 2 of the problem introduces the concept of nouns and verbs, which are the second and third values in a string of integers to be parsed. This is called "intcode" and is made just to annoy me, I think. The integers should be broken into 4-integer segments, with the first indicating whether the following two integers (the noun and verb) should be added together (denoted by a 1) or multiplied together (denoted by a 2.) Should the first integer be a 99, the program ends. The fourth integer indicates the position that the result of the operation should replace. To make this even more of a pain for me personally, this is zero-indexed, whereas R is... not. 

I originally had this built out a very different way, but this is a much faster and better laid out function, so here's my final answer. I went with a wide matrix, because I can loop through it fast and intuitively, and still call the positions in a format that looks like `data[position]` rather than having to try to transpose and then untranspose ad nauseum. The function I built will look at the first integer in a column, do the operation on the next two integers, and replace the value indicated by the 4th integer. I have added the ability to change the noun and verb on the fly, because that is sort of needed in this part, and integral in the next part.


```r
data <- as.numeric(
  unlist(
    str_split(read_lines(here('data/advent_of_code/2019/day2_input.txt')), ','))
)
```

I read the data in as a vector of integers, because that's what it is. Then, I build my function.


```r
int_parser <- function(data, noun = NULL, verb = NULL){
  if(length(data) %% 4 != 0){
    data[(length(data)+1):(length(data)+(4-(length(data) %% 4)))] <- NA
  }
  .data <- matrix(data, nrow = 4)
  if(!is.null(noun)){
    .data[2] <- noun
  }
  if(!is.null(verb)){
    .data[3] <- verb
  }
  
  for(i in 1:ncol(.data)){
    if(.data[1, i] == 1){
      .data[.data[4, i]+1] <- .data[.data[2, i]+1] + .data[.data[3, i]+1]
    } else if(.data[1, i] == 2){
      .data[.data[4, i]+1] <- .data[.data[2, i]+1] * .data[.data[3, i]+1]
    } else if(.data[1, i] == 99){
      return(as.vector(.data[which(!is.na(.data))]))
    }
  }
  as.vector(.data[which(!is.na(.data))])
}
```

I want my function to return a vector of integers as well, so there's a bit of fancy footwork to get it back out of the matrix format, but nothing too serious. Finally, we can arrive at our first answer, which is the value in the first position (position 0 if your nasty) after replacing the noun and verb with 12 and 2 respectively and running the vector through the parser.


```r
data %>% 
  int_parser(12, 2) %>% 
  `[`(1)
```

```
## [1] 2782414
```

The second part of the problem asks us to find the noun and verb combo that returns 19690720 in the first position. We're told that these numbers must be between 0 and 99, so we can limit or search to that. Again, I built a function, this time a nested for loop that uses the earlier `int_parser` function to run through the data until the first integer is the expected value. This function returns the noun and verb in a list.


```r
value_finder <- function(data, value){
  res <- data
  for(i in 0:99){
    for(j in (0:99)){
      res <- data %>% 
        int_parser(i, j)
      if(res[1] == value){
        break
      }
    } 
  }
  return(list(noun = res[2],
              verb = res[3]))
}

value_finder(data, 19690720)
```

```
## $noun
## [1] 99
## 
## $verb
## [1] 99
```


# [2018](https://adventofcode.com/2018/day/2) 

Back to basics here in 2018, which, in the universe of Advent of Code, is actually 1518. If you'll remember, we are only just recovering from the big North Pole Labor Dispute of 1490, a topic about which I still refuse to discuss. 
We're given a list of box IDs and told to find a checksum of sorts, in which we multiply the count of box IDs with exactly 2 of any letter and the count of box IDs with exactly 3 of any letter. I kind of got lazy on this one and just broke each ID up into individual letters with `str_split()`, sorted those character vectors, and then ran `rle()` which counts the consecutive instances of a character. Then, using the `any()` function to find whether any lengths are 2 or 3, we'll get a vector of TRUE and FALSE values. Summing across these will give the count of boxes with exactly 2 or exactly 3 of any number. From there, just multiply, throw it in a pot, add some broth, a potato. Baby, you've got a stew goin'.


```r
data <- read_lines(here('data/advent_of_code/2018/day2_input.txt'))
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> box_id </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> krdmtuqjmwfoevnadixyclzspv </td>
  </tr>
  <tr>
   <td style="text-align:center;"> yrdmtuqjiwfoevnabfxyclzsph </td>
  </tr>
  <tr>
   <td style="text-align:center;"> kqjvtuqjgwfoevnabixyclzsph </td>
  </tr>
</tbody>
</table>



```r
lns <- str_split(data, '') %>% 
  map(sort) %>% 
  map(rle) %>% 
  map('lengths')

sum(unlist(map(lns, ~any(.x == 2)))) * sum(unlist(map(lns, ~any(.x == 3))))
```

```
## [1] 5952
```

The second part of the question is looking for the shared letters of the two box IDs that are exactly one character apart. For this, I'm dusting off my old friend, `expand_grid()` and using the `stringdist()` function to look for a Hamming distance of 1. The Hamming distance is the number of mismatched characters, or the number of substitutions needed to create an exact match.


```r
match_box <- expand_grid(x = data,
                         y = data) %>% 
  mutate(dist = stringdist::stringdist(x, y, method = 'hamming')) %>% 
  filter(dist == 1) %>% 
  slice(1) %>% 
  mutate(matching = unlist(str_split(x, ''))[which(unlist(str_split(x, '')) %in% unlist(str_split(y, '')))] %>% 
  paste(., collapse = ''))
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> name </th>
   <th style="text-align:center;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> x </td>
   <td style="text-align:center;"> krdmtuqjgwfoevnaboxyglzjph </td>
  </tr>
  <tr>
   <td style="text-align:center;"> y </td>
   <td style="text-align:center;"> krdmtuqjgwfoevnaboxpglzjph </td>
  </tr>
  <tr>
   <td style="text-align:center;"> matching </td>
   <td style="text-align:center;"> krdmtuqjgwfoevnaboxglzjph </td>
  </tr>
</tbody>
</table>


# [2017](https://adventofcode.com/2017/day/2)  

Over in the 2017 CyberZone, we've got to find a different sort of checksum in which we find the difference between the largest and smallest value of each row and add them together. Using `diff()` on the `range()` function should knock this out in short order.


```r
data <- read_lines(here('data/advent_of_code/2017/day2_input.txt')) %>% 
  str_split('\\t')
```


```
##  [1] "179"  "2358" "5197" "867"  "163"  "4418" "3135" "5049" "187"  "166" 
## [11] "4682" "5080" "5541" "172"  "4294" "1397"
```

Since `str_split()` breaks this into a list, mapping across the list is necessary.


```r
map(data, ~diff(range(as.numeric(.x)))) %>% 
  unlist() %>% 
  sum()
```

```
## [1] 39126
```

The second part of this problem asks for the sum of the results of the two numbers in each row that are evenly divisible. So, if the two numbers that were evenly divisible in the first row were 8 and 2, the result would be 4. The sum of those results is the answer to the second part. I just made a function that uses `expand_grid()`, because again, I may be lazy. I use the modulo operator again to find which numbers are evenly divisible, return the result of the division, and sum them all up.


```r
find_div <- function(line){
  expand_grid(x = as.numeric(line), y = as.numeric(line)) %>% 
    filter(x != y,
           x %% y == 0) %>% 
    summarize(z = x / y) %>% 
    pull(z)
}

map(data, find_div) %>% 
  unlist() %>% 
  sum()
```

```
## [1] 258
```


# [2016](https://adventofcode.com/2016/day/2)  

We're trying to break into the Easter Bunny's bathroom, because... reasons. Actually, the fact that the Easter Bunny has a dedicated bathroom that I, a human/Santa/elf hybrid, can presumably use brings up a lot of questions about either me or the Easter Bunny. I'm counting on a deeper dive into this in later days.

The first puzzle provides a series of directions composed of Up, Down, Left, and Right. With a standard number pad, starting at 5, the goal is to use the directions to determine the bathroom code. As with the other positional challenges, I'm going to just break this down into X and Y movements. I originally broke this down into more of a math problem, determining the number we'd actually end up at, but the second part of this problem kind of blew that up, so I went for the positional approach.


```r
data <- read_lines(here('data/advent_of_code/2016/day2_input.txt'))
```


```
## [1] "URULLLLLRLDDUUR..."
```

Here I set up the directional key for each direction. Up and down are positive and negative 1 in the y direction, and right and left are the same in the x direction. I also set up the pad as a long tibble that indicates each key's position.


```r
key <- tibble(dir = c('U', 'D', 'L', 'R'),
              x = c(0, 0, -1, 1),
              y = c(1, -1, 0, 0))

pad <- tibble(num = 1:9,
              x = rep(-1:1, 3),
              y = c(1, 1, 1, 0, 0, 0, -1, -1, -1),
              xy = paste(x, y))
```

The only real tricky parts after that are that A) each line starts not from the center of the pad (5 in this case) but from the previous number. So if the result of the first set of directions puts us at the 1 key, the next set starts at 1, not 5. And B) if the directions take you off the pad, you stay at the current number. For instance, if we reach the 9 key at some point in these sets, a Right or Down direction would leave us on 9. 

So I initialize at position (0, 0) and then run a nested for loop. The outer loop runs through each set of directions, and the inner loop runs through each individual direction in the set of directions, checking if the result would be within our pad's coordinates. If it is, we move, if not, we don't.  


```r
x = 0
y = 0
code <- c()

for(j in seq_along(data)){
  dir <- unlist(str_split(data[j], ''))
  for(i in seq_along(dir)){
    new_x <- x + key$x[which(key$dir == dir[i])]
    new_y <- y + key$y[which(key$dir == dir[i])]
    new_pos <- paste(new_x, new_y)
    if(new_pos %in% pad$xy){
      x = new_x
      y = new_y
    }
  }
  code[j] <- pad$num[which(pad$xy == paste(x, y))]
}

code
```

```
## [1] 1 4 8 9 4
```

The second portion of this problem changes our number pad and our starting position, but nothing else. So the key stays the same, the code stays the same, but the number pad is recoded and the starting position is shifted.


```
## [1] "2" "6" "B" "9" "6"
```

# [2015](https://adventofcode.com/2015/day/2)  

Let's wrap. 

This puzzle provides us with gift dimensions and a proprietary calculation from the elves for optimal gift wrapping. This calculation is just the surface area of the gift plus the area of the smallest side. 

The data originally comes as `LxWxH`, but I just parse this out into separate columns during the data reading process. 


```r
data <- read_delim(here('data/advent_of_code/2015/day2_input.txt'),
                   delim = 'x', 
                   col_names = c('l', 'w', 'h'))
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> l </th>
   <th style="text-align:center;"> w </th>
   <th style="text-align:center;"> h </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 29 </td>
   <td style="text-align:center;"> 13 </td>
   <td style="text-align:center;"> 26 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> 11 </td>
   <td style="text-align:center;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 27 </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> 5 </td>
  </tr>
</tbody>
</table>

The only trick here is for finding the smallest side per gift. While this could be done a number of ways, I think the easiest way is to use `dplyr`'s `rowwise()` function to do calculations by row and then run `min()` across the 3 areas in a row.


```r
data %>% 
  mutate(sa = 2*l*w + 2*w*h + 2*h*l) %>% 
  rowwise() %>% 
  mutate(slack = min(l*w, w*h, h*l),
         paper = sa+slack) %>% 
  ungroup() %>% 
  summarize(total = sum(paper))
```

```
## # A tibble: 1 x 1
##     total
##     <dbl>
## 1 1586300
```

The second part of the problem is looking for the length of ribbon needed to wrap the present around it's shortest sides and tie a bow. The ribbon needed for the bow is equal to the volume of the box, so length x width x height. The ribbon needed to wrap the gift is the shortest distance around its sides, or the perimeter of the smallest face. Again, it feels like the easiest method is using `rowwise()` to pick the smallest two values from each row using `sort()`.


```r
data %>% 
  mutate(bow = l*w*h) %>% 
  rowwise() %>% 
  mutate(wrap = sort(c(l, w, h))[1] * 2 + sort(c(l, w, h))[2] * 2,
         ribbon = bow + wrap) %>% 
  ungroup() %>% 
  summarize(total = sum(ribbon))
```

```
## # A tibble: 1 x 1
##     total
##     <dbl>
## 1 3737498
```

That's the second day of 6 Years of Advent of Code in the books. Formatting may get a bit uneven soon as I try to speed through a few days to catch up, but rest assured, the danger must be growing, for the rowers keep on rowing and they're certainly not showing any signs that they are slowing. AHHHHHHHHH...
