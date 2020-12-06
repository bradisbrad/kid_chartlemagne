---
title: 'Advent of Code: Day 1'
author: Brad Hill
date: '2020-12-06'
slug: adventofcode_day1
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
  - Day 1
---

Island time, space travel, and good ol' fashioned gift delivery. Today's Advent of Code is spent earning stars to... kill the Easter Bunny, I think? 

<!--more-->

_Wow,_ did I underestimate the time it was going to take to backfill this stuff. Not that I thought this was going to a cake walk, but in running through the first 3 days I ran in to so many quirky positional puzzles, and something called [intcode](https://esolangs.org/wiki/Intcode) which... woof. This may have been a more ambitious goal than I thought, but I am pretty certain I'll be totally caught up with the posts by Wednesday. 



# [2020](https://adventofcode.com/2020/day/1)  

The year without a Santa Claus, but not in claymation. This year, Santa's going on a vacation, and I guess we're... Santa? Or maybe, I guess, we're a helpful elf and Santa isn't taking a vacation, which makes more sense but also brings up the North Pole Labor Dispute of 1490, and I'm not trying to be #PoliticalOnMain. 

The first puzzle provides us an "expense report" of sorts, and requires we find the two entries that sum to 2020 and multiply them together. Easy enough. Let's read in the data and take a glance at it.


```r
data <- read_tsv(here('data/advent_of_code/2020/day1_input.txt'), col_names = 'elfspense')
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> elfspense </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 1,772 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1,065 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1,827 </td>
  </tr>
</tbody>
</table>

Quick and dirty method, on which I pride myself here, is just copy the column, essentially a cross join, and sum across them. Again, this almost certainly not the most efficient method, but none of these are going to be. I'm not going to rehash this every time, just go with it. That method, which will use the `expand_grid()` function, looks like this:


```r
elf_grid <- expand_grid(data, 
                        rename(data, elfspense2 = elfspense)) %>% 
  filter(elfspense != elfspense2)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> elfspense </th>
   <th style="text-align:center;"> elfspense2 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 1,772 </td>
   <td style="text-align:center;"> 1,065 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1,772 </td>
   <td style="text-align:center;"> 1,827 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 1,772 </td>
   <td style="text-align:center;"> 1,671 </td>
  </tr>
</tbody>
</table>

This grid has essentially joined each value to every other value. From there, the answer is just whichever row sums to 2020 and find the product.


```r
elf_grid %>% 
  mutate(sum = elfspense + elfspense2,
         prod = elfspense * elfspense2) %>% 
  filter(sum == 2020)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> elfspense </th>
   <th style="text-align:center;"> elfspense2 </th>
   <th style="text-align:center;"> sum </th>
   <th style="text-align:center;"> prod </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 911 </td>
   <td style="text-align:center;"> 1,109 </td>
   <td style="text-align:center;"> 2,020 </td>
   <td style="text-align:center;"> 1,010,299 </td>
  </tr>
</tbody>
</table>

Technically this will return 2 rows, but I'm using a little movie magic here.  

The second part of this question asks the same thing, but 3 values instead of 2. So I'll just add an extra `expand_grid` call and do this all in one fell swoop.


```r
expand_grid(data, 
            rename(data, elfspense2 = elfspense), 
            rename(data, elfspense3 = elfspense)) %>% 
  filter(elfspense != elfspense2,
         elfspense != elfspense3) %>% 
  mutate(sum = elfspense + elfspense2 + elfspense3,
         prod = elfspense * elfspense2 * elfspense3) %>% 
  filter(sum == 2020)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> elfspense </th>
   <th style="text-align:center;"> elfspense2 </th>
   <th style="text-align:center;"> elfspense3 </th>
   <th style="text-align:center;"> sum </th>
   <th style="text-align:center;"> prod </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 1,488 </td>
   <td style="text-align:center;"> 60 </td>
   <td style="text-align:center;"> 472 </td>
   <td style="text-align:center;"> 2,020 </td>
   <td style="text-align:center;"> 42,140,160 </td>
  </tr>
</tbody>
</table>

# [2019](https://adventofcode.com/2019/day/1)  

This year, Santa's lost in space. Evidently the big man doesn't know his way around the Solar System. Honestly, the biggest take away from this story is that other planets don't have their own Santas. You assume that there'd be a Martian Santa, but evidently Earth Santa has to pick up the slack where other planet's dropped the ball. Now, thanks to the other planets' selfishness and lack of preparation, Santa is at the edge of the galaxy and our first puzzle is measuring fuel to get to him.  

The first puzzle gives a table of masses, and requires we do a calculation and return the sum of the results. 



```r
data <- read_table(here('data/advent_of_code/2019/day1_input.txt'), col_names = 'mass')
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> mass </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 83,281 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 110,963 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 137,849 </td>
  </tr>
</tbody>
</table>

The calculation is divide by 3, round down, and subtract 2. Soooo...


```r
data %>% 
  mutate(fuel = floor(mass / 3) - 2)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> mass </th>
   <th style="text-align:center;"> fuel </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 83,281 </td>
   <td style="text-align:center;"> 27,758 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 110,963 </td>
   <td style="text-align:center;"> 36,985 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 137,849 </td>
   <td style="text-align:center;"> 45,947 </td>
  </tr>
</tbody>
</table>

From there, the answer is just going to be the sum of the fuel column.


```r
data %>% 
  mutate(fuel = floor(mass / 3) - 2) %>% 
  summarize(total = sum(fuel))
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 3,210,097 </td>
  </tr>
</tbody>
</table>

The second puzzle gets a little trickier, but is likely a thing that people smarter than me working on actual problems like this need to think about. That fuel calculation doesn't account for the additional mass from the fuel, which of course, doesn't account for that additional fuel, and on and on. 

My method of dealing with this was creating a function that loops through the fuel calculation until the additional fuel needed is 0 or lower. Then I just map across the table and summarize, just like last time. The function to loop through looks like this:


```r
fuel_counter_upper <- function(mass){
  fuel_int <- floor(mass / 3) - 2
  fuel <- fuel_int
  while((floor(fuel / 3) - 2) > 0){
    fuel <- floor(fuel / 3) - 2
    fuel_int = fuel_int + fuel
  }
  fuel_int
}
```

And using it in a sentence looks like this:


```r
data %>% 
  mutate(fuel = map_dbl(mass, fuel_counter_upper))
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> mass </th>
   <th style="text-align:center;"> fuel </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 83,281 </td>
   <td style="text-align:center;"> 41,609 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 110,963 </td>
   <td style="text-align:center;"> 55,447 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 137,849 </td>
   <td style="text-align:center;"> 68,892 </td>
  </tr>
</tbody>
</table>

Cap it off with a good ol' summation and we're moving on! 


```r
data %>% 
  mutate(fuel = map_dbl(mass, fuel_counter_upper)) %>% 
  summarize(total = sum(fuel))
```


<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 4,812,287 </td>
  </tr>
</tbody>
</table>

# [2018](https://adventofcode.com/2018/day/1)  

Somebody is going back in time and trying to sabotage Santa. My money is on Gary Busey. I'm not sure why, but I just have a hunch. I don't anticipate this being a Scooby-Doo mystery, I don't think the villain will be unmasked at the end of this. I think I'm going to have to just always assume it's Gary Busey. And that's okay, but somebody's gotta go back in time and fix all these little sabotage attempts, and it might as well be us.

The first puzzle gives us a set of frequency changes and we have to follow the changes, starting at 0, to find the final frequency.


```r
data <- read_table(here('data/advent_of_code/2018/day1_input.txt'), col_names = 'drift')
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> drift </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> -7 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 16 </td>
  </tr>
</tbody>
</table>

This calls for finding the cumulative sum! R makes this super easy with the `cumsum` function. 


```r
data %>% 
  summarize(drift = cumsum(drift)) %>% 
  tail(1)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> drift </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 477 </td>
  </tr>
</tbody>
</table>

The second puzzle needs us to find the first frequency that is repeated twice. Not the frequency change, but the first frequency. So, the example that is given is, starting at 0, a change of +1 and then -1 would mean 0 is the first frequency reached twice. This could (and does) require multiple trips through the frequency change list. I just built a while loop that checks for duplicates in frequency across multiple runs. The answer is the first duplicate value.


```r
pos_history <- c(0)
pos <- 0

while(!any(duplicated(pos_history))){
  while_data <- data$drift
  while_data[1] <- while_data[1] + pos
  new_hist <- cumsum(while_data)
  pos_history <- c(pos_history, new_hist)
  pos <- tail(pos_history, n = 1)
}

pos_history[which(duplicated(pos_history))][1]
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> drift </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 390 </td>
  </tr>
</tbody>
</table>


# [2017](https://adventofcode.com/2017/day/1)  

This time, Santa can't work the printer. Or the elves can't work the printer. Someone can't work a printer. Hell, I can't work a printer. Anyway, we get zapped into the computer like Spy Kids 3D: Game Over, because no one at the North Pole has heard of checking the toner. 

The first puzzle gives us a series of digits, and the goal is to find the sum of all digits that match the next digit in the list. So, if the list was 1122, then the sum would be 3, because the first 1 and the first 2 match the second 1 and second 2 respectively. The list is circular, so if the list was 104521, the sum would be 2, because the last 1 matches the first 1. 


```r
data <- read_lines(here('data/advent_of_code/2017/day1_input.txt'))
```

Since this is just a series of numbers, I'm not going to give a data preview. It's just a bunch of numbers. I figured the easiest course of action would be to make a for loop. The only maybe "fancy" part of this is the handling of the wrap around. I use the modulo operator (`%%`) to divide the next index value by the length of the data. The modulo returns the remainder (i.e. `4 %% 3` would return 1), so for the value past the length of the data, the result would be 1. Exactly what we want. 


```r
sum_vec <- c()
for(i in 1:str_length(data)){
  curr_index <- i
  next_index <- (i+1) %% str_length(data)
  curr <- str_sub(data, curr_index, curr_index)
  nxt <- str_sub(data, next_index, next_index)
  if(curr == nxt){
    sum_vec[i] <- as.numeric(curr)
  }
}
```

Each iteration adds the value that is followed by a matching value to a vector, called `sum_vec`, and I sum them all to find the answer.

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> sum </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 1,119 </td>
  </tr>
</tbody>
</table>

The second part of the puzzle wants to match against the value halfway around the circle, so half the length of the list forward. In this case, our earlier example of 1212 results in 6 instead of 3, because each 1 and 2 matches the other. The only change on my end is changing how the next index is calculated. Instead of adding 1 to the current index, I'm just adding half the length of the list.


```r
sum_vec <- c()
for(i in 1:str_length(data)){
  curr_index <- i
  next_index <- (i + (str_length(data)/2)) %% str_length(data)
  curr <- str_sub(data, curr_index, curr_index)
  nxt <- str_sub(data, next_index, next_index)
  if(curr == nxt){
    sum_vec[i] <- as.numeric(curr)
  }
}
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> sum </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 1,420 </td>
  </tr>
</tbody>
</table>


# [2016](https://adventofcode.com/2016/day/1)  

This year, the Easter Bunny is trying to steal Christmas. I know Easter isn't as fun as Christmas, but c'mon. Now I've got to go on a spy mission to steal these stolen stars _back_ from the rejected Chuck E. Cheese mascot, and it's just a whole thing.

This one was pretty difficult to start. We're given a list of directions to Easter Bunny HQ, left or right and a number of blocks that direction, and we have to determine where the list takes us. We start facing north, so if we go right and then right again, we're facing south. Here's a glimpse of what we start with:


```r
data <- read_lines(here('data/advent_of_code/2016/day1_input.txt'))
```


```
## [1] "L2, L3, L3"
```

I wanted to split this into a tibble, with a column for the original direction, a column for the number of blocks, and a column for what actual direction we'd be going in on that turn. The first two, easy enough, the last, a little trickier. Using both `cumsum` and the modulo again, I made left indicate -1, right indicate +1, and I took the cumulative sum %% 4. This way, going clockwise from the north, we'd be looking for 0, 1, 2, and 3. 


```r
tibble(dir = c(0, unlist(str_split(data, ', '))),
       turn = replace_na(str_extract(dir, '[LR]'), 0),
       blocks = as.numeric(str_extract(dir, '[0-9]+')),
       turn_num = cumsum(case_when(turn == 'L' ~ -1,
                                   turn == 'R' ~ 1,
                                   T ~ 0))%%4)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> dir </th>
   <th style="text-align:center;"> turn </th>
   <th style="text-align:center;"> blocks </th>
   <th style="text-align:center;"> turn_num </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> L2 </td>
   <td style="text-align:center;"> L </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> L3 </td>
   <td style="text-align:center;"> L </td>
   <td style="text-align:center;"> 3 </td>
   <td style="text-align:center;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> L3 </td>
   <td style="text-align:center;"> L </td>
   <td style="text-align:center;"> 3 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
</tbody>
</table>

From there, I only really wanted to focus on the x and y directions in positive/negative terms, not up/down/left/right. So If the turn_num was 3, I multiply the blocks by negative 1 and put that in the x column, 1 is positive x, 0 is positive y, and 2 is negative y. From there, it's taking the cumulative sum, finding the absolute sum of the x and y directions, and returning the last value.


```r
coords <- dir %>% 
  mutate(x = cumsum(case_when(turn_num == 3 ~ -1*blocks,
                              turn_num == 1 ~ blocks,
                              T ~ 0)),
         y = cumsum(case_when(turn_num == 2 ~ -1*blocks,
                              turn_num == 0 ~ blocks,
                              T ~ 0)),
         blocks_away = abs(x) + abs(y))
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> dir </th>
   <th style="text-align:center;"> turn </th>
   <th style="text-align:center;"> blocks </th>
   <th style="text-align:center;"> turn_num </th>
   <th style="text-align:center;"> x </th>
   <th style="text-align:center;"> y </th>
   <th style="text-align:center;"> blocks_away </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> L2 </td>
   <td style="text-align:center;"> L </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 139 </td>
   <td style="text-align:center;"> 162 </td>
   <td style="text-align:center;"> 301 </td>
  </tr>
</tbody>
</table>

The second part of this puzzle wants to find the first location that is passed twice. The easiest way to do that would just be plot the paths and find the intersection, but where's the fun in that? Why not build a function that creates the path between each point, do another cumulative sum on grouped coordinates and find the first one that pops up twice? 


```r
find_path <- function(x1, y1, x2, y2){
  x <- seq(x1, x2, by = (sign(x2 - x1) * 1))
  y <- seq(y1, y2, by = (sign(y2 - y1) *1))
  tibble(x = x, y = y)[-1, ]
}

path <- tibble(x = 0, y = 0)
for(i in 1:(nrow(coords)-1)){
  path <- bind_rows(path, find_path(coords$x[i], coords$y[i],
                                    coords$x[i+1], coords$y[i+1]))
}
```

The path now looks a lot more granular, with each step explicitly laid out.

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> x </th>
   <th style="text-align:center;"> y </th>
   <th style="text-align:center;"> dist </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> -1 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> -2 </td>
   <td style="text-align:center;"> 0 </td>
   <td style="text-align:center;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> -2 </td>
   <td style="text-align:center;"> -1 </td>
   <td style="text-align:center;"> 3 </td>
  </tr>
</tbody>
</table>

From here, we look for the first one that pops up twice. 


```r
path %>% 
  mutate(dist = abs(x) + abs(y)) %>% 
  mutate(instance = 1) %>% 
  group_by(x, y) %>% 
  mutate(instance = cumsum(instance)) %>% 
  filter(instance > 1) %>% 
  head(n = 1)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> x </th>
   <th style="text-align:center;"> y </th>
   <th style="text-align:center;"> dist </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 127 </td>
   <td style="text-align:center;"> -3 </td>
   <td style="text-align:center;"> 130 </td>
  </tr>
</tbody>
</table>

# [2015](https://adventofcode.com/2015/day/1)  

Santa's lost in an apartment building, and we have instructions. There are evidently infinite floors and subbasements, so Santa is also delivering gifts to Heaven and Hell, presumably. 

The directions are a list of opening (`(`) and closing (`)`) parentheses, indicating up and down one floor, respectively. We're looking for which floor the directions take Santa.



This first puzzle is easy enough. Count the number of opening parentheses, subtract the number of closing parentheses. Be careful of text parsing.


```r
str_count(data, '\\(') - str_count(data, '\\)')
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> floor </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 74 </td>
  </tr>
</tbody>
</table>

The next puzzle wants to know the location of the specific instruction in the list that is the first to take Santa to the basement. You know me, I love a chance to unnecessarily turn a text field into a long tibble and use `cumsum`.


```r
tibble(inst = unlist(str_split(data, ''))) %>% 
  mutate(pos = row_number(),
         move = ifelse(inst == '(', 1, -1),
         floor = cumsum(move)) %>% 
  filter(floor == -1)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> inst </th>
   <th style="text-align:center;"> pos </th>
   <th style="text-align:center;"> move </th>
   <th style="text-align:center;"> floor </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> ) </td>
   <td style="text-align:center;"> 1,795 </td>
   <td style="text-align:center;"> -1 </td>
   <td style="text-align:center;"> -1 </td>
  </tr>
</tbody>
</table>


So that's it. 6 years of 1 day of Advent of Code. I _severely_ underestimated the time it was going to take to put these together, but I'm going to do my best. [And if it took this much work to do one day of these, think about how much work it takes to put something like this together!](https://adventofcode.com/2020/support) Look out for the next one sometime between later today and when the Sun explodes! 
