---
title: 'Advent of Code: Day 3'
author: Brad Hill
date: '2020-12-07 19:42:00'
slug: adventofcode_day3
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

Dashing through the snow, in a broken rocket ship, o'er the cloth we cut, spirals all the way. Advent of Code: Day 3 has taken a toll.

<!--more-->

These are undoubtedly getting harder and more time consuming, and I'm only getting further behind. But I can weather the storm! What do I care how much it may storm? I've got your love to keep me warm. (I also have, like, 10 bags of hot cocoa, so if your love doesn't suffice, that'll probably do the trick.)




# [2020](https://adventofcode.com/2020/day/3)

Do you remember that old game that came on Windows that had you skiing down a hill, and a yeti would chase you, and you had to avoid trees? Today's challenge is basically that, but with math. (Editor's note: I looked it up, it's called [Ski Free](https://en.wikipedia.org/wiki/SkiFree) and it was part of [Microsoft Entertainment Pack 3](https://en.wikipedia.org/wiki/Microsoft_Entertainment_Pack).)   

So we're provided with a map, where a period indicates open space and a hash represents a tree. The map repeats forever to the right, but does not repeat down. Starting on the top-left open square, and given a slope of right 3 down 1, the first goal is to determine how many trees, or hashes, we would run in to. 


```r
data <- read_tsv(here('data/advent_of_code/2020/day3_input.txt'), col_names = F)
```


```
## # A tibble: 10 x 1
##    X1                             
##    <chr>                          
##  1 ...#...###......##.#..#.....##.
##  2 ..#.#.#....#.##.#......#.#....#
##  3 ......#.....#......#....#...##.
##  4 ...#.....##.#..#........##.....
##  5 ...##...##...#...#....###....#.
##  6 ...##...##.......#....#...#.#..
##  7 ..............##..#..#........#
##  8 #.#....#.........#...##.#.#.#.#
##  9 .#..##......#.#......#...#....#
## 10 #....#..#.#.....#..#...#...#...
```

A quick count shows that each row is 31 characters. I needed this for looping around the front. If you recall the way I did the wrap around in the [day 1 2017 puzzle](https://bradisblogging.com/2020/12/adventofcode_day1/), then the method here should be familiar, using the modulo with the string length to find the adjusted position. 

To find the number of trees hit, I use a while loop to run through new positions until the y position is past the bottom of the map. The function returns the final position of the toboggan and the number of trees encountered for a given slope. 


```r
trees_encountered <- function(.data, right, down){
  
  args <- list(right = right,
               down = down)
  pos <- c(1, 1)
  hit <- 0
  while(pos[2] < nrow(data)){
    pos <- c(pos[1]+args$right, pos[2]+args$down)
    adj_pos <- c(ifelse(pos[1] %% 31 == 0, 31, pos[1] %% 31), pos[2])
    hit <- ifelse(str_sub(data[adj_pos[2],1], adj_pos[1], adj_pos[1]) == '#', 
                  hit+1,
                  hit)
  }
  final_pos <- pos
  hit_count <- hit
  
  list(`Final Position` = final_pos,
       `Trees Encountered` = hit_count)
}
```


```r
trees_encountered(data, 3, 1)
```

```
## $`Final Position`
## [1] 967 323
## 
## $`Trees Encountered`
## [1] 230
```

The second half of this challenge has us trying out different slopes and returning the product of number of trees encountered for each slope. This can easily be done now that I've got a function built out by simply mapping across a list of slopes. (Notice that running `map('Trees Encountered')` pulls out just that result from our list, allowing us to pipe into the `prod()` function without doing much extra work.)



```r
pmap(list(right  = c(1, 3, 5, 7, 1), 
          down = c(1, 1, 1, 1, 2)),
     trees_encountered, 
     .data = data) %>% 
  map('Trees Encountered') %>% 
  unlist() %>% 
  prod()
```

```
## [1] 9533698720
```

# [2019](https://adventofcode.com/2019/day/3)

We got shot into space without the fuel management system totally installed, so now we're tasked with doing extensive electrical work on a ship that is hurtling through space at a blistering speed! No pressure. Hopefully we don't get pulled over by any Spacetime Inspectors asking for some sort of Electrician's License. I'm also assuming we have no Blorgon stowaways, but fingers crossed.

We're given two sets of directions, using that same UDLR directional instruction and a number of spaces/blocks to move. The difference between this one and the [day 1 2016 puzzle](https://bradisblogging.com/2020/12/adventofcode_day1) is that we're looking for the intersection of two paths rather than one path with itself. I don't know whether this actually was more work, or just felt like more work, but either way, I'm not particularly happy with the way this answer turned out. 

```r
data <- read_lines(here('data/advent_of_code/2019/day3_input.txt'))
```


```
## [[1]]
## [1] "R993,U847,R868,D286..."
## 
## [[2]]
## [1] "L1002,D658,L695,U170..."
```

I decided to parse these out the same way I did in the 2016 puzzle, with x and y directions, blocks, etc. I add a wire indicator for good measure, and row numbers for the second part of the puzzle.


```r
wire1 <- tibble(dir = c(0, unlist(str_split(data[1], ','))),
                turn = replace_na(str_extract(dir, '[LRUD]'), 0),
                blocks = as.numeric(str_extract(dir, '[0-9]+')),
                wire = 1,
                x = cumsum(case_when(turn == 'R' ~ blocks,
                                     turn == 'L' ~ blocks * -1,
                                     T ~ 0)),
                y = cumsum(case_when(turn == 'U' ~ blocks,
                                     turn == 'D' ~ blocks * -1,
                                     T ~ 0))) %>% 
  select(x, y, wire) %>% 
  mutate(blocks_away = abs(x) + abs(y),
         row = row_number())

wire2 <- tibble(dir = c(0, unlist(str_split(data[2], ','))),
                turn = replace_na(str_extract(dir, '[LRUD]'), 0),
                blocks = as.numeric(str_extract(dir, '[0-9]+')),
                wire = 2,
                x = cumsum(case_when(turn == 'R' ~ blocks,
                                     turn == 'L' ~ blocks * -1,
                                     T ~ 0)),
                y = cumsum(case_when(turn == 'U' ~ blocks,
                                     turn == 'D' ~ blocks * -1,
                                     T ~ 0))) %>% 
  select(x, y, wire) %>% 
  mutate(blocks_away = abs(x) + abs(y),
         row = row_number())
```


```
## # A tibble: 3 x 5
##       x     y  wire blocks_away   row
##   <dbl> <dbl> <dbl>       <dbl> <int>
## 1     0     0     1           0     1
## 2   993     0     1         993     2
## 3   993   847     1        1840     3
```

```
## # A tibble: 3 x 5
##       x     y  wire blocks_away   row
##   <dbl> <dbl> <dbl>       <dbl> <int>
## 1     0     0     2           0     1
## 2 -1002     0     2        1002     2
## 3 -1002  -658     2        1660     3
```

It's relatively smooth sailing from here. This is the same `find_path()` function I built for the day 1 2016 puzzle. Running it across each of the wire tables gives us the full path with each individual step.


```r
find_path <- function(x1, y1, x2, y2){
  x <- seq(x1, x2, by = (sign(x2 - x1) * 1))
  y <- seq(y1, y2, by = (sign(y2 - y1) *1))
  tibble(x = x, y = y)[-1, ]
}

wire1_path <- tibble(x = 0, y = 0)
for(i in 1:(nrow(wire1)-1)){
  wire1_path <- bind_rows(wire1_path, find_path(wire1$x[i], wire1$y[i],
                                                wire1$x[i+1], wire1$y[i+1]))
}

wire2_path <- tibble(x = 0, y = 0)
for(i in 1:(nrow(wire2)-1)){
  wire2_path <- bind_rows(wire2_path, find_path(wire2$x[i], wire2$y[i],
                                                wire2$x[i+1], wire2$y[i+1]))
}
```

Now, to close this out, the first part asks for the nearest intersection by Manhattan distance (x and y movement only) and the second part asks for the fewest combined steps to get to an intersection (hence the row numbers.) An inner join gives us our intersections, arranging by distance gives us our first answer, and arranging by the sum of the row numbers to get to each intersection gives us the answer to the second part. (Quick tip here, I subtract each row number by 1 because technically the first row for each isn't a step but an initialization at point (0, 0) which throws off the calculation.)


```r
wire1_path %>% 
  mutate(row = row_number()) %>% 
  inner_join(wire2_path %>% 
               mutate(row = row_number()), by = c('x', 'y')) %>% 
  mutate(dist = abs(x) + abs(y)) %>% 
  filter(dist != 0) %>% 
  arrange(dist) %>% 
  slice(1)
```

```
## # A tibble: 1 x 5
##       x     y row.x row.y  dist
##   <dbl> <dbl> <int> <int> <dbl>
## 1    -3  -526 25286 15994   529
```


```r
wire1_path %>% 
  mutate(row = row_number()) %>% 
  inner_join(wire2_path %>% 
               mutate(row = row_number()), by = c('x', 'y')) %>% 
  mutate(dist = abs(x) + abs(y),
         time = row.x-1 + row.y-1) %>% 
  filter(dist != 0) %>% 
  arrange(time) %>% 
  slice(1)
```

```
## # A tibble: 1 x 6
##       x     y row.x row.y  dist  time
##   <dbl> <dbl> <int> <int> <dbl> <dbl>
## 1   993   355  1349 19039  1348 20386
```

# [2018](https://adventofcode.com/2018/day/3)

You'd think for a group of highly trained, task specific elves, they'd be better at organizing their fabric cutting sessions. As it stands, there are over a thousand elves sitting around this massive piece of fabric with, presumably, tiny elf-sized scissors, trying to cut the same 4 inches from the middle of the thing. Hey bud, just pick an edge and cut. 

This challenge gives us data that represents a few things: an ID, a starting location, and an area of cloth to be cut. The goal here is to find the amount of fabric in square inches that falls within two or more IDs. Luckily, splitting up the data here is very easy, but the rest is extra tough if you don't use the right packages (spoiler: I don't.)


```r
data <- read_lines(here('data/advent_of_code/2018/day3_input.txt'))
```


```
## [1] "#1 @ 265,241: 16x26" "#2 @ 584,382: 12x8"  "#3 @ 584,823: 22x10"
```

Splitting this data just takes a few carefully parametered calls of `str_extract()` and `str_remove()`.


```r
info <- tibble(claim = str_extract(data, '#[0-9]+'),
               start_x = as.numeric(str_remove(str_extract(data, '[0-9]+,'), ','))+1,
               start_y = as.numeric(str_remove(str_extract(data, '[0-9]+:'), ':'))+1,
               x = as.numeric(str_remove(str_extract(data, '[0-9]+x'), 'x')),
               y = as.numeric(str_remove(str_extract(data, 'x[0-9]+'), 'x'))) 
```


```
## # A tibble: 3 x 5
##   claim start_x start_y     x     y
##   <chr>   <dbl>   <dbl> <dbl> <dbl>
## 1 #1        266     242    16    26
## 2 #2        585     383    12     8
## 3 #3        585     824    22    10
```

From this point, I do this weird Frankenstein's Monster of a `mutate()` call in which I create _**THREE**_ different list columns, a sure sign that I _definitely_ know what I'm doing, consisting of a sequence of x coordinates, a sequence of y coordindates, and the `expand_grid()` call that combines the two to find all coordinates that belong to that ID. Unnesting that final `fab` column gives us every square inch of fabric claimed by each ID, and grouping by x and y allows us to see which square inches are claimed multiple times. 


```r
claimed <- info %>% 
  mutate(x_coords = pmap(list(from = start_x,
                              to = start_x + (x-1),
                              by = 1),
                         seq),
         y_coords = pmap(list(from = start_y,
                              to = start_y + (y-1),
                              by = 1),
                         seq),
         fab = map2(x_coords, y_coords, ~expand_grid(x = .x, y = .y))) %>% 
  select(claim, fab) %>% 
  unnest(fab)
```


```
## # A tibble: 3 x 3
##   claim     x     y
##   <chr> <dbl> <dbl>
## 1 #1      266   242
## 2 #1      266   243
## 3 #1      266   244
```

From here, a quick filter and summarize gives us the number of square inches claimed multiple times.


```r
claimed %>% 
  group_by(x, y) %>% 
  summarize(claims = n()) %>% 
  ungroup() %>% 
  filter(claims > 1) %>% 
  summarize(claims = n())
```

```
## # A tibble: 1 x 1
##   claims
##    <int>
## 1 101781
```

We can use this same table to answer the second question: which ID doesn't overlap with any other ID. 


```r
claimed %>% 
  group_by(x, y) %>% 
  mutate(claims = n()) %>% 
  ungroup() %>% 
  group_by(claim) %>% 
  filter(all(claims == 1)) %>% 
  pull(claim) %>% 
  unique()
```

```
## [1] "#909"
```

# [2017](https://adventofcode.com/2017/day/3)

I have no quips for this. We're still in a computer, but this time, there are spirals. It sucks. I mean, it's a good challenge, but oof.

It's another one where I tried to make it too mathy and ended up finding a better way by just brute force looping it. Our input data this time is a single number, `361527`, and we're supposed to find how many steps from the center of a spiral (Manhattan distance) the number is.   

The secret here is recreating the problem layout, which is a spiral of numbers. I, at first, wanted to use the fact that every layer of the spiral contained exactly 2 square roots, a fact that allowed me to figure out the first one initially, after a _lot_ of trial and error, but completely left me helpless in the second part. 

The pattern of the spiral is right, up, left, down, so what did I do? I created a table of coordinates, of course! Noticeably, after 2 moves, you then extend how many times you need to move in a direction. So, starting from the center of a spiral, you move right once, up once, but then you must move left twice and down twice to get to a point where you can continue the spiral. From there it's right three, up three, on and on. This pattern made it more convenient to set up the spiral, and let me tell you, I set up an absolutely massive spiral.



```r
data <- 361527

move <- as_tibble(data.frame(times = unlist(map(1:750, rep, 2)),
                             x = c(1, 0, -1, 0),
                             y = c(0, 1, 0, -1)))

paths <- tibble(x = c(0, rep(move$x, move$times)),
                y = c(0, rep(move$y, move$times))) %>% 
  mutate(x = cumsum(x),
         y = cumsum(y),
         val = 1:nrow(.))
```


```
## # A tibble: 563,251 x 3
##       x     y   val
##   <dbl> <dbl> <int>
## 1     0     0     1
## 2     1     0     2
## 3     1     1     3
## # ... with 563,248 more rows
```

This table now gives the coordinates and values of each position in the spiral. Now we've just got to find our puzzle input and determine the distance from the center to get our first answer.


```
## # A tibble: 1 x 4
##       x     y    val  dist
##   <dbl> <dbl>  <int> <dbl>
## 1   301    25 361527   326
```

The second part of this problem uses a slightly different spiral that, instead of each value being 1 above the last, it's a sum of all adjacent values. This is tricky, but doable by setting all values of the spiral to `NA` and using a for loop to find and sum all surrounding coordinates for a given coordinate pair. We could keep the move table the same, but since we have to loop through this table, and we will reach our input number much faster than we did when increasing by 1, we'll change it to only do a few layers of the spiral. We also have to change our path table to use the `NA` approach.


```r
move <- as_tibble(data.frame(times = unlist(map(1:10, rep, 2)),
                             x = c(1, 0, -1, 0),
                             y = c(0, 1, 0, -1)))

paths <- tibble(x = c(0, rep(move$x, move$times)),
                y = c(0, rep(move$y, move$times))) %>% 
  mutate(x = cumsum(x),
         y = cumsum(y),
         val = ifelse(row_number() == 1, 1, NA))
```


```
## # A tibble: 111 x 3
##       x     y   val
##   <dbl> <dbl> <dbl>
## 1     0     0     1
## 2     1     0    NA
## 3     1     1    NA
## 4     0     1    NA
## 5    -1     1    NA
## # ... with 106 more rows
```

Now we sum the value of every position where the absolute distance from the current coordinate is less than or equal to 1, making sure to set the `na.rm` argument to `TRUE` to allow us to add new values.


```r
for(i in 2:nrow(paths)){
  paths$val[i] <- sum(paths$val[which(abs(paths$x - paths$x[i])<=1 & 
                                        abs(paths$y - paths$y[i])<=1)], na.rm = T)
}
```


```
## # A tibble: 111 x 3
##       x     y   val
##   <dbl> <dbl> <dbl>
## 1     0     0     1
## 2     1     0     1
## 3     1     1     2
## 4     0     1     4
## 5    -1     1     5
## # ... with 106 more rows
```


```r
paths %>% 
  filter(val > data) %>% 
  head(n = 1)
```

```
## # A tibble: 1 x 3
##       x     y    val
##   <dbl> <dbl>  <dbl>
## 1    -3     4 363010
```

# [2016](https://adventofcode.com/2016/day/3)

Oh thank God. Back to the normalcy of trying to kidnap the Easter Bunny and steal its... eggs? Frankly, I'm still pretty unclear on the whole Easter Bunny thing in the first place, but here we are, deep in the Easter Bunny's lair, or office, or dungeon. Like I said, I don't understand the Easter Bunny. There are a bunch of specs for triangles scrawled all over the walls, serial killer style.  

We're checking for valid triangles now. We're provided three side lengths and must determine whether the triangle is valid (the sum of any 2 sides must be larger than the 3rd.)


```r
data <- read_table(here('data/advent_of_code/2016/day3_input.txt'), col_names = F)
```


```
## # A tibble: 1,734 x 3
##      X1    X2    X3
##   <dbl> <dbl> <dbl>
## 1   330   143   338
## 2   769   547    83
## 3   930   625   317
## # ... with 1,731 more rows
```

Hearkening back to the [day 2 2015 puzzle,](https://bradisblogging.com/2020/12/adventofcode_day2/) we get to use `rowwise()` again! There are certainly other ways to do this, but just running across the 3 lengths and using `min()`, `median()`, and `max()` to compare sides made this very fast.


```r
data %>% 
  rowwise() %>% 
  summarize(low = min(X1, X2, X3),
            mid = median(c(X1, X2, X3)),
            high = max(X1, X2, X3),
            valid = low + mid > high)
```


```
## # A tibble: 1,734 x 4
##     low   mid  high valid
##   <dbl> <dbl> <dbl> <lgl>
## 1   143   330   338 TRUE 
## 2    83   547   769 FALSE
## 3   317   625   930 TRUE 
## # ... with 1,731 more rows
```


The final answer is the sum of all valid triangles, or 917. 

The second part of this question got a little trickier by indicating that, instead of the dimensions of a triangle being displayed by row, it was actually by 3s in columns. Cool curveball! Be a shame if something... happened to it. 

Essentially, I broke our data up into groups of 3 using the world famous modulo and used the `pivot_longer()` function to get it into a long format. From there, grouping by both the newly added group number and the column, I was able to check for validity of these new triangle dimensions.


```r
data %>% 
  mutate(grp = ((row_number()-1)%/%3)+1) %>% 
  pivot_longer(cols = c(X1, X2, X3)) %>% 
  group_by(grp, name) %>% 
  summarize(valid = sort(value)[1] + sort(value)[2] > sort(value)[3])
```


```
## # A tibble: 1,734 x 3
## # Groups:   grp [578]
##     grp name  valid
##   <dbl> <chr> <lgl>
## 1     1 X1    TRUE 
## 2     1 X2    TRUE 
## 3     1 X3    TRUE 
## # ... with 1,731 more rows
```

Once again, the answer is the sum of all valid triangles: 1649.

# [2015](https://adventofcode.com/2015/day/3)

We've got more directions! This time, we're given symbols to mean up, right, left, and down. `^><v`. This time, though, they're given to use by a very drunk elf, so we hit a lot of these houses twice. I'm not going to be responsible for when we're inevitably sued by these kids who don't get presents because this eggnogged elf decided he "knew this block like he grew up here." 

As usual, I'm splitting this into a coordinate table starting with position 0.


```r
data <- read_lines(here('data/advent_of_code/2015/day3_input.txt'))

dir <- tibble(dir = c(0, unlist(str_split(data, ''))))
```


```
## [1] "^><^>>>^<^..."
```

```
## # A tibble: 8,193 x 1
##   dir  
##   <chr>
## 1 0    
## 2 ^    
## 3 >    
## 4 <    
## # ... with 8,189 more rows
```

The first part of this challenge asks how many houses receive at least one gift. We can do this pretty effectively by just looking at the number of distinct coordinates we're taken to: 2081.


```r
dir %>% 
  mutate(x = case_when(dir == '>' ~ 1,
                       dir == '<' ~ -1,
                       T ~ 0),
         y = case_when(dir == '^' ~ 1,
                       dir == 'v' ~ -1,
                       T ~ 0),
         x = cumsum(x),
         y = cumsum(y)) %>% 
  distinct(x, y)
```


```
## # A tibble: 2,081 x 2
##       x     y
##   <dbl> <dbl>
## 1     0     0
## 2     0     1
## 3     1     1
## # ... with 2,078 more rows
```


The next part of the question takes place during The Santa Clause 2 when they decide to build a robot Santa. Or I guess technically a big toy Santa. He turns evil, it's a whole thing. Fun for the whole family! Anyway, Plastic Tim Allen and the real Santa start at the same location and alternate following the directions. So Santa gets the first move, PTA gets the second, Santa gets the third, etc. 

Using, you guessed it, the modulo, we're going to assign each direction to either Santa or Plastic Tim Allen. After we build this new coordinate table, we can combine both Santa's and PTA's coordinates into one long table, look for the distinct houses, and determine the answer to our second question. How many houses get at least one present this year? 2341!


```r
plastic_tim_allen <- dir %>% 
  mutate(row = row_number(),
         x1 = case_when(dir == '>' & row %% 2 == 0 ~ 1,
                        dir == '<' & row %% 2 == 0  ~ -1,
                        T ~ 0),
         y1 = case_when(dir == '^' & row %% 2 == 0  ~ 1,
                        dir == 'v' & row %% 2 == 0  ~ -1,
                        T ~ 0),
         x1 = cumsum(x1),
         y1 = cumsum(y1),
         x2 = case_when(dir == '>' & row %% 2 != 0 ~ 1,
                        dir == '<' & row %% 2 != 0  ~ -1,
                        T ~ 0),
         y2 = case_when(dir == '^' & row %% 2 != 0  ~ 1,
                        dir == 'v' & row %% 2 != 0  ~ -1,
                        T ~ 0),
         x2 = cumsum(x2),
         y2 = cumsum(y2))

bind_rows(plastic_tim_allen %>% 
            select(x = x1, y = y1),
          plastic_tim_allen %>% 
            select(x = x2, y = y2)) %>% 
  distinct() 
```


```
## # A tibble: 2,341 x 2
##       x     y
##   <dbl> <dbl>
## 1     0     0
## 2     0     1
## 3    -1     1
## # ... with 2,338 more rows
```

Two in a day!? Who am I, a famous TikTok child pointing at random names in a video? Unfortunately, I am now out of days that I've pre-completed, so these next few days (how am I still 4 behind?) might be a little more erratic and stream of consciousness style. Or maybe I'll suddenly get really good at writing! But, ya know, don't bet on it. 
