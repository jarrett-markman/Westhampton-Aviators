library(tidyverse)
library(readxl)
library(ggplot2)
# 6/11
koeppel_1 <- read_excel("excel files/Game 1.xlsx")
koeppel_1 %>%
  filter(Inning == 1 | Inning == 2 | Inning == 3 | Inning == 4) %>%
  filter(`Pitch Type` == "fastball") %>%
  ggplot(aes(x= `Pitch No.`, y = MPH)) +
  geom_point(shape = 1, color = 'black') +
  geom_smooth(method = 'lm') +
  labs(
    x = "Pitch Number",
    y = "Velocity",
    title = "Cole Koeppel Fastball Velocity")
koeppel_1 %>%
  filter(`Pitcher:` == "Cole Koeppel") %>%
  filter(`Pitch Type` == "off-speed") %>%
  ggplot(aes(x= `Pitch No.`, y = MPH)) +
  geom_point(shape = 1, color = 'black') +
  geom_smooth(method = 'lm') +
  labs(
    x = "Pitch Number",
    y = "Velocity",
    title = "Cole Koeppel Off-Speed Velocity")
koeppel_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Cole Koeppel") %>%
  select(`Pitcher:`, MPH, Inning, `Times seeing batter`, `Pitch Result`, `Pitch Type`, `PA Result`)
koeppel_1 %>%
  filter(`Pitcher:` == "Cole Koeppel") %>%
  filter(`Pitch Type` == "off-speed") %>%
  summarize(avg_velo = mean(MPH))
# babip
5/14
#fastball babip
koeppel_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Cole Koeppel") %>%
  filter(`Pitch Type` == "fastball") %>%
  select(`Pitcher:`, MPH, Inning, `Times seeing batter`, `Pitch Result`, `Pitch Type`, `PA Result`)
3/9
#offspeed babip
koeppel_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Cole Koeppel") %>%
  filter(`Pitch Type` == 'off-speed') %>%
  select(`Pitcher:`, MPH, Inning, `Times seeing batter`, `Pitch Result`, `Pitch Type`, `PA Result`)
2/5
# k, bb & h per nine
koeppel_1 %>%
  filter(`Pitcher:` == "Cole Koeppel") %>%
  filter(`PA Result` == 'strikeout')
2/4 * 9 
koeppel_1 %>%
  filter(`Pitcher:` == "Cole Koeppel") %>%
  filter(`PA Result` == 'walk')
5/4 * 9
# already know he gave up 5 hits so h/9 and bb/9 are both 11.25
koeppel_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Cole Koeppel") %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(
    x = "Pitch Location",
    title = "Pitch Location on Hits")
koeppel_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Cole Koeppel") %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double') %>%
  ggplot(aes(x=MPH, y=Location)) +
  geom_point() +
  labs(
    x = "Velocity",
    y = "Pitch Location",
    title = "Field Hit Pitch Location and Velocity")
koeppel_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Cole Koeppel") %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(
    x = "Pitch Location",
    title = "Pitch Location on Field Outs")
koeppel_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Cole Koeppel") %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double') %>%
  ggplot(aes(x=MPH, y=Location)) +
  geom_point() +
  labs(
    x = "Velocity",
    y = "Pitch Location",
    title = "Field Out Pitch Location and Velocity")
# 6/12 gm 1
levine_1 <- read_excel("excel files/Game 2.xlsx")
levine_1 %>%
  filter(`Pitch Type` == 'fastball') %>%
  filter(`Pitcher:` == "Ben Levine") %>%
  ggplot(aes(x= `Pitch No.`, y = MPH)) +
  geom_point(shape = 1, color = 'black') +
  geom_smooth(method = 'lm') +
  labs(
    x = "Pitch Number",
    y = "Velocity",
    title = "Ben Levine Fastball Velocity")
levine_1 %>%
  filter(`Pitch Type` == "off-speed") %>%
  filter(`Pitcher:` == "Ben Levine") %>%
  ggplot(aes(x= `Pitch No.`, y = MPH)) +
  geom_point(shape = 1, color = 'black') +
  geom_smooth(method = 'lm') +
  labs(
    x = "Pitch Number",
    y = "Velocity",
    title = "Ben Levine Off-Speed Velocity")
levine_1 %>%
  filter(`Pitcher:` == "Ben Levine") %>%
  filter(`Pitch Type` == "off-speed") %>%
  summarize(avg_velo = mean(MPH))
levine_1 %>%
  filter(`Pitcher:` == "Ben Levine") %>%
  filter(`Pitch Type` == "fastball") %>%
  summarize(avg_velo = mean(MPH))
levine_1 %>%
  filter(`Pitcher:` == "Ben Levine") %>%
  filter(`PA Result` == 'strikeout')
2/(13/3) * 9
levine_1 %>%
  filter(`Pitcher:` == "Ben Levine") %>%
  filter(`PA Result` == 'walk')
1/(13/3) * 9
levine_1 %>%
  filter(`Pitcher:` == "Ben Levine") %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitch Type` == 'fastball') %>%
  select(`PA Result`)
2/(13/3) * 9
levine_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Ben Levine") %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(
    x = "Pitch Location",
    title = "Pitch Location on Hits")
levine_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Ben Levine") %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double') %>%
  ggplot(aes(x=MPH, y=Location)) +
  geom_point() +
  labs(
    x = "Velocity",
    y = "Pitch Location",
    title = "Field Hit Pitch Location and Velocity")
levine_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Ben Levine") %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(
    x = "Pitch Location",
    title = "Pitch Location on Field Outs")
levine_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Ben Levine") %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double') %>%
  ggplot(aes(x=MPH, y=Location)) +
  geom_point() +
  labs(
    x = "Velocity",
    y = "Pitch Location",
    title = "Field Out Pitch Location and Velocity")
# 6/12 gm 2
torrado_1 <- read_excel("excel files/Game 3.xlsx")
torrado_1 %>%
  filter(`Pitch Type` == "fastball") %>%
  ggplot(aes(x= `Pitch No.`, y = MPH)) +
  geom_point(shape = 1, color = 'black') +
  geom_smooth(method = 'lm') +
  labs(
    x = "Pitch Number",
    y = "Velocity",
    title = "Teo Torrado Fastball Velocity")
torrado_1 %>%
  filter(`Pitch Type` == "off-speed") %>%
  ggplot(aes(x= `Pitch No.`, y = MPH)) +
  geom_point(shape = 1, color = 'black') +
  geom_smooth(method = 'lm') +
  labs(
    x = "Pitch Number",
    y = "Velocity",
    title = "Teo Torrado Off-Speed Velocity")
torrado_1 %>%
  filter(`Pitcher:` == "Teo Torrado") %>%
  filter(`Pitch Type` == "off-speed") %>%
  summarize(avg_velo = mean(MPH))
torrado_1 %>%
  filter(`Pitcher:` == "Teo Torrado") %>%
  filter(`Pitch Type` == "fastball") %>%
  summarize(avg_velo = mean(MPH))
torrado_1 %>%
  filter(`Pitcher:` == "Teo Torrado") %>%
  filter(`PA Result` == 'strikeout')
6/5 * 9
torrado_1 %>%
  filter(`Pitcher:` == "Teo Torrado") %>%
  filter(`PA Result` == 'walk')
1/5 * 9
torrado_1 %>%
  filter(`Pitch Result` == "in-play") %>%
  filter(`Pitch Type` == "fastball") %>%
  select(`PA Result`)
5/5
5/11
5/13
torrado_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Teo Torrado") %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(
    x = "Pitch Location",
    title = "Pitch Location on Hits")
torrado_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Teo Torrado") %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double') %>%
  ggplot(aes(x=MPH, y=Location)) +
  geom_point() +
  labs(
    x = "Velocity",
    y = "Pitch Location",
    title = "Field Hit Pitch Location and Velocity")
torrado_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Teo Torrado") %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(
    x = "Pitch Location",
    title = "Pitch Location on Field Outs")
torrado_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`Pitcher:` == "Teo Torrado") %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double') %>%
  ggplot(aes(x=MPH, y=Location)) +
  geom_point() +
  labs(
    x = "Velocity",
    y = "Pitch Location",
    title = "Field Out Pitch Location and Velocity")
# 6-13
diforte_1 <- read_excel("excel files/6-13.xlsx")
diforte_1 <- diforte_1 %>%
  filter(MPH != "N/A") %>%
  mutate(MPH = as.numeric(MPH))
diforte_1 %>%
  filter(Pitcher == "Michael DiForte") %>%
  filter(`Pitch Type` == "Fastball") %>%
  summarize(x = mean(MPH))
diforte_1 %>%
  filter(Pitcher == "Michael DiForte") %>%
  filter(`Pitch Type` == "Sinker") %>%
  summarise(x = mean(MPH))
diforte_1 %>%
  filter(Pitcher == "Michael DiForte") %>%
  filter(`Pitch Type` == "Slider") %>%
  summarise(x = mean(MPH))
diforte_1 %>%
  filter(Pitcher == "Michael DiForte") %>%
  filter(`Pitch Type` == "Curveball") %>%
  summarise(x = mean(MPH))
diforte_1 %>%
  filter(Pitcher == "Michael DiForte") %>%
  view()
diforte_1 %>%
  filter(Pitcher == "Michael DiForte") %>%
  filter(`Pitch Result...11` == "In Play") %>%
  filter(`PA Result` == "Single" | `PA Result` == "Double" | `PA Result` == "Home Run") %>%
  ggplot(aes(x = `Pitch Type`, y = Location)) +
  geom_point()
diforte_1 %>%
  filter(Pitcher == "Michael DiForte") %>%
  filter(`Pitch Result...11` == "In Play") %>%
  view()
diforte_1 %>%
  filter(Pitcher == "Michael DiForte") %>%
  filter(`PA Result` == "Walk")
# 6-16 gm2
rosa_1 <- read_excel("excel files/6-16 game 2.xlsx")
rosa_1 %>%
  filter(Pitcher == "Jomari Rose") %>%
  filter(`Pitch Type` == "fastball") %>%
  ggplot(aes(x= `Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    x = "Pitch Number",
    y = "Pitch Velocity",
    title = "Jomari Rose Fastball Velocity"
  )
rosa_1 %>%
  filter(Pitcher == "Jomari Rose") %>%
  filter(`Pitch Type` == "change-up") %>%
  ggplot(aes(x= `Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    x = "Pitch Number",
    y = "Pitch Velocity",
    title = "Jomari Rose Change-Up Velocity"
  )
rosa_1 %>%
  filter(Pitcher == "Jomari Rose") %>%
  filter(`Pitch Type` == "off-speed") %>%
  ggplot(aes(x= `Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    x = "Pitch Number",
    y = "Pitch Velocity",
    title = "Jomari Rose Off-Speed Velocity"
  )
rosa_1 %>%
  filter(Pitcher == "Jomari Rose") %>%
  filter(`Pitch Result` == "in-play") %>%
  view()
rosa_1 %>%
  filter(Pitcher == "Jomari Rose") %>%
  filter(`Pitch Result` == "in-play") %>%
  filter(`PA Result` == "single" | `PA Result` == 'double') %>%
  ggplot(aes(x= `Pitch Type`, y = `Pitch Location`)) +
  geom_point() +
  labs(title = "Pitch Location on Hits")
rosa_1 %>%
  filter(Pitcher == "Jomari Rose") %>%
  filter(`Pitch Result` == "in-play") %>%
  filter(`PA Result` != "single" & `PA Result` != 'double') %>%
  ggplot(aes(x= `Pitch Type`, y = `Pitch Location`)) +
  geom_point() +
  labs(
    title = "Pitch Location on Field Outs"
  )
# 6-18
torrado_2 <- read_excel("excel files/6-18-22.xlsx")
torrado_2 %>%
  filter(Pitcher == "Teo Torrado") %>%
  view()
torrado_2 %>%
  filter(Pitcher == "Teo Torrado") %>%
  filter(`Pitch Type` == "FB") %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Torrado Fastball Velocity"
  )
# 62 fastballs
torrado_2 %>%
  filter(Pitcher == "Teo Torrado") %>%
  filter(`Pitch Type` == "BB") %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Torrado Breaking Ball Velocity"
  )
# 32 breaking balls
torrado_2 %>%
  filter(Pitcher == "Teo Torrado") %>%
  filter(`Pitch Type` == "CH")
# 3 change ups
torrado_2 %>%
  filter(Pitcher == "Teo Torrado") %>%
  filter(`Pitch Type` == "FB") %>%
  summarise(x = mean(MPH))
torrado_2 %>%
  filter(Pitcher == "Teo Torrado") %>%
  filter(`Pitch Type` == "BB") %>%
  summarise(x = mean(MPH))
torrado_2 %>%
  filter(Pitcher == 'Teo Torrado') %>%
  filter(`PA Result` == 'walk')
3/16
3/6.67 * 9
7/6.67 * 9
2/6.67 * 9
torrado_2 %>%
  filter(Pitcher == 'Teo Torrado') %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single 1' & `PA Result` != 'single 6' & `PA Result` != 'single 8') %>%
  ggplot(aes(x= `Pitch Type`, y = `Pitch Location`)) +
  geom_point()
# 6/20
chunming_1 <- read_excel("excel files/6-20.xlsx")
chunming_1 %>%
  filter(Pitcher == "Branden Chun-Ming") %>%
  filter(`Pitch Type` == "F") %>%
  ggplot(aes(x= `Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Branden Chun-Ming Fastball Velocity"
  )
# 50 fastballs
chunming_1 %>%
  filter(Pitcher == "Branden Chun-Ming") %>%
  filter(`Pitch Type` == "BR") %>%
  ggplot(aes(x= `Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Branden Chun-Ming Breaking Pitch Velocity"
  )
# 25 more pitches
chunming_1 %>%
  filter(Pitcher == "Branden Chun-Ming") %>%
  filter(`Pitch Type` == "CH") 
# 7 pitches
50/82
25/82
chunming_1 %>%
  filter(Pitcher == "Branden Chun-Ming") %>%
  filter(`Pitch Type` == "BR") %>%
  summarise(x = mean(MPH))
chunming_1 %>%
  filter(Pitcher == "Branden Chun-Ming") %>%
  filter(`Pitch Type` == "F") %>%
  summarise(x = mean(MPH))
chunming_1 %>%
  filter(Pitcher == "Branden Chun-Ming") %>%
  view()
.75 * 9
#6-25
knipe_3 <- read_excel("excel files/6-25.xlsx")
knipe_3 <- knipe_3 %>%
  mutate(MPH = as.numeric(MPH))
knipe_3 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Type` == "Fastball") %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Evan Knipe Fastball Velocity"
  )
knipe_3 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Type` == "Fastball") %>%
  summarise(x = mean(MPH))
# 49 fastballs
knipe_3 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Type` == "Curveball") %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Evan Knipe Curveball Velocity"
  )
knipe_3 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Type` == "Curveball") %>%
  summarise(x = mean(MPH))
# 15 curveballs
knipe_3 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Type` == "Changeup") %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Evan Knipe Changeup Velocity"
  )
knipe_3 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Type` == "Changeup") %>%
  summarise(x = mean(MPH))
# 19 changeups
49/84
15/84
19/84
knipe_3 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Result...8` == "In Play") %>%
  filter(`PA Result` == 'Single' | `PA Result` == 'Double' | `PA Result` == 'Triple' | `PA Result` == 'Home Run') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(
    title = "Hit Locations on Evan Knipe"
  )
knipe_3 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Result...8` == "In Play") %>%
  filter(`PA Result` == 'Single' | `PA Result` == 'Double' | `PA Result` == 'Triple' | `PA Result` == 'Home Run') %>%
  ggplot(aes(x= `Pitch Type`, y= Location)) +
  geom_point() +
  labs(
    title = "Pitch Type and Location on Hits on Evan Knipe"
  )
8/5 * 9
knipe_3 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`PA Result` == "Strikeout")
3/5 * 9
knipe_3 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`PA Result` == "Walk")
1/5 * 9
knipe_3 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Result...8` == "In Play") %>%
  filter(`PA Result` != 'Single') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(
    title = "Pitch Location on Field Outs"
  )
knipe_3 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Result...8` == "In Play") %>%
  filter(`PA Result` != 'Single') %>%
  ggplot(aes(x= `Pitch Type`, y= Location)) +
  geom_point() +
  labs(
    title = "Pitch Location and Type on Field Outs"
  )
# 6-14
knipe_1 <- read_excel("excel files/6-14-22.xlsx")
knipe_1 %>%
  filter(Pitcher == "KNIPE") %>%
  filter(`Pitch Type` =="FB") %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Knipe Fastball Velocity"
  )
knipe_1 %>%
  filter(Pitcher == "KNIPE") %>%
  filter(`Pitch Type` =="FB") %>%
  filter(MPH != "N/A") %>%
  summarise(
    x = mean(MPH)
  )
# 48 fastballs
knipe_1 %>%
  filter(Pitcher == "KNIPE") %>%
  filter(`Pitch Type` =="CH") %>%
  summarise(
    x = mean(MPH)
  )
knipe_1 %>% 
  filter(Pitcher == "KNIPE") %>%
  filter(`Pitch Type` =="CUR") %>%
  summarise(
    x = mean(MPH)
  )
# 28 curveballs
# 14 changeups
knipe_1 %>%
  filter(Pitcher == "KNIPE") %>%
  filter(`Pitch Type` =="CUR") %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Knipe Curveball Velocity"
  )
knipe_1 %>%
  filter(Pitcher == "KNIPE") %>%
  filter(`Pitch Type` =="CH") %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Knipe Changeup Velocity"
  )
48/91
28/91
14/91
knipe_1 %>%
  filter(Pitcher == "KNIPE") %>% 
  filter(`Pitch Result...10` == "IP")
knipe_1 %>%
  filter(Pitcher == "KNIPE") %>% 
  filter(`Pitch Result...10` == "IP") %>%
  filter(`PA Result...14` == "1")
knipe_1 %>%
  filter(Pitcher == "KNIPE") %>% 
  filter(`Pitch Result...10` == "IP") %>%
  filter(`PA Result...14` == "1") %>%
  ggplot(aes(x=Location...12)) +
  geom_bar()
knipe_1 %>%
  filter(Pitcher == "KNIPE") %>% 
  filter(`Pitch Result...10` == "IP") %>%
  filter(`PA Result...14` == "1") %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar()
7/15
7/4 * 9
knipe_1 %>%
  filter(Pitcher == "KNIPE") %>%
  filter(`PA Result...14` == "K" | `PA Result...14` == "KK")
5/4 * 9
knipe_1 %>%
  filter(Pitcher == "KNIPE") %>%
  filter(`PA Result...14` == "BB")
2/4 * 9
# 6-16 game 1
koeppel_2 <- read_excel("excel files/6-16-22.xlsx")
# 4.67
koeppel_2 %>%
  filter(Pitcher == "KOEPPEL") %>%
  filter(`Pitch Type` == "FB") %>%
  filter(MPH != "N/A") %>%
  summarise(x = mean(MPH))
koeppel_2 %>%
  filter(Pitcher == "KOEPPEL") %>%
  filter(`Pitch Type` == "FB") %>%
  filter(MPH != "N/A") %>%
  ggplot(aes(x=`Pitch #`, y= MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Koeppel Fastball Velocity"
  )
# 63 fastballs
koeppel_2 %>%
  filter(Pitcher == "KOEPPEL") %>%
  filter(`Pitch Type` == "CUR") %>%
  filter(MPH != "N/A") %>%
  summarise(x = mean(MPH))
koeppel_2 %>%
  filter(Pitcher == "KOEPPEL") %>%
  filter(`Pitch Type` == "CUR") %>%
  filter(MPH != "N/A") %>%
  ggplot(aes(x=`Pitch #`, y= MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Koeppel Curveball Velocity"
  )
# 22 curveballs
22/89
# 89 pitches
63/89
koeppel_2 %>%
  filter(Pitcher == "KOEPPEL") %>%
  filter(`Pitch Result...10` == "IP") %>%
  view()
koeppel_2 %>%
  filter(Pitcher == "KOEPPEL") %>%
  filter(`Pitch Result...10` == "IP") %>%
  filter(`PA Result...22` == "Single") %>%
  ggplot(aes(x = Location...20)) +
  geom_bar() +
  labs(title = "Koeppel Hit Location")
koeppel_2 %>%
  filter(Pitcher == "KOEPPEL") %>%
  filter(`Pitch Result...10` == "IP") %>%
  filter(`PA Result...22` == "Single") %>%
  ggplot(aes(x = `Pitch Type`)) +
  geom_bar() +
  labs(title = "Koeppel Hit Pitch Type")
koeppel_2 %>%
  filter(Pitcher == "KOEPPEL") %>%
  filter(`Pitch Result...10` == "IP") %>%
  filter(`PA Result...22` != "Single") %>%
  ggplot(aes(x = Location...20)) +
  geom_bar() +
  labs(title = "Koeppel Field Out Location")
koeppel_2 %>%
  filter(Pitcher == "KOEPPEL") %>%
  filter(`Pitch Result...10` == "IP") %>%
  filter(`PA Result...22` != "Single") %>%
  ggplot(aes(x = `Pitch Type`)) +
  geom_bar() +
  labs(title = "Koeppel Field Out Pitch Type")
6/17
6/4.67 * 9
3/4.67 * 9
2/4.67 * 9
# 6/19 double header
#gm1
diforte_2 <- read_excel("excel files/6-19-22 game 1.xlsx")
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`Pitch Type` == "FB")
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`Pitch Type` == "CH")
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`Pitch Type` == "BB")
# 66 fb, 14 ch, 25 bb
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`Pitch Type` == "FB") %>%
  summarise(x = mean(MPH))
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`Pitch Type` == "CH") %>%
  summarise(x = mean(MPH))
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`Pitch Type` == "BB") %>%
  summarise(x = mean(MPH))
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`Pitch Type` == "FB") %>%
  ggplot(aes(x= `Pitch No.`, y= MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "DiForte Fastball Velocity"
  )
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`Pitch Type` == "BB") %>%
  ggplot(aes(x= `Pitch No.`, y= MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "DiForte Breaking Ball Velocity"
  )
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`Pitch Type` == "CH") %>%
  ggplot(aes(x= `Pitch No.`, y= MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "DiForte Changeup Velocity"
  )
66/105
14/105
25/105
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`PA Result` == 'single') %>%
  ggplot(aes(x= `Pitch Location`)) +
  geom_bar() +
  labs(title = "DiForte Pitch Location on Hits")
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`PA Result` == 'single') %>%
  ggplot(aes(x= `Pitch Type`)) +
  geom_bar() +
  labs(title = "DiForte Pitch Type on Hits")
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single') %>%
  ggplot(aes(x= `Pitch Location`)) +
  geom_bar() +
  labs(title = "DiForte Pitch Location on Field Outs")
diforte_2 %>%
  filter(Pitcher == 'Michael DiForte') %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single') %>%
  ggplot(aes(x= `Pitch Type`)) +
  geom_bar() +
  labs(title = "DiForte Pitch Type on Field Outs")
4/19
1/15
4/7 * 9
diforte_2 %>%
  filter(`PA Result` == 'strikeout')
5/7 * 9
diforte_2 %>%
  filter(`PA Result` == 'walk')
5/7 * 9
#gm2
knipe_2 <- read_excel("excel files/6-19-22 game 2.xlsx")
knipe_2 <- knipe_2 %>%
  filter(Pitcher == "Evan Knipe")
knipe_2 %>%
  filter(`Pitch Type` == "FB") %>%
  summarise(x = mean(MPH))
knipe_2 %>%
  filter(`Pitch Type` == 'FB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Knipe Fastball Velocity"
  )
# 60 fastballs
60/95
knipe_2 %>%
  filter(`Pitch Type` == "BB") %>%
  summarise(x = mean(MPH))
knipe_2 %>%
  filter(`Pitch Type` == 'BB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Knipe Breaking Ball Velocity"
  )
# 16 breaking balls
16/95
knipe_2 %>%
  filter(`Pitch Type` == "CH") %>%
  summarise(x = mean(MPH))
knipe_2 %>%
  filter(`Pitch Type` == 'CH') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Knipe Changeup Velocity"
  )
# 19 changeups
19/95
# 95 pitches
knipe_2 %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
knipe_2 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(
    title = "Pitch Location on Hits"
  )
knipe_2 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(
    title = "Pitch Type on Hits"
  )
knipe_2 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(
    title = "Pitch Location on Field Outs"
  )
knipe_2 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(
    title = "Pitch Type on Field Outs"
  )
# 21 total batted balls
# 6 total hits
6/21
# 6 innings
knipe_2 %>%
  filter(`PA Result` == 'strikeout')
3/6 * 9
knipe_2 %>%
  filter(`PA Result` == 'walk')
1/6 * 9
# 6/23
koeppel_3 <- read_excel("excel files/6-23-22.xlsx") %>%
  filter(Pitcher == "KOEPPEL")
koeppel_3 %>%
  filter(`Pitch Type` == 'FB')
# 48 fastballs
48/65
koeppel_3 %>%
  filter(`Pitch Type` == "FB") %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Koeppel Fastball Velocity"
  )
koeppel_3 %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x=mean(MPH))
# 65 pitches
koeppel_3 %>%
  filter(`Pitch Type` == "CUR") %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Koeppel Curveball Velocity"
  )
koeppel_3 %>%
  filter(`Pitch Type` == 'CUR') %>%
  summarise(mean = mean(MPH))
10/65
koeppel_3 %>%
  filter(`Pitch Result...10` == "IP") %>%
  view()
# 11 batted balls
koeppel_3 %>%
  filter(`PA Result...14` == "OUT") %>%
  ggplot(aes(x=Location...20)) +
  geom_bar() +
  labs(title = "Koeppel Pitch Location on Batted Outs")
koeppel_3 %>%
  filter(`PA Result...14` != "OUT") %>%
  filter(`Pitch Result...10` == "IP") %>%
  ggplot(aes(x=Location...20)) +
  geom_bar() +
  labs(title = "Koeppel Pitch Location on Hits")
koeppel_3 %>%
  filter(`PA Result...14` == "OUT") %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Koeppel Pitch Type on Batted Outs')
koeppel_3 %>%
  filter(`PA Result...14` != "OUT") %>%
  filter(`Pitch Result...10` == "IP") %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = "Koeppel Pitch Type on Hits")
8/11
8/3 * 9
koeppel_3 %>%
  filter(`PA Result...22` == 'Walk')
1/3 * 9
# 6/28
diforte_3 <- read_excel("excel files/6-28-22.xlsx")
diforte_3 <- diforte_3 %>%
  filter(Pitcher == "Michael DiForte")
diforte_3 %>%
  filter(`Pitch Type` == "F") %>%
  summarise(x = mean(MPH))
# 45 fastballs
45/95
diforte_3 %>%
  filter(`Pitch Type` == "F") %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = 'DiForte Fastball Velocity'
  )
diforte_3 %>%
  filter(`Pitch Type` == "CH") %>%
  summarise(mean = mean(MPH))
# 17 changeups
17/95
diforte_3 %>%
  filter(`Pitch Type` == "CH") %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = 'DiForte Changeup Velocity'
  )
diforte_3 %>%
  filter(`Pitch Type` == 'BR') %>%
  summarise(x = mean(MPH))
33/95
diforte_3 %>%
  filter(`Pitch Type` == 'BR') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = 'DiForte Breaking Ball Velocity'
  )
diforte_3 %>%
  filter(`Pitch Result` != 'Strike' & `Pitch Result` != 'Ball' & `Pitch Result` != 'Foul' & `Pitch Result` != "Swinging Strike") %>%
  view()
diforte_3 %>%
  filter(`Pitch Result` != 'Strike' & `Pitch Result` != 'Ball' & `Pitch Result` != 'Foul' & `Pitch Result` != "Swinging Strike") %>%
  select(`Pitch Type`, `Pitch Location (from behind home)`, `Pitch Details`, `Batter Handedness`) %>%
  view()
8/22
8/5 * 9
2/5 * 9
3/5 * 9
# 22 batted balls
# 6/29
koeppel_4 <- read_excel("excel files/06-29-22.xlsx")
koeppel_4 <- koeppel_4 %>%
  filter(Pitcher == "Cole Koeppel")
koeppel_4 %>%
  filter(`Pitch Type` == 'Fastball')
52/70
koeppel_4 %>%
  filter(`Pitch Type` == "Curveball")
16/70
koeppel_4 <- koeppel_4 %>%
  filter(MPH != 'N/A') %>% 
  mutate(MPH = as.numeric(MPH))
koeppel_4 %>%
  filter(`Pitch Type` == 'Fastball') %>%
  summarise(x = mean(MPH))
koeppel_4 %>%
  filter(`Pitch Type` == 'Curveball') %>%
  summarise(x = mean(MPH))
koeppel_4 %>%
  filter(`Pitch Type` == 'Fastball') %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Koeppel Fastball Velocity')
koeppel_4 %>%
  filter(`Pitch Type` == 'Curveball') %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Koeppel Curveball Velocity')
koeppel_4 %>%
  filter(`Pitch Result...8` == "In Play") %>%
  view()
# 10 batted balls
koeppel_4 %>%
  filter(`PA Result` == 'Single' | `PA Result` == 'Double') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(title = "Koeppel Pitch Location on Hits")
koeppel_4 %>%
  filter(`PA Result` == 'Single' | `PA Result` == 'Double') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = "Koeppel Pitch Type on Hits")
koeppel_4 %>%
  filter(`PA Result` != 'Single' & `PA Result` != 'Double') %>%
  filter(`Pitch Result...8` == "In Play") %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(title = "Koeppel Pitch Location on Hits")
6/3 * 9
koeppel_4 %>%
  filter(`PA Result` == "Strikeout")
1/3 * 9
koeppel_4 %>%
  filter(`PA Result` == "Walk")
4/3 * 9
# 7/3 - 1
knipe_4 <- read_excel("excel files/7-3 gm1.xlsx")
knipe_4 %>%
  filter(`Pitch Type` == 'Fastball') %>%
  filter(MPH != 'N/A') %>%
  summarise(x = mean(MPH))
# 40 fastballs
40/70
knipe_4 %>%
  filter(`Pitch Type` == 'Changeup') %>%
  filter(MPH != 'N/A') %>%
  summarise(x = mean(MPH))
# 19 changeups
19/70
knipe_4 %>%
  filter(`Pitch Type` == 'Curveball') %>% 
  filter(MPH != 'N/A') %>%
  summarise(x = mean(MPH))
# 11 cbs
11/70
knipe_4 %>%
  filter(`Pitch Type` == "Fastball") %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Knipe Fastball Velocity"
  )
knipe_4 %>%
  filter(`Pitch Type` == "Changeup") %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Knipe Changeup Velocity"
  )
knipe_4 %>%
  filter(`Pitch Type` == "Curveball") %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Knipe Curveball Velocity"
  )
knipe_4 %>%
  filter(`Pitch Result...8` == "In Play") %>%
  view()
knipe_4 %>%
  filter(`Pitch Result...8` == "In Play") %>%
  filter(`PA Result` == 'Single') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(title = "Pitch Location on Hits")
knipe_4 %>%
  filter(`Pitch Result...8` == "In Play") %>%
  filter(`PA Result` == 'Single') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = "Pitch Type on Hits")
knipe_4 %>%
  filter(`Pitch Result...8` == "In Play") %>%
  filter(`PA Result` != 'Single') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(title = "Pitch Location on Field Outs")
knipe_4 %>%
  filter(`Pitch Result...8` == "In Play") %>%
  filter(`PA Result` != 'Single') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = "Pitch Type on Field Outs")
10/23
10/6 * 9
knipe_4 %>%
  filter(`PA Result` == 'Walk')
4/6 * 9
# 7/3 - 2
rosa_2 <- read_excel("excel files/7-3 gm2.xlsx")
rosa_2 <- rosa_2 %>%
  filter(Pitcher == 'Jomari Rosa')
rosa_2 <- rosa_2 %>%
  filter(MPH != 'N/A') %>%
  mutate(MPH = as.numeric(MPH))
rosa_2 %>%
  filter(`Pitch Type` == 'Fastball') %>%
  filter(MPH != 'N/A') %>%
  summarise(x = mean(MPH))
#39 fb
39/65
rosa_2 %>%
  filter(`Pitch Type` == 'Fastball') %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = 'Rosa Fastball Velocity'
  )
rosa_2 %>%
  filter(`Pitch Type` == 'Curveball') %>%
  summarise(x = mean(MPH))
18/65
rosa_2 %>%
  filter(`Pitch Type` == 'Curveball') %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = 'Rose Curveball Velocity'
  )
rosa_2 %>%
  filter(`Pitch Result...8` == 'In Play') %>%
  view()
# 3.1 innings
rosa_2 %>%
  filter(`Pitch Result...8` == 'In Play') %>%
  filter(`PA Result` == 'Single' | `PA Result` == 'Double') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(title = "Rosa Pitch Location on Hits")
rosa_2 %>%
  filter(`Pitch Result...8` == 'In Play') %>%
  filter(`PA Result` == 'Single' | `PA Result` == 'Double') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = "Rosa Pitch Type on Hits")
# 8 hits
# 15 batted balls
8/15
rosa_2 %>%
  filter(`Pitch Result...8` == 'In Play') %>%
  filter(`PA Result` != 'Single' & `PA Result` != 'Double') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(title = "Rosa Pitch Location on Batted Outs")
rosa_2 %>%
  filter(`Pitch Result...8` == 'In Play') %>%
  filter(`PA Result` != 'Single' & `PA Result` != 'Double') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = "Rosa Pitch Type on Batted Outs")
8/3.33 * 9
rosa_2 %>%
  filter(`PA Result` == 'Strikeout')
1/3.33 * 9
rosa_2 %>%
  filter(`PA Result` == 'Walk')
2/3.33 * 9
# 7-4
torrado_3 <- read_excel('excel files/7-4-22.xlsx')
torrado_3 <- torrado_3 %>%
  filter(Pitcher == 'TORRADO')
torrado_3 %>%
  filter(`Pitch Type` == 'FB')
# 55 fastballs
torrado_3 %>%
  filter(`Pitch Type` == 'CUR')
# 11 curveballs
torrado_3 %>%
  filter(`Pitch Type` == 'SL')
# 21 sliders
torrado_3 %>%
  filter(`Pitch Type` == 'CH')
# 5 changeups
torrado_3 %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x = mean(MPH))
torrado_3 %>%
  filter(`Pitch Type` == 'FB') %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Torrado Fastball Velocity')
55/92
torrado_3 %>%
  filter(`Pitch Type` == 'SL') %>%
  summarise(x = mean(MPH))
torrado_3 %>%
  filter(`Pitch Type` == 'SL') %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Torrado Slider Velocity')
21/92
torrado_3 %>%
  filter(`Pitch Type` == 'CUR') %>%
  summarise(x = mean(MPH))
torrado_3 %>%
  filter(`Pitch Type` == 'CUR') %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Torrado Curveball Velocity')
11/92
torrado_3 %>%
  filter(`Pitch Result...10` == 'IP') %>%
  view()
# 5.2 innings
torrado_3 %>%
  filter(`Pitch Result...10` == 'IP') %>%
  filter(`PA Result...22` == 'Single' | `PA Result...22` == 'Double') %>%
  ggplot(aes(x=Location...20)) +
  geom_bar() +
  labs(title = 'Torrado Pitch Location on Hits')
torrado_3 %>%
  filter(`Pitch Result...10` == 'IP') %>%
  filter(`PA Result...22` == 'Single' | `PA Result...22` == 'Double') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Torrado Pitch Type on Hits')
torrado_3 %>%
  filter(`Pitch Result...10` == 'IP') %>%
  filter(`PA Result...22` != 'Single' & `PA Result...22` != 'Double') %>%
  ggplot(aes(x=Location...20)) +
  geom_bar() +
  labs(title = 'Torrado Pitch Location on Batted Outs')
torrado_3 %>%
  filter(`Pitch Result...10` == 'IP') %>%
  filter(`PA Result...22` != 'Single' & `PA Result...22` != 'Double') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Torrado Pitch Type on Batted Outs')
1/7
9/22
9/5.67 * 9
2/5.67 * 9
3/5.67 * 9
# 7/8
knipe_5 <- read_excel('excel files/7-8-22.xlsx')
knipe_5 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Type` == 'FB')
# 53 fastballs
knipe_5 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Type` == 'BB')
# 25 breaking balls
knipe_5 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Type` == 'CH')
# 19 changeups
# 97 pitches, 6 innings
53/97
25/97
19/97
knipe_5 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x = mean(MPH))
knipe_5 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Type` == 'BB') %>%
  summarise(x = mean(MPH))
knipe_5 %>%
  filter(Pitcher == "Evan Knipe") %>%
  filter(`Pitch Type` == 'CH') %>%
  summarise(x = mean(MPH))
knipe_5 %>%
  filter(Pitcher == 'Evan Knipe') %>%
  filter(`Pitch Type` == 'FB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Evan Knipe Fastball Velocity')
knipe_5 %>%
  filter(Pitcher == 'Evan Knipe') %>%
  filter(`Pitch Type` == 'BB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Evan Knipe Breaking Ball Velocity')
knipe_5 %>%
  filter(Pitcher == 'Evan Knipe') %>%
  filter(`Pitch Type` == 'CH') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Evan Knipe Changeup Velocity')
knipe_5 %>%
  filter(Pitcher == 'Evan Knipe') %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
knipe_5 %>%
  filter(Pitcher == 'Evan Knipe') %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double' | `PA Result` == 'Home Run') %>%
  ggplot(aes(x=`Pitch Location`))+
  geom_bar() +
  labs(title = 'Pitch Location on Hits')
knipe_5 %>%
  filter(Pitcher == 'Evan Knipe') %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double' | `PA Result` == 'Home Run') %>%
  ggplot(aes(x=`Pitch Type`))+
  geom_bar() +
  labs(title = 'Pitch Type on Hits')
knipe_5 %>%
  filter(Pitcher == 'Evan Knipe') %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double' & `PA Result` != 'Home Run') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Field Outs')
knipe_5 %>%
  filter(Pitcher == 'Evan Knipe') %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double' & `PA Result` != 'Home Run') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Field Outs')
8/23
8/6 * 9
knipe_5 %>%
  filter(Pitcher == 'Evan Knipe') %>%
  filter(`PA Result` == 'strikeout')
3/6 * 9
knipe_5 %>%
  filter(Pitcher == 'Evan Knipe') %>%
  filter(`PA Result` == 'walk')
2/6 * 9
# 7/9
diforte_4 <- read_excel('excel files/7-9-22.xlsx')
diforte_4 <- diforte_4 %>%
  filter(Pitcher == 'Michael Diforte')
diforte_4 %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x = mean(MPH))
# 57 fastballs
57/99
diforte_4 %>%
  filter(`Pitch Type` == 'FB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'DiForte Fastball Velocity')
diforte_4 %>% 
  filter(`Pitch Type` == 'BB') %>%
  summarise(x = mean(MPH))
34/99
diforte_4 %>%
  filter(`Pitch Type` == 'BB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'DiForte Breaking Ball Velocity')
diforte_4 %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
8/28
7/8 * 9
diforte_4 %>%
  filter(`PA Result` == 'walk')
2/8 * 9
diforte_4 %>%
  filter(`PA Result` == 'strikeout')
2/8 * 9
2/32 * 100
diforte_4 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'DiForte Pitch Location on Hits')
diforte_4 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'DiForte Pitch Type on Hits')
diforte_4 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'DiForte Pitch Location on Field Outs')
diforte_4 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'DiForte Pitch Type on Field Outs')
# 7/10
rosa_3 <- read_excel('excel files/7-10-22 game 1.xlsx')
rosa_3 <- rosa_3 %>%
  filter(Pitcher == 'Jomari Rosa')
rosa_3 %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x = mean(MPH))
# 57 fbs
57/79
rosa_3 %>%
  filter(`Pitch Type` == 'FB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Rosa Fastball Velocity')
rosa_3 %>%
  filter(`Pitch Type` == 'BB')
rosa_3 %>%
  filter(`Pitch Type` == 'CH')
13/79
9/79
rosa_3 %>%
  filter(`Pitch Type` == 'CH') %>%
  summarise(x=mean(MPH))
rosa_3 %>%
  filter(`Pitch Type` == 'CH') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = "Rosa Changeup Velocity")
rosa_3 %>%
  filter(`Pitch Type` == 'BB') %>%
  summarise(x = mean(MPH))
rosa_3 %>%
  filter(`Pitch Type` == 'BB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Rosa Breaking Ball Velocity')
rosa_3 %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
rosa_3 %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double' | `PA Result` == 'Home Run') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Hits')
rosa_3 %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double' | `PA Result` == 'Home Run') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Hits')
rosa_3 %>%
  filter(`Pitch Location` == 'low-left' | `Pitch Location` == 'left') %>%
  view()
rosa_3 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'groundout' | `PA Result` == 'flyout') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Batted Outs')
rosa_3 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'groundout' | `PA Result` == 'flyout') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Batted Outs')
6/2 * 9
5/2 * 9
5/9
6/18
torrado_4 <- read_excel('excel files/7-10-22 game 2.xlsx')
torrado_4 <- torrado_4 %>%
  filter(Pitcher == 'Theo Torrado')
torrado_4 %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x=mean(MPH))
66/97
torrado_4 %>%
  filter(`Pitch Type` == 'BB') %>%
  summarize(x = mean(MPH))
28/97
torrado_4 %>%
  filter(`Pitch Type` == "FB") %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Torrado Fastball Velocity')
torrado_4 %>%
  filter(`Pitch Type` == "BB") %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Torrado Breaking Ball Velocity')
torrado_4 %>%
  filter(`PA Result` == 'strikeout')
torrado_4 %>%
  filter(`PA Result` == 'walk')
2/6 * 9
5/6 * 9
torrado_4 %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
torrado_4 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Hits')
torrado_4 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Hits')
torrado_4 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Batted Outs')
torrado_4 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Batted Outs')
8/6 * 9
# 7/11
koeppel_5 <- read_excel('excel files/7-11 game 1.xlsx')
koeppel_5 <- koeppel_5 %>%
  filter(Pitcher == 'Cole Koeppel')
koeppel_5 %>%
  filter(`Pitch Type` ==  'FB') %>%
  summarise(x=mean(MPH))
62/89
koeppel_5 %>%
  filter(`Pitch Type` == "FB") %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Koeppel Fastball Velocity')
koeppel_5 %>%
  filter(`Pitch Type` == 'BB') %>%
  summarise(x=mean(MPH))
23/89
koeppel_5 %>%
  filter(`Pitch Type` == 'BB') %>%
  ggplot(aes(`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  labs(title = 'Koeppel Breaking Ball Velocity')
koeppel_5 %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
koeppel_5 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double' | `PA Result` == 'Home Run') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Hits')
koeppel_5 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double' | `PA Result` == 'Home Run') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Hits')
koeppel_5 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double' & `PA Result` != 'Home Run') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Field Outs')
koeppel_5 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double' & `PA Result` != 'Home Run') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Field Outs')
7/17
7/5 * 9
koeppel_5 %>%
  filter(`PA Result` == 'strikeout')
koeppel_5 %>%
  filter(`PA Result` == 'walk')
3/5 * 9
aurandt_1 <- read_excel('excel files/7-11 game 2.xlsx')
aurandt_1 <- aurandt_1 %>%
  filter(Pitcher == 'Charles Aranist')
aurandt_1 %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x=mean(MPH))
57/97
aurandt_1 %>%
  filter(`Pitch Type` == 'FB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Charles aurandt Fastball Velocity')
aurandt_1 %>%
  filter(`Pitch Type` == 'BB') %>%
  summarise(x=mean(MPH))
34/97
aurandt_1 %>%
  filter(`Pitch Type` == 'BB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Charles aurandt Breaking Ball Velocity')
aurandt_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
aurandt_1 %>%
  filter(`PA Result` == 'single') %>%
  ggplot(aes(x=`Pitch Type`, y=`Pitch Location`, label = `Batter Hand`)) +
  geom_point() +
  geom_text(vjust = -1, hjust = 0.5) +
  labs(title = 'Pitch Type and Location on Hits')
aurandt_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Field Outs')
aurandt_1 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Field Outs')
3/5 * 9
3/14
aurandt_1 %>%
  filter(`PA Result` == 'strikeout')
4/5 * 9
aurandt_1 %>%
  filter(`PA Result` == 'walk')
# 7-13
knipe_6 <- read_excel('excel files/7-13-22.xlsx')
knipe_6 %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x=mean(MPH))
58/73
knipe_6 %>%
  filter(`Pitch Type` == 'FB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Evan Knipe Fastball Velocity')
knipe_6 %>%
  filter(`Pitch Type` == 'CH') %>%
  summarise(x=mean(MPH))
6/73
knipe_6 %>%
  filter(`Pitch Type` == 'BB') %>%
  summarise(x=mean(MPH))
9/73
knipe_6 %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
knipe_6 %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double') %>%
  ggplot(aes(x=`Pitch Type`, y=`Pitch Location`)) +
  geom_point() +
  labs(title = 'Pitch Type and Location on Hits')
knipe_6 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'flyout' | `PA Result` == 'groundout') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Batted Outs')
knipe_6 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'flyout' | `PA Result` == 'groundout') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Batted Outs')
3/11
3/5 * 9
knipe_6 %>%
  filter(`PA Result` == 'walk')
1/5 * 9
# 7-15
diforte_5 <- read_excel('excel files/7-15-22.xlsx')
diforte_5 <- diforte_5 %>%
  filter(Pitcher == 'Michael DiForte')
diforte_5 %>%
  filter(`Pitch Type` == 'F') %>%
  summarise(x=mean(MPH))
61/97
diforte_5 %>%
  filter(`Pitch Type` == 'F') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Michael DiForte Fastball Velocity')
diforte_5 %>%
  filter(`Pitch Type` == 'BR') %>%
  summarise(x=mean(MPH))
36/97
diforte_5 %>%
  filter(`Pitch Type` == 'BR') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Michael DiForte Breaking Ball Velocity')
diforte_5 %>%
  filter(`Pitch Result` == 'Groundball' | `Pitch Result` == 'Popup' | `Pitch Result` == 'Linedrive' | `Pitch Result` == 'Flyball') %>%
  view()
4/18
4/6 * 9
3/6 * 9
6/4.5
# 7/16
torrado_5 <- read_excel('excel files/7-16-22.xlsx') %>%
  filter(Pitcher == 'Teo Torrado')
torrado_5 %>%
  filter(`Pitch Type` == 'Fastball')
50/92
torrado_5 <- torrado_5 %>%
  filter(MPH != 'N/A') %>%
  mutate(MPH = as.numeric(MPH))
torrado_5 %>%
  filter(`Pitch Type` == 'Fastball') %>%
  summarise(x=mean(MPH))
torrado_5 %>%
  filter(`Pitch Type` == 'Fastball') %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Teo Torrado Fastball Velocity')
torrado_5 %>%
  filter(`Pitch Type` == 'Slider') %>%
  summarise(x=mean(MPH))
32/92
torrado_5 %>%
  filter(`Pitch Type` == 'Slider') %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Teo Torrado Slider Velocity')
torrado_5 %>%
  filter(`Pitch Result...8` == 'In Play') %>%
  view()
torrado_5 %>%
  filter(`Pitch Result...8` == 'In Play') %>%
  filter(`PA Result` == 'Single') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title ='Pitch Type on Hits')
torrado_5 %>%
  filter(`Pitch Result...8` == 'In Play') %>%
  filter(`PA Result` == 'Single') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(title ='Pitch Location on Hits')
torrado_5 %>%
  filter(`Pitch Result...8` == 'In Play') %>%
  filter(`PA Result` != 'Single') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title ='Pitch Type on Batted Outs')
torrado_5 %>%
  filter(`Pitch Result...8` == 'In Play') %>%
  filter(`PA Result` != 'Single') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(title ='Pitch Location on Batted Outs')
5/22
5/7 *9
3/7 * 9
# 7-17
# game 1
aurandt_2 <- read_excel('excel files/7-17-22 game 1.xlsx') %>%
  filter(Pitcher == 'Charles Aurandt')
aurandt_2 %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x=mean(MPH))
57/97
aurandt_2 %>%
  filter(`Pitch Type` == 'BB') %>%
  summarise(x=mean(MPH))
37/97
aurandt_2 %>%
  filter(`Pitch Type` == 'FB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Charles Aurandt Fastball Velocity')
aurandt_2 %>%
  filter(`Pitch Type` == 'BB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Charles Aurandt Breaking Ball Velocity')
aurandt_2 %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
aurandt_2 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Hits')
aurandt_2 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Hits')
aurandt_2 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Batted Outs')
aurandt_2 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Batted Outs')
4/17
6/17
4/5.33 * 9
3/5.33 * 9
# game 2
chunming_2 <- read_excel('excel files/7-17-22 game 2.xlsx') %>%
  filter(Pitcher == 'Branden Chun-Ming')
chunming_2 %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x=mean(MPH))
41/85
chunming_2 %>%
  filter(`Pitch Type` == 'BB') %>%
  summarise(x=mean(MPH))
39/85
chunming_2 %>%
  filter(`Pitch Type` == 'FB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Branden Chun-Ming Fastball Velocity')
chunming_2 %>%
  filter(`Pitch Type` == 'BB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Branden Chun-Ming Breaking Ball Velocity')
chunming_2 %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
chunming_2 %>%
  filter(`PA Result` == 'double' | `PA Result` == 'single') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Hits')
chunming_2 %>%
  filter(`PA Result` == 'double' | `PA Result` == 'single') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Hits')
chunming_2 %>%
  filter(`PA Result` == 'groundout' | `PA Result` == 'flyout') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Batted Outs')
chunming_2 %>%
  filter(`PA Result` == 'groundout' | `PA Result` == 'flyout') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Batted Outs')
5/13
5/4 * 9
3/4 * 9
# 7/19
knipe_7 <- read_excel('excel files/7-19-22.xlsx')
knipe_7 <- knipe_7 %>%
  filter(Pitcher == 'Evan Knipe')
knipe_7 %>%
  filter(`Pitch Type` == 'Fastball') %>%
  filter(MPH != 'N/A') %>%
  mutate(MPH = as.numeric(MPH)) %>%
  summarise(x=mean(MPH))
70/102
knipe_7 %>%
  filter(`Pitch Type` == 'Fastball') %>%
  filter(MPH != 'N/A') %>%
  mutate(MPH = as.numeric(MPH)) %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Evan Knipe Fastball Velocity')
knipe_7 %>%
  filter(`Pitch Type` == 'Changeup') %>%
  filter(MPH != 'N/A') %>%
  mutate(MPH = as.numeric(MPH)) %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Evan Knipe Changeup Velocity')
19/102
knipe_7 %>%
  filter(`Pitch Type` == 'Changeup') %>%
  filter(MPH != 'N/A') %>%
  mutate(MPH = as.numeric(MPH)) %>%
  summarise(x=mean(MPH))
knipe_7 %>%
  filter(`Pitch Type` == 'Slider' | `Pitch Type` == 'Curveball') %>%
  filter(MPH != 'N/A') %>%
  mutate(MPH = as.numeric(MPH)) %>%
  summarise(x=mean(MPH))
knipe_7 %>%
  filter(`Pitch Type` == 'Slider' | `Pitch Type` == 'Curveball') %>%
  filter(MPH != 'N/A') %>%
  mutate(MPH = as.numeric(MPH)) %>%
  ggplot(aes(x=`Pitch #`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Evan Knipe Breaking Ball Velocity')
13/102
knipe_7 %>%
  filter(`Pitch Result...8` == 'In Play') %>%
  view()
knipe_7 %>%
  filter(`PA Result` == 'Single' | `PA Result` == 'Double' | `PA Result` == 'Home Run') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Hits')
knipe_7 %>%
  filter(`PA Result` == 'Single' | `PA Result` == 'Double' | `PA Result` == 'Home Run') %>%
  ggplot(aes(x=Location)) +
  geom_bar() +
  labs(title = 'Pitch Location on Hits')
13/23
13/4.67 * 9
4/4.67 * 9
1/4.67 * 9
knipe_7 %>%
  filter(`PA Result` == 'Flyout' | `PA Result` == 'Groundout') %>%
  ggplot(aes(`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Batted Outs')
knipe_7 %>%
  filter(`PA Result` == 'Flyout' | `PA Result` == 'Groundout') %>%
  ggplot(aes(Location)) +
  geom_bar() +
  labs(title = 'Pitch Location on Batted Outs')
# 7/21
torrado_6 <- read_excel('excel files/7-21-22 game 1.xlsx')
torrado_6 %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x=mean(MPH))
40/64
torrado_6 %>%
  filter(`Pitch Type` == 'FB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Teo Torrado Fastball Velocity')
torrado_6 %>%
  filter(`Pitch Type` == 'BB') %>%
  summarise(x=mean(MPH))
19/64
torrado_6 %>%
  filter(`Pitch Type` == 'BB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Teo Torrado Breaking Ball Velocity')
torrado_6 %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
torrado_6 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double' | `PA Result` == 'Home Run') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Hits')
torrado_6 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double' | `PA Result` == 'Home Run') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Hits')
torrado_6 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double' & `PA Result` != 'Home Run') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Batted Outs')
torrado_6 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double' & `PA Result` != 'Home Run') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Batted Outs')
7/22
7/5 * 9
1/5 * 9
diforte_6 <- read_excel('excel files/7-21-22 game 2.xlsx')
diforte_6 <- diforte_6 %>%
  filter(Pitcher == 'Michael Diforte')
diforte_6 %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x=mean(MPH))
52/75
diforte_6 %>%
  filter(`Pitch Type` == 'FB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Michael DiForte Fastball Velocity')
diforte_6 %>%
  filter(`Pitch Type` == 'BB') %>%
  summarise(x=mean(MPH))
23/75
diforte_6 %>%
  filter(`Pitch Type` == 'BB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Michael DiForte Breaking Ball Velocity')
diforte_6 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Hits')
diforte_6 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` == 'single' | `PA Result` == 'double') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Hits')
diforte_6 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double') %>%
  ggplot(aes(x=`Pitch Type`)) +
  geom_bar() +
  labs(title = 'Pitch Type on Batted Outs')
diforte_6 %>%
  filter(`Pitch Result` == 'in-play') %>%
  filter(`PA Result` != 'single' & `PA Result` != 'double') %>%
  ggplot(aes(x=`Pitch Location`)) +
  geom_bar() +
  labs(title = 'Pitch Location on Batted Outs')
8/4 * 9
# 7/22
chunming_3 <- read_excel('excel files/7-22-22.xlsx')
chunming_3 <- chunming_3 %>%
  filter(Pitcher == 'Branden Chun-Ming')
chunming_3 %>%
  filter(`Pitch Type` == 'FB') %>%
  summarise(x=mean(MPH))
59/75
chunming_3 %>%
  filter(`Pitch Type` == 'BB')
16/75
chunming_3 %>%
  filter(`Pitch Type` == 'BB') %>%
  summarise(x=mean(MPH))
chunming_3 %>%
  filter(`Pitch Type` == 'FB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Branden Chun-Ming Fastball Velocity')
chunming_3 %>%
  filter(`Pitch Type` == 'BB') %>%
  ggplot(aes(x=`Pitch No.`, y=MPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'Branden Chun-Ming Breaking Ball Velocity')
chunming_3 %>%
  filter(`Pitch Result` == 'in-play') %>%
  view()
chunming_3 %>%
  filter(`Pitch Result` == 'in-play') %>%
  select(`Pitch Type`, `Pitch Location`, `PA Result`) %>%
  view()
5/3 * 9
