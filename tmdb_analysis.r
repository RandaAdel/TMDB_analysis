---
title: "Project"
author: "Randa"
date: "May 7, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
tmdb_cast_crew <- read.csv("tmdb_cast_crew.csv")
tmdb_movies_metadata <- read.csv("tmdb_movies_metadata.csv")
```

```{r}
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(splitstackshape)
```

```{r}
#Q1: Is there a relationship between movie the number of budget and the movie rating?
x <- tmdb_movies_metadata$budget
y <- tmdb_movies_metadata$vote_average
std_budget <- {(x-median(x))/(sd(x))}
std_average <- {(y-median(y))/(sd(y))}
cov(std_average, std_budget)
#No, There is no significance relation between budget and vote_average although they are standrdized using median to avoid outliers which means (Bigger budget doesn't gaurantee better vote average on IMDB for your movie)
```
```{r}
plot(tmdb_movies_metadata$budget, tmdb_movies_metadata$vote_average)
```


```{r}
#Q2: What are the genres that have the highest average rating?
#Finding the number of vote_count for each voting rate per genres 
vote_avg_count_per_genres <- cSplit(tmdb_movies_metadata, "genres", sep = "|", direction = "long") %>% 
na.omit() %>% 
group_by(genres, vote_average) %>%
summarise(total_vote_count = sum(vote_count, na.rm = T))
vote_avg_count_per_genres %>% head(10)
#A weighted average needs to be calculated
```
```{r}
 total_count_per_genres <- vote_avg_count_per_genres %>%
  group_by(genres) %>%
  summarise(sum_total = sum(total_vote_count)) %>%
  arrange(desc(sum_total))
  
total_count_per_genres%>% head(10)
```

```{r}
# the table from which weighted rate can be calculated =D
temp_per_gen <- merge (vote_avg_count_per_genres ,total_count_per_genres, all=TRUE)
temp_per_gen%>% head(10)
```


```{r}
#weighted average
weigthed_avg_per_genre <- temp_per_gen %>%
  group_by(genres) %>%
  summarise(w_avg = sum((total_vote_count * vote_average)/sum_total)) %>% 
  arrange(desc(w_avg))
  
weigthed_avg_per_genre%>% head(10)
```


```{r}
#The above results indicates that people give war, history, .. etc movies higher rates, although thier vote count are lower than other genres and going further to see if this is caued by the low number of production in those genres or not
tmdb_movies_metadata$dummy<-1     #used for counting number of movies
movie_count_per_genres <- cSplit(tmdb_movies_metadata, "genres", sep = "|", direction = "long") %>% 
 na.omit() %>% 
 group_by(genres) %>%
 summarise(nu_of_movies_per_genre = sum(dummy)) %>%
 arrange(desc(nu_of_movies_per_genre))
movie_count_per_genres%>% head(10)
```
```{r}
#The final conclusion:
#*People like drama movies and the number of movies in this section is the highest [ideal situation]
#*People like War,History, .. etc movies and the number of movies in this genre is really low [potential investment by #producing more of this genre]
#*People doesn't like Comedy, Thriller .. etc although they are heavily produces [potential improvement by producing #higher quality movies or investigating more to find the exact reason]
#Future wise: Adding the profit from Q3 to the brevious analysis will make it more into profitablity
```

```{r}
#Q5: Who are the most profitable directors? Who are the most profitable actors?
#after searching the least budget for a movie is 50,000 - the budget cannot be indicated using the revenue because there are some movies with really low budgets lead to a hit and vice versa
plot(tmdb_movies_metadata$budget)
```

```{r}
movies_budget_limited <- tmdb_movies_metadata %>% filter(budget > 50000)
plot(movies_budget_limited$budget)
```


```{r}
profit_of_movies <- movies_budget_limited %>% 
  select(1,2,4,5,6)  %>% 
  na.omit %>% 
  mutate(profit = (revenue - budget)/budget) %>% 
  arrange(desc(profit)) 
cast_of_movies <- tmdb_cast_crew 
joined_tables <- merge(profit_of_movies, cast_of_movies, by = "movie_id")  %>% arrange(desc(profit)) 
joined_tables%>% head(10)
```


```{r}
#Q7: How did the number of movie raters evolved over the years?
voters_per_year <- tmdb_movies_metadata %>%
  select(4,9) %>% 
  mutate(year = substring(release_date,1,4))  %>% 
  na.omit() %>% 
  group_by(year) %>%
  summarise(total_vote_count = sum(vote_count, na.rm = T)) %>%
  arrange(desc(year))
voters_per_year_arr <- voters_per_year %>% arrange(desc(total_vote_count)) 
voters_per_year%>% head(10)
```
```{r}
#The obvious low number of total_count_vote in 2017 reflects that this is a dataset from 2017 and it is not complete by the time it was collected
```

```{r}
voters_per_year_arr%>% head(10)
```

```{r}
ggplot(data=voters_per_year, aes(x=year, y=total_vote_count)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text( color="#993333", 
                           size=7, angle=90),
          axis.text.y = element_text(color="#993333", 
                           size=14, angle=45)) + scale_x_discrete(breaks = seq(1915, 2018, by = 3)) 
```
