oscars <- read.csv("/kaggle/input/the-oscar-award/the_oscar_award.csv")

library(tidyverse)
library(dplyr)

head(oscars)

library(dplyr)
options(width = 200, dplyr.print_max = Inf)
top_movies_nominations <- oscars %>%
  group_by(film) %>%
  summarise(
    nominations = n(),
    oscars_won = sum(as.logical(winner)),
    percentage_won = round(sum(oscars_won) / nominations * 100, 1),
    year_ceremony = first(year_ceremony),  
    .groups = 'drop'
  ) %>%
  filter(film != "") %>%
  arrange(desc(nominations), desc(oscars_won)) %>%
  head(n = 20)
print(top_movies_nominations)

top_movies <- oscars %>%
  group_by(film, year_film) %>%
  summarise(
    nominations = n(),
    oscars_won = sum(as.logical(winner)),
    percentage_won = round(sum(oscars_won) / nominations * 100, 1),
    year_ceremony = first(year_ceremony),  
    .groups = 'drop'
  ) %>%
  filter(film != "") %>%
  arrange(desc(nominations), desc(oscars_won)) %>%
  head(n = 20)
options(width = 150, dplyr.print_max = Inf)
print(top_movies)

library(dplyr)
options(width = 200, dplyr.print_max = Inf)
top_movies_wins <- oscars %>%
  group_by(film, year_film) %>%
  summarise(
    nominations = n(),
    oscars_won = sum(as.logical(winner)),
    percentage_won = round(sum(oscars_won) / nominations * 100, 1),
    year_ceremony = first(year_ceremony),  
    .groups = 'drop'
  ) %>%
  filter(film != "") %>%
  arrange(desc(oscars_won), desc(nominations)) %>%
  head(n = 20)
print(top_movies_wins)

library(dplyr)
options(width = 200, dplyr.print_max = Inf)
top_movies_win_percentage <- oscars %>%
  group_by(film, year_film) %>%
  summarise(
    nominations = n(),
    oscars_won = sum(as.logical(winner)),
    percentage_won = round(sum(oscars_won) / nominations * 100, 1),
    year_film = first(year_film), 
    year_ceremony = first(year_ceremony),  
    .groups = 'drop'
  ) %>%
  filter(film != "") %>%
  arrange(desc(percentage_won), desc(nominations)) %>%
  head(n = 20)
print(top_movies_win_percentage)

# Most nominations per year and film
library(dplyr)
options(width = 200, dplyr.print_max = Inf)
most_nominations <- oscars %>%
  filter(!is.na(film) & film != "") %>%  # Exclude rows where the 'film' column is NA or empty
  group_by(year_ceremony, film) %>% 
  summarise(num_nominations = n()) %>%
  group_by(year_ceremony) %>%  
  mutate(max_nominations = max(num_nominations)) %>%
  filter(num_nominations == max_nominations) %>%
  arrange(year_ceremony, desc(num_nominations)) %>%
  ungroup()
# Print the result
head(most_nominations)

most_wins <- oscars %>%
  filter(!is.na(film) & film != "") %>% 
  filter(winner == "True") %>%
  group_by(year_ceremony, film) %>% 
  summarise(num_wins = n()) %>% 
  group_by(year_ceremony) %>%  
  mutate(max_wins = max(num_wins)) %>%
  filter(num_wins == max_wins) %>%
  arrange(year_ceremony, desc(num_wins)) %>%
  ungroup() 
# Print the result
head(most_wins)


options(width = 200, dplyr.print_max = Inf)
nottop_movies_win_percentage <- oscars %>%
  group_by(film, year_film) %>%
  summarise(
    nominations = n(),
    oscars_won = sum(as.logical(winner)),
    percentage_won = round(sum(oscars_won) / nominations * 100, 1),
    year_film = first(year_film), 
    year_ceremony = first(year_ceremony),  
    .groups = 'drop'
  ) %>%
  filter(film != "") %>%
  arrange((percentage_won), desc(nominations)) %>%
  head(n = 20)
head(nottop_movies_win_percentage)

library(dplyr)
category_list <- oscars %>%
  count(category)
total_categories <- nrow(category_list)
head(total_categories)

category <- oscars %>%
  group_by(category) %>%
  summarise(
    nominations = n(),
    oscars_won = sum(as.logical(winner)),
    ave_nominees_category = round(sum(nominations) / oscars_won, 0), #average nominees per category
    .groups = 'drop'
  ) %>%
  filter(category != "") %>%
  arrange((category))
options(width = 100, dplyr.print_max = Inf)
head(category)

actors_nominations <- oscars %>%
  filter(category %in% c("ACTOR", "ACTRESS", "ACTOR IN A SUPPORTING ROLE", "ACTRESS IN A SUPPORTING ROLE", "ACTOR IN A LEADING ROLE", "ACTRESS IN A LEADING ROLE")) %>%
  group_by(name) %>%
  summarise(
    nominations = n(),
    oscars_won = sum(as.logical(winner)),
    percentage_won = round(sum(oscars_won) / nominations * 100, 0),
    .groups = 'drop'
  ) %>%
  filter(name != "") %>%
  arrange(desc(nominations)) %>%
  head(n = 20)
options(width = 100, dplyr.print_max = Inf)
head(actors_nominations)

actors_won <- oscars %>%
  filter(category %in% c("ACTOR", "ACTRESS", "ACTOR IN A SUPPORTING ROLE", "ACTRESS IN A SUPPORTING ROLE", "ACTOR IN A LEADING ROLE", "ACTRESS IN A LEADING ROLE")) %>%
  group_by(name) %>%
  summarise(
    nominations = n(),
    oscars_won = sum(as.logical(winner)),
    percentage_won = round(sum(oscars_won) / nominations * 100, 0),
    .groups = 'drop'
  ) %>%
  filter(name != "") %>%
  arrange(desc(oscars_won)) %>%
  head(n = 20)
options(width = 100, dplyr.print_max = Inf)
head(actors_won)
