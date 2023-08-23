################### STQD6114 Unstructured Data Analytics
################### Project 1 Question 2

library(rvest)
library(stringr)
library(lubridate)
library(dplyr)

################### Scrap data
######### Genres = musical   ######### As of date xx May 2023
# sort by popularity from IMDb
# first 3 pages - top 150 musical movie
m_pages<-paste0("https://www.imdb.com/search/title/?title_type=feature&genres=musical&start=",c(1,51,101))

# function to scrap relevant info
# page = m_pages[1|2|3]

# movie title - may different from webpage due to translation
m_title<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url,".lister-item-header a")
  html_text(nodes)
}

# release year
m_year<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url,".text-muted.unbold")
  html_text(nodes)
}

# duration
m_time<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url,".lister-item-header+ .text-muted")
  html_text(nodes)
}

# rating from users
m_rating<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url,".lister-item-content")
  html_text(nodes)
}

# number of votes
m_vote<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url,".lister-item-content")
  html_text(nodes)
}

# director(s)
m_director<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url,".text-muted~ .text-muted+ p , .rating-bar~ .text-muted+ p")
  html_text(nodes)
}

# Extract values from the function
musical_title<-do.call(c,lapply(m_pages,m_title))

musical_year<-do.call(c,lapply(m_pages,m_year))
# take only the digit of year and change the structure
musical_year<-str_extract(musical_year,"\\d+")
musical_year<-year(as.Date(musical_year,format = "%Y"))

musical_time<-do.call(c,lapply(m_pages,m_time))
# take only the digits right before "min" and change the structure
musical_time<-str_extract(musical_time,"\\d+ min")
musical_time<-as.integer(str_extract(musical_time,"\\d+"))

musical_rating<-do.call(c,lapply(m_pages,m_rating))
# take the pattern of rating (0.0-10.0) and change the structure
musical_rating<-as.numeric(str_extract(musical_rating,"\\d+\\.\\d"))

musical_vote<-do.call(c,lapply(m_pages,m_vote))
# take the part with number of votes
musical_vote<-str_extract(musical_vote,"\\bVotes:\n\\s*.*\n")
# clear all "," to get the digits and change the structure
musical_vote<-str_remove_all(musical_vote,",")
musical_vote<-as.integer(str_extract(musical_vote,"\\d+"))

musical_director<-do.call(c,lapply(m_pages,m_director))
# Extract a string of words between two specific words in R
# take all before "Star"
musical_director<-sub("(.*?) *Star.*","\\1",musical_director)
# take all after director(s):\n
musical_director<-sub(".*Director:\n(.*?) *","\\1",musical_director)
musical_director<-sub(".*Directors:\n(.*?) *","\\1",musical_director)
# remove other characters and extra spaces
musical_director<-str_remove(musical_director,"[|]")
musical_director<-str_remove_all(musical_director,"\\n")
musical_director<-str_trim(musical_director)


######### Genres = animation   ######### As of date xx May 2023
# sort by popularity from IMDb
# first 3 pages - top 150 animation movie
a_pages<-paste0("https://www.imdb.com/search/title/?title_type=feature&genres=animation&start=",c(1,51,101))

# function to scrap relevant info
# page = a_pages[1|2|3]

# movie title - may different from webpage due to translation
a_title<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url,".lister-item-header a")
  html_text(nodes)
}

# release year
a_year<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url,".text-muted.unbold")
  html_text(nodes)
}

# duration
a_time<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url,".lister-item-header+ .text-muted")
  html_text(nodes)
}

# rating from users
a_rating<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url,".lister-item-content")
  html_text(nodes)
}

# number of votes
a_vote<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url,".lister-item-content")
  html_text(nodes)
}

# director(s)
a_director<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url,".text-muted~ .text-muted+ p , .rating-bar~ .text-muted+ p")
  html_text(nodes)
}

# Extract values from the function
animation_title<-do.call(c,lapply(a_pages,a_title))

animation_year<-do.call(c,lapply(a_pages,a_year))
# take only the digit of year and change the structure
animation_year<-str_extract(animation_year,"\\d+")
animation_year<-year(as.Date(animation_year,format = "%Y"))

animation_time<-do.call(c,lapply(a_pages,a_time))
# take only the digits right before "min" and change the structure
animation_time<-str_extract(animation_time,"\\d+ min")
animation_time<-as.integer(str_extract(animation_time,"\\d+"))

animation_rating<-do.call(c,lapply(a_pages,a_rating))
# take the pattern of rating (0.0-10.0) and change the structure
animation_rating<-as.numeric(str_extract(animation_rating,"\\d+\\.\\d"))

animation_vote<-do.call(c,lapply(a_pages,a_vote))
# take the part with number of votes
animation_vote<-str_extract(animation_vote,"\\bVotes:\n\\s*.*\n")
# clear all "," to get the digits and change the structure
animation_vote<-str_remove_all(animation_vote,",")
animation_vote<-as.integer(str_extract(animation_vote,"\\d+"))

animation_director<-do.call(c,lapply(a_pages,a_director))
# Extract a string of words between two specific words in R
# take all before "Star"
animation_director<-sub("(.*?) *Star.*","\\1",animation_director)
# take all after director(s):\n
animation_director<-sub(".*Director:\n(.*?) *","\\1",animation_director)
animation_director<-sub(".*Directors:\n(.*?) *","\\1",animation_director)
# remove other characters and extra spaces
animation_director<-str_remove(animation_director,"[|]")
animation_director<-str_remove_all(animation_director,"\\n")
animation_director<-str_trim(animation_director)



################### Prepare data for comparison
# combine the variables of musical movies
musical<-data.frame(title = musical_title,
                    year = musical_year,
                    duration = musical_time,
                    rating = musical_rating,
                    votesNo = musical_vote,
                    director = musical_director)

# combine the variables of animation movies
animation<-data.frame(title = animation_title,
                      year = animation_year,
                      duration = animation_time,
                      rating = animation_rating,
                      votesNo = animation_vote,
                      director = animation_director)

# split the elements in "director" into pieces
# put them into new rows under same column
# duplicate others variables data
directors_m<-data.frame(separate_rows(musical,director,sep = ", "))
directors_a<-data.frame(separate_rows(animation,director,sep = ", "))

# check rows with missing values in any column
musical[!complete.cases(musical),]
animation[!complete.cases(animation),]

# removes data with missing values
#na.omit(musical)
#na.omit(animation)



################### Data analysis
# find the min/max values of each numerical variables
summary(musical)
summary(animation)

# find out the director(s) with highest number of movie productions
# it will be in decreasing order and just check on the top 6 director(s)
summary(as.factor(directors_m$director),maxsum = 6)
summary(as.factor(directors_a$director),maxsum = 6)

# check the movie titles with min/max votes
filter(musical, votesNo == 149 | votesNo == 1081243)
filter(animation, votesNo == 3470 | votesNo == 1138970)


# combine movies with both genres
msc_ani_n<-inner_join(musical,animation)
nrow(msc_ani_n)
directors_n<-data.frame(separate_rows(msc_ani_n,director,sep = ", "))

# combine all movies involved
msc_ani_u<-full_join(musical,animation)
nrow(msc_ani_u)
directors_u<-data.frame(separate_rows(msc_ani_u,director,sep = ", "))

# find out the director(s) with highest number of movie productions
summary(as.factor(directors_n$director),maxsum = 10)
summary(as.factor(directors_u$director),maxsum = 10)

# check the directors and their movies

# John Musker & Ron Clements
filter(directors_u, director == "John Musker" | director == "Ron Clements")
filter(directors_a, director == "John Musker")
filter(directors_m, director == "Ron Clements")

# Hamilton Luske & Wilfred Jackson
filter(directors_u, director == "Hamilton Luske" | director == "Wilfred Jackson")
filter(directors_n, director == "Hamilton Luske" | director == "Wilfred Jackson")
filter(directors_m, director == "Hamilton Luske")
filter(directors_m, director == "Wilfred Jackson")

# Hamilton Luske & Clyde Geronimi
filter(directors_u, director == "Hamilton Luske" | director == "Clyde Geronimi")

# Hayao Miyazaki
filter(directors_u, director == "Hayao Miyazaki")
filter(directors_n, director == "Hayao Miyazaki")
filter(directors_a, director == "Hayao Miyazaki")

# John Lasseter
filter(directors_u, director == "John Lasseter")
filter(directors_n, director == "John Lasseter")
filter(directors_a, director == "John Lasseter")

# Lee Unkrich
filter(directors_u, director == "Lee Unkrich")
filter(directors_n, director == "Lee Unkrich")
filter(directors_a, director == "Lee Unkrich")
