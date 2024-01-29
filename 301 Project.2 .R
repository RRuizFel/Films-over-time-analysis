library(ggplot2movies)
library(ggplot2)
data(movies)
library(dplyr)

#1
range(movies$year)

#2
movies.with.budget = sum(!is.na(movies$budget))
movies.without.budget = sum(is.na(movies$budget))
total.movies = nrow(movies)
budget.proportions = movies.with.budget / total.movies
nobudget.proportions = movies.without.budget / total.movies

top.5.expensive = head(movies[order(-movies$budget), ], 5)

#3
top.5.longest = head(movies[order(-movies$length), ], 5)

#4
short.movies = movies[movies$Short == 1, ]

  #Get indices of shortest short and then retrieve title
  #No new Data frame is selected
shortest.short = which.min(short.movies$length)
shortest.short.title = as.character(short.movies$title[shortest.short])
shortest.short.time = min(short.movies$length)

longest.short = which.max(short.movies$length)
longest.short.title = as.character(short.movies$title[longest.short])
longest.short.time = max(short.movies$length)

#   ISSUE: New Df is created
# shortest.short.df = short.movies[which.min(short.movies$length), ]
# shortest.short.df.title = shortest.short.df$title[1]
# 
# longest.short.df = short.movies[which.max(short.movies$length), ]
# longest.short.df.title = longest.short.df$title[1]


#5
Filtered.movies = subset(movies, Action == 1 | Comedy == 1 | 
                           Drama == 1 | Documentary == 1 | Romance == 1)
Genre.count = c(
  Action = sum(Filtered.movies$Action),
  Comedy = sum(Filtered.movies$Comedy), 
  Drama = sum(Filtered.movies$Drama), 
  Documentary = sum(Filtered.movies$Documentary),
  Romance = sum(Filtered.movies$Romance)
)

barplot(Genre.count, main = 'Number of Movies by Genre', 
        xlab = 'Genre', ylab = 'Number of Movies', 
        col = 'coral', 
        ylim = c(0, max(Genre.count) + 10), cex.names = 0.67)
text(x = 1:length(Genre.count) , y = Genre.count - 1000, 
     labels = Genre.count, cex = 0.9, col = "black", pos = 1)

#6
Action.avg.rating = mean((movies %>% filter(Action == 1))$rating, na.rm = TRUE)
Animation.avg.rating = mean((movies %>% filter(Animation == 1))$rating, na.rm = TRUE)
Comedy.avg.rating = mean((movies %>% filter(Comedy == 1))$rating, na.rm = TRUE)
Drama.avg.rating = mean((movies %>% filter(Drama == 1))$rating, na.rm = TRUE)
Romance.avg.rating = mean((movies %>% filter(Romance == 1))$rating, na.rm = TRUE)
Short.avg.rating = mean((movies %>% filter(Short == 1))$rating, na.rm = TRUE)

avg.genre.ratings = c(Action = Action.avg.rating, 
                      Animation = Animation.avg.rating,
                      Comedy = Comedy.avg.rating, 
                      Drama = Drama.avg.rating, 
                      Romance = Romance.avg.rating,
                      Short = Short.avg.rating)

barplot(avg.genre.ratings, 
        main = 'Average Rating by Genre', xlab = 'Genre', ylab = 'Average Raging', 
        col = 'coral', ylim = c(0, max(avg.genre.ratings) + 1),
        names.arg = names(avg.genre.ratings), cex.names = 0.67)
text(x = 1:length(avg.genre.ratings) , y = avg.genre.ratings - 1, 
     labels = round(avg.genre.ratings, 2), cex = 0.8, col = "black", pos = 3)

#     #ISSUE: Columns are not unique
# Filtered.genre = movies %>% filter(Action == 1 |
#                                      Comedy == 1 | Drama == 1 |
#                                      Documentary == 1 | Romance == 1)
# avg.genre.rating = Filtered.genre %>%
#   group_by(Action, Animation, Comedy, Drama, Documentary, Romance) %>%
#   summarize(avg.rating = mean(rating), .groups = 'drop')


#7
Action.avg.rating.7 = mean((movies %>% filter(Action == 1 & 2000 <= year & year <= 2000))$rating, na.rm = TRUE)
Animation.avg.rating.7 = mean((movies %>% filter(Animation == 1 & 2000 <= year & year <= 2000))$rating, na.rm = TRUE)
Comedy.avg.rating.7 = mean((movies %>% filter(Comedy == 1 & 2000 <= year & year <= 2000))$rating, na.rm = TRUE)
Drama.avg.rating.7 = mean((movies %>% filter(Drama == 1 & 2000 <= year & year <= 2000))$rating, na.rm = TRUE)
Romance.avg.rating.7 = mean((movies %>% filter(Romance == 1 & 2000 <= year & year <= 2000))$rating, na.rm = TRUE)
Short.avg.rating.7 = mean((movies %>% filter(Short == 1 & 2000 <= year & year <= 2000))$rating, na.rm = TRUE)

avg.genre.ratings.7 = c(Action = Action.avg.rating.7, 
                      Animation = Animation.avg.rating.7,
                      Comedy = Comedy.avg.rating.7, 
                      Drama = Drama.avg.rating.7, 
                      Romance = Romance.avg.rating.7,
                      Short = Short.avg.rating.7)

barplot(avg.genre.ratings.7, 
        main = 'Average Rating by Genre \n Produced 2000-2005', xlab = 'Genre', ylab = 'Average Raging', 
        col = 'coral', ylim = c(0, max(avg.genre.ratings.7) + 1),
        names.arg = names(avg.genre.ratings.7), cex.names = 0.67)
text(x = 1:length(avg.genre.ratings.7) , y = avg.genre.ratings.7 - 1, 
     labels = round(avg.genre.ratings.7, 2), cex = 0.99, col = "black", pos = 1)

#8
Action.avg.rating.8 = movies %>% filter(Action == 1 & 1990 <= year & year <= max(year, na.rm = TRUE))
Animation.avg.rating.8 = movies %>% filter(Animation == 1 & 1990 <= year & year <= max(year, na.rm = TRUE))
Comedy.avg.rating.8 = movies %>% filter(Comedy == 1 & 1990 <= year & year <= max(year, na.rm = TRUE))
Drama.avg.rating.8 = movies %>% filter(Drama == 1 & 1990 <= year & year <= max(year, na.rm = TRUE))
Romance.avg.rating.8 = movies %>% filter(Romance == 1 & 1990 <= year & year <= max(year, na.rm = TRUE))

Genre.movies.time = bind_rows(
                      data.frame(Genre = 'Action', year = Action.avg.rating.8$year),
                      data.frame(Genre = 'Animation',year =  Animation.avg.rating.8$year),
                      data.frame(Genre = 'Comedy', year = Comedy.avg.rating.8$year),
                      data.frame(Genre = 'Drama', year = Drama.avg.rating.8$year),
                      data.frame(Genre = 'Romance', year = Romance.avg.rating.8$year)
                )

plot = ggplot(Genre.movies.time, aes(x = year, color = Genre)) + 
  geom_line(stat = "count") +
  labs(title = "Number of Movies Produced since 1990",
       x = "Year", y = "Number of Movies", color = "Genre") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "right")
print(plot)

#9
library(dplyr)
library(ggplot2)

# Calculate decade for each movie
movies <- movies %>%
  mutate(decade = 10 * (year %/% 10))

# Create boxplot
ggplot(movies, aes(x = factor(decade), y = length)) +
  geom_boxplot() +
  labs(title = "Distribution of Movie Lengths by Decade",
       x = "Decade",
       y = "Movie Length",
       caption = "Data Source: Your Dataset") +
  theme_minimal()



movies_per_year <- movies %>%
  group_by(year) %>%
  summarize(num_movies = n())

# Create line plot
ggplot(movies_per_year, aes(x = year, y = num_movies)) +
  geom_line() +
  labs(title = "Number of Movies Produced Each Year",
       x = "Year",
       y = "Number of Movies",
       caption = "Data Source: Your Dataset") +
  theme_minimal()
