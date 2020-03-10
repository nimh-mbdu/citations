library(nycflights13)
library(tidyverse)

filter(flights, month == 1, day ==1)

jan1 <- filter(flights, month == 1, day ==1)

nov_dec <- filter(flights, month==11 | month ==12)

nov_dec1 <- filter(flights, month %in% c(11,12) )

dim(nov_dec)
dim(nov_dec1)


View (flights)
#Find all flights that

#Had an arrival delay of two or more hours
del_two <- filter(flights, arr_delay>=120)
dest_two <- filter(flights, dest=="IAH" | dest=="HOU")
#
dest_two <- filter(flights, dest %in% c("HOU", "IAH") )

#mutate
flights_1 <- mutate(flights, gain = dep_delay - arr_delay)



summer<- filter(flights, month >= 7, month <= 9)
del_two <- filter(flights, arr_delay>=120 & dep_delay<=0)

#Flew to Houston (IAH or HOU)
#Were operated by United, American, or Delta
#Departed in summer (July, August, and September)
#Arrived more than two hours late, but didn’t leave late
#Were delayed by at least an hour, but made up over 30 minutes in flight
#Departed between midnight and 6am (inclusive)



#summarise
summarise(flights, delay=mean(dep_delay, na.rm=TRUE))
flights %>%
  group_by( year) %>%
  summarise( delay=mean(dep_delay, na.rm=TRUE), n=n())


ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut))
ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=carat
                         ), binwidth = 0.4)

diamonds %>%
count(cut_width)

diamonds %>%
  summarise(count(carat))


diamonds %>%
  count(select(cut_width==0.5))

test<-tribble(
  ~x, ~y, ~z,
  "a", 2, 3.6,
  "b", 1, 8.5
)

test_1 <- tibble(
  x = c("alpha","beta"),
  y = c(2,3),
  z = c(3.6, 4)
)

var <- "mpg"
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)


mutate(annoying, `3` = `2`/`1`)
mutate(annoying, `3` = `2` / `1`)



(196/46)*365


(citations/as.numeric(days_left))

end_of_year <-as.Date("2020-12-31")


# a function to estimate my end of year citations every day
citations <- 196
citations_today <- function(citations) {
  end_of_year <-as.Date("2020-12-31")
  days_left<- (end_of_year  - Sys.Date())
  projected_citations <- (citations/(365-as.numeric(days_left))*365)
  print(projected_citations)
}

collection <- c(190, 200, 300, 350 , 450 , 550, 600)
years <- c(1, 2, 3, 4, 5, 6, 7)

repository <- vector(mode= "integer", length = length(collection))
for (i in collection){
  repository[i] <-citations_today(i)
}

# R nested for loop

for(i in 1:5)
{
  for(j in 1:2)
  {
    print(i*j);
  }
}


repository <- vector(mode= "integer", length = length(collection))
for (i in collection)
{
    for(j in years)
    {

  repository[i] <-citations_today(i)
    }
}

# what you need to create is a database where the dates and the citations are extracted from
# create a dataset that is updated with Sys.date daily and number of citations
# then you probably need a double loop or write a different function.


days_left<-vector(mode = "numeric", length = length(collection))
dates <- as.Date (c("2020-02-16", "2020-02-15", "2020-02-14"))
cites <- c(196, 180, 170)
end_of_year <- as.Date("2020-12-31")
test <- function (years, collection){
for (i in 1:length(years)) {
  days_left <- as.numeric(end_of_year - dates[i])
}
for (j in 1:length(collection)) {
  total_citations <- (collection[j]/(365-days_left)*365)
  print(total_citations)
}
}




for (i in 1:length(years)) {
  days_left <- as.numeric(end_of_year - dates[i])
  print(days_left)
}
for (j in 1:length(collection)) {
  total_citations <- (collection[j]/(365-days_left)*365)
  print(total_citations)
  print(mean(total_citations))
}


# A function to store and plot your citations throughout the year
dates <- as.Date (c("2020-02-16", "2020-02-17","2020-02-18", "2020-02-19", "2020-02-20", "2020-02-21", "2020-02-22", "2020-02-23" ,"2020-02-24", "2020-02-25", "2020-02-26",
                    "2020-02-27", "2020-02-28", "2020-02-29", "2020-03-01", "2020-03-02", "2020-03-03", "2020-03-04",
                    "2020-03-05", "2020-03-06", "2020-03-07", "2020-03-08", "2020-03-09"))
cites <- c(196, 200, 200, 200, 201, 201, 221,221, 229,229, 229, 234, 234, 243, 243, 243, 267, 267, 271,
           267, 267, 267, 267)
end_of_year <- as.Date("2020-12-31")
# total citations on "2020-02-21": 7385

citations_2020 <- function (dates, cites){
projected_citations <- (cites/(365-as.numeric(end_of_year - dates))*365)
#print(projected_citations)
days_left <- abs(end_of_year-dates)
cite_df <- as.data.frame(cbind(projected_citations, days_left, cites))
print(cite_df)
statistics <- c(Mean = mean(cite_df$projected_citations), Median = median(cite_df$projected_citations),
              "Std Dv" = sd(cite_df$projected_citations), "Minimum" = min(cite_df$projected_citations),
              Maximum = max(cite_df$projected_citations))
print (statistics)
#print(mean(cite_df$projected_citations))
#print(median(cite_df$projected_citations))
#print(range(cite_df$projected_citations))
cite_plot <-ggplot(data=cite_df, aes(x=rev(days_left), y=projected_citations, group=1)) +
  geom_line()+
  geom_point()
p <- cite_plot + ylim(1200, 2000)
 p <- p + geom_line(aes(y = cites*5, colour = "cites"))
 p <- p + scale_y_continuous(sec.axis = sec_axis(~./5, name = "cites"))
 p
}


