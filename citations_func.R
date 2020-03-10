

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


