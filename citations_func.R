

# A function to store and plot your citations throughout the year
dates <- as.Date (c("2020-02-16", "2020-02-17","2020-02-18", "2020-02-19", "2020-02-20", "2020-02-21", "2020-02-22",
                    "2020-02-23" ,"2020-02-24", "2020-02-25", "2020-02-26",
                    "2020-02-27", "2020-02-28", "2020-02-29", "2020-03-01", "2020-03-02", "2020-03-03", "2020-03-04",
                    "2020-03-05", "2020-03-06", "2020-03-07", "2020-03-08", "2020-03-09", "2020-03-10",
                    "2020-03-11", "2020-03-12", "2020-03-13", "2020-03-14", "2020-03-15",
                    "2020-03-16", "2020-03-17", "2020-03-18", "2020-03-19","2020-03-20",
                    "2020-03-21", "2020-03-22", "2020-03-23","2020-03-24", "2020-03-25", "2020-03-26",
                    "2020-03-27", "2020-03-28", "2020-03-29", "2020-03-30", "2020-03-31", "2020-04-01", "2020-04-02", "2020-04-03",
                    "2020-04-04", "2020-04-05", "2020-04-06", "2020-04-07", "2020-04-08",
                    "2020-04-09", "2020-04-10", "2020-04-11", "2020-04-12", "2020-04-13",
                    "2020-04-14", "2020-04-15", "2020-04-16", "2020-04-17"))
cites <- c(196, 200, 200, 200, 201, 201, 221,221, 229,229, 229, 234, 234, 243, 243, 243, 267, 267, 271,
           267, 267, 267, 284, 284, 295, 295, 295, 305, 305, 324, 324, 324, 338, 333, 333,
           333, 333, 331, 348, 348, 348, 356, 356, 365, 365, 374, 374, 377, 377, 377, 377, 394,
           395, 395, 395, 398, 398, 398, 414, 414, 414, 414)
end_of_year <- as.Date("2020-12-31")
# total citations on "2020-02-21": 7385

citations_2020 <- function (dates, cites){
  options(warn = -1) # to suppress warnings
  library(ggplot2)
  library(patchwork)
projected_citations <- (cites/(365-as.numeric(end_of_year - dates))*365)
#print(projected_citations)
days_left <- abs(end_of_year-dates)
cite_df <- as.data.frame(cbind(projected_citations, days_left, cites))
print(cite_df)
statistics <- c(Mean = mean(cite_df$projected_citations), Median = median(cite_df$projected_citations),
              "Std Dv" = sd(cite_df$projected_citations), "Minimum" = min(cite_df$projected_citations),
              Maximum = max(cite_df$projected_citations))
print (statistics)

if (mean(cite_df$projected_citations)>1500) {
  print("BETTER THAN LAST YEAR!")
} else if (mean(cite_df$projected_citations)<1500){
  print("OOPS, THERE IS A DIP!")
}

#print(mean(cite_df$projected_citations))
#print(median(cite_df$projected_citations))
#print(range(cite_df$projected_citations))

#cite_plot <-ggplot(data=cite_df, aes(x=rev(days_left), y=projected_citations, group=1)) +
#  geom_line()+
 # geom_point()
#p <- cite_plot + ylim(1200, 2000)
# p <- p + geom_line(aes(y = cites*5, colour = "cites"))
# p <- p #+ scale_y_continuous(sec.axis = sec_axis(~./5, name = "cites"))
# p

correlation <- cor.test(cite_df$cites, cite_df$days_left)
print(correlation)
reg_mod <- lm(cites~ days_left, data = cite_df)
linear_reg_coefficient = reg_mod$coeff[2]
print(linear_reg_coefficient)
linear_reg_prediction <- linear_reg_coefficient*365
print(linear_reg_prediction)


proj_citations_plot <-ggplot(data=cite_df, aes(x=(1-days_left), y=projected_citations, group=1)) +
  geom_line()+
  geom_point()
proj_citations_plot <-proj_citations_plot +  geom_hline(yintercept = mean(cite_df$projected_citations), color="blue")


n_citations_plot <-ggplot(data=cite_df, aes(x=(1-days_left), y=cites, group=1)) +
  geom_line()+
  geom_point()
n_citations_plot

proj_citations_plot + n_citations_plot
}


