

### perform changepoint regression analysis for sequence and article counts

library(segmented)
library(tidyverse)


### load data
WOS_taxa_trend <- read_csv2("./output/text/WOS_recap_table_articles_per_year_acc.csv")
GenBank_taxa_trend <- read_csv2("./output/text/GenBank_all_taxa_year_tab_acc.csv")
# WOS_taxa_trend <- read_csv2("./output/text/WOS_recap_table_articles_per_year.csv")
# GenBank_taxa_trend <- read_csv2("./output/text/GenBank_all_taxa_year_tab.csv")

GenBank_taxa_trend <- GenBank_taxa_trend[, -c(2,3,4)] # drop years 1987 to 1989

GenBank_taxa_trend <- subset(GenBank_taxa_trend, GenBank_taxa_trend$taxa != "lumbricina")

# mydata = GenBank_taxa_trend
# mytaxa = "lumbricina"

get_breakpoints <- function(mydata) {
  
  taxa_vect <- unique(mydata$taxa)
  year1990 <- c(1:30)
  empty_vect <- rep(NA, nrow(mydata))
  result_table <- data.frame(taxa = empty_vect, breakpoint = empty_vect, lowerCI = empty_vect,
                             upperCI = empty_vect, p_value = empty_vect)
  
  index <- 0
  
  for (mytaxa in taxa_vect) {
    
    index <- index + 1
    # mytaxa = taxa_vect[9] # c'est les lombrics qui buggent
    taxa_tab <- subset(mydata, mydata$taxa == mytaxa)
    
    taxa_tab <- taxa_tab %>%
      pivot_longer(cols = "1990":"2019", names_to = "year", values_to = "n_references")
    
    # fit regression
    fit.glm <- glm(n_references ~ year1990, data = taxa_tab, family = "quasipoisson") # with a poisson glm
    
    # # plot this model
    # x0 <- seq(min(year1990), max(year1990), length = 30)  # prediction grid
    # y0 <- predict.glm(fit.glm, newdata = list(x = x0))  # predicted values
    # y0 <- exp(y0) # backtransform estimates
    # plot(x = year1990, y = taxa_tab$n_references)
    # lines(x0, y0, col = 2)  ## add regression curve (colour: red)
    
    # fit the segmented regression with one breakpoint
    fit.seg <- segmented(fit.glm, seg.Z = ~ year1990, npsi = 1)
    # seg.predict <- predict(fit.seg)
    # seg.predict <- exp(seg.predict) # backtransform estimates
    # lines(year1990, seg.predict, col = 2)
    # lines(fit.seg, col = 2, pch = 19, bottom = TRUE, lwd = 2) # for the CI for the breakpoint
    
    # get the estimated breakpoint and its CI
    myconfint <- confint(fit.seg)
    
    breakpoint <- 1990 + round(myconfint[1], digits = 2) # breakpoint estimate
    lowerCI <- 1990 + round(myconfint[2], digits = 2) # lower IC
    upperCI <- 1990 + round(myconfint[3], digits = 2) # upper IC
    
    # get davies test p-values
    mytest <- davies.test(fit.glm, ~ year1990, k = 30) # to test the breakpoint and get a p-value 
    myp_value <- mytest$p.value
    
    result_vect <- c(taxa_tab$taxa[1], breakpoint, lowerCI, upperCI, myp_value)
    
    result_table[index, ] <- result_vect
    
  }
  
  return(result_table)
}

WOS_breakpoints <- get_breakpoints(mydata = WOS_taxa_trend)
GenBank_breakpoints <- get_breakpoints(mydata = GenBank_taxa_trend)


### fit segmented regression for lumbricina separately because the excess of zeros induces convergence error
GenBank_taxa_trend <- read_csv2("./output/text/GenBank_all_taxa_year_tab_acc.csv")
lumbri_tab <- subset(GenBank_taxa_trend, GenBank_taxa_trend$taxa == "lumbricina")
lumbri_tab <- lumbri_tab[, -c(2:14)] # drop years 1987 to 1989

lumbri_tab <- lumbri_tab %>%
  pivot_longer(cols = "2000":"2019", names_to = "year", values_to = "n_references")
year2000 <- c(1:20)

# fit the 2 regressions
fit.glm <- glm(n_references ~ year2000, data = lumbri_tab, family = "quasipoisson") # with a poisson glm

# # plot this model
# x0 <- seq(min(year2000), max(year2000), length = 20)  # prediction grid
# y0 <- predict.glm(fit.glm, newdata = list(x = x0))  # predicted values
# y0 <- exp(y0) # backtransform estimates
# plot(x = year2000, y = lumbri_tab$n_references)
# lines(x0, y0, col = 2)  ## add regression curve (colour: red)

fit.seg <- segmented(fit.glm, seg.Z = ~ year2000, npsi = 1)
# seg.predict <- predict(fit.seg)
# seg.predict <- exp(seg.predict) # backtransform estimates
# lines(year2000, seg.predict, col = 2)
# lines(fit.seg, col = 2, pch = 19, bottom = TRUE, lwd = 2) # for the CI for the breakpoint

myconfint <- confint(fit.seg)

breakpoint <- 2000 + round(myconfint[1], digits = 2) # breakpoint estimate
lowerCI <- 2000 + round(myconfint[2], digits = 2) # lower IC
upperCI <- 2000 + round(myconfint[3], digits = 2) # upper IC

# get davies test p-values
mytest <- davies.test(fit.glm, ~ year2000, k = 30) # to test the breakpoint and get a p-value 
myp_value <- mytest$p.value

result_vect <- c(taxa_tab$taxa[1], breakpoint, lowerCI, upperCI, myp_value)
GenBank_breakpoints <- rbind(GenBank_breakpoints[c(1:11), ], result_vect, GenBank_breakpoints[12, ])

write_csv2(WOS_breakpoints, file = "./output/text/WOS_breakpoints.csv")
write_csv2(GenBank_breakpoints, file = "./output/text/GenBank_breakpoints.csv")
