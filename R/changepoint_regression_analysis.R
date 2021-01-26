

### perform changepoint regression analysis for sequence and article counts

library(segmented)
library(tidyverse)


### load data
WOS_taxa_trend <- read_csv2("./output/text/WOS_recap_table_articles_per_year_acc.csv")
GenBank_taxa_trend <- read_csv2("./output/text/GenBank_all_taxa_year_tab_acc.csv")

GenBank_taxa_trend <- GenBank_taxa_trend[, -c(2,3,4)] # drop years 1987 to 1989


# mydata = WOS_taxa_trend
# mytaxa = "plant"

get_breakpoints <- function(mydata) {
  
  taxa_vect <- unique(mydata$taxa)
  year1990 <- c(1:30)
  empty_vect <- rep(NA, nrow(mydata))
  result_table <- data.frame(taxa = empty_vect, breakpoint = empty_vect, lowerCI = empty_vect,
                             upperCI = empty_vect, p_value = empty_vect)
  
  index <- 0
  
  for (mytaxa in taxa_vect) {
    index <- index + 1
    
    taxa_tab <- subset(mydata, mydata$taxa == mytaxa)
    
    taxa_tab <- taxa_tab %>%
      pivot_longer(cols = "1990":"2019", names_to = "year", values_to = "n_references")
    
    # fit regression
    fit.glm <- glm(n_references ~ year1990, data = taxa_tab, family = "poisson") # with a poisson glm
    
    # plot this model
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
    # lines(fit.seg, col = 2, pch = 19, bottom = TRUE, lwd = 2) #for the CI for the breakpoint
    
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

write_csv2(WOS_breakpoints, file = "./output/text/WOS_breakpoints.csv")
write_csv2(GenBank_breakpoints, file = "./output/text/GenBank_breakpoints.csv")
