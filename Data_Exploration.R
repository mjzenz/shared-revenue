##This R scrip uploads shared revenue data, processes, and provides analyses

library(tidyverse)
library(rvest)

###Process shared revenue data input

#Read in .csv data with shared revenue and income/sales tax data by county
#This was provided by the WI Legislative Fiscal Bureau at the request of Rep. Francesca Hong 
shared.revenue.data.unprocessed <-  read_csv("Shared_Rev_Data.csv")

#Converts values given in milions.
shared.revenue.data <- shared.revenue.data.unprocessed %>%
                        mutate(`Net Individual Income Tax` = `Net Individual Income Tax` * 1000000, 
                               `State Sales Tax Collections` =  `State Sales Tax Collections`  * 1000000)

#Scrapes county population and area data from wikipedia table
wiki.county.table <- rvest::read_html("https://en.wikipedia.org/wiki/List_of_counties_in_Wisconsin") %>%
            rvest::html_node(., ".wikitable") %>%
     rvest::html_table(., fill = TRUE)

#Removes "county" from county names and renames variables.
county.pop.area <- wiki.county.table %>%
                    mutate(County = substr(County, 1,str_length(County)-7)) %>%
                  select(County, Population = `Population[2]`, Area = `Area[2]`)

#Removes commas from population values
county.pop.area$Population <- as.numeric(gsub("[\\,]","",county.pop.area$Population))

#Scrapes WI 2020 presidential election values from wikipedia
wiki.trump.vote.table <- rvest::read_html("https://en.wikipedia.org/wiki/2020_United_States_presidential_election_in_Wisconsin") %>%
  html_nodes(.,"table")
wiki.trump.vote.table <-html_table(wiki.trump.vote.table[35], fill = TRUE)[[1]] 

#Keep Trump 2020 vote share.
trump.vote.prop.table <- wiki.trump.vote.table[-1,c(1,4)] %>%
                      rename(Trump_2020_Prop = Trump) %>%
                      mutate(Trump_2020_Prop = as.numeric(substr(Trump_2020_Prop, 1, 
                                                      str_length(Trump_2020_Prop)-1))/100)

#Process data and make further calculations.
data <- shared.revenue.data %>%
        ungroup() %>%
        left_join(county.pop.area, by = "County") %>%
        left_join(trump.vote.prop.table, by = "County") %>%
        mutate(`Area (sq mi)` = as.numeric(str_replace_all(str_extract(Area, "\\d+[\\d,.]*"), ",", "")) ,
               `Density (pop sq mi)` = Population/`Area (sq mi)`,
                `Combined Tax` = `Net Individual Income Tax` + `State Sales Tax Collections`,
               `Total Shared Revenue Overage` = `Total Shared Revenue` - `Combined Tax`,
               `Per capita Shared Revenue Overage` =  `Total Shared Revenue Overage` / Population,
               `Per capita Relative Shared Revenue Overage` = (`Total Shared Revenue` - `Combined Tax`)/Population - 
  (sum(`Total Shared Revenue`) - sum(`Combined Tax`))/sum(Population),
               `Per capita Income Tax` = `Net Individual Income Tax`/Population,
               `Per capita Combined Tax` = `Combined Tax`/Population,
               `Trump Majority` = Trump_2020_Prop > .5, 
               )





income.lm <- lm(`Per capita Relative Shared Revenue Overage` ~ log(`Per capita Income Tax`), data = data)
summary(income.lm)

combinedtax.lm <- lm(`Per capita Relative Shared Revenue Overage` ~ log(`Per capita Combined Tax`), data = data)
summary(income.lm)


income.lm.no_outlier <- lm(`Per capita Shared Revenue Overage` ~ log(`Per capita Income Tax`), 
                           data = data[which(log(data$`Per capita Income Tax`)>6),])
summary(income.lm.no_outlier)

population.lm <- lm(log(`Total Shared Revenue`) ~ log(Population), data)
summary(population.lm)
intercept <- coef(population.lm)[1]
slope <- coef(population.lm)[2]


ggplot(data = data, aes(x = log(Population), y = log(`Total Shared Revenue`))) + 
            geom_point(aes(color = `Trump Majority`)) +
  geom_abline(slope = slope, intercept = intercept)


density.incometax.lm <- lm(log(`Per capita Income Tax`) ~ log(`Density (pop sq mi)`), data = data)
summary(density.incometax.lm)
intercept <- coef(density.incometax.lm)[1]
slope <- coef(density.incometax.lm)[2]


ggplot(data = data, aes(x = log(`Density (pop sq mi)`), y = log(`Per capita Income Tax`) )) + 
  geom_point(aes(color = `Trump Majority`)) +
  #coord_trans(x="log1", y="log1")   +  
  geom_abline(slope = slope, intercept = intercept)

#linear model of density and  per capita shared revenue
density.percapita.combinedtax.lm <- lm(log((`Total Shared Revenue`)/Population) ~ log(`Density (pop sq mi)`), data = data)
summary(density.percapita.combinedtax.lm)
intercept <- coef(density.percapita.combinedtax.lm)[1]
slope <- coef(density.percapita.combinedtax.lm)[2]


ggplot(data = data, aes(x = log(`Density (pop sq mi)`), y = log((`Total Shared Revenue`)/Population))) + 
  geom_point(aes(color = `2020 Trump Majority`)) +
  geom_abline(slope = slope, intercept = intercept)  
  

#linear model of (log) per capita subsidy vs log density.
Min_Subsidy <- abs(min(data$`Per capita Shared Revenue Subsidy`))
density.percapita.subsidy.lm <- data %>% mutate(`Per capita Shared Revenue Subsidy tran` = `Per capita Shared Revenue Subsidy` + (Min_Subsidy+1)) %>%
 lm(log(`Per capita Shared Revenue Subsidy tran`) ~ log(`Density (pop sq mi)`),
                                       data = .)
summary(density.percapita.subsidy.lm)
intercept <- coef(density.percapita.subsidy.lm)[1]
slope <- coef(density.percapita.subsidy.lm)[2]

data %>% mutate(`Per capita Shared Revenue Subsidy tran` = `Per capita Shared Revenue Subsidy` + (Min_Subsidy+1)) %>% 
ggplot(aes(x = log(`Density (pop sq mi)`), y = log(`Per capita Shared Revenue Subsidy tran`))) + 
  geom_point(aes(color = `2020 Trump Majority`)) +
  geom_abline(slope = slope, intercept = intercept)  



