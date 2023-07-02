##This R scrip uploads shared revenue data, processes, and provides analyses

library(tidyverse)
library(rvest)

###Process shared revenue data input

shared.revenue.data.unprocessed <-  read_csv("Shared_Rev_Data.csv")


shared.revenue.data <- shared.revenue.data.unprocessed %>%
                        mutate(`Net Individual Income Tax` = `Net Individual Income Tax` * 1000000, 
                               `State Sales Tax Collections` =  `State Sales Tax Collections`  * 1000000)


wiki.county.table <- rvest::read_html("https://en.wikipedia.org/wiki/List_of_counties_in_Wisconsin") %>%
            rvest::html_node(., ".wikitable") %>%
     rvest::html_table(., fill = TRUE)

county.pop.area <- wiki.county.table %>%
                    mutate(County = substr(County, 1,str_length(County)-7)) %>%
                  select(County, Population = `Population[2]`, Area = `Area[2]`)

county.pop.area$Population <- as.numeric(gsub("[\\,]","",county.pop.area$Population))

wiki.trump.vote.table <- rvest::read_html("https://en.wikipedia.org/wiki/2020_United_States_presidential_election_in_Wisconsin") %>%
  html_nodes(.,"table")
wiki.trump.vote.table <-html_table(wiki.trump.vote.table[35], fill = TRUE)[[1]] 

trump.vote.prop.table <- wiki.trump.vote.table[-1,c(1,4)] %>%
                      rename(Trump_2020_Prop = Trump) %>%
                      mutate(Trump_2020_Prop = as.numeric(substr(Trump_2020_Prop, 1, 
                                                      str_length(Trump_2020_Prop)-1))/100)

data <- shared.revenue.data %>%
        left_join(county.pop.area, by = "County") %>%
        left_join(trump.vote.prop.table, by = "County") %>%
        mutate(`Combined Tax` = `Net Individual Income Tax` + `State Sales Tax Collections`,
               `Total Shared Revenue Overage` = `Total Shared Revenue` - `Combined Tax`,
               `Per capita Shared Revenue Overage` =  `Total Shared Revenue Overage` / Population,
               `Per capita Income Tax` = `Net Individual Income Tax`/Population)
  
                

income.lm <- lm(`Per capita Shared Revenue Overage` ~ log(`Per capita Income Tax`), data = data)
summary(income.lm)

income.lm.no_outlier <- lm(`Per capita Shared Revenue Overage` ~ log(`Per capita Income Tax`), 
                           data = data[which(log(data$`Per capita Income Tax`)>6),])
summary(income.lm.no_outlier)
