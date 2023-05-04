library(tidyverse)
library(gridExtra)
library(usmap)



# load dataset
attorneytimeentries <- read.csv(file = paste0(getwd(),"/LukVill/DataFest/data/attorneytimeentries.csv"), header = TRUE)
glimpse(attorneytimeentries)

#
nrow(attorneytimeentries)

# make bar graph of hours spent based on state
hours_by_state <- attorneytimeentries %>% group_by(StateAbbr) %>% summarise(sumHours = sum(Hours))
colnames(hours_by_state)


# plot of each state's logged hours
ggplot(hours_by_state, mapping = aes(x = StateAbbr, y = sumHours)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + labs(x = "State", y = "Total Number of Hours Logged")

# most hours logged per state
hours_by_state %>% arrange(desc(sumHours))
names(hours_by_state) <- c("state", "Total Hours Logged")


# geospatial plot of hours logged per state
plot_usmap(regions = "state", data = hours_by_state, values = "Total Hours Logged") + theme(legend.position = "right")

