library(tidyverse)
library(gridExtra)
library(maps)
library(tidycensus)
library(usmap)
library(gcookbook)
# cabbage_exp

# load dataset
attorneys <- read.csv(file = paste0(getwd(),"/LukVill/DataFest/data/attorneys.csv"), header = TRUE)
glimpse(attorneys)

# dates and times are logged as character strings
class(attorneys$CreatedUtc[1])

# observations
nrow(attorneys)

length(unique(attorneys$StateAbbr))
unique(attorneys$StateName)

states_in_attorneys <- unique(attorneys$StateAbbr)
# found the data that are in "states" but are actually territories
states_in_attorneys[!(states_in_attorneys %in% state.abb)]

# get actual states
real_states_in_attorneys <- states_in_attorneys[states_in_attorneys != "VI" & states_in_attorneys != "US"]
length(real_states_in_attorneys)

# get states with no attorneys
states_no_attorneys <- state.abb[!(state.abb %in% real_states_in_attorneys)]
states_no_attorneys

# counties
unique(attorneys$County)

# cities 
length(unique(attorneys$City))



# check for nas
for(i in seq_along(colnames(attorneys)))
{
  if(any(is.na(attorneys[i])))
  {
    print(paste0("Column ", i, " has NAs"))
  }
  else
  {
    print(paste0("Column ", i, " has no NAs"))
  }
}


# group each state with each city
# how many observations 
state_by_city_count <- attorneys %>% group_by(StateAbbr) %>% count(City)
arrange(state_by_city_count, desc(n))

# find most in specific state
state_by_city_count %>% filter(StateAbbr == "AK") %>% arrange(desc(n))

# most attorneys in the country
state_by_city_count %>% arrange(desc(n))

d <- as.POSIXlt(attorneys$CreatedUtc[1])
d  
d > as.POSIXlt("2019-02-14 00:00:00")

# get date range of attorney creation

# make new column of date string converted to date object
attorneys <- attorneys %>% mutate(DateTime = as.POSIXlt(attorneys$CreatedUtc))
colnames(attorneys)

# make date column
attorneys <- attorneys %>% mutate(Date = as.Date(substr(attorneys$CreatedUtc,1,10)))

# get earliest date and latest
min(attorneys$Date)
max(attorneys$Date)

colnames(attorneys)

# graph of attorneys by time
# get count for each date
attorneys_joined_by_day <- table(attorneys$Date)
ggplot() + geom_bar(aes(x = unique(sort(attorneys$Date)), y = attorneys_joined_by_day), stat = "identity") + scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") + labs(x = "Dates (you cant see them lol)", y = "Attorneys Joined per Day")

data(fips_codes)

glimpse(fips_codes)

# state fips table
state_fips <- fips_codes %>% select(state, state_code) %>% distinct() %>% arrange(state_code)

# make table of attorney sum for each state
attorneySum <- attorneys %>% group_by(StateAbbr) %>% count()
attorneySum <- as.data.frame(attorneySum)

# sorted attorney sum
arrange(attorneySum, desc(n))

# make indexing for states
state_fips$index <- seq_along(state_fips$state)

# plot usmap based on attorney amount in each state
state_fips
plot_usmap(regions = "states")
help(plot_usmap)

# rename attorney sum cols
names(attorneySum) <- c("state","n")

state_fips

# table of attorney sum with state code
state_attorney_sum <- merge(attorneySum, state_fips, by.y = "state")
state_attorney_sum <- as.data.frame(select(state_attorney_sum, state, n))

state_attorney_sum %>% arrange(desc(sum))

names(state_attorney_sum) <- c("state", "sum")

# plot
plot_usmap(regions = "states", data = state_attorney_sum, values = "sum") + labs(fill = "Attorney Sum") + scale_fill_gradient(low = "yellow", high = "red", na.value = "black") + theme(legend.position = "right")


