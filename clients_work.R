library(tidyverse)

# load 
clients <- read.csv(file = paste0(getwd(),"/LukVill/DataFest/data/clients.csv"), header = TRUE)

glimpse(clients)

# mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

colnames(clients)

clients %>% group_by(StateAbbr) %>% summarise(modeRace = getmode(EthnicIdentity))

unique(clients$EthnicIdentity)

# separate race character vector
# for each observation, parse through the ethnicidentity and split it by comma, 
# make a character vector with split string and append to column
clients <- clients %>% mutate(Ethnicities = strsplit(EthnicIdentity, split = ","))

# tally each observation's ethnicities
# res
res <- numeric(0)
for(i in seq_len(nrow(clients)))
{
  # for each observation, access the ethnicity string, get length
  # and append to res
  res <- c(res, length(unlist(clients[i,"Ethnicities"])))
}

# cbind to clients
EthnicCount <- res
clients <- cbind(clients, EthnicCount)
clients
res
nrow(clients)
clients %>% group_by(StateAbbr) %>% summarise(raceMode = getmode())
glimpse(clients)


# SENTIMENTAL ANALYSIS

# load 
sentimental <- read.csv(file = paste0(getwd(),"/LukVill/DataFest/first_appearances.csv"), header = TRUE)
glimpse(sentimental)

# load category 
categories <- read.csv(file = paste0(getwd(),"/LukVill/DataFest/data/categories.csv"), header = TRUE)
glimpse(categories)

# load questions
questions <- read.csv(file = paste0(getwd(),"/LukVill/DataFest/data/questions.csv"), header = TRUE)
glimpse(questions)

colnames(questions)
unique(questions$Category)

testSentimental <- sentimental[1:100,]
testSentimental
glimpse(testSentimental)

# make frequency table (x = score, y = frequency)
# filter sentimental based on Juvenile and Education
# check questionUno, check on questions, if Category is Juvenile 
# or Education, grab it 
indices <- numeric(0)
for(i in seq_len(nrow(sentimental)))
{
  categ <- subset(questions, QuestionUno == sentimental[i,"QuestionUno"])$Category
  if(categ == "Juvenile" | categ == "Education")
  {
    indices <- c(indices, i) 
  }
}

# sort out juvenile and education questions
q <- questions %>% filter(Category == "Juvenile" | Category == "Education")
# get q's only in juvenile
juv_q <- q %>% filter(Category == "Juvenile")
# get q's only in education
edu_q <- q %>% filter(Category == "Education")

colnames(sentimental)

# for Juvenile
juv_sent <- subset(sentimental, QuestionUno %in% juv_q$QuestionUno)
glimpse(juv_sent)

# for Education
edu_sent <- subset(sentimental, QuestionUno %in% edu_q$QuestionUno)
glimpse(edu_sent)

# freq table of juv
juv_sent <- juv_sent %>% select(sentiment)
juv_sent <- juv_sent[[1]]
juv_freq <- table(juv_sent)
juv_freq <- as.data.frame(juv_freq)
ggplot() + geom_bar(data = juv_freq, aes(x = juv_sent, y = Freq), stat = "identity") + labs(title = "Frequency of Juvenile Related Questions", x = "Sentiment Level", y = "Frequency")


# freq table of edu
edu_sent <- edu_sent %>% select(sentiment)
edu_sent <- edu_sent[[1]]
edu_freq <- table(edu_sent)
edu_freq <- as.data.frame(edu_freq)
ggplot() + geom_bar(data = edu_freq, aes(x = edu_sent, y = Freq), stat = "identity") + labs(title = "Frequency of Education Related Questions", x = "Sentiment Level", y = "Frequency")

# alt: mean of juv > mean of edu
# assume equal variance
t.test(x = juv_sent, y = edu_sent, alternative = "g", var.equal = TRUE)
t.test(x = juv_sent, y = edu_sent, alternative = "g", var.equal = FALSE)
# pvals are good

#

mean(juv_sent)
mean(edu_sent)
sd(juv_sent)
sd(edu_sent)


####### AMERICAN BAR ASSOCIATION ########
bar <- table(1)
names(bar) <- "American Bar Association"
ggplot() + geom_histogram(aes(bar), binwidth = 1) + scale_x_continuous(breaks = 1) + labs(x = "American Bar Association")


# t test two vectors, alt hyp: v1 > v2
greaterTTest <- function(v1, v2)
{
  print(t.test(x = v1, y = v2, alternative = "g", var.equal = TRUE))
  print(t.test(x = v1, y = v2, alternative = "g", var.equal = FALSE))
}

# load senti_updated
senti_updated <- read.csv(file = paste0(getwd(),"/LukVill/DataFest/senti_updated.csv"), header = TRUE)
glimpse(senti_updated)
unique(senti_updated$Category.x)

house_sent <- senti_updated %>% filter(Category.x == "Housing and Homelessness") %>% select(sentiment, Category.x)
fam_sent <- senti_updated %>% filter(Category.x == "Family and Children") %>% select(sentiment, Category.x)
other_sent <- senti_updated %>% filter(Category.x == "Other") %>% select(sentiment, Category.x)
consumer_sent <- senti_updated %>% filter(Category.x == "Consumer Financial Questions") %>% select(sentiment, Category.x)
work_sent <- senti_updated %>% filter(Category.x == "Work, Employment and Unemployment") %>% select(sentiment, Category.x)
individual_sent <- senti_updated %>% filter(Category.x == "Individual Rights") %>% select(sentiment, Category.x)
health_sent <- senti_updated %>% filter(Category.x == "Health and Disability") %>% select(sentiment, Category.x)
income_sent <- senti_updated %>% filter(Category.x == "Income Maintenance") %>% select(sentiment, Category.x)
edu_sent <- senti_updated %>% filter(Category.x == "Education") %>% select(sentiment, Category.x)
juv_sent <- senti_updated %>% filter(Category.x == "Juvenile") %>% select(sentiment, Category.x)

names(fam_sent)

# function to list means and sd of all categories
# INPUT: list, col = index of col to calculate
mean_sd_all <- function(ls)
{
  res <- numeric(0)
  
  # for all elems in list
  for(i in seq_along(ls))
  {
    # print mean and sd
    print(paste0("Mean -> ", mean(ls[[i]][[1]]), " SD -> ", sd(ls[[i]][[1]])))
    res <- rbind(res,c(mean(ls[[i]][[1]]),sd(ls[[i]][[1]])))
  }
  
  # return matrix of mean and sd cols
  res <- as.data.frame(res)
  names(res) <- c("Mean","SD")
  res
  
}

# list of categories' sentiments
sent_list <- list(house_sent, fam_sent, other_sent, consumer_sent, work_sent, individual_sent, health_sent, income_sent, edu_sent, juv_sent)

res <- mean_sd_all(sent_list)
res

# add index col
res <- cbind(seq_len(nrow(res)), res)
names(res)[1] <- "Index"

# make names of categories
names_categories <- unique(senti_updated$Category.x)

# sort high to low mean
res <- res %>% arrange(desc(Mean))
res

# sort the names
names_categories[res$Index]
# unsorted categories
names_categories[-11]



greaterTTest(juv_sent$sentiment, fam_sent$sentiment)

## special case needed to check
greaterTTest(juv_sent$sentiment, other_sent$sentiment)

greaterTTest(fam_sent$sentiment,other_sent$sentiment)
greaterTTest(other_sent$sentiment,individual_sent$sentiment)

# HIGH P VALUE
greaterTTest(individual_sent$sentiment,income_sent$sentiment)


greaterTTest(income_sent$sentiment, consumer_sent$sentiment)

# HIGH P VALUE
greaterTTest(consumer_sent$sentiment, health_sent$sentiment)

greaterTTest(health_sent$sentiment, work_sent$sentiment)
greaterTTest(work_sent$sentiment, house_sent$sentiment)

# HIGH P VALUE
greaterTTest(house_sent$sentiment,edu_sent$sentiment)


#---------------------

# p checking: less female proportion clients in individual households
# vs family households
prop.test(x = c(39963, 130929), n = c(68450, 187969), alternative = "l")

# p checking: more female prop clients in divorced/sep than married/single
# female divorced/sep: 55438, total divorced/sep: 73724
# female married/single: 75783, total married/single: 113574
prop.test(x = c(55438,75783), n = c(73724, 113574), alternative = "g")


#------------------------

# p checking: female prop in clients that asked questions is 
# equal to female prop total
# female prop asked ques: 101703, clients asked: 149096, female prop total: 185433, total: 280227
prop.test(x = c(101703, 185433), n = c(149096, 280227), alternative = "g")
