#TIME LOG:

library(data.table)
library(openxlsx)
library(reshape2)
library(ggplot2)
library(Hmisc) 
library(dplyr)
library(car)
library(caret)
library(readxl)
library(ggsignif)
library(tidyr)
library(vegan)
library(purrr)
library(egg)
library(tidyverse)

#### Load the data ####
data = fread("data_30_9.csv")

# Quality Control:
data
dim(data)
head(data)
colnames(data)

numerical <- unlist(lapply(data, is.numeric))
datan = data[,numerical]
summary(data[,numerical])
data$grou


#### Choosing the right variables ####
variable.names.full  = read.xlsx("variables2.xlsx")
Y.variable.names = na.omit(variable.names.full[1])
X.variable.names = na.omit(variable.names.full[2])
Z.variable.names = na.omit(variable.names.full[3])

Y.variable.names = Y.variable.names[is.na(Y.variable.names)]

Y.variable.names

X.Data = data %>% select(as.character(X.variable.names))
Y.Data = data %>% select(as.character(Y.variable.names))
dim(X.Data)
dim(data)



#### Comparison of Averages ####

ggplot(data, aes(x = data$Out_emp_hum3_totalMean)) +   geom_histogram() +   facet_grid(Group_T2 ~ .) 

data.group.1 = data %>% filter (Group_T2 == 1)
data.group.2 = data %>% filter (Group_T2 == 2)
dim(data.group.2)

?wilcox.test
wilcox.test(data.group.1$Out_em_per3_totalMean, data.group.2$Out_emp_hum3_totalMean)
wilcox.test.default(data.group.1$Out_em_per3_totalMean, data.group.2$Out_emp_hum3_totalMean)

shapiro.test((data.group.2$Out_em_per3_totalMean))

t.test((data.group.1$Out_em_per3_totalMean, data.group.2$Out_emp_hum3_totalMean))



# First you must check normality: 


# NORMALITY: Shapiro test
# From the output, the p-value > 0.05 implies that the distribution
#of the data are not significantly different from a normal distribution.
#In other words, we can assume the normality.

Normality.Table = as.data.frame(
  do.call(rbind, lapply(data, function(x) shapiro.test(x)[c("p.value")] # Calculates Shapiro test for each variable, and attaches the pvalue to a new table
  ))) 
Normality.Table$Normal = Normality.Table$p.value < 0.05                             # This will show you which rows have pvalue<0.05

#And now the same for logs:
logstable = log10(data[,numerical])                                                 # Calculates the log of the data
Logs.Normality.Table = as.data.frame(
  do.call(rbind, lapply(logstable, function(x) shapiro.test(x)[c("p.value")])))     # Calculates Shapiro test for each variable, and attaches the pvalue to a new table
Logs.Normality.Table$Normal = Logs.Normality.Table$p.value < 0.05                   # This will show you which rows have pvalue<0.05

Normality.Table.Combined = cbind(Normality.Table, Logs.Normality.Table)             # Combines the regular and Log shapiro test results
colnames(Normality.Table.Combined)[3:4] = c("LOG: p.value", "LOG: Normal")          # renames the columns

colnames(data)

data.for.normality %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free", ncol = 3) +
  geom_histogram()

data.for.normality %>%
  keep(is.numeric) %>% 
  gather() %>% 
  # you have to specify the sample you want to use
  ggplot(aes(sample = value)) + 
  facet_wrap(~ key, scales = "free", ncol = 3) +
  stat_qq() + 
  stat_qq_line()


ggplot(data, aes(sample = Appointments1..NEW..Secs.)) + stat_qq() + stat_qq_line()


# This implies all distributions are non-normal
# Which makes sense given that you have ~50 human samples involved
