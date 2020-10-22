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

#### Choosing the right variables ####
variable.names.full  = read.xlsx("variables2.xlsx")
Y.variable.names = na.omit(variable.names.full[1])
X.variable.names = na.omit(variable.names.full[2])
Z.variable.names = na.omit(variable.names.full[3])

Y.variable.names = Y.variable.names[is.na(Y.variable.names)]

X.Data = as.data.frame(data %>% select((X.variable.names[,1])))
Y.Data = as.data.frame(data %>% select((Y.variable.names[,1])))
Z.Data = data %>% select((Z.variable.names[,1]))

Y.Data.num = as.data.frame(lapply(Y.Data, as.numeric))

XYdata = cbind(X.Data, Y.Data.num)
XYdata$Group_T2 = as.factor(XYdata$Group_T2)
lapply(XYdata, class)

# First you must check normality: 

#### NORMALITY: Shapiro test ####
# From the output, the p-value > 0.05 implies that the distribution
#of the data are not significantly different from a normal distribution.
#In other words, we can assume the normality.

Normality.Table = as.data.frame(
  do.call(rbind, lapply(XYdata[,2:28], function(x) shapiro.test(x)[c("p.value")] # Calculates Shapiro test for each variable, and attaches the pvalue to a new table
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

XYdata[,2:28] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free", ncol = 3) +
  geom_histogram()

XYdata[,2:28]  %>%
  keep(is.numeric) %>% 
  gather() %>% 
  # you have to specify the sample you want to use
  ggplot(aes(sample = value)) + 
  facet_wrap(~ key, scales = "free", ncol = 3) +
  stat_qq() + 
  stat_qq_line()


# This implies all distributions are non-normal
# Which makes sense given that you have ~50 human samples involved

#### Comparing groups ####
# We will use the wilcoxon Ranked Sum Test as a non-parametric test:
colnames(XYdata)

#### Function explained ####
# This function will work as follows: 
# Input: two variable names from your data set
# Output: Boxplot with Wilcoxon Rank Sum Test statistic printed on it


# Notes: 
# 1. You can ignore the warnings: 
# No id variables; using all as measure variables
# Removed 2 rows containing non-finite values
# It's just the way it handles NA lines. Which you have

# 2. Box plots: 
# The middle line is the median
# 
# The lower and upper hinges correspond to the first and third quartiles
#  
# The whiskers extend from the hinge to the furthest value but no further than 1.5 * IQR from the hinge 
# (where IQR is the inter-quartile range, or distance between the first and third quartiles). 

# Outliers: Points beyond the whisker limit

Y.data.group1 = XYdata %>% select()










wilcox.plus.boxes <- function(variable) {  # Defines the function requiring two parameters
  data.for.function = XYdata[,c(1,"Out_em_per2.4.COG")]      # creates a dataframe from your data which is only the two parameters you chose
  colnames(data.for.function) = c(var1,var2)   # Gives the relevant columns names (for later)
  
  varname = gsub("\\..*","",var1)
  levs    = c("New","Old")
  
  result = wilcox.test(                        # Calculates the wilcoxon test between the two variables
    data.for.function %>% filter(XYdata = 1),
    data.for.function %>% filter(XYdata = 2)
  ) 
  
  mean.diff = mean(data.for.function[,1], na.rm = T)-mean(data.for.function[,2],na.rm = T)
  
  melted = melt(data.for.function)             # Transforms the data for ggplot
  
  plot = ggplot(                               # This is a very long function to create the plot
    melted, aes(x=variable,y=value,fill=variable)) + # Defines the releant data
    geom_boxplot()+                                 # Defines the plot type as Box Plot
    theme(panel.background = element_rect(fill="white")) + # Makes it visually clean
    labs(x = varname,y="Time [Mins]") +                  # Adds X, Y axis labels
    theme(panel.grid.major = element_line(colour = "gray")) + # adds gray lines on the Y axis
    theme(panel.grid.minor = element_line(colour = "gray")) + # does the same
    guides(fill=FALSE) +                                      # Removes the legend (not needed for this)
    labs(                                                     # Startes to create the complicated titles
      title = paste("Comparison of", varname, sep = " "), # Main title: Comparison between the two variables
      
      
      
      
      subtitle = paste(                                                # Subtitle: gives the different statistics pasted together
        "Result of the Wilcoxon Rank Sum Test: \ntest statistic = ", 
        result$statistic, "\n p-value = ", 
        formatC(result$p.value, format = "g", digits = 2), 
        "\n mean difference = ", round(mean.diff, digits = 1), "minutes", sep = " " )
    ) + 
    scale_x_discrete(breaks = c(var1,var2), labels=c("Old","New")) +
    
    geom_signif(comparisons = list(c(var1, var2)),             # Creates a significance star when the pvalue<0.05
                test="wilcox.test", map_signif_level =  c("*"=0.05))
  
  
  plot                                                         # Saves the final plot 
  
}

names = colnames(data) # This is for my own reference to know which variables need to be tabulated

# Calculations:
# Each Q is a question from your list
Q1 = wilcox.plus.boxes(names[33],names[32]) #SUM
Q2 = wilcox.plus.boxes(names[19],names[18]) # Appointments1
Q3 = wilcox.plus.boxes(names[27],names[26]) # Mirshm
Q4 = wilcox.plus.boxes(names[29],names[28]) # Payments
Q5 = wilcox.plus.boxes(names[31],names[30]) # Update
Q6 = wilcox.plus.boxes(names[21],names[20]) # Appointments 2
Q7 = wilcox.plus.boxes(names[25],names[24]) # Medical File
Q8 = wilcox.plus.boxes(names[23],names[22]) # Blood Test

FIG3 = ggarrange(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8, ncol = 4)

