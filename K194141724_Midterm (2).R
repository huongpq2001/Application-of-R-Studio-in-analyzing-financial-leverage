library(ggplot2) #for plotting
library(tidyverse)  #for data frame manipulation
library(forcats) #for handling factors
library(scales) #for axis scale formatting

require(tidyverse)

## Task 1: Complete in word file
## Import data
library("readxl")
df = read_excel('040522 Data Mid-term test Final.xlsx')
View(df)
df

## Task 2: Create data set
set.seed(724)
new_df <- df[sample(1:752, 100, replace =F), ]
View(new_df)

# Check and fill NA
new_df %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x)) -> new_df

# Count the remained missing values
colSums(is.na(new_df)) 

## Task 3: Report
# Create 'leverage' variable
new_df <-new_df %>%
  mutate(leverage=totaldebt/totalasset)
View(new_df)

# report 5 firms with highest leverage
max(new_df$leverage)
highest <-new_df[order(new_df$leverage,decreasing=T)[1:5],]
View(highest)
cat(paste('Top 5 companies with the highest leverage: \n', list(highest$firmname)))

# report 5 firms with lowest leverage
min(new_df$leverage)
lowest <-new_df[order(new_df$leverage,decreasing=F)[1:5],]
View(lowest)
cat(paste('Top 5 companies with the lowest leverage: \n', list(lowest$firmname)))

# Name of industries which the firms belong to
a=highest$industry
b=lowest$industry
c=c(a,b)
cat(paste('Name of industries which the firms belong to: \n', list(unique(c))))

# 'Create Size' variables
size=list()
for (i in rownames(new_df)) {
  if (new_df[i, 'totalequity'] <= 20000000000 | new_df[i, 'revenue'] <= 50000000000) {
    size <- append(size, 'Small')
  } else if (20000000000 < new_df[i, 'totalequity'] & new_df[i, 'totalequity'] <= 100000000000 | 50000000000 < new_df[i, 'revenue'] & new_df[i, 'revenue'] <= 200000000000) {
    size <- append(size, 'Medium')
  } else {
    size <- append(size, 'Big')
  }
}
size
length(size)
new_df$size = size  # Append size to data set

sapply(new_df, typeof)   # Check types of data columns

new_df$size = as.character(new_df$size)   # Convert type of list to character 
View(new_df)

table(new_df$size)

# Descriptive statistics
# For discrete variable
new_df %>% 
  group_by(size) %>% 
  summarise(mean_lev=mean(leverage), median_lev=median(leverage), max_lev=max(leverage), min_lev=min(leverage), std_lev=sd(leverage))

# For continuous variables
# ROA
median(new_df$roa)
new_df %>% 
  mutate(x=cut(new_df$roa,breaks=c(min(new_df$roa), median(new_df$roa), max(new_df$roa)),labels=c("below","above"))) %>% 
  group_by(x) %>% 
  summarise(mean_lev=mean(leverage), median_lev=median(leverage), max_lev=max(leverage), min_lev=min(leverage), std_lev=sd(leverage))

# PPE
# Calculate PPE ratio
new_df$ppe = new_df$ppe / new_df$totalasset
median(new_df$ppe)
new_df %>% 
  mutate(x=cut(new_df$ppe,breaks=c(min(new_df$ppe), median(new_df$ppe), max(new_df$ppe)),labels=c("below","above"))) %>% 
  group_by(x) %>% 
  summarise(mean_lev=mean(leverage), median_lev=median(leverage), max_lev=max(leverage), min_lev=min(leverage), std_lev=sd(leverage))

# Current ratio
# Calculate 'current ratio' variable
new_df$current_ratio = new_df$totalcurrentasset / new_df$currentliabilities
median(new_df$current_ratio)
new_df %>% 
  mutate(x=cut(new_df$current_ratio,breaks=c(min(new_df$current_ratio), median(new_df$current_ratio), max(new_df$current_ratio)),labels=c("below","above"))) %>% 
  group_by(x) %>% 
  summarise(mean_lev=mean(leverage), median_lev=median(leverage), max_lev=max(leverage), min_lev=min(leverage), std_lev=sd(leverage))


## Task 4: Data Visualization
# Draw histogram of leverage
ggplot(new_df, aes(x=leverage)) +
  geom_histogram(binwidth=.5, fill='grey', color='white')   

# Draw scatter plot of leverage with the continuous variable
# ROA
new_df %>% 
  filter(!is.na(leverage), !is.na(roa)) %>% 
  ggplot(aes(x=leverage, y=roa)) +   
  geom_point(size=3)

# PPE
new_df %>% 
  filter(!is.na(leverage), !is.na(ppe)) %>% 
  ggplot(aes(x=leverage, y=ppe)) +   
  geom_point(size=3)

# Current ratio
new_df %>% 
  filter(!is.na(leverage), !is.na(current_ratio)) %>% 
  ggplot(aes(x=leverage, y=current_ratio)) +   
  geom_point(size=3) 

# Draw box plot of leverage with the discrete variable (different color for different categories of discrete variable)
new_df %>% 
  filter(!is.na(size), !is.na(leverage)) %>% 
  ggplot(aes(x = size, y = leverage, fill=size)) +
  geom_boxplot() 

# Draw a plot that allow the combination of continuous, discrete variables and leverage
# ROA
new_df %>% 
  filter(!is.na(leverage), !is.na(roa)) %>% 
  ggplot(aes(x=leverage, y=roa, color=factor(size))) +   
  geom_point(size=3)

# PPE
new_df %>% 
  filter(!is.na(leverage), !is.na(ppe)) %>% 
  ggplot(aes(x=leverage, y=ppe, color=factor(size))) +   
  geom_point(size=2)

# Current ratio
new_df %>% 
  filter(!is.na(leverage), !is.na(current_ratio)) %>% 
  ggplot(aes(x=leverage, y=current_ratio, color=factor(size))) +   
  geom_point(size=3)

## Task 5: 
# Count the number of firms in a identified industry
ind = readline(prompt = "Please enter a industry name : ")
num=0
for (i in rownames(new_df)) {
  if (new_df[i, 'industry'] == ind) {
    num=num+1
  } 
}
print(paste('The number of ', ind, 'companies is: ', num))

# Count the number of firms in an industry and with leverage above a certain value 
ind = readline(prompt = "Please enter a industry name : ")
pct = as.numeric(readline(prompt = 'PLease enter a percentage value: '))
num=0
for (i in rownames(new_df)) {
  if (new_df[i, 'industry'] == ind & new_df[i, 'leverage'] > pct) {
    num=num+1
  }
}
print(paste('The number of ', ind, 'companies which has the higher leverage value of', pct, 'is: ', num))
