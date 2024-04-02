rm(list=ls())

library(data.table)
library(dplyr)
library(readxl)
library(ggplot2)

df1 <- read_excel('~/Downloads/LCA_Disclosure_Data_FY2023_Q1.xlsx')
df2 <- read_excel('~/Downloads/LCA_Disclosure_Data_FY2023_Q2.xlsx')
df3 <- read_excel('~/Downloads/LCA_Disclosure_Data_FY2023_Q3.xlsx')
df4 <- read_excel('~/Downloads/LCA_Disclosure_Data_FY2023_Q4.xlsx')

h1b <- rbind(df1,df2,df3,df4)
gc()

names(h1b)
str(h1b)
View(head(h1b))

table(h1b$VISA_CLASS)
h1b <- h1b %>% filter(VISA_CLASS == "H-1B")

table(h1b$CASE_STATUS)
h1b_pos <- h1b %>% filter(CASE_STATUS=='Certified')
min(h1b_pos$DECISION_DATE)
max(h1b_pos$DECISION_DATE)

table(h1b_pos$FULL_TIME_POSITION)
h1b_pos <- h1b_pos %>% filter(FULL_TIME_POSITION == 'Y')

employers <- as.data.frame(table(h1b_pos$EMPLOYER_NAME))
colnames(employers) <- c('Company', 'Count of H1B')
employers <- employers %>% arrange(desc(`Count of H1B`))

ggplot(h1b_pos[h1b_pos$WAGE_RATE_OF_PAY_TO<=300000 & !(is.na(h1b_pos$WAGE_RATE_OF_PAY_TO)),], aes(x = WAGE_RATE_OF_PAY_FROM, fill = "Lower End")) +
  geom_histogram(bins = 20, alpha = 0.5) +
  geom_histogram(aes(x = WAGE_RATE_OF_PAY_TO, fill = "Higher End"), bins = 20, alpha = 0.5) +
  labs(title = "Wage ranges for successful H1B applications",
       x = "Values",
       y = "Frequency",
       fill = "Legend") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal()
