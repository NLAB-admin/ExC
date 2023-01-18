## Exogenous Cognition
## Delegation Survey Demo
library(readxl)
df <- read_excel("Delegation Questionnaire Demo (Responses).xlsx") 

df$subject = c(1:dim(df)[1])
names(df)[2] <- "holiday"
names(df)[3] <- "car"
names(df)[4] <- "savings"
names(df)[5] <- "present"
names(df)[6] <- "energy"
names(df)[7] <- "takeout"
names(df)[8] <- "furniture"
names(df)[9] <- "smartphone"
names(df)[10] <- "clothes"
names(df)[11] <- "grocery"
names(df)[12] <- "holiday"
names(df)[13] <- "car"
names(df)[14] <- "savings"
names(df)[15] <- "present"
names(df)[16] <- "energy"
names(df)[17] <- "takeout"
names(df)[18] <- "furniture"
names(df)[19] <- "smartphone"
names(df)[20] <- "clothes"
names(df)[21] <- "grocery"
names(df)[22] <- "holiday"
names(df)[23] <- "car"
names(df)[24] <- "savings"
names(df)[25] <- "present"
names(df)[26] <- "energy"
names(df)[27] <- "takeout"
names(df)[28] <- "furniture"
names(df)[29] <- "smartphone"
names(df)[30] <- "clothes"
names(df)[31] <- "grocery"

# from wide to long format
df1 = df[c(2:11,32)]
df2 = df[c(12:21,32)]
df3 = df[c(22:32)]
library(tidyverse)
df1l = gather(df1, cats, enjoy, holiday:grocery)
df2l = gather(df2, cats, use, holiday:grocery)
df3l = gather(df3, cats, importance, holiday:grocery)
library(dplyr)
df1l = df1l %>%
  mutate(enjoy_r = case_when(enjoy == 'Enjoy very much' ~ 2,
                             enjoy == 'Enjoy' ~ 1,
                             enjoy == 'Neither enjoy or don\'t enjoy' ~ 0,
                             enjoy == 'Don\'t enjoy' ~ -1,
                             enjoy == 'Don\'t enjoy at all' ~ -2))
df2l = df2l %>%
  mutate(use_r = case_when(use == 'Definitely use' ~ 2,
                             use == 'Use' ~ 1,
                             use == 'Neutral' ~ 0,
                             use == 'Will not use' ~ -1,
                             use == 'Definitely will not use' ~ -2))
df3l = df3l %>%
  mutate(importance_r = case_when(importance == 'Very important' ~ 2,
                                  importance == 'Fairly important' ~ 1,
                                  importance == 'Important' ~ 0,
                                  importance == 'Slightly important' ~ -1,
                                  importance == 'Fairly important, Slightly important' ~ 0,
                                  importance == 'Not at all important' ~ -2))

## Aggregation for the plot
df1ls <- df1l %>% group_by(cats) %>% summarize(avenjoy = mean(enjoy_r))
df2ls <- df2l %>% group_by(cats) %>% summarize(avuse = mean(use_r))
df3ls <- df3l %>% group_by(cats) %>% summarize(avimportance = mean(importance_r))

df12ls <- merge(df1ls,df2ls,by="cats")
dfs <- merge(df12ls,df3ls,by="cats")

p1 <- ggplot(dfs, aes(y = avenjoy, x = avuse, label = cats)) + 
  geom_point(aes(color = avimportance)) +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) +
  geom_text(hjust=-0.25, vjust=0.25) +
  xlab("Intention to Use") +
  ylab("Enjoyment") +
  ylim(-1.5,1.5) +
  xlim(-1,1.5) +
  theme_bw() +
  ggtitle("Demo Delegation Survey")
p1

## Merge individual data
df12l <- merge(df1l,df2l,by=c("subject","cats"))
dfl <- merge(df12l,df3l,by=c("subject","cats"))
p2 <- ggplot(dfl, aes(y = enjoy_r, x = use_r)) + 
  geom_point(aes(color = importance_r)) +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) +
  facet_wrap(~ cats) +
  #geom_text(hjust=-0.25, vjust=0.25) +
  xlab("Intention to Use") +
  ylab("Enjoyment") +
  #ylim(-1.5,1.5) +
  #xlim(-1,1.5) +
  theme_bw() +
  ggtitle("Demo Delegation Survey")
p2


