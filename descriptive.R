library(dplyr)
library(ggplot2)
library(tidyr)
df <- read.csv('RegularSeasonData.csv', fileEncoding = 'latin1')

df_ot = df %>% filter(ot==1)

home_ot = df_ot %>% group_by(home) %>% summarise(win_h = sum(h>v), lose_h = sum(h<v))

ot = df_ot %>% group_by(visitor) %>% summarise(win_v = sum(v>h), lose_v = sum(v<h)) %>% full_join(home_ot, by=c('visitor'='home'))
ot = ot %>% mutate(Team = visitor, sum = win_v + lose_v + win_h + lose_h, win = win_v + win_h, lose = lose_h + lose_v, win_percent = win/sum*100) %>%
  select(Team, sum, win, lose, win_percent)

ot %>% select(Team, win, lose) %>% gather(type, value, -1) %>%
  ggplot(aes(x=Team, y=value, fill=type)) + geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=60),axis.title = element_blank(), legend.title = element_blank())

ggsave('ot_teams.jpg')

non_ot = df %>% filter(ot==0)

home_nonot = non_ot %>% group_by(home) %>% summarise(win_h = sum(h>v), lose_h = sum(h<v))

non_ot = non_ot %>% group_by(visitor) %>% summarise(win_v = sum(h<v), lose_v = sum(h>v)) %>% full_join(home_nonot, by=c('visitor'='home'))

non_ot = non_ot %>% mutate(Team = visitor, win = win_h + win_v, lose=lose_h + lose_v, sum=win+lose, win_percent = win/sum)

non_ot

non_ot %>% select(Team, win, lose) %>% gather(type, value, -1) %>%
  ggplot(aes(x=Team, y=value, fill=type)) + geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=60),axis.title = element_blank(), legend.title = element_blank())

ggsave('non_ot.jpg')

home_wins = df %>% group_by(home) %>% summarise(hwin = sum(h>v), hlose = sum(h<v), hsum = hwin+hlose, hwinning_percent = hwin/hsum)


all_wins = df %>% group_by(visitor) %>% summarise(vwin = sum(h<v), vlose = sum(h>v), vsum = vwin+vlose, vwinning_percent = vwin/vsum) %>%
  full_join(home_wins,by=(c('visitor'='home')))
all_wins

all_wins %>% select(visitor,vwinning_percent, hwinning_percent) %>%
  rename(Team = visitor, Home = hwinning_percent, Away = vwinning_percent) %>%
  gather(type, value, -1) %>%
  ggplot(aes(x=Team, y=value, fill=type)) + geom_bar(stat='identity', position='dodge') +
  scale_fill_brewer(palette = 'Set1') +
  theme(axis.text.x = element_text(angle = 60), legend.title = element_blank(), axis.title = element_blank())
  
ggsave('all_wins.jpg')

df %>% summarise(h_goals = mean(h), v_goals = mean(v))

visit_goals = df %>% group_by(visitor) %>% summarise(visit_goals = sum(v), mean_v = mean(v))
visit_goals %>% ggplot(aes(x= visitor, y =mean_v))+ geom_bar(stat='identity', fill ="#4971b2") + theme(axis.text.x = element_text(angle=60))

df %>% summarise(h_win = sum(h>v), v_win=sum(h<v), draw = sum(h==v), phwin = h_win/n(), pvwin = v_win/n())


all_goals = df %>% group_by(home) %>% summarise(home_goals = sum(h), mean_h = mean(h)) %>% full_join(visit_goals, by=c('home'='visitor'))
all_goals
all_goals %>% select(home, mean_h, mean_v) %>%
  rename(Home = mean_h, Away = mean_v) %>%
  gather(type,mean,-1) %>%
  ggplot(aes(x=home,y=mean,fill=type)) + geom_bar(stat='identity',position='dodge') +
  scale_fill_brewer(palette = 'Dark2')+
  theme(axis.text.x = element_text(angle = 60), legend.title = element_blank(), axis.title = element_blank())
ggsave('all_goals.jpg')


h_lost = df %>% group_by(home) %>% summarise(h_lost = mean(v))
goals_lost = df %>% group_by(visitor) %>% summarise(v_lost = mean(h)) %>% full_join(h_lost, by=c('visitor'='home'))

goals_lost %>% rename(Team = visitor, Home = h_lost, Away = v_lost) %>%
  gather(type,value,-1) %>%
  ggplot(aes(x=Team,y=value,fill=type)) + geom_bar(stat='identity',position='dodge') +
  theme(axis.text.x = element_text(angle = 60), legend.title = element_blank(), axis.title = element_blank())
ggsave('goals_lost.jpg')
  


require("gridExtra")
grid.arrange(arrangeGrob(p2))
ggsave('visitor_goals.jpg')

max(df1$mean_v)
min(df1$mean_v)
