library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(cetcolor)
library(stringr)

parse_split_time <- function(x){
  seconds = as.numeric(gsub('.*:','',x))
  minutes = as.numeric(gsub('^.*?:??(\\d+):[^:]+$','\\1',x))
  hours = as.numeric(gsub('^(\\d+):\\d{2}:[^:]+$','\\1',x))
  return(seconds + coalesce(minutes, 0) * 60 + coalesce(hours, 0) * 3600)
}

format_time <- function(x){
  hours = x %/% 3600
  x = x %% 3600
  minutes = x %/% 60
  x = x %% 60
  seconds = x
  return(
    case_when(
      hours > 0 ~ paste(hours, str_pad(minutes, 2, pad='0'), str_pad(seconds, 2, pad='0'), sep=':'),
      minutes > 0 ~ paste(minutes, str_pad(seconds, 2, pad='0'), sep=':'),
      TRUE ~ as.character(seconds)
    )
    
  )
}

df = read_csv('/Users/mcandocia/Downloads/Export.csv', col_types=cols(.default='c')) %>%
  mutate(
    is_me = ifelse(Name=='Max Candocia', 'me', 'not me'),
    athlete = case_when(Name=='Max Candocia'~'Me', Name=='#firstlast' ~ '#first', Name=='#firstlast'~'#first', 
                        TRUE ~ 'Other')
  ) %>%
  mutate_at(
    c('Run','Swim','Bike','T1','T2','Time'),
    parse_split_time
  ) %>%
  mutate_at(
    c('Run','Swim','Bike','T1','T2','Time'),
    list(rank=~rank(.))
  )

ggplot(df) + geom_point(aes(x=Swim, y=Run, color=athlete)) + 
  scale_x_continuous('300m Swim Time', label=format_time) + 
  scale_y_continuous('5K Run Time', label=format_time) +
  scale_color_manual(values=c('Me'='#FF0000','#first1'='#00FF00','#first2'='#00FFFF','Other'='#00000077'))


ggplot(df) + geom_point(aes(x=Swim, y=Run, color=is_me)) + 
  scale_x_continuous('300m Swim Time', label=format_time) + 
  scale_y_continuous('5K Run Time', label=format_time) +
  theme_bw() + 
  ggtitle('Run and Swim Split Times')

ggplot(df) + geom_point(aes(x=Swim_rank, y=Run_rank, color=is_me))
  
vdf = df %>% group_by(Name) %>% summarize(variation = sd(c(Swim_rank, Run_rank, Bike_rank))) %>% 
  ungroup() %>%
  arrange(desc(variation)) %>%
  mutate(rank=1:n())

ggplot(vdf %>% mutate(is_me=ifelse(Name=='Max Candocia','me','not me')) )+
  geom_point(aes(x=rank, y=variation, color=is_me)) + 
  theme_bw() + 
  ylab('Deviation among Run, Bike, and Swim rankings')
