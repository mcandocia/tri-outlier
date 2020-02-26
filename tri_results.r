library(tidyverse)
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(cetcolor)
library(stringr)
library(GGally)
library(knitr)
library(ape)



parse_split_time <- function(x){
  elements = strsplit(x, ':')
  seconds = unlist(lapply(elements, function(x) sum( as.numeric(x) * rev(60^(1:length(x) - 1)))))
  return(seconds)
}

format_time_list <- function(x){
  print(x)
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


format_time <- function(x){
  x = as.numeric(x)
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

df = read_csv('Export.csv', col_types=cols(.default='c')) %>%
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

pair_plot <- function(data, v1, v2,...){
  ggplot(data) + 
    geom_point(aes_string(x=v1,y=v2,color='is_me', ...)) + 
    scale_x_continuous(labels=format_time) + 
    scale_y_continuous(labels=format_time) +
    scale_color_manual('Is Me?', values=c('me'='#FF0000','not me'='#000000'))
}

pair_plot_rank <- function(data, v1, v2,...){
  ggplot(data) + 
    geom_point(aes_string(x=v1,y=v2,color='is_me', ...)) + 
    scale_color_manual('Is Me?', values=c('me'='#FF0000','not me'='#000000'))
}

dens_plot <- function(data, v, ...){
  ggplot(data) + geom_density(aes_string(x=v,...)) +
    scale_x_continuous(labels=format_time) + 
    geom_vline(color='red', xintercept=data %>% filter(is_me=='me') %>% pull(!!v))
}

dens_plot_rank <- function(data, v, ...){
  ggplot(data) + geom_density(aes_string(x=v,...)) +
    geom_vline(color='red', xintercept=data %>% filter(is_me=='me') %>% pull(!!v))
}

p_rr = dens_plot(df, 'Run')
p_bb = dens_plot(df, 'Bike')
p_ss = dens_plot(df, 'Swim')

p_sr = pair_plot(df, 'Swim','Run')
p_sb = pair_plot(df, 'Swim','Bike')
p_br = pair_plot(df, 'Bike','Run')

pr_rr = dens_plot_rank(df, 'Run_rank')
pr_bb = dens_plot_rank(df, 'Bike_rank')
pr_ss = dens_plot_rank(df, 'Swim_rank')

pr_sr = pair_plot_rank(df, 'Swim_rank','Run_rank')
pr_sb = pair_plot_rank(df, 'Swim_rank','Bike_rank')
pr_br = pair_plot_rank(df, 'Bike_rank','Run_rank')

p0 = ggplot() + geom_blank() + theme_void()

plot_list = list(p_ss, p0, p0, p_sb, p_bb, p0, p_sr, p_br, p_rr)

rank_plot_list = list(pr_ss, p0, p0, pr_sb, pr_bb, p0, pr_sr, pr_br, pr_rr)

ggmatrix(plot_list, ncol=3,nrow=3, xAxisLabels=c('Swim','Bike','Run'), 
         yAxisLabels=c('Swim','Bike','Run'), legend=4, title='Tri the Illini Event Times')

ggmatrix(rank_plot_list, ncol=3, nrow=3, xAxisLabels=c('Swim','Bike','Run'), 
         yAxisLabels=c('Swim','Bike','Run'), legend=4, title='Tri the Illini Event Rankings')


ggpairs(df %>% select(Swim, Bike, Run))


ggplot(df) + geom_point(aes(x=Swim, y=Run, color=is_me)) + 
  scale_x_continuous('300m Swim Time', label=format_time) + 
  scale_y_continuous('5K Run Time', label=format_time) +
  theme_bw() + 
  ggtitle('Run and Swim Split Times')

ggplot(df) + geom_point(aes(x=Swim_rank, y=Run_rank, color=athlete)) +
  scale_color_manual(values=c('Me'='#FF0000','#first1'='#00FF00','#first2'='#00FFFF','Other'='#00000077'))
  
vdf = df %>% group_by(Name) %>% summarize(variation = sqrt(2/3) * sd(c(Swim_rank, Run_rank, Bike_rank))) %>% 
  ungroup() %>%
  arrange(desc(variation)) %>%
  mutate(rank=1:n())

ggplot(vdf %>% mutate(is_me=ifelse(Name=='Max Candocia','Me','Other')) )+
  geom_point(aes(x=rank, y=variation, color=is_me)) + 
  theme_bw() + 
  ylab('Deviation among Run, Bike, and Swim rankings') +
  scale_color_manual('Is Me', values=c('Me'='#FF0000','Other'='#000000'))

# calculate mahalanobis distance

center = colMeans(df[,c('Swim','Bike','Run')])
center_rank = colMeans(df[,c('Swim_rank','Bike_rank','Run_rank')])

sigma2 = var(df[,c('Swim','Bike','Run')])
sigma2_rank = var(df[,c('Swim_rank','Bike_rank','Run_rank')])

df$mahalanobis_distance = mahalanobis(df[,c('Swim','Bike','Run')], center, sigma2)
df$mahalanobis_distance_rank = mahalanobis(df[,c('Swim_rank','Bike_rank','Run_rank')], center_rank, sigma2_rank)

df$euclidean_distance = mahalanobis(df[,c('Swim','Bike','Run')], center, sigma2 * diag(3))
df$euclidean_distance_rank = mahalanobis(df[,c('Swim_rank','Bike_rank','Run_rank')], center_rank, sigma2_rank * diag(3))

df = df %>% 
  mutate(
    mahalanobis_ranking = rank(-mahalanobis_distance),
    mahalanobis_rank_ranking = rank(-mahalanobis_distance_rank),
    euclidean_ranking = rank(-euclidean_distance),
    euclidean_rank_ranking = rank(-euclidean_distance_rank)
  )

# sort by mahalanobis distance

ggplot(df) + 
  geom_point(aes(x=mahalanobis_ranking, y = mahalanobis_distance, color=is_me)) + 
  scale_color_manual('Is Me?', values=c('me'='red','not me'='black')) + 
  xlab('Ranking') + ylab('Mahalanobis Distance') + 
  ggtitle('Mahalanobis distances of Tri the Illini 2019 athlete\'s swim, bike, and run times') + 
  theme_bw()

ggplot(df) + 
  geom_point(aes(x=mahalanobis_rank_ranking, y = mahalanobis_distance_rank, color=is_me)) + 
  scale_color_manual('Is Me?', values=c('me'='red','not me'='black')) + 
  xlab('Ranking') + ylab('Mahalanobis Distance') + 
  ggtitle('Mahalanobis distances of Tri the Illini 2019 athlete\'s swim, bike, and run rankings') + 
  theme_bw()

# run rank color
ggplot(df) + 
  geom_point(aes(x=mahalanobis_ranking, y = mahalanobis_distance, color=Run_rank), alpha=1, shape='|',size=5) + 
  scale_color_gradientn(colors=rev(cet_pal(7, 'inferno'))) + 
  xlab('Ranking') + ylab('Mahalanobis Distance') + 
  ggtitle('Mahalanobis distances of Tri the Illini 2019 athlete\'s swim, bike, and run times') + 
  theme_bw()

ggplot(df) + 
  geom_point(aes(x=mahalanobis_rank_ranking, y = mahalanobis_distance_rank, color=Run_rank), alpha=1, shape='|',size=5) + 
  scale_color_gradientn(colors=rev(cet_pal(7, 'inferno'))) + 
  xlab('Ranking') + ylab('Mahalanobis Distance') + 
  ggtitle('Mahalanobis distances of Tri the Illini 2019 athlete\'s swim, bike, and run rankings') + 
  theme_bw()

# bike rank color
ggplot(df) + 
  geom_point(aes(x=mahalanobis_ranking, y = mahalanobis_distance, color=Bike_rank), alpha=1, shape='|',size=5) + 
  scale_color_gradientn(colors=rev(cet_pal(7, 'inferno'))) + 
  xlab('Ranking') + ylab('Mahalanobis Distance') + 
  ggtitle('Mahalanobis distances of Tri the Illini 2019 athlete\'s swim, bike, and run times') + 
  theme_bw()

ggplot(df) + 
  geom_point(aes(x=mahalanobis_rank_ranking, y = mahalanobis_distance_rank, color=Bike_rank), alpha=1, shape='|',size=5) + 
  scale_color_gradientn(colors=rev(cet_pal(7, 'inferno'))) + 
  xlab('Ranking') + ylab('Mahalanobis Distance') + 
  ggtitle('Mahalanobis distances of Tri the Illini 2019 athlete\'s swim, bike, and run rankings') + 
  theme_bw()

# swim rank color
ggplot(df) + 
  geom_point(aes(x=mahalanobis_ranking, y = mahalanobis_distance, color=Swim_rank), alpha=1, shape='|',size=5) + 
  scale_color_gradientn(colors=rev(cet_pal(7, 'inferno'))) + 
  xlab('Ranking') + ylab('Mahalanobis Distance') + 
  ggtitle('Mahalanobis distances of Tri the Illini 2019 athlete\'s swim, bike, and run times') + 
  theme_bw()

ggplot(df) + 
  geom_point(aes(x=mahalanobis_rank_ranking, y = mahalanobis_distance_rank, color=Swim_rank), alpha=1, shape='|',size=5) + 
  scale_color_gradientn(colors=rev(cet_pal(7, 'inferno'))) + 
  xlab('Ranking') + ylab('Mahalanobis Distance') + 
  ggtitle('Mahalanobis distances of Tri the Illini 2019 athlete\'s swim, bike, and run rankings') + 
  theme_bw()

df_mahalanobis = df %>%
  select(
    mahalanobis_ranking,
    mahalanobis_distance,
    Swim_rank,
    Run_rank,
    Bike_rank,
    `Overall Rank`=Pos
  ) %>%
  melt(id.vars=c('mahalanobis_ranking','mahalanobis_distance'))

df_mahalanobis_rank = df %>%
  select(
    mahalanobis_rank_ranking,
    mahalanobis_distance_rank,
    Swim_rank,
    Run_rank,
    Bike_rank,
    `Overall Rank`=Pos
  ) %>%
  melt(id.vars=c('mahalanobis_rank_ranking','mahalanobis_distance_rank'))

# plot above molten dfs

ggplot(df_mahalanobis %>% mutate(variable = gsub('_rank',' Rank', variable), value=as.numeric(value)) %>%
         mutate(variable = factor(variable, levels=c('Swim Rank','Bike Rank','Run Rank','Overall Rank')))) + 
  geom_point(aes(x=mahalanobis_ranking, y=mahalanobis_distance, color=value), size=5, pch='|') +
  facet_grid(variable~.) + 
  scale_color_gradientn('Ranking', colors=rev(cet_pal(7, 'inferno'))) + 
  xlab('Ranking') + ylab('Mahalanobis Distance') + 
  ggtitle('Mahalanobis distance rankings of run, bike, and swim times of \nTri the Illini 2019 athletes ',
  subtitle='colored by event/overall rankings')

ggplot(df_mahalanobis_rank %>% mutate(variable = gsub('_rank',' Rank', variable), value=as.numeric(value)) %>%
         mutate(variable = factor(variable, levels=c('Swim Rank','Bike Rank','Run Rank','Overall Rank')))) + 
  geom_point(aes(x=mahalanobis_rank_ranking, y=mahalanobis_distance_rank, color=value), size=5, pch='|') +
  facet_grid(variable~.) + 
  scale_color_gradientn('Ranking', colors=rev(cet_pal(7, 'inferno'))) + 
  xlab('Ranking') + ylab('Mahalanobis Distance') + 
  ggtitle('Mahalanobis distance rankings of run, bike, and swim rankings of ]\nTri the Illini 2019 athletes',
  subtitle='colored by event/overall rankings')

# euclidean

ggplot(df) + 
  geom_point(aes(x=euclidean_ranking, y = euclidean_distance, color=is_me),alpha=0.3) + 
  scale_color_manual('Is Me?', values=c('me'='red','not me'='black')) + 
  xlab('Ranking') + ylab('Mahalanobis Distance') + 
  ggtitle('Euclidean distances of Tri the Illini 2019 athlete\'s swim, bike, and run times') + 
  theme_bw()

ggplot(df) + 
  geom_point(aes(x=euclidean_rank_ranking, y = euclidean_distance_rank, color=is_me), alpha=0.3) + 
  scale_color_manual('Is Me?', values=c('me'='red','not me'='black')) + 
  xlab('Ranking') + ylab('Mahalanobis Distance') + 
  ggtitle('Euclidean distances of Tri the Illini 2019 athlete\'s swim, bike, and run rankings') + 
  theme_bw()

df %>% arrange(mahalanobis_ranking) %>% head(10) %>% select(Pos, Name, Swim_rank, Run_rank, Bike_rank, mahalanobis_distance)

df %>% arrange(mahalanobis_rank_ranking) %>% head(10) %>% select(Pos, Name, Swim_rank, Run_rank, Bike_rank, mahalanobis_distance_rank)

df %>% arrange(euclidean_ranking) %>% head(10) %>% select(Pos, Name, Swim_rank, Run_rank, Bike_rank, mahalanobis_distance)

df %>% arrange(euclidean_rank_ranking) %>% head(10) %>% select(Pos, Name, Swim_rank, Run_rank, Bike_rank, mahalanobis_distance_rank)

# clustering

# hierarchical clustering
distmat = dist(df %>% select(Swim, Bike, Run) %>% mutate_all(scale))
clusters = hclust(distmat, method='ward.D2')
trees = cutree(clusters, 5)

cet_pal(9, 'bkr')

plot(as.phylo(clusters))

df$cluster= trees

ranked_distmat = dist(df %>% select(Swim_rank, Bike_rank, Run_rank))
ranked_clusters = hclust(ranked_distmat, method='ward.D2')
ranked_trees = cutree(ranked_clusters, 5)

df$ranked_cluster= ranked_trees

#https://stackoverflow.com/a/18749392/1362215
map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}

tip_colors = map2color(as.numeric(df$Pos),rev(cet_pal(200,'bkr')))

plot(as.phylo(ranked_clusters),show.tip.label=FALSE, main='Clusters of Tri the Illini 2019 athletes by swim, bike, and run ranks')
tiplabels(rep('---',424), col=tip_colors, frame='none', pos=4, offset=-30.0)
tiplabels(c(rep('',125),'--o',rep('', 298)), cex=2, frame='none', pos=4, offset=-30.0, col='#333333AA')

plot(as.phylo(clusters),show.tip.label=FALSE, main='Clusters of Tri the Illini 2019 athletes by swim, bike, and run times')
tiplabels(rep('---',424), col=tip_colors, frame='none', pos=4, offset=-0.25)
tiplabels(c(rep('',125),'--o',rep('', 298)), cex=2, frame='none', pos=4, offset=-0.25, col='#333333AA')

#png('large_phylo.png', height=4000, width=900); plot(ape::as.phylo(clusters));dev.off()

#png('large_ranked_phylo.png', height=4400, width=900); plot(ape::as.phylo(ranked_clusters));dev.off()

# knit code

knitr::knit('article.Rhtml')



