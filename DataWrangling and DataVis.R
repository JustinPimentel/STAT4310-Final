library(dragracer)
library(lubridate)
library(tidyverse)
library(gridExtra)

episodes <- dragracer::rpdr_ep
contep <- dragracer::rpdr_contep
contestants <- dragracer::rpdr_contestants

## Astrology Information Source: https://www.allure.com/story/zodiac-sign-personality-traits-dates
##################################################################################################
contestants$sameYear <- ymd(gsub("....-","2000-",contestants$dob))
signIntervals <- c(interval(ymd("2000-01-20"), ymd("2000-02-18")), interval(ymd("2000-02-19"), ymd("2000-03-20")),
                   interval(ymd("2000-03-21"), ymd("2000-04-19")), interval(ymd("2000-04-20"), ymd("2000-05-20")),
                   interval(ymd("2000-05-21"), ymd("2000-06-20")), interval(ymd("2000-06-21"), ymd("2000-07-22")),
                   interval(ymd("2000-07-23"), ymd("2000-08-22")), interval(ymd("2000-08-23"), ymd("2000-09-22")),
                   interval(ymd("2000-09-23"), ymd("2000-10-22")), interval(ymd("2000-10-23"), ymd("2000-11-21")),
                   interval(ymd("2000-11-22"), ymd("2000-12-21")), interval(ymd("2000-12-22"), ymd("2000-12-31")),
                   interval(ymd("2000-01-01"), ymd("2000-01-19")))
signNames <- c("Aquarius","Pisces","Aries","Taurus","Gemini","Cancer","Leo","Virgo","Libra","Scorpio","Sagittarius","Capricorn","Capricorn")
signdf <- data.frame(signNames, signIntervals)

for (i in 1:13){contestants$signs[contestants$sameYear %within% signdf[i,2]] <- as.character(signdf[i,1])}
##################################################################################################
##################################################################################################

## Insta Followers Information Source: https://instagram.com & https://twitter.com
##################################################################################################
idf <- data.frame(contestant = c('BeBe Zahara Benet','Tyra Sanchez','Raja','Sharon Needles','Jinkx Monsoon',
                                 'Bianca Del Rio','Violet Chachki','Bob the Drag Queen','Sasha Velour',
                                 'Aquaria'),
                  InstaFollowers = c(384000,15200,784000,998000,930000,1800000,1400000,1000000,1500000,1300000),
                  TwitterFollowers = c(83800,0,217000,422000,385000,559000,244000,310000,250000,197000))
##################################################################################################
##################################################################################################

champions <- idf %>% inner_join(contestants) %>% 
  select(contestant, InstaFollowers, TwitterFollowers,season, age, signs, bd_score) # Used by Graph 1

contepSubset <- contep %>% 
  inner_join(episodes, by = c('season','episode')) %>% 
  select(season, episode, rank, contestant, outcome, participant) %>% replace_na(list(outcome = 'Other')) %>%
  mutate(outcome = ifelse(outcome %in% c('LOST3RD ROUND', 'LOST2ND ROUND', 'LOST1ST ROUND'), 'LOW', outcome)) %>%
  filter(outcome %in% c('WIN','HIGH','SAFE','LOW','BTM', 'Other'))  %>%
  mutate(outcomeNum = ifelse(outcome == 'WIN', 2, ifelse(outcome == 'HIGH',1, ifelse(outcome == 'SAFE', 0,
                      ifelse(outcome == 'LOW', -1, ifelse(outcome == 'BTM', -2, 0)))))) %>%
  group_by(season, contestant) %>% 
  mutate(`Cum. DB Score` = cumsum(outcomeNum)) %>% mutate(maxDB = max(`Cum. DB Score`)) %>% ungroup() %>%
  group_by(season,contestant) %>% mutate(lastEp = ifelse(season %in% c('S01','S02','S06','S07'), max(episode)-1,
                                                  ifelse(season %in% c('S04','S05','S09','S10','S03'), max(episode)-2,max(episode))),
                                         isWinner = ifelse(rank == 1, T,F),
                                         firstName = ifelse(grepl(" ",contestant), substr(contestant, 1, str_locate(contestant," ")-1), contestant)) %>%
  mutate(episode = ifelse(season == 'S03',episode - 1, episode)) %>%
  mutate(episode = ifelse(episode - ifelse(is.na(lag(episode)),0,lag(episode)) > 1, lag(episode) + 1, episode)) %>% ungroup() %>%
  group_by(season) %>% mutate(Winner = ifelse(rank == 1, firstName, NA)) %>% ungroup() %>%
  group_by(season,contestant) %>% mutate(lastEp = max(episode)) %>% filter(participant == 1) 

colors1 <- c('Aquaria' = '#FFE09E','BeBe' = '#FF7B23', 'Bianca' = '#B0E5C2', 'Bob' = '#F8C7CC','Jinkx' = '#1D97C4', 
             'Raja' = '#EBEDCE', 'Sasha' = '#E29A76', 'Sharon' = '#A4E25D','Tyra' = '#EF5660', 'Violet' = '#7E6BD3')

colors2 <- c('Aquaria' = '#E2BA63','BeBe' = '#D85F0F', 'Bianca' = '#6BAF82', 'Bob' = '#F799A2',
             'Jinkx' = '#006A91', 'Raja' = '#B5B796', 'Sasha' = '#B56741', 'Sharon' = '#6AA328','Tyra' = '#C11924', 'Violet' = '#503F9B')
##################################################################################################
## Graph 1: Best Winner ##########################################################################

p1 <- ggplot(champions, aes(TwitterFollowers, InstaFollowers)) +
   geom_point(shape = 21, stroke = 2, size = 25, aes(fill = bd_score, color = bd_score)) +
   labs(x = '# Twitter Followers', y = '# Insta Followers', title = 'Tyra Sanchez is CANCELLED.',
        subtitle = 'Queens are described based on social media popularity and performance on the show (BD Score). \nBD Score compared by gold, silver, & bronze colors and to OTHER winners.',
        caption = 'This data describes winners of season 1-10 and excludes All Stars seasons using the dragracer R package. Social media data collected on 4/15/19.') +
   scale_y_continuous(breaks = seq(0,2000000,500000), labels = c('0','500k','1M','1.5M','2M'), limits = c(-5000, 2000000)) +
   scale_x_continuous(breaks = seq(0,600000,200000), labels = c('0','200k','400k','600k'), limits = c(0,600001)) +
   scale_color_gradientn(colors = c('#CD7F32', '#C0C0C0','#FFDF00')) + 
   scale_fill_gradientn(colors = c('#E8C4A1','#E2E2E2','#FFEA5C')) +
   guides(color = FALSE, fill = FALSE) + scale_size_continuous(range = c(6,20)) +
   geom_label(aes(label = contestant), size = 2.3, fontface = "bold") + 
   theme(panel.background = element_rect(fill = '#04061D'), plot.background = element_rect(fill = '#04061D'),
         panel.grid = element_line(size = 1.25), axis.ticks = element_blank(),
         panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, size = 25, face = 'bold', color = 'white'),
         plot.subtitle = element_text(hjust = 0.5, face = 'italic', color = '#FFF7E5'),
         axis.text = element_text(color = '#FFFAF0'), axis.title = element_text(color = '#FFFAF0'),
         plot.caption = element_text(hjust = 1, face = 'italic', color = '#D1CBBC', size = 7))

##################################################################################################
## Graph 2: BD Score over Season #################################################################

p2 <- contepSubset %>% filter(rank == 1) %>%
ggplot(aes(episode, `Cum. DB Score`, group = firstName, color = firstName, fill = firstName)) +
    geom_line(size = 1.5, alpha = 0.8) + 
    geom_point(aes(x = lastEp, y = maxDB), size = 3, alpha = 0.5) +
    labs(title = 'Champion Performance',caption = 'This data describes winners of season 1-10 and excludes All Stars seasons using the dragracer R package.',
         subtitle = 'Depicts the overall performance of the champion over the course of their season compared to OTHER winners.') +
    scale_x_continuous(name = 'Episode',breaks = c(1,5,10,15), limits = c(1,15.5), expand = c(0,0), minor_breaks = seq(1,15,1)) +
    scale_y_continuous(name = 'DB Score', breaks = c(0, 7, 14), limits = c(0,15), expand = c(0,0)) +
    scale_color_manual(values = colors2) +
    scale_fill_manual(values = colors1) +
    geom_label(aes(x = lastEp, y = maxDB, label = firstName), size = 3, fontface = 'bold', nudge_x = -.3) +
    theme(panel.background = element_rect(fill = '#04061D'), plot.background = element_rect(fill = '#04061D'),
          panel.grid = element_line(size = .5), axis.ticks = element_blank(), panel.grid.minor.y = element_blank(),
          panel.grid.minor = element_line(color = '#B5B3AD'), plot.title = element_text(hjust = 0.5, size = 25, face = 'bold', color = 'white'),
          plot.subtitle = element_text(hjust = 0.5, face = 'italic', color = '#FFF7E5'),
          axis.text = element_text(color = '#FFFAF0'), axis.title = element_text(color = '#FFFAF0'),
          plot.caption = element_text(hjust = 1, face = 'italic', color = '#D1CBBC', size = 7)) +
    guides(color = F, fill = F)

##################################################################################################
## Graph 3: BD Score for each Season #############################################################

p3 <- contepSubset %>%
  ggplot(aes(episode, `Cum. DB Score`, group = contestant, color = Winner)) +
    geom_line(aes(alpha = isWinner), size = 1.5) + 
    geom_point(data = contepSubset %>% filter(rank == 1), aes(x = lastEp, y = maxDB), size = 3) +
    labs(x = 'Episode', y = 'DB Score', title = 'Champion Performance in Their Season',caption = 'This data describes winners of season 1-10 and excludes All Stars seasons using the dragracer R package.',
         subtitle = 'Depicts the overall performance of the champion over the course of their season compared to their competitors.') +
    scale_fill_manual(values = colors1, na.value = '#9EA3B0') +
    scale_color_manual(values = colors2, na.value = '#9EA3B0') +
    scale_alpha_manual(values = c('FALSE' = 0.5, 'TRUE' = 1)) + 
    theme(panel.background = element_rect(fill = '#04061D'), plot.background = element_rect(fill = '#04061D'),
          panel.grid = element_line(size = .5), axis.ticks = element_blank(), panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(), plot.title = element_text(hjust = 0.5, size = 25, face = 'bold', color = 'white'),
          plot.subtitle = element_text(hjust = 0.5, face = 'italic', color = '#FFF7E5'),
          axis.text = element_text(color = '#FFFAF0'), axis.title = element_text(color = '#FFFAF0'),
          plot.caption = element_text(hjust = 1, face = 'italic', color = '#D1CBBC', size = 7),
          strip.text = element_text(face = 'bold', size = 10, color = '#04061D')) +
    guides(color = F, fill = F, alpha = F) +
    geom_label(aes(x = lastEp, y = maxDB, label = Winner, fill = Winner), size = 2, fontface = 'bold', nudge_y = 1.3, nudge_x = -1) +
    facet_wrap(.~season, nrow = 2)

p1
grid.arrange(p2,p3)
