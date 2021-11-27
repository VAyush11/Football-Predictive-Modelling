library(data.table)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(dplyr)

feable <- fread("Class 1 Team 3 match_outcome_model.csv", header = T)
sum(is.na(feable))

theme_set(theme_classic())

# Appendix F1
feable$home_formation <- as.factor(feable$home_formation)
ggplot(feable, aes(home_formation)) + 
  scale_fill_brewer(palette = "Spectral") +
  geom_bar(aes(fill=label), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Bar Plot of Label and Home Team Formation", 
       x="Home Team Formation", y = "Number of records")

# Appendix F2
ggplot(feable, aes(home_formation)) +
  scale_fill_brewer(palette = "Spectral") +
  geom_bar(aes(fill=label), width = 0.5, position = 'fill') + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Bar Plot of Label and Home Team Formation", 
       x="Home Team Formation", y = "Proportion")

# Appendix G
ggplot(feable, aes(B365_Win)) +  
  scale_fill_brewer(palette = "Spectral") +
  geom_histogram(aes(fill=label), col=I("black"), binwidth = 0.05) +  
  labs(title="Histogram of B365 Odds", x = "Betting Odds in Winning (B365)", y = "Number of records")

# Appendix H1
ggplot(feable, aes(home_team_buildUpPlaySpeedClass)) +
  scale_fill_brewer(palette = "Spectral") +
  geom_bar(aes(fill=label), width = 0.5, position = 'fill') + 
  labs(title="Bar Plot of Label and Home Team BuildUp Play Speed Class", 
       x="Class", y = "Proportion")

# Appendix H2
ggplot(feable, aes(home_team_chanceCreationPassingClass)) +
  scale_fill_brewer(palette = "Spectral") +
  geom_bar(aes(fill=label), width = 0.5, position = 'fill') + 
  labs(title="Bar Plot of Label and Chance Creation Passing Class", 
       x="Class", y = "Proportion")

# Appendix H3
ggplot(feable, aes(home_team_defenceDefenderLineClass)) +
  scale_fill_brewer(palette = "Spectral") +
  geom_bar(aes(fill=label), width = 0.5, position = 'fill') + 
  labs(title="Bar Plot of Label and Defence Defender Line Class", 
       x="Class", y = "Proportion")

# Appendix J
player <- fread("Class 1 Team 3 player_ratings_model.csv", stringsAsFactors = T)
player[player == "null"] <- NA

clean.player = player[,!c('V1','player_name', 'player_fifa_api_id', 'birthday', 
                          'date', 'potential', 'attacking_work_rate', 
                          'defensive_work_rate')]
clean.player = na.omit(clean.player)

ggplot(clean.player, aes(overall_rating)) + scale_fill_brewer(palette = "Spectral") + 
  geom_histogram(aes(fill=preferred_foot), 
                 binwidth = 1, 
                 col="black") +  
  labs(title="Histogram of Player Rating", x = "Player Rating", y = "Number of records")

# Appendix K
corrdata = clean.player[,!c('preferred_foot')]
corrdata <- corrdata %>% mutate_if(is.integer,as.numeric)
corr <- round(cor(corrdata), 2)

ggcorrplot(corr, sig.level=0.05, lab_size = 2, p.mat = NULL,
           insig = c("pch", "blank"), pch = 1, pch.col = "black", pch.cex =1,
           tl.cex = 8) +
  theme(axis.text.x = element_text(margin=margin(-2,0,0,0)),
        axis.text.y = element_text(margin=margin(0,-2,0,0)),
        panel.grid.minor = element_line(size=5)) + 
  geom_tile(fill="white") +
  geom_tile(height=0.8, width=0.8)

corr[4,]

# Appendix L1
ggplot(clean.player, aes(reactions, overall_rating)) + 
  geom_point() +
  geom_smooth(method="lm", se=F) +
  labs(title="Reactions and Player Ratings", x = "Reactions", y = "Player Rating")

# Appendix L2
ggplot(clean.player, aes(long_passing, overall_rating)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess", se=F) +
  labs(title="Long Passing Score and Player Ratings", x = "Long Passing Score", y = "Player Rating")

# Appendix L3
ggplot(clean.player, aes(vision, overall_rating)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess", se=F) +
  labs(title="Vision and Player Ratings", x = "Vision", y = "Player Rating")
