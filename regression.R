df <- read.csv('free_throws.csv')
df$last15 <- df$seconds_left <= 15
df$last15 <- as.factor(df$last15)
df$last30 <- df$seconds_left > 15 & df$seconds_left <= 30
df$last30 <- as.factor(df$last30)
df$last60 <- df$seconds_left > 30 & df$seconds_left <= 60
df$last60 <- as.factor(df$last60)
df$score_diff<-factor(df$score_difference,levels=c('bigDiff',c(-5:5)),ordered=FALSE)
df$score_diff[is.na(df$score_diff)]<-'bigDiff'
df$prev_season_year<-as.numeric(sapply(strsplit(as.character(df$season),' - '),'[',1)) - 1

stats_df <- read.csv('player_stats.csv')
stats_df <- stats_df[c('Player','SeasonYear','FT','FTA','FT.')]
df<-merge(df,stats_df,by.x=c('player','prev_season_year'),by.y=c('Player','SeasonYear'),all=FALSE)
df<-na.omit(df)

logit_model_last60 <- glm(shot_made ~ FT. + last60*score_diff, data = df, family = "binomial")
summary(logit_model_last60)

logit_model_last30 <- glm(shot_made ~ FT. + last30*score_diff, data = df, family = "binomial")
summary(logit_model_last30)
