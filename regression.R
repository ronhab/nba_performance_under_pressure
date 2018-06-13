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
colnames(stats_df)[colnames(stats_df)=='FT.'] <- 'FTPerc'
df<-merge(df,stats_df,by.x=c('player','prev_season_year'),by.y=c('Player','SeasonYear'),all=FALSE)
df<-na.omit(df)

under_pressure <- df[df$seconds_left <= 30 & df$score_difference >= -5 & df$score_difference <= 5,]
options(scipen=6)
choking <- by(under_pressure, under_pressure$score_diff, FUN = function(x) binom.test(x=sum(x$shot_made), n=length(x$shot_made), p=mean(x$FTPerc), alternative="less"))
choking_pvalue <- round(as.numeric(unlist(sapply(choking,'[','p.value'))),6)
choking_actual <- round(as.numeric(unlist(sapply(choking,'[','estimate'))),6)
choking_expected <- round(as.numeric(unlist(sapply(choking,'[','null.value'))),6)
choking_diff <- choking_actual - choking_expected
cbind(score_diff=c(-5:5),p.value=choking_pvalue,actual=choking_actual,expected=choking_expected,difference=choking_diff)

exceling <- by(under_pressure, under_pressure$score_diff, FUN = function(x) binom.test(x=sum(x$shot_made), n=length(x$shot_made), p=mean(x$FTPerc), alternative="greater"))
exceling_pvalue <- round(as.numeric(unlist(sapply(exceling,'[','p.value'))),6)
exceling_actual <- round(as.numeric(unlist(sapply(exceling,'[','estimate'))),6)
exceling_expected <- round(as.numeric(unlist(sapply(exceling,'[','null.value'))),6)
exceling_diff <- exceling_actual - exceling_expected
cbind(score_diff=c(-5:5),p.value=exceling_pvalue,actual=exceling_actual,expected=exceling_expected,difference=exceling_diff)

data_to_graph<-c(0,0,0,choking_diff[4],choking_diff[5],0,choking_diff[7],0,0,exceling_diff[10],exceling_diff[11])*100
barplot(data_to_graph, main="Performance under Pressure", names.arg=c(-5:5), xlab="score difference", ylab="Performance Difference (in %)", ylim=c(-10,5))

logit_model <- glm(shot_made ~ FTPerc + last30full*score_diff + exp_years, data = df, family = "binomial")
summary(logit_model)

# need to test for significancy of each regression block