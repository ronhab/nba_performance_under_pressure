df <- read.csv('free_throws.csv')
df$last15 <- df$seconds_left <= 15
df$last30 <- df$seconds_left > 15 & df$seconds_left <= 30
df$last30full <- df$seconds_left <= 30
df$last45 <- df$seconds_left > 30 & df$seconds_left <= 45
df$last60 <- df$seconds_left > 45 & df$seconds_left <= 60
df$score_diff<-factor(df$score_difference,levels=c('bigDiff',c(-5:5)),ordered=FALSE)
df$score_diff[is.na(df$score_diff)]<-'bigDiff'
df$season_year<-as.numeric(sapply(strsplit(as.character(df$season),' - '),'[',1))
df$prev_season_year<-df$season_year - 1

stats_df <- read.csv('player_stats.csv')
stats_df <- stats_df[c('Player','SeasonYear','FT','FTA','FT.')]
colnames(stats_df)[colnames(stats_df)=='FT.'] <- 'FTPerc'
df<-merge(df,stats_df,by.x=c('player','prev_season_year'),by.y=c('Player','SeasonYear'),all=FALSE)
df<-na.omit(df)

performance_diff_summary <- function(under_pressure)
{
    choking <- by(under_pressure, under_pressure$score_diff, FUN = function(x) binom.test(x=sum(x$shot_made), n=length(x$shot_made), p=mean(x$FTPerc), alternative="less"))
    choking_pvalue <- round(as.numeric(unlist(sapply(choking,'[','p.value'))),6)
    choking_actual <- round(as.numeric(unlist(sapply(choking,'[','estimate'))),6)
    choking_expected <- round(as.numeric(unlist(sapply(choking,'[','null.value'))),6)
    choking_diff <- choking_actual - choking_expected
    choking_summary <- cbind(score_diff=c(-5:5),p.value=choking_pvalue,actual=choking_actual,expected=choking_expected,difference=choking_diff)

    exceling <- by(under_pressure, under_pressure$score_diff, FUN = function(x) binom.test(x=sum(x$shot_made), n=length(x$shot_made), p=mean(x$FTPerc), alternative="greater"))
    exceling_pvalue <- round(as.numeric(unlist(sapply(exceling,'[','p.value'))),6)
    exceling_actual <- round(as.numeric(unlist(sapply(exceling,'[','estimate'))),6)
    exceling_expected <- round(as.numeric(unlist(sapply(exceling,'[','null.value'))),6)
    exceling_diff <- exceling_actual - exceling_expected
    exceling_summary <- cbind(score_diff=c(-5:5),p.value=exceling_pvalue,actual=exceling_actual,expected=exceling_expected,difference=exceling_diff)
    full_summary <- merge(data.frame(choking_summary),data.frame(exceling_summary),by=c('score_diff','actual','expected','difference'),sort=FALSE,suffixes=c('.choking','.exceling'))
    full_summary['effect'] <- ifelse(full_summary['p.value.choking'] < full_summary['p.value.exceling'],'Choking','Exceling')
    full_summary['p.value'] <- as.numeric(apply(full_summary,1,FUN=function(x) min(cbind(x['p.value.choking'],x['p.value.exceling']))))
    full_summary['Sig. Level']<-cut(full_summary$p.value, breaks=c(-0.0001,0.001,0.01,0.05,0.1,1), labels=c('***','**','*','.',' '))
    full_summary[,-which(names(full_summary) %in% c("p.value.choking","p.value.exceling"))]
}

perf60 <- performance_diff_summary(df[df$last60 & df$score_difference >= -5 & df$score_difference <= 5,])
perf45 <- performance_diff_summary(df[df$last45 & df$score_difference >= -5 & df$score_difference <= 5,])
perf30 <- performance_diff_summary(df[df$last30 & df$score_difference >= -5 & df$score_difference <= 5,])
perf15 <- performance_diff_summary(df[df$last15 & df$score_difference >= -5 & df$score_difference <= 5,])

y_data = cbind(perf60$difference,perf45$difference,perf30$difference,perf15$difference)
significancy = cbind(perf60['Sig. Level'],perf45['Sig. Level'],perf30['Sig. Level'],perf15['Sig. Level'])
par(mfrow=c(3,3))
for (i in c(3:11))
{
    x_data = c(-60, -45, -30, -15)
    colors = as.character(significancy[i,])
    colors[colors=='1'] <- 'red'
    colors[colors=='2'] <- 'orange'
    colors[colors=='3'] <- 'yellow'
    colors[colors=='4'] <- 'white'
    colors[colors=='5'] <- 'white'
    plot(x_data, y_data[i,],axes=FALSE,xlim=c(-60, -15),ylim=c(-0.11,0.11),xlab='Seconds Left',ylab='FT% difference',main=paste('Score Difference=',as.character(perf60$score_diff[i])),pch=19,col=colors)
    axis(1, at=c(-60, -45, -30, -15), labels=c('60','45','30','15'))
    axis(2, at=seq(-0.1,0.1,by=0.01), labels=seq(-10,10,1))
    legend('topright',legend=c('p<0.001','p<0.01','p<0.05'),pch=19,col=c('red','orange','yellow'),cex=0.9,pt.cex=0.9, bty='n')
    lines(x_data, y_data[i,])
}

par(mfrow=c(1,1))

run_binom_test_on_stats <- function(data, column, labels, x_label)
{
    perf_diff<-by(data, data[column], FUN = function(x) binom.test(x=sum(x$shot_made), n=length(x$shot_made), p=mean(x$FTPerc), alternative='less'))
    means<-as.numeric(sapply(perf_diff,'[','estimate'))-as.numeric(sapply(perf_diff,'[','null.value'))
    pvalues<-as.numeric(sapply(perf_diff,'[','p.value'))
    colors=rep('red',length(means))
    colors[pvalues > 0.05] <- 'gray'
    dev.new()
    barplot(means,col=colors, axes=FALSE, xlab=x_label, ylab='FT% difference', ylim=c(-0.06,0.01), names.arg=labels)
    axis(2, at=seq(-0.06,0.01,by=0.01), labels=seq(-6,1,1))
    legend('bottom', legend=c('p<0.05','p>0.05'),col=c('red','gray'), pch=15)
}

df$exp_years_factor<-cut(df$exp_years, breaks=c(1:11,20), right=FALSE)
high_pressure <- df[df$seconds_left<=30 & df$score_difference>=-3 & df$score_difference<=3,]

run_binom_test_on_stats(high_pressure, 'exp_years_factor', c(as.character(c(1:10)), '11+'), 'Seasons in NBA')

high_pressure <- df[df$seconds_left<=30 & df$score_difference>=-3 & df$score_difference<=3,]
pressure_shots<-aggregate(shot_made ~ player + season_year, data=high_pressure, FUN=function(x) attempts=length(x))
high_pressure<-merge(high_pressure,pressure_shots,by.x=c('player','prev_season_year'),by.y=c('player','season_year'),all=FALSE)
colnames(high_pressure)[colnames(high_pressure)=="shot_made.x"] <- "shot_made"
colnames(high_pressure)[colnames(high_pressure)=="shot_made.y"] <- "pressure_attempts"
high_pressure$pressure_attempts[is.na(high_pressure$pressure_attempts)]<-0
high_pressure$pressure_attempts_factor<-cut(high_pressure$pressure_attempts, breaks=c(0,5,10,15,20,40), right=FALSE)
pressure_shots_made<-aggregate(shot_made ~ player + season_year, data=high_pressure, FUN=function(x) attempts=sum(x))
high_pressure<-merge(high_pressure,pressure_shots_made,by.x=c('player','prev_season_year'),by.y=c('player','season_year'),all=FALSE)
colnames(high_pressure)[colnames(high_pressure)=="shot_made.x"] <- "shot_made"
colnames(high_pressure)[colnames(high_pressure)=="shot_made.y"] <- "pressure_attempts_made"
high_pressure$pressure_attempts_made[is.na(high_pressure$pressure_attempts_made)]<-0
high_pressure$pressure_attempts_made_factor<-cut(high_pressure$pressure_attempts_made, breaks=c(0,5,10,15,40), right=FALSE)
high_pressure$pressure_attempts_miss<-high_pressure$pressure_attempts - high_pressure$pressure_attempts_made
high_pressure$pressure_attempts_miss_factor<-cut(high_pressure$pressure_attempts_miss, breaks=c(0,1,2,3,4,5,10), right=FALSE)
high_pressure$pressure_attempts_perc<-high_pressure$pressure_attempts_made / high_pressure$pressure_attempts

run_binom_test_on_stats(high_pressure, 'pressure_attempts_factor', c('[0-5)','[5-10)','[10-15)','[15-20)','20+'), 'Attempts under Pressure in previous season')

library(data.table)
dt = data.table(read.csv('free_throws.csv'))
dt<-dt[, shot_index:=cumsum(shot_made + (1-shot_made)), by = .(game_id, player)]
dt<-dt[, made_index:=cumsum(shot_made), by = .(game_id, player)]
dt<-dt[, miss_index:=cumsum(1-shot_made), by = .(game_id, player)]
dt<-dt[, prev_throw:=shift(shot_made, 1, type = 'lag'), by = .(game_id, player)]

dt<-dt[seconds_left <= 30 & score_difference >= -3 & score_difference <= 3, pressure_shot_index:=cumsum(shot_made + (1-shot_made)), by = .(game_id, player)]
dt<-dt[seconds_left <= 30 & score_difference >= -3 & score_difference <= 3, pressure_made_index:=cumsum(shot_made), by = .(game_id, player)]
dt<-dt[seconds_left <= 30 & score_difference >= -3 & score_difference <= 3, pressure_miss_index:=cumsum(1-shot_made), by = .(game_id, player)]
dt<-dt[seconds_left <= 30 & score_difference >= -3 & score_difference <= 3, pressure_prev_throw:=shift(shot_made, 1, type = 'lag'), by = .(game_id, player)]

dt_merged<-merge(dt[prev_throw==0,mean(shot_made),by=player],dt[prev_throw==1,mean(shot_made),by=player],by = "player")
dt_merged[,avg_diff:=V1.y-V1.x]
mean(dt_merged[,avg_diff])

dt_merged<-merge(dt[pressure_prev_throw==0,mean(shot_made),by=player],dt[pressure_prev_throw==1,mean(shot_made),by=player],by = "player")
dt_merged[,avg_diff:=V1.y-V1.x]
mean(dt_merged[,avg_diff])