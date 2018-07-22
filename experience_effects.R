options(scipen=999)
library(data.table)
dt <- data.table(read.csv('free_throws.csv'))

dt<-dt[,season_year:=as.numeric(sapply(strsplit(as.character(season),' - '),'[',1))]
dt<-dt[,prev_season_year:=season_year-1]
dt<-dt[,shot_index:=cumsum(shot_made + (1-shot_made)), by = .(game_id, player)]
dt<-dt[,made_index:=cumsum(shot_made), by = .(game_id, player)]
dt<-dt[,miss_index:=cumsum(1-shot_made), by = .(game_id, player)]
dt<-dt[,prev_throw:=shift(shot_made, 1, type = 'lag'), by = .(game_id, player)]
dt<-dt[seconds_left <= 30 & score_difference >= -3 & score_difference <= 3, pressure_shot_index:=cumsum(shot_made + (1-shot_made)), by = .(game_id, player)]
dt<-dt[seconds_left <= 30 & score_difference >= -3 & score_difference <= 3, pressure_made_shots:=shift(cumsum(shot_made), 1, 'lag'), by = .(game_id, player)]
dt<-dt[seconds_left <= 30 & score_difference >= -3 & score_difference <= 3, pressure_miss_shots:=shift(cumsum(1-shot_made), 1, 'lag'), by = .(game_id, player)]
dt<-dt[seconds_left <= 30 & score_difference >= -3 & score_difference <= 3, pressure_prev_throw:=shift(shot_made, 1, type = 'lag'), by = .(game_id, player)]
dt$exp_years_factor<-cut(dt$exp_years, breaks=c(1:11,20), right=FALSE)
dt$age_factor<-cut(dt$Age, breaks=c(20:35,50), right=FALSE)

stats_dt <- data.table(read.csv('player_stats.csv'))
stats_dt <- stats_dt[,.(player=Player,season_year=SeasonYear,FT,FTA,FTPerc=FT.)]
dt <- merge(dt,stats_dt,by.x=c('player','prev_season_year'),by.y=c('player','season_year'),all=FALSE)
dt <- na.omit(dt)

run_binom_test_on_stats_dt <- function(data_table, column, labels, x_label, y_limit)
{
    if(missing(y_limit)) {
        y_limit <- c(-0.06,0.01)
    }
    perf_diff<-data_table[,binom.test(x=sum(shot_made), n=length(shot_made), p=mean(FTPerc), alternative='less')[c('p.value','estimate','null.value')],by=column]
    setorderv(perf_diff, c(column))
    means<-perf_diff[,estimate-null.value]
    pvalues<-perf_diff[,'p.value']
    colors=rep('red',length(means))
    colors[pvalues > 0.05] <- 'gray'
    dev.new()
    barplot(means,col=colors, axes=FALSE, xlab=x_label, ylab='FT% difference', ylim=y_limit, names.arg=labels)
    axis(2, at=seq(y_limit[1],y_limit[2],by=0.01), labels=seq(y_limit[1]*100,y_limit[2]*100,1))
    legend('bottom', legend=c('p<0.05','p>0.05'),col=c('red','gray'), pch=15)
}

run_binom_test_text_output_dt <- function(data_table, column)
{
    perf_diff<-data_table[,binom.test(x=sum(shot_made), n=length(shot_made), p=mean(FTPerc), alternative='less')[c('p.value','estimate','null.value')],by=column]
    perf_diff[,avg_diff:=estimate-null.value]
    setorderv(perf_diff, c(column))
    print(perf_diff)
}

par(mfrow=c(1,1))

# general experience

dt$exp_years_factor<-cut(dt$exp_years, breaks=c(1:11,20), right=FALSE)
dt$age_factor<-cut(dt$Age, breaks=c(20:35,50), right=FALSE)
high_pressure <- dt[dt$seconds_left<=30 & dt$score_difference>=-3 & dt$score_difference<=3,]

run_binom_test_on_stats_dt(high_pressure, 'exp_years_factor', c(as.character(c(1:10)), '11+'), 'Seasons in NBA')
run_binom_test_on_stats_dt(high_pressure, 'age_factor', c(as.character(c(20:34)), '35+'), 'Age')
run_binom_test_on_stats_dt(high_pressure, 'playoffs', c('Regular Season', 'Playoffs'), 'Game Importance')

# long-term experience

high_pressure <- dt[dt$seconds_left<=30 & dt$score_difference>=-3 & dt$score_difference<=3,]
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
run_binom_test_on_stats(high_pressure, 'pressure_attempts_made_factor', c('[0-5)','[5-10)','[10-15)','15+'), 'Successful Attempts under Pressure in previous season')
run_binom_test_on_stats(high_pressure, 'pressure_attempts_miss_factor', c('0','1','2','3','4','5+'), 'Missed Attempts under Pressure in previous season')

# short-term experience

dt<-dt[,season_year:=as.numeric(sapply(strsplit(as.character(season),' - '),'[',1))]
dt<-dt[,prev_season_year:=season_year-1]
dt<-dt[, shot_index:=cumsum(shot_made + (1-shot_made)), by = .(game_id, player)]
dt<-dt[, made_index:=cumsum(shot_made), by = .(game_id, player)]
dt<-dt[, miss_index:=cumsum(1-shot_made), by = .(game_id, player)]
dt<-dt[, prev_throw:=shift(shot_made, 1, type = 'lag'), by = .(game_id, player)]

dt<-dt[seconds_left <= 30 & score_difference >= -3 & score_difference <= 3, pressure_shot_index:=cumsum(shot_made + (1-shot_made)), by = .(game_id, player)]
dt<-dt[seconds_left <= 30 & score_difference >= -3 & score_difference <= 3, pressure_made_shots:=shift(cumsum(shot_made), 1, 'lag'), by = .(game_id, player)]
dt<-dt[seconds_left <= 30 & score_difference >= -3 & score_difference <= 3, pressure_miss_shots:=shift(cumsum(1-shot_made), 1, 'lag'), by = .(game_id, player)]
dt<-dt[seconds_left <= 30 & score_difference >= -3 & score_difference <= 3, pressure_prev_throw:=shift(shot_made, 1, type = 'lag'), by = .(game_id, player)]

dt_high_pressure <- dt_merged[!is.na(prev_throw) & !is.na(FTPerc) & score_difference >= -3 & score_difference<=3 & seconds_left<=30]
dt_high_pressure[,pressure_shot_index_factor:=cut(pressure_shot_index, breaks=c(1,2,3,10), right=FALSE)]
dt_high_pressure[,pressure_miss_shots_factor:=cut(pressure_miss_shots, breaks=c(0,1,2,10), right=FALSE)]
dt_high_pressure$pressure_miss_shots_factor[is.na(dt_high_pressure$pressure_miss_shots_factor)]<-"[0,1)"
dt_high_pressure[,pressure_made_shots_factor:=cut(pressure_made_shots, breaks=c(0,1,2,10), right=FALSE)]
dt_high_pressure$pressure_made_shots_factor[is.na(dt_high_pressure$pressure_made_shots_factor)]<-"[0,1)"
run_binom_test_on_stats_dt(dt_high_pressure,'pressure_shot_index_factor',c('0','1','2+'),'Number of Previous Shots under Pressure (per-Game)')
run_binom_test_on_stats_dt(dt_high_pressure,'pressure_miss_shots_factor',c('0','1','2+'),'Number of Previous Missed Shots under Pressure (per-Game)')
run_binom_test_on_stats_dt(dt_high_pressure,'pressure_made_shots_factor',c('0','1','2+'),'Number of Previous Successful Shots under Pressure (per-Game)')
run_binom_test_text_output_dt(dt_high_pressure, 'pressure_shot_index_factor')
run_binom_test_text_output_dt(dt_high_pressure, 'pressure_miss_shots_factor')
run_binom_test_text_output_dt(dt_high_pressure, 'pressure_made_shots_factor')

numeric_cols <- which(sapply(dt_merged, is.numeric))
write.csv(cor(na.omit(dt_merged[, numeric_cols, with=FALSE])),'correlation.csv')
