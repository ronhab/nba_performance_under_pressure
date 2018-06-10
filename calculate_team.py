import numpy as np
import pandas as pd
import requests
from bs4 import BeautifulSoup

ft_df = pd.read_csv('free_throws.csv', encoding='utf-8')
player_df = pd.read_csv('player_stats.csv', encoding='utf-8')

ft_df['game_id'] = ft_df['game_id'].astype(int)

ft_df['prev_score'] = ft_df.score.shift()

def who_shot(row):
    if 'makes free throw 2 of 2' in row.play or 'makes free throw 3 of 3' in row.play:
        if row.score.split(' - ')[0] != row.prev_score.split(' - ')[0]:
            return row.game.split(' - ')[0]
        else:
            return row.game.split(' - ')[1]
    else:
        return np.nan

ft_df['team'] = ft_df.apply(who_shot, axis=1)
print('percentage of teams determined: {0}'.format(len(ft_df[ft_df.team.notnull()]) / len(ft_df)))

players = {}

for index, row in ft_df[ft_df.team.notnull()].iterrows():
    try:
        players[row.player][row.game_id] = row.team
    except KeyError:
        players[row.player] = { row.game_id: row.team }

def get_player_team(player, game_id, game):
    try:
        return players[player][game_id]
    except KeyError:
        response = requests.get('http://www.espn.com/nba/boxscore?gameId={0}'.format(game_id))
        response.raise_for_status()
        soup = BeautifulSoup(response.content,'lxml')
        away_team_text = soup.find(class_='gamepackage-away-wrap').get_text()
        home_team_text = soup.find(class_='gamepackage-home-wrap').get_text()
        player_tokens = player.split(' ')
        player_last_name = player_tokens[-1].title()
        player_first_name = player_tokens[0].title()
        player_search_names = []
        player_search_names.append('{0}. {1}'.format(player_first_name[0], player_last_name))
        player_search_names.append('{0}'.format(player_last_name))
        team = 'TOT'
        for search_string in player_search_names:
            if (search_string in away_team_text) and (not search_string in home_team_text):
                team = game.split(' - ')[0]
                break
            elif (search_string not in away_team_text) and (search_string in home_team_text):
                team = game.split(' - ')[1]
                break
        if team == 'TOT':
            away_team_text = away_team_text.lower()
            home_team_text = home_team_text.lower()
            if (player_last_name.lower() in away_team_text) and (not player_last_name.lower() in home_team_text):
                team = game.split(' - ')[0]
            elif (player_last_name.lower() not in away_team_text) and (player_last_name.lower() in home_team_text):
                team = game.split(' - ')[1]
        try:
            players[player][game_id] = team
        except KeyError:
            players[player] = { game_id: team }
        print('scraping player {0} for game_id {1} returned {2}'.format(player, game_id, team))
        return team

def who_shot2(row):
    try:
        return players[row.player][row.game_id]
    except KeyError:
        season = int(row.season.split(' - ')[0])
        team = player_df[(player_df['Player'] == row.player) & (player_df['SeasonYear'] == season)].iloc[0].Tm
        if team == 'TOT':
            return get_player_team(row.player, row.game_id, row.game)
        else:
            try:
                players[row.player][row.game_id] = team
            except KeyError:
                players[row.player] = { row.game_id: team }
        return team

ft_df['team'] = ft_df.apply(who_shot2, axis=1)

print('percentage of teams determined: {0}'.format(len(ft_df[ft_df.team.notnull()]) / len(ft_df)))
print('percentage of TOT: {0}'.format(len(ft_df[ft_df['team'] == 'TOT']) / len(ft_df)))

ft_df.to_csv('free_throws.csv', index=False, encoding='utf-8')
