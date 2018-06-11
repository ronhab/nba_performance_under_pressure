import pandas as pd
import numpy as np

def save():
    new_df.to_csv('free_throws.csv', index=False, encoding='utf-8')

df = pd.read_csv('free_throws.csv', encoding='utf-8')
players = pd.read_csv('player_stats.csv', encoding='utf-8')

def get_season_first_year(row):
    return int(row.season.split(' - ')[0])

df['season_first_year'] = df.apply(get_season_first_year, axis=1)

new_df = pd.merge(df, players,  how='left', left_on=['player','season_first_year'], right_on = ['Player','SeasonYear'])
new_df.drop(['season_first_year', 'Season', 'Tm', 'Lg', 'Pos', 'G', 'GS', 'MP', 'FG', 'FGA', 'FG%', '3P', '3PA', '3P%', '2P', '2PA', '2P%', 'eFG%', 'FT', 'FTA', 'FT%', 'ORB', 'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS', 'Player', 'SeasonYear'], axis=1, inplace=True)
new_df.rename(index=str, columns={'PlayerUrl': 'player_url', 'AccActiveSeasons': 'exp_years'}, inplace=True)
save()
