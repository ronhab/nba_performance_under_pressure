import pandas as pd
import numpy as np

def save():
    df.to_csv('free_throws.csv', index=False, encoding='utf-8')

df = pd.read_csv('free_throws.csv', encoding='utf-8')
df['period'] = df['period'].astype(int)

# if 'prev_score' in df.columns:
#     df.drop('prev_score', axis=1, inplace=True)
#     save()

# def determining_seconds_left(row):
#     minutes_left_period = int(row.time.split(':')[0])
#     seconds_left_period = int(row.time.split(':')[1])
    
#     if row.period <= 4:
#         remaining_periods = 4 - row.period
#         remaining_seconds = remaining_periods * 12 * 60
#     else:
#         remaining_seconds = 0
    
#     seconds_left_total = minutes_left_period * 60 + seconds_left_period + remaining_seconds
    
#     return seconds_left_total

# if 'seconds_left' not in df.columns:
#     df['seconds_left'] = df.apply(determining_seconds_left, axis=1)
#     df['seconds_left'] = df['seconds_left'].astype(int)
#     save()

# map_team_names_to_symbols = {'UTAH': 'UTA', 'PHX': 'PHO', 'NJ': 'NJN', 'NO': 'NOH', 'NOK': 'NOH', 'NOP': 'NOH', 'GS': 'GSW', 'SA': 'SAS', 'NY': 'NYK', 'WSH': 'WAS'}

# def rename_match(row):
#     teams = row.game.split(' - ')
#     if teams[0] in map_team_names_to_symbols:
#         teams[0] = map_team_names_to_symbols[teams[0]]
#     if teams[1] in map_team_names_to_symbols:
#         teams[1] = map_team_names_to_symbols[teams[1]]
#     return '{0} - {1}'.format(teams[0], teams[1])

# def rename_team(row):
#     if row.team in map_team_names_to_symbols:
#         return map_team_names_to_symbols[row.team]
#     else:
#         return row.team

# df['game'] = df.apply(rename_match, axis=1)
# df['team'] = df.apply(rename_team, axis=1)
# save()

def find_score_difference(row):
    teams = row.game.split(' - ')
    scores = row.score.split(' - ')
    
    if row.team == teams[0]:
        own_score = int(scores[0])
        opponent_score = int(scores[1])
        
        if row.shot_made == 1:
            own_score = own_score - 1

    elif row.team == teams[1]:
        own_score = int(scores[1])
        opponent_score = int(scores[0])
        
        if row.shot_made == 1:
            own_score = own_score - 1
    else:
        print(row)
        raise Exception('problem!')
    
    return own_score - opponent_score

if 'score_difference' not in df.columns:
    df['score_difference'] = df.apply(find_score_difference, axis=1)
    df['score_difference'] = df['score_difference'].astype(int)
    save()