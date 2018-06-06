import pandas as pd
import numpy as np
import re

play_regex = re.compile('(.*) (misses|makes)')
df = pd.read_csv('free_throws.csv')
for index, row in df.iterrows():
    play = row['play']
    player = row['player']
    play_match = play_regex.match(play)
    if play_match == None:
        print(row)
        raise Exception()
    new_player = play_match.group(1)
    shot_made = 1 if play_match.group(2) == 'makes' else 0
    if new_player != player:
        df.loc[index, 'player'] = new_player
    if shot_made != row['shot_made']:
        print('ERROR - dropping the line!!! line {0}: {1}'.format(index, row['play']))
        df.drop(index, inplace=True)
df.to_csv('free_throws.csv', index=False)
