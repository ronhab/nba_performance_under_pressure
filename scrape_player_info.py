# <table class="row_summable sortable stats_table now_sortable" id="per_game" data-cols-to-freeze="1">
# <tr id="per_game.2014" class="full_table" data-row="8">
# https://www.basketball-reference.com/search/search.fcgi?hint=Leandro+Barbosa&search=Leandro+Barbosa&pid=&idx=

from time import sleep
from random import randint
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions import NoSuchElementException
import pandas as pd
import os

class PlayerScraper():
    stats_names = ['Age','Tm','Lg','Pos','G','GS','MP','FG','FGA','FG%','3P','3PA','3P%','2P','2PA','2P%','eFG%','FT','FTA','FT%','ORB','DRB','TRB','AST','STL','BLK','TOV','PF','PTS']

    # Open headless chromedriver
    def start_driver(self):
        print('starting driver...')
        chrome_options = Options()
        chrome_options.add_argument("--headless")
        chrome_options.add_argument("--window-size=800x600")
        chrome_driver = os.getcwd() +"\\chromedriver.exe"
        self.driver = webdriver.Chrome(chrome_options=chrome_options, executable_path=chrome_driver, service_log_path='chrome_log.txt')
        sleep(4)

        # Close chromedriver
    def close_driver(self):
        print('closing driver...')
        self.driver.quit()
        print('closed!')

    # Tell the browser to get a page
    def get_page(self, url):
        print('getting page {0}...'.format(url))
        self.driver.get(url)
        sleep(randint(2,3))

    def get_player_info(self, player_name):
        player_name_encoded = player_name.replace(' ', '+')
        url = 'https://www.basketball-reference.com/search/search.fcgi?hint={0}&search={0}&pid=&idx='.format(player_name_encoded)
        print('url={0}'.format(url))
        self.get_page(url)
        if 'Search Results' in self.driver.title:
            if 'Found 0 hits that match your search' in self.driver.find_element_by_id('content').text:
                player_name_tokens = player_name.split(' ')
                if len(player_name_tokens) > 2:
                    player_name = ' '.join(player_name_tokens[:-1])
                    return self.get_player_info(player_name)
                else:
                    return None
            else:
                self.driver.find_element_by_class_name('search-results')
                search_results_name = self.driver.find_elements_by_class_name('search-item-name')
                search_results_url = self.driver.find_elements_by_class_name('search-item-url')
                player_desc = search_results_name[0].text.replace('\n','\t')
                player_url = 'https://www.basketball-reference.com' + search_results_url[0].text
                print('{0} {1}'.format(player_desc, player_url))
                self.get_page(player_url)
        if 'https://www.basketball-reference.com/players' not in self.driver.current_url:
            player_url = self.driver.find_element_by_link_text('Full Record').get_attribute('href')
            self.get_page(player_url)
        seasons = self.driver.find_element_by_id('totals').find_elements_by_class_name('full_table')
        all_stats = []
        last_season = 0
        acc_active_seasons = 0
        for season in seasons:
            season_name = season.find_element_by_tag_name('th').text
            stats = season.find_elements_by_tag_name('td')
            if len(stats) != len(self.stats_names):
                raise Exception('player {0} season {1}'.format(player_name, season_name))
            season_stats = {}
            season_stats['Season'] = season_name
            for i in range(len(stats)):
                stat_text = stats[i].text
                if len(stat_text) > 0 and stat_text[0] == '.':
                    stat_text = '0' + stat_text
                season_stats[self.stats_names[i]] = stat_text
            season_stats['Player'] = player_name
            season_stats['SeasonYear'] = int(season_name.split('-')[0])
            season_stats['AccActiveSeasons'] = acc_active_seasons
            if season_stats['SeasonYear'] <= last_season:
                raise Exception('Current season number ({0}) is not greater than previous ({1})'.format(season_stats['SeasonYear'], last_season))
            last_season = season_stats['SeasonYear']
            acc_active_seasons += 1
            all_stats.append(season_stats)
        return all_stats

ft_dataset = pd.read_csv('free_throws.csv')
all_players = set(ft_dataset.player.unique())
start_from_scratch = os.path.exists('player_stats.csv') == False
if not start_from_scratch:
    player_dataset = pd.read_csv('player_stats.csv')
    all_players.difference_update(set(player_dataset.Player.unique()))
print('total players to scrape: {0}'.format(len(all_players)))
scraper = PlayerScraper()
scraper.start_driver()
for player in all_players:
    player_stats = scraper.get_player_info(player)
    with open('player_stats.csv','a', encoding='utf-8') as player_stats_file:
        if start_from_scratch:
            header = ''
            for stat_name in player_stats[0].keys():
                header += '{0},'.format(stat_name)
            header = header[:-1]
            player_stats_file.write(header+'\n')
            start_from_scratch = False
        for season_stats in player_stats:
            line = ''
            for stat in season_stats.values():
                line += '{0},'.format(stat)
            line = line[:-1]
            player_stats_file.write(line+'\n')
    print('Player {0} - done!'.format(player))
scraper.close_driver()