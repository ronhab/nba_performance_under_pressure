# <table class="row_summable sortable stats_table now_sortable" id="per_game" data-cols-to-freeze="1">
# <tr id="per_game.2014" class="full_table" data-row="8">
# https://www.basketball-reference.com/search/search.fcgi?hint=Leandro+Barbosa&search=Leandro+Barbosa&pid=&idx=

from time import sleep
from random import randint
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions import NoSuchElementException
import os

class PlayerScraper():
    stats_names = ['Season','Age','Tm','Lg','Pos','G','GS','MP','FG','FGA','FG%','3P','3PA','3P%','2P','2PA','2P%','eFG%','FT','FTA','FT%','ORB','DRB','TRB','AST','STL','BLK','TOV','PF','PTS']

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
        too_many_results = True
        try:
            self.driver.find_element_by_class_name('search-results')
        except NoSuchElementException:
            too_many_results = False
        if too_many_results:
            search_results_name = self.driver.find_elements_by_class_name('search-item-name')
            search_results_url = self.driver.find_elements_by_class_name('search-item-url')
            player_desc = search_results_name[0].text.replace('\n','\t')
            player_url = 'https://www.basketball-reference.com' + search_results_url[0].text
            print('{0} {1}'.format(player_desc, player_url))
            self.get_page(player_url)
        sessions = self.driver.find_element_by_id('totals').find_elements_by_class_name('full_table')
        all_stats = []
        last_session = 0
        acc_active_sessions = 0
        for session in sessions:
            stats = session.text.split(' ')
            session_stats = {}
            for i in range(len(stats)):
                session_stats[self.stats_names[i]] = stats[i]
            session_stats['SeasonYear'] = int(session_stats['Season'].split('-')[0])
            session_stats['AccActiveSeasons'] = acc_active_sessions
            if session_stats['SeasonYear'] <= last_session:
                raise Exception('Current season number ({0}) is not greater than previous ({1})'.format(session_stats['SeasonYear'], last_session))
            last_session = session_stats['SeasonYear']
            acc_active_sessions += 1
            all_stats.append(session_stats)
        return all_stats
    
scraper = PlayerScraper()
scraper.start_driver()
print(scraper.get_player_info('Michael Jordan'))
print(scraper.get_player_info('Omri Casspi'))
scraper.close_driver()