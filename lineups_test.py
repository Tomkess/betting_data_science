# initial page:
# http://www.11v11.com/competitions/premier-league/2016/matches/
# 380 matches

from lxml import html
import requests
import json
import re
from time import gmtime, strftime
import urllib
from bs4 import BeautifulSoup
offset = 0

# specify the url
quote_page = "http://www.11v11.com/competitions/premier-league/2016/matches/"

# query the website and return the html to the variable ‘page’
page = urllib.urlopen(quote_page)

# parse the html using beautiful soup and store in variable `soup`
soup = BeautifulSoup(page, 'html.parser')

for a in soup.find_all('a', href=True):
    print(a['href'])
