# -*- coding: utf-8 -*-
import urllib
import json
import re
import numpy as np
import pandas as pd

# your key word or key word variable goes into str()
# works for a single word but you can easily modify to work for a list of words

link='http://chronicle.nytlabs.com/?keyword='+str()

page = urllib.urlopen(link)
html=page.read()
years=re.findall("year\': (.*?),",html,re.MULTILINE)
years=[int(year) for year in years]
articles=re.findall("{\'article_matches\': (.*?),",html,re.MULTILINE)
articles=[int(article) for article in articles]
totals=re.findall("\'total_articles_published\': (.*?)}",html,re.MULTILINE)
totals=[int(total) for total in totals]

d={'Year':years,'NoArticles':articles,'TotalArticles':totals}
df = pd.DataFrame(data=d)

df.to_csv('/Users/alphan/Desktop/nytimes.csv')
