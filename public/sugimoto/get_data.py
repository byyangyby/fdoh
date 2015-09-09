# -*- coding: utf-8 -*-
# <nbformat>3.0</nbformat>

# <codecell>

# Get today's date
import time
today = time.strftime("%Y-%m-%d")

from datetime import datetime, timedelta
yesterday = (datetime.today() - timedelta(days=1)).strftime("%b %d %Y")

# <codecell>

# Specify which directories we'll be using (different on Ben vs. Joe-linux vs. Joe-Windows)
import platform
private = '/home/joebrew/Documents/private/sugimoto'
public = '/home/joebrew/Documents/fdoh/public/sugimoto'
downloads = '/home/joebrew/Documents/private/sugimoto/downloads'

# <codecell>

# Import necessary libraries
import mechanize
import cookielib
from BeautifulSoup import BeautifulSoup
import html2text
import pandas as pd
import subprocess
import rpy2.robjects as robjects

# <codecell>

# Run the r script to get the dates and URLs for today's data download
import os
os.chdir(public)
robjects.r['source']("make_links.R")

# <codecell>

# Read in which links I need for today
import pandas as pd
os.chdir(public)
sugimoto_links = pd.read_csv('links_to_download.csv')

# <codecell>

# Browser
br = mechanize.Browser()

# Cookie Jar
cj = cookielib.LWPCookieJar()
br.set_cookiejar(cj)

# <codecell>

# Browser options
br.set_handle_equiv(True)
br.set_handle_gzip(True)
br.set_handle_redirect(True)
br.set_handle_referer(True)
br.set_handle_robots(False)
br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=1)

br.addheaders = [('User-agent', 'Chrome')]

# <codecell>

# The site we will navigate into, handling it's session
br.open('https://www.essencefl.com/florida_5_1_14/servlet/Login')
#br.open('https://github.com/login')

# View available forms
#for f in br.forms():
#    print f

# Select the second (index one) form (the first form is a search query box)
br.select_form(nr=0)

# <codecell>

# Read in credentials
os.chdir(private)
pu = pd.read_csv('pu.csv', dtype = 'string')

# <codecell>

u = list(pu['u'])[0]
p = list(pu['p'])[0]

# <codecell>

# User credentials
br.form['j_username'] = u
br.form['j_password'] = p

# Login
br.submit()

# <codecell>

# Set working directory to private downloads folder
os.chdir(downloads)

# <codecell>

nrow = sugimoto_links.shape[0]

# <codecell>

# Get and write gi data
for i in range(0,nrow,1):
    my_file = br.open(sugimoto_links[0:]['gi'][i])
    # Write a text file
    f = open('gi'+sugimoto_links[0:]['date'][i]+'.txt', 'w')
    f.write(my_file.read())
    f.close()

# <codecell>

# Get and write ili data
for i in range(0,nrow,1):
    my_file = br.open(sugimoto_links[0:]['ili'][i])
    # Write a text file
    f = open('ili'+sugimoto_links[0:]['date'][i]+'.txt', 'w')
    f.write(my_file.read())
    f.close()

# <codecell>

# Get and write injury data
for i in range(0,nrow,1):
    my_file = br.open(sugimoto_links[0:]['injury'][i])
    # Write a text file
    f = open('injury'+sugimoto_links[0:]['date'][i]+'.txt', 'w')
    f.write(my_file.read())
    f.close()

# <codecell>

# Get and write alless data
for i in range(0,nrow,1):
    my_file = br.open(sugimoto_links[0:]['alless'][i])
    # Write a text file
    f = open('alless'+sugimoto_links[0:]['date'][i]+'.txt', 'w')
    f.write(my_file.read())
    f.close()

# <codecell>


