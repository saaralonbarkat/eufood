import re
import requests
import requests.exceptions
from urllib.parse import urlsplit
from collections import deque
from bs4 import BeautifulSoup
import pandas as pd
import time

def websiteScrapper(url_of_interest, depth_from_site):
    # starting url.
    starting_url = url_of_interest
    if starting_url.endswith('/'):
        starting_url = starting_url[:-1]

    starting_base_url = "{0.scheme}://{0.netloc}".format(urlsplit(starting_url))

    unprocessed_urls = deque([starting_url])
    processed_urls = set()

    emails = set()
    email_dict = dict()

    counter = 0
    depth = depth_from_site

    # process urls one by one from unprocessed_url queue until queue is empty
    while len(unprocessed_urls) and counter < depth:

        # move next url from the queue to the set of processed urls
        url = unprocessed_urls.popleft()
        processed_urls.add(url)

        # extract base url to resolve relative links
        parts = urlsplit(url)
        base_url = "{0.scheme}://{0.netloc}".format(parts)
        if base_url[-1] == '.':
            base_url = base_url[:-1]
        path = url[:url.rfind('/')+1] if '/' in parts.path else url

        counter = checkBase(base_url, starting_base_url, counter)

        # get url's content
        print("Crawling URL %s" % url)
        time.sleep(1)
        try:
            response = requests.get(url)
        except (requests.exceptions.MissingSchema, requests.exceptions.ConnectionError):
            print('error accessing page')
            continue
        except:
            print('final error')
            continue

        # get email addresses
        new_emails = set(re.findall(r"([a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+)", response.text, re.I))
        emails.update(new_emails)
        for e in new_emails:
            if not re.search('.*\.jpg',e) or not re.search('.*\.png',e):
                if e not in email_dict:
                    email_dict[e] = set()
                email_dict[e].add(url)

        # create a beutiful soup for the html document
        soup = BeautifulSoup(response.text, 'lxml')

        # Once this document is parsed and processed, now find and process all the anchors i.e. linked urls in this document
        for anchor in soup.find_all("a"):
            # extract link url from the anchor
            link = anchor.attrs["href"] if "href" in anchor.attrs else ''
            # resolve relative links (starting with /)
            if link.startswith('/'):
                link = base_url + link
            elif not link.startswith('http'):
                link = path + link
            # add the new url to the queue if it was not in unprocessed list nor in processed list yet
            if not link in unprocessed_urls and not link in processed_urls and 'twitter' not in link and 'facebook' not in link and '.pdf' not in link and 'flickr' not in link and 'youtube' not in link and 'vimeo' not in link and 'plus.google' not in link:
                unprocessed_urls.append(link)
    print('Url crawled')
    return email_dict

def checkBase(base_url, starting_base_url, counter):
    mod_base_url = base_url
    mod_starting_base_url = starting_base_url
    if 'http:' in base_url:
        mod_base_url = re.sub('http://','',mod_base_url)
    if 'https:' in base_url:
        mod_base_url = re.sub('https://','',mod_base_url)
    if 'http:' in starting_base_url:
        mod_starting_base_url = re.sub('http://','',mod_starting_base_url)
    if 'https:' in starting_base_url:
        mod_starting_base_url = re.sub('https://','',mod_starting_base_url)
    if mod_base_url != mod_starting_base_url:
        counter += 1
    else:
        counter = 0
    return counter

def runEmailCollector(url_list, depth_from_website = 10):
    collected_emails = dict()
    for url in url_list:
        email_dictionary = websiteScrapper(url, depth_from_website)
        email_dataframe = pd.DataFrame.from_dict(email_dictionary, orient='index')
        if 'http:' in url:
            url = re.sub('http://','',url)
        if 'https:' in url:
            url = re.sub('https://','',url)
        url = re.sub('\.','-',url)
        if url.endswith('/'):
            url = url[:-1]
        filename = str(url)+'_email_addresses.csv'
        email_dataframe.to_csv(filename, sep=",")
        collected_emails.update(email_dictionary)
    print('All Done!')