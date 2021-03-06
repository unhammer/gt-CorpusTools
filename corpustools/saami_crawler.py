# -*- coding:utf-8 -*-

#
#   This file contains routines to crawl sites containing saami text
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this file. If not, see <http://www.gnu.org/licenses/>.
#
#   Copyright 2013-2014 Børre Gaup <borre.gaup@uit.no>
#
import os
import sys
import re
import argparse

import requests
import lxml.html

import argparse_version
import namechanger


class SamediggiFiCrawler(object):
    '''Notes about samediggi.fi

    Start page is:
    http://www.samediggi.fi/index.php?option=com_frontpage&Itemid=39


    Empty pages contain either
    * "Käännöstä ei ole saatavilla"
    * "There are no translations available"

    Follow links starting with:
    * http://www.samediggi.fi/index (www.samediggi.fi == samediggi.fi)
    * index: prepend these adresses with http://www.samediggi.fi

    Remove any &lang=* parts from links,
    then re-add languages
    * &lang=finnish
    * &lang=davvi
    * &lang=anaras
    * &lang=nuortta
    * &lang=english

    Main content in div id="keski_mainbody"
    Content parts in table class="contentpaneopen"

    www.samediggi.fi/nuorat is a separate "domain"
    Links start with:
    * http://www.samediggi.fi/nuorat
    * nuorat/index

    languages
    * &lang=fi
    * &lang=sme
    * &lang=smn
    * &lang=sms

    Same procedure with links here
    '''
    def __init__(self):
        self.unvisited_links = set()
        self.unvisited_links.add(
            'http://www.samediggi.fi/')
        self.visited_links = set()
        self.download_links = set()
        self.langs = {u'finnish': u'fin',
                      u'davvi': u'sme',
                      u'anaras': u'smn',
                      u'nuortta': u'sms',
                      u'english': u'eng'}

    def download_pages(self):
        while len(self.unvisited_links) > 0:
            link = self.unvisited_links.pop()
            if link not in self.visited_links:

                print >>sys.stderr
                print >>sys.stderr, 'Before: unvisited_links',
                print >>sys.stderr, len(self.unvisited_links)

                try:
                    pages = []
                    found_saami = False
                    for lang in self.langs.keys():
                        r = requests.get(link, params={'lang': lang})
                        if len(r.history) > 0:
                            print 'history', r.history
                        if 'samediggi.fi' not in r.url:
                            print 'url', r.url

                        if ('www.samediggi.fi' in r.url and
                                r.status_code == requests.codes.ok and
                                not self.invalid_content(r.content)):
                            if lang in ['davvi', 'anaras', 'nuortta']:
                                found_saami = True
                            self.harvest_links(r.content)
                            pages.append(namechanger.AddFileToCorpus(
                                r.url,
                                os.getenv('GTFREE'),
                                self.langs[lang],
                                'admin/sd/www.samediggi.fi'))
                        else:
                            if 'samediggi.fi' not in r.url:
                                print >>sys.stderr, (
                                    '\nNot fetching {} which was {}\n'.format(
                                        r.url, link))

                    if found_saami:
                        self.save_pages(pages)
                except UserWarning:
                    print >>sys.stderr, '{} does not exist'.format(link)

                print >>sys.stderr, ('After: unvisited_links {}\n'.format(
                    self.unvisited_links))

            self.visited_links.add(link)
            print >>sys.stderr, 'visited_links {}'.format(
                len(self.visited_links))

    def invalid_content(self, content):
        '''Return true if the page does not contain the strings
        * "Käännöstä ei ole saatavilla"
        * "There are no translations available"
        '''
        return ('ei ole saatavilla' in content or
                'There are no translations available' in content or
                '<div class="login-form">' in content)

    def harvest_links(self, content):
        '''Harvest all links, bar some restrictions

            Insert links into a set

            Discard links containing a href=
            * "#"
            * "*do_pdf*"
            * "pop=1"
            * com_events
            * com_search
            * www.samediggi.fi/haettavana
            * http://klemetti.blogspot.com/

            Don't follow (don't save content), but save links containg
            doc_download
        '''
        tree = lxml.html.document_fromstring(content)

        for address in tree.findall('.//a'):
            href = address.get('href')
            if href is not None:
                href = href.replace('&lang=finnish', '')
                href = href.replace('&lang=davvi', '')
                href = href.replace('&lang=anaras', '')
                href = href.replace('&lang=nuortta', '')
                href = href.replace('&lang=english', '')
                if not href.startswith('http'):
                    href = os.path.join('http://www.samediggi.fi', href)

                if (href not in self.visited_links and not
                        re.search(
                            'klemetti.blogspot|/nuorat|/#|com_events|'
                            'com_search|haettavana|do_pdf|pop=1|com_docman|'
                            '/images|com_weblink|task=vcard', href) and
                        href.startswith('http://www.samediggi.fi')):
                    self.unvisited_links.add(href)

    def save_pages(self, pages):
        '''Write pages to disk

        pages is a dict of langage:content pairs
        '''
        for adder in pages:
            adder.copy_orig_to_corpus()
            other_langs = {}
            for langer in pages:
                if langer != adder:
                    other_langs[u'parallel_texts'] = u'1'
                    other_langs[u'para_{}'.format(langer.mainlang)] = unicode(
                        langer.new_filename)
            adder.make_metadata_file(other_langs)
        print


def parse_options():
    parser = argparse.ArgumentParser(
        parents=[argparse_version.parser],
        description='Crawl saami sites (for now, only www.samediggi.fi).')

    parser.add_argument('sites',
                        nargs='+',
                        help="The sites to crawl")

    args = parser.parse_args()
    return args


def main():
    args = parse_options()

    for site in args.sites:
        if site == 'www.samediggi.fi':
            print 'Will crawl samediggi.fi'
            sc = SamediggiFiCrawler()
            sc.download_pages()
        else:
            print 'Can not crawl {} yet'.format(site)


if __name__ == "__main__":
    main()
