# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class WikiBooksItem(scrapy.Item):
    book_title = scrapy.Field()
    book_year = scrapy.Field()
    book_author = scrapy.Field()
    film_title = scrapy.Field()
    film_year = scrapy.Field()