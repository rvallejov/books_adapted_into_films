# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html

import scrapy

class GoodreadsItem(scrapy.Item):
    year = scrapy.Field()
    genre = scrapy.Field()
    book_title = scrapy.Field()
    author = scrapy.Field()
    avg_rating = scrapy.Field()
    ratings = scrapy.Field()
    reviews = scrapy.Field()