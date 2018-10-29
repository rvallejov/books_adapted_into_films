from scrapy import Spider, Request
from goodreads.items import GoodreadsItem
import re

class GoodReadsSpider(Spider):
    name = 'goodreads_spider'
    allowed_urls = ['https://www.goodreads.com']
    start_urls = ['https://www.goodreads.com/list/show/39332']

    def parse(self,response):
        result_urls = response.xpath('//div[@class="mediumText"]/a/@href').extract()[:7]
        result_urls[3] = "".join(['https://www.goodreads.com/list/show/',result_urls[3]])
        
        #'https://www.goodreads.com/list/show/36647.Books_with_ratings_from_200_000_to_500_000?page=1'
        
        for url in result_urls:
            yield Request(url=url, callback=self.parse_result_page,
                            meta={'first_url':url})

    def parse_result_page(self,response):
        first_url = response.meta['first_url']
        try:
            page_urls = list(map(lambda x: "".join(['https://www.goodreads.com',x]),list(set(response.xpath('//div[@class="pagination"]/a/@href').extract()))))
        except:
            page_urls = []
        
        page_urls = [first_url] + page_urls

        print(len(page_urls))
        print('+'*25)

        for url in page_urls:
            yield Request(url=url, callback=self.parse_top_page, dont_filter=True)

    def parse_top_page(self,response):
        book_urls = list(map(lambda x: "".join(['https://www.goodreads.com',x]),response.xpath('//tr[@itemtype="http://schema.org/Book"]/td/a/@href').extract()))
        
        print(len(book_urls))
        print('='*50)

        for url in book_urls:
            yield Request(url=url, callback=self.parse_book_page)

    def parse_book_page(self,response):
        try:
            year = int(re.findall('(\d\d\d\d)',response.xpath('//div[@id="details"]/div/nobr/text()').extract_first())[0])
        except:
            year = int(re.findall('(\d\d\d\d)',"".join(response.xpath('//div[@id="details"]/div/text()').extract()))[0])
        genre = response.xpath('//div[@class="bigBoxBody"]/div/div/div/a/text()').extract_first()
        book_title = re.sub('\n','',response.xpath('//h1[@id="bookTitle"]/text()').extract_first()).strip()
        author = response.xpath('//a[@class="authorName"]/span/text()').extract_first()
        avg_rating = float(response.xpath('//span[@itemprop="ratingValue"]/text()').extract_first())
        ratings = int(re.sub('[\s,]','',response.xpath('//span[@class="votes value-title"]/text()').extract_first()))
        reviews = int(re.sub('[\s,]','',response.xpath('//span[@class="count value-title"]/text()').extract_first()))

        print('-'*50)

        item = GoodreadsItem()
        item['year'] = year
        item['genre'] = genre
        item['book_title'] = book_title
        item['author'] = author
        item['avg_rating'] = avg_rating
        item['ratings'] = ratings
        item['reviews'] = reviews
        yield item