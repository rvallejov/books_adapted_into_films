from scrapy import Spider, Request
from wiki_books.items import WikiBooksItem
import re

class Wiki_books(Spider):
    name = 'wiki_books_spider'
    allowed_urls = ['https://en.wikipedia.org']
    start_urls = ['https://en.wikipedia.org/wiki/Lists_of_fiction_works_made_into_feature_films']

    def parse(self,response):
        result_urls = list(map(lambda x: "".join(['https://en.wikipedia.org',x]),response.xpath('//a[@class="mw-redirect"]/@href').extract()[1:5]))

        print(len(result_urls))
        print('o'*50)

        for url in result_urls:
            yield Request(url=url, callback=self.parse_result_page)

    def parse_result_page(self, response):
        books = response.xpath('//table[@class="wikitable"]/tbody/tr')
        print(len(books))
        print('+'*50)

        for book in books[1:]:
            try:
                # Getting the titles:
                # Case when the title has no link
                book_title = book.xpath('.//td[1]/i/text()').extract()
                # Case when the title has link (we have to go inside <a>)
                if (book_title == []):
                    book_title = book.xpath('.//td[1]/i/a/text()').extract()
                # Getting the authors:
                # Case when the author has no link
                # This will contain year + author
                td_text = book.xpath('.//td[1]/text()').extract() # extract all text under the td tag
                td_text = list(map(lambda x:re.sub('[()]','',x.strip()).strip(),td_text)) # remove all brackets
                td_text = list(filter(lambda x: re.search('\w',x) != None, td_text))
                book_year = [x for x in list(map(lambda x: re.findall('\d\d\d\d',x),td_text)) if x != []]
                # Assume the author has a link:
                book_author = book.xpath('.//td[1]/a/text()').extract()
                # If the path turns out empty, get the author from the <td> text()
                if (book_author == []):
                    book_author = list(map(lambda x: re.sub('[^a-zA-Z .]','',x).strip(),td_text))
                
                # Moving on the the film column
                td_text = book.xpath('.//td[2]/text()').extract()
                td_text = list(map(lambda x:re.sub('[()]','',x.strip()).strip(),td_text)) # remove all brackets
                td_text = list(filter(lambda x: re.search('\w',x) != None, td_text))
                film_year = [x for x in list(map(lambda x: re.findall('\d\d\d\d',x),td_text)) if x != []]
                # Assume the film has a link:
                film_title = book.xpath('.//td[2]/i/a/text()').extract()
                if (film_title == []):
                    film_title = book.xpath('.//td[2]/i/text()').extract()

                for ,b_year in zip(film_title, film_year):
                    item = WikiBooksItem()
                    item['book_title'] = list(map(lambda x: [x],book_title))
                    item['book_year'] = book_year
                    item['book_author'] = book_author
                    item['film_title'] = film
                    item['film_year'] = year
                    yield item

                # item = WikiBooksItem()
                # item['book_title'] = list(map(lambda x: [x],book_title))
                # item['book_year'] = book_year
                # item['book_author'] = book_author
                # item['film_title'] = film_title
                # item['film_year'] = film_year
                # yield item
            except:
                print("Next letter")
                print('-'*50)