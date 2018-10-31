from scrapy import Spider, Request
from wiki_books.items import WikiBooksItem
from fuzzywuzzy import fuzz,process
import re

class Wiki_books(Spider):
    name = 'wiki_books_spider'
    allowed_urls = ['https://en.wikipedia.org']
    start_urls = ['https://en.wikipedia.org/wiki/Lists_of_fiction_works_made_into_feature_films']

    def parse(self,response):
        result_urls = list(map(lambda x: "".join(['https://en.wikipedia.org',x]),response.xpath('//a[@class="mw-redirect"]/@href').extract()[1:5]))

        for url in result_urls:
            yield Request(url=url, callback=self.parse_result_page)

    def parse_result_page(self, response):
        books = response.xpath('//table[@class="wikitable"]/tbody/tr')

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
                
                film_wiki = list(map(lambda x: "".join(['https://en.wikipedia.org',x]),book.xpath('.//td[2]/i/a/@href').extract()))

                print(film_wiki)
                print('*'*50)
                for url,f_title,f_year in zip(film_wiki,film_title, film_year):
                    if (url != [] or re.search('[^(en)]\.wikipedia',url) == None):
                        yield Request(url=url, callback=self.parse_film_wiki,
                                        meta={'book_title': book_title,
                                        'book_year': book_year,
                                        'book_author': book_author,
                                        'f_title': f_title,
                                        'f_year': f_year})
                # for b_title,b_year in zip(book_title, book_year):
                #     for f_title,f_year in zip(film_title, film_year):
                #         item = WikiBooksItem()
                #         item['book_title'] = b_title
                #         item['book_year'] = b_year
                #         item['book_author'] = book_author
                #         item['film_title'] = f_title
                #         item['film_year'] = f_year
                #         yield item

            except:
                print("Next letter")
                print('-'*50)

    def parse_film_wiki(self,response):
        book_title = response.meta['book_title']
        book_year = response.meta['book_year']
        book_author = response.meta['book_author']
        f_title = response.meta['f_title']
        f_year = response.meta['f_year']

        links = response.xpath('//a[@class="external text"]/@href').extract()
        try:
            imdb_url = list(filter(lambda x: re.search('www.imdb.com',x) != None,links))[-1]
            
            print(f_title)
            print('='*50)
            
            yield Request(url=imdb_url, callback=self.parse_imdb_page,
                            meta={'book_title': book_title,
                                    'book_year': book_year,
                                    'book_author': book_author,
                                    'f_title': f_title,
                                    'f_year': f_year})
        except:
            print('No imdb url')

    def parse_imdb_page(self,response):
        book_title = response.meta['book_title']
        book_year = response.meta['book_year']
        book_author = response.meta['book_author']
        f_title = response.meta['f_title']
        f_year = response.meta['f_year']

        film_director = ", ".join(response.xpath('//div[@class="plot_summary "]/div[2]/a/text()').extract())
        try:
            film_avg_rating = float(response.xpath('//div[@class="ratingValue"]/strong/span/text()').extract_first())
        except:
            film_avg_rating = ''
        try:
            film_ratings = int(re.sub(',','',response.xpath('//span[@class="small"]/text()').extract_first()))
        except:
            film_ratings = ''
        try:
            film_meta_score = int(response.xpath('//div[@class="titleReviewBarItem"]/a/div/span/text()').extract_first())
        except:
            film_meta_score = ''
        user_critic = response.xpath('//div[@class="titleReviewBarItem titleReviewbarItemBorder"]/div/span/a/text()').extract()
        try:
            film_user_reviews = int(re.sub('[\D]','',list(filter(lambda x: re.search('user',x) != None,user_critic))[0]))
        except:
            film_user_reviews = ''
        try:
            film_critic_reviews = int(re.sub('[\D]','',list(filter(lambda x: re.search('critic',x) != None,user_critic))[0]))
        except:
            film_critic_reviews = ''
        print('++'*50)
        print(book_title)
        print(book_year)
        print(f_title)
        print('++'*50)
        if len(book_year) > 1:
            try:
                b_f_title = process.extractOne(f_title, book_title, scorer = fuzz.ratio, score_cutoff = 80)[0]
            except:
                b_f_title = book_title[0]
            b_f_year = "-".join(book_year[book_title.index(b_f_title)])
        else:
            b_f_title = ", ".join(book_title)
            b_f_year = "-".join(book_year[0])
        item = WikiBooksItem()
        item['book_title'] = b_f_title
        item['book_year'] = b_f_year
        item['book_author'] = book_author
        item['film_title'] = f_title
        item['film_year'] =  "-".join(f_year)
        item['film_director'] = film_director
        item['film_avg_rating'] = film_avg_rating
        item['film_ratings'] = film_ratings
        item['film_meta_score'] = film_meta_score
        item['film_user_reviews'] = film_user_reviews
        item['film_critic_reviews'] = film_critic_reviews
        yield item

        # for b_title,b_year in zip(book_title, book_year):
        #     if len(book_year) > 1:
        #         try:
        #             b_f_title = process.extractOne(f_title, book_title, scorer = fuzz.ratio, score_cutoff = 80)[0]
        #         except:
        #             b_f_title = b_title
        #         b_f_year = "-".join(book_year[book_title.index(b_f_title)])
        #     else:
        #         b_f_title = b_title
        #         b_f_year = "-".join(b_year)
        #     item = WikiBooksItem()
        #     item['book_title'] = b_f_title
        #     item['book_year'] = b_f_year
        #     item['book_author'] = book_author
        #     item['film_title'] = f_title
        #     item['film_year'] =  "-".join(f_year)
        #     item['film_director'] = film_director
        #     item['film_avg_rating'] = film_avg_rating
        #     item['film_ratings'] = film_ratings
        #     item['film_meta_score'] = film_meta_score
        #     item['film_user_reviews'] = film_user_reviews
        #     item['film_critic_reviews'] = film_critic_reviews
        #     yield item