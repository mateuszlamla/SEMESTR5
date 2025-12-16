import scrapy

class PurseSpider(scrapy.Spider):
    name = 'purse'
    start_urls = ['https://ccc.eu/pl/torby/torby-damskie']

    def parse(self, response):
        for purse in response.css('div.c-offerBox_row'):
            name = purse.css('div.c-offerBox_data a span::text').get()
            price_zl = purse.css('span.a-price_price::text').get()
            price_gr = purse.css('span.a-price_meta span.a-price_rest::text').get()

            price = f"{price_zl}.{price_gr}".strip()

            yield {
                'name': name,
                'price': price
            }

            next_page = response.css('a.is-nextLink::attr(href)').get()
            if next_page:
                yield response.follow(next_page, callback=self.parse)
