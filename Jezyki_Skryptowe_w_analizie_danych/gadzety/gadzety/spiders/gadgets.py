import scrapy

class GadgetsSpider(scrapy.Spider):
    name = "gadgets"
    allowed_domains = ["esklep.polsl.pl"]
    start_urls = ["https://esklep.polsl.pl/kategoria-produktu/gadzety"]

    def parse(self, response):
        for p in response.css('ul.woo-product-info'):
            title = p.css('li.title h2 a::text').get()
            price_text = p.css('li.price-wrap span.price span.amount bdi::text').get()
            yield {
                'title': title,
                'price': price_text.strip(),
            }
            next_page = response.css('a.next.page-numbers::attr(href)').get()
            if next_page:
                yield response.follow(next_page, callback=self.parse)
