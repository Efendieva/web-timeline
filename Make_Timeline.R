# Упражнение 1 -----------------------------------------------------------------

# Вариант #13===================================================================

# Загрузка пакетов
library('XML')                 
library('RCurl')               

# Ищем результаты по следующим трем запросам:
# "Модные тенденции",
# "Изменен* в моде",
# "Модн* тренды"

# Вектор URL страниц поиска соответственно трем вариантам запросов (Яндекс)
request_y <- c("https://yandex.ru/search/?text=%D0%9C%D0%BE%D0%B4%D0%BD%D1%8B%D0%B5%20%D1%82%D0%B5%D0%BD%D0%B4%D0%B5%D0%BD%D1%86%D0%B8%D0%B8%20",
               "https://yandex.ru/search/?text=%D0%98%D0%B7%D0%BC%D0%B5%D0%BD%D0%B5%D0%BD*%20%D0%B2%20%D0%BC%D0%BE%D0%B4%D0%B5%20",
               "https://yandex.ru/search/?text=%D0%9C%D0%BE%D0%B4%D0%BD*%20%D1%82%D1%80%D0%B5%D0%BD%D0%B4%D1%8B%20")
# Посмотрим результаты на 10 лет с помощью Яндекс


# Создадим вектора для последующего заполнения ...  
years_y <- vector(mode = "numeric", length = 0) # ..годов
headers_y <- vector(mode = "character", length = 0) # ...заголовков
sources_y <- vector(mode = "character", length = 0) # ...источников информации
urls_y <- vector(mode = "character", length = 0) # ... рабочих ссылок
i = request_y[]

for (i in request_y){
  
  # Sys.sleep(120)
  
  for  (n in 2017:2026){
    # URL страницы поиска в Яндекс:
    fileURL <- paste(i, n,
                     "&lr=213",
                     sep = "")
    
    # Загружаем текст страницы
    html <- getURL(fileURL)
    # разбираем как html
    doc <- htmlTreeParse(html, useInternalNodes = T)
    
    # Sys.sleep(40)
    
    # корневой элемент
    rootNode <- xmlRoot(doc)
    # Теперь следует избавиться от записей, предоставленных "Яндекс Новости", а
    # также Яндекс Видео
    # Эти записи создают путаницу, так как под одним заголовком может быть 
    # несколько новостей, и из-за этого получается разное число ссылок и 
    # источников.
    
    # Примечание: тег заголовков основной - первый, остальные (//div[@class="z-video__item"]/a
    # |  //div[@class="z-video__item z-video__item_big-screen_yes"]/a) добавлены
    # для того, чтобы учесть, если в результате поиска появятся видео-материалы
    # и добавить ихю
    
    # выбираем ссылки результатов запроса
    h <- xpathSApply(rootNode, 
                     '//h2[@class="serp-item__title"]/a | //div[@class="z-video__item"]/a |  //div[@class="z-video__item z-video__item_big-screen_yes"]/a',
                     xmlGetAttr, 'href')
    
    # ищем позицию(и), относящуюся к Яндекс Новостям
    u <- c(grep("http://news.yandex.ru/*", h))
    
    
    # собираем заголовки результатов запроса
    if (length(u) != 0) {
      urls_y <- c(urls_y, h[-u])
      # выбираем заголовки результатов запроса статей
      h <- xpathSApply(rootNode, '//h2[@class="serp-item__title"] | //span[@class = "link__inner z-video__link"]',
                       xmlValue)
      
      # собираем заголовки результатов запроса
      headers_y <- c(headers_y, h[-u])
      # заполняем столбец для лет
      years_y <- c(years_y, rep(n,length(h[-u])))
    } else {
      urls_y <- c(urls_y, h)
      # выбираем заголовки результатов запроса статей
      h <- xpathSApply(rootNode, '//h2[@class="serp-item__title"] | //span[@class = "link__inner z-video__link"]',
                       xmlValue)
      # собираем заголовки результатов запроса
      headers_y <- c(headers_y, h)
      # заполняем столбец для лет
      years_y <- c(years_y, rep(n,length(h)))
    }
    
    
    # выбираем все источники результатов запроса, исключая ссылки на ЯндексВидео
    # Мы это делаем по тем же причинам, что и когда исключали Яндекс Новости - 
    # это создает путаницу. Так, из-за Яндекс Видео мы найдем несколько лишних 
    # источников (sources_y), тогда как ни заголовков (headers_y), ни ссылок 
    # (urls_y) мы не найдем по используемым тегам.
    
    # выбираем ссылки результатов запроса
    h <- xpathSApply(rootNode, 
                     '//span[@class="serp-url__item"] | //span[@class="serp-url__item path"]',
                     xmlValue)
    # ищем позицию(и), относящуюся к Яндекс Видео
    u <- c(grep("video.yandex.ru", h))
    if (length(u) != 0) {
      sources_y <- c(sources_y, h[-u])
    } else {
      sources_y <- c(sources_y, h)
    }
    
    # Sys.sleep(40)
    
  }
  
}

# собираем в один фрейм
DF <- data.frame(Year = years_y, Header = headers_y,
                 Source = sources_y, URL = urls_y)

# Приведем заголовки к более красивому виду - уберем лишние ссылки и ...
DF$Header <- gsub(' [/||] .*$', '', DF$Header)
DF$Header <- gsub('[...]', '', DF$Header)

# запишем результат в файл
write.csv(DF, './Timeline.csv', row.names = F)
