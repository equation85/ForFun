# encoding=utf-8

library(RCurl)
library(XML)

# 抓取当前页面的商品信息
extractBrandItem <- function(node, save.img=FALSE, save.path='jpg') {
  base.path <- '//div[@class="cont_box"]/dl'
  item.name <- xpathSApply(node, paste(base.path,'//dd//strong',sep=''), xmlValue)
  item.url <- xpathSApply(node, paste(base.path,'//dd/div[@class="pro_tit"]/a',sep=''), xmlGetAttr, name='href')
  item.img <- xpathSApply(node, paste(base.path,'//img',sep=''), xmlGetAttr, name='src')
  if(save.img==TRUE) {
    for(img in item.img) {
      system(sprintf('wget -P %s %s',save.path, img), ignore.stdout=TRUE, ignore.stderr=TRUE)
    }
  }
  item.tag <- getNodeSet(node, paste(base.path,'//dd'))
  item.tag <- sapply(item.tag,
                     function(x) {
                       tmp <- xpathSApply(x, 'span', xmlValue)
                       paste(tmp, collapse=',')
                     })
  item.desc <- xpathSApply(node, paste(base.path,'//dd'), xmlValue)
  item.desc <- sub('网友心得.*', '', item.desc)
  no.price <- !grepl('￥', item.desc)
  item.price <- sub('.*￥([0-9.]+)元.*', '\\1', item.desc)
  item.price[no.price] <- NA
  item.price <- as.numeric(item.price)
  item.volumn <- sub('.*容量：(.*?)[[:space:]].*', '\\1', item.desc)
  res <- data.frame(name=item.name, url=item.url, img=item.img, tag=item.tag, price=item.price, vol=item.volumn, stringsAsFactors=FALSE)
  return(res)
}

# 在当前页面上抓取下一页的网址，未找到则返回NA
findNextPage <- function(node) {
  next.page <- getNodeSet(node, '//div[@class="page"]/a[@class="next"]')
  if(length(next.page)==0) {
    return(NA)
  } else {
    res <- xmlGetAttr(next.page[[1]],'href')
    return(res)
  }
}

### Test:
#curr.page <- getURI('http://brand.lady8844.com/s/44-0-0-0-0-0-0-0-0-8-10.html#index', .encoding='UTF-8')
#curr.page.tree <- htmlTreeParse(curr.page, asText=TRUE, useInternalNodes=TRUE, encoding='UTF-8')
#print(extractBrandItem(curr.page.tree, FALSE))
#print(findNextPage(curr.page.tree))
#
#last.page <- getURI('http://brand.lady8844.com/s/44-0-0-0-0-0-0-0-0-64-10.html#index', .encoding='UTF-8')
#last.page.tree <- htmlTreeParse(last.page, asText=TRUE, useInternalNodes=TRUE, encoding='UTF-8')
#print(extractBrandItem(last.page.tree, FALSE))
#print(findNextPage(last.page.tree))

