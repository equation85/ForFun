#!/usr/bin/env Rscript
# encoding=utf-8

library(plyr)
source('getItems.R')

options(stringsAsFactors=FALSE) 

### 参数设置
#INPUT.FILENAME <- 'test.txt'
INPUT.FILENAME <- 'result/brands.txt'
ITEMS.FILENAME <- 'result/items.txt'
VISITEDURL.FILENAME <- 'result/visited_url.txt'


brands.url <- read.table(INPUT.FILENAME, header=TRUE, sep='\t', quote='')
items.file <- file(ITEMS.FILENAME, 'w')
on.exit(close(items.file))
visited.url.file <- file(VISITEDURL.FILENAME, 'w')
on.exit(close(visited.url), add=TRUE)

progress <- create_progress_bar('text')
progress$init(nrow(brands.url))
count <- 0
for(i in 1:nrow(brands.url)) {
  curr.brand <- brands.url[i,'name']
  curr.url <- brands.url[i,'url']

  # 抓取该品牌下所有商品信息
  repeat {
    count <- count + 1
    cat(curr.url, '\n', file=visited.url.file, sep='')
    
    curr.page <- htmlTreeParse(curr.url, isURL=TRUE, useInternalNodes=TRUE, encoding='UTF-8')
    curr.df <- extractBrandItem(curr.page, TRUE, 'jpg')
    curr.df <- cbind(brand=curr.brand, curr.df)
    if(count==1) {
      write.table(curr.df, file=items.file, append=FALSE, sep='\t', quote=FALSE, row.names=FALSE, col.names=TRUE)
    } else {
      write.table(curr.df, file=items.file, append=TRUE, sep='\t', quote=FALSE, row.names=FALSE, col.names=FALSE)
    }
    curr.url <- findNextPage(curr.page)
    if(is.na(curr.url)) break
  }
  progress$step()
}
progress$term()

