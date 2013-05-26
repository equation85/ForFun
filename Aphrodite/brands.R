# encoding=utf-8

library(RCurl)
library(XML)

brands.url <- 'http://brand.lady8844.com/s/#index'
brands.menu <- getURI(brands.url, .encoding='UTF-8')
brands.menu.tree <- htmlTreeParse(brands.menu, asText=TRUE, useInternalNodes=TRUE, encoding='UTF-8')

brands.menu.tree <- getNodeSet(brands.menu.tree, '//dl[@class="lei"]')
tmp.idx <- which(sapply(brands.menu.tree, function(node) grepl('品牌', xmlValue(node))))   # 第一个元素是品牌名称
brands.url <- getNodeSet(brands.menu.tree[[tmp.idx]], 'dd/ul/li')
brands <- lapply(brands.url, function(node) c(name=xmlValue(node[['a']]), url=xmlGetAttr(node[['a']],'href')))
brands <- as.data.frame(do.call(rbind, brands))

write.table(brands, file='brands.txt', sep='\t', quote=FALSE, row.names=FALSE, col.names=TRUE)

