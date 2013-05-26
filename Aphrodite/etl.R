# encoding=utf-8

library(ggplot2)

options(stringsAsFactors=FALSE)

items <- readLines('result/items.txt')
items <- strsplit(items, '\t')
tmp.name <- items[[1]]
items <- items[-1]
tmp.len <- sapply(items, length)
print(table(tmp.len))
items <- do.call(rbind, items[tmp.len==7])
items <- as.data.frame(items)
colnames(items) <- tmp.name

items <- items[,c('brand','name','tag','price','vol')]
items$price <- as.numeric(items$price)

items.tag <- strsplit(items$tag, ',')
items.tag <- lapply(1:length(items.tag), function(i) setdiff(items.tag[[i]], items$brand[i]) )

items <- items[,-3]
                      
delete.idx <- which(is.na(items$price))
items <- items[-delete.idx,]
items.tag <- items.tag[-delete.idx]

tmp <- tapply(items$price, items$brand, function(x) c(mean(x), length(x)))
tmp <- do.call(rbind, tmp)
brand.info <- data.frame(brand=rownames(tmp), price=tmp[,1], num=tmp[,2])
row.names(brand.info) <- 1:nrow(brand.info)

known.brand <- c('露得清','雅诗兰黛','巴黎欧莱雅','资生堂','碧欧泉','美即')
known.col <- ifelse(brand.info$brand %in% known.brand, 'blue', 'grey')

op <- par(family='STKaiti')
plot(brand.info$num, brand.info$price, type='n', xlab='商品种类', ylab='均价')
abline(h=mean(brand.info$price), col='red')
abline(v=mean(brand.info$num), col='red')
text(brand.info$num, brand.info$price, brand.info$brand, cex=0.6, col=known.col)
par(op)

