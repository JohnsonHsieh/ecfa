library(XML)
library(RCurl)
library(tm)
library(tmcn)
library(Rwordseg)
library(wordcloud)
library(RColorBrewer)

library(vegan)

words1 <- readLines("http://wubi.sogou.com/dict/download_txt.php?id=9182") # ptt字庫
words2 <- readLines("http://wubi.sogou.com/dict/download_txt.php?id=9912") # 繁體字庫
words <- c("服貿","反服貿", "馬英九", "江宜樺", "立法院", toTrad(c(words1,words2))) # 自建字庫
insertWords(words)
myStopWords <- c(stopwordsCN(), "編輯", "時間", "標題", "發信", "實業", "作者", "要聞", "即時新聞", "聯合新聞網", "全文網址", "全文", "網址")
#----------------------------------------------------------
# 爬ptt 服貿版文章 http://www.ptt.cc/bbs/FuMouDiscuss/
#----------------------------------------------------------
data <- list()
for( i in 38:289){ #38~289是3/24號00:00至18:00的所有文章
  tmp <- paste(i, '.html', sep='')
  if(i<100) tmp <- paste('0',i,'.html',sep='')
  url <- paste('www.ptt.cc/bbs/FuMouDiscuss/index', tmp, sep='')
  html <- htmlParse(getURL(url))
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
  data <- rbind(data, paste('www.ptt.cc', url.list, sep=''))
}
data <- unlist(data)

# 利用所有文章的url連結去抓所有文章的html網頁, 並用xpathSApply去解析出文章的內容並儲存
getdoc <- function(line){
  start <- regexpr('www', line)[1]
  end <- regexpr('html', line)[1]
  
  if(start != -1 & end != -1){
    url <- substr(line, start, end+3)
    html <- htmlParse(getURL(url), encoding='UTF-8')
    doc <- xpathSApply(html, "//div[@id='main-content']", xmlValue)
    name <- strsplit(url, '/')[[1]][4]
    write(doc, gsub('html', 'txt', name))
  }      
}

setwd("ptt")
sapply(data, getdoc)  # 爬服貿版文章內容
setwd("../")

#--------------------------------------------------------
# 爬蘋果日報文章 
# http://www.appledaily.com.tw/realtimenews/article/politics/20140324/366426/
# http://www.appledaily.com.tw/realtimenews/article/politics/20140324/366301/
#--------------------------------------------------------
setwd("apple/")
for( i in 366329:366426){ #38~289是3/24號00:00至18:00的所有文章
  tmp <- paste(i, '/', sep='')
  url <- paste('http://www.appledaily.com.tw/realtimenews/article/politics/20140324/', tmp, sep='')
  html <- htmlParse(getURL(url),encoding='UTF-8')
  doc <- xpathSApply(html, "//p[@id='summary']", xmlValue)
  write(doc, paste(i,'.txt',sep=""))
}
setwd("../")


#--------------------------------------------------------
# 聯合新聞 未完成
# http://fe3.udn.com/search/udnsearch.jsp?project=&Keywords=%AA%41%B6%54+&f_PAGE=1
# http://fe3.udn.com/search/udnsearch.jsp?project=&Keywords=%AA%41%B6%54+&f_PAGE=50
#--------------------------------------------------------
setwd("udn/")
data <- list()
for( i in 1:50){ 
  tmp <- paste(i)
  url <- paste('http://fe3.udn.com/search/udnsearch.jsp?project=&Keywords=%AA%41%B6%54+&f_PAGE=', tmp, sep='')
  html <- htmlParse(getURL(url),encoding='UTF-8')
  url.list <- xpathSApply(html, "//dt/a[@href]", xmlAttrs)
  data[[i]] <- url.list[1,]
}
data <- unlist(data)






# 把文章進行分詞, 匯出名詞的頻率 
myText <- function(fileDir="g0v", output="g0v-freq.txt"){
  d.corpus <- Corpus(DirSource(fileDir), list(language = NA))
  d.corpus <- tm_map(d.corpus, removeWords, myStopWords)
  d.corpus <- tm_map(d.corpus, removePunctuation) #清除標點符號
  d.corpus <- tm_map(d.corpus, removeNumbers) #清除數字
  d.corpus <- tm_map(d.corpus, function(word) { #清除英文字母
    gsub("[A-Za-z0-9]", "", word)
  })
  
  
  
  d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
  d.corpus <- tm_map(d.corpus, function(sentence) {
    noun <- lapply(sentence, function(w) {
      w[names(w) == "n"]
    })
    unlist(noun)
  })
  d.corpus <- Corpus(VectorSource(d.corpus))
  tdm <- TermDocumentMatrix(d.corpus, control = list(wordLengths = c(2, Inf)))
  m1 <- as.matrix(tdm)
  v <- sort(rowSums(m1), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  write.table(data.frame(freq=d$freq, word=d$word), file=output,quote=FALSE,sep="\t",row.names=FALSE, col.names=FALSE)
  
  data.frame(freq=d$freq, word=d$word)
}
fau <- list()
fau[[1]] <- myText(fileDir="0324/g0v", output="g0v-freq.txt") 
fau[[2]] <- myText(fileDir="0324/ptt", output="ptt-freq.txt") # 讀取很久 請慎用
fau[[2]] <- fau[[2]][-(3:6),]
fau[[3]] <- myText(fileDir="0324/apple", output="apple-freq.txt") # 蘋果日報
fau[[4]] <- myText(fileDir="0324/ct", output="ct-freq.txt") # 中時電子報
fau[[5]] <- myText(fileDir="0324/udn", output="udn-freq.txt") # 聯合新聞網



myPng <- function(input=fau0, output="wordcloud.png",min.freq=2){
  library(RColorBrewer)
  pal2 <- brewer.pal(8,"Dark2")
  par(family="Arial Unicode MS")
  png(output, width=1280,height=800)
  wordcloud(input$word,input$freq, scale=c(10,.4),min.freq=min.freq,
            max.words=Inf, random.order=FALSE, colors=pal2)
  dev.off()
}

# myPng(fau[[1]], "g0v-wordcloud.png")
# myPng(fau[[2]], "ptt-wordcloud.png",min.freq=100)
# myPng(fau[[3]], "apple-wordcloud.png")
# myPng(fau[[4]], "ct-wordcloud.png")
# myPng(fau[[5]], "udn-wordcloud.png")

u <- as.character(unique(unlist(lapply(fau, function(x)x$word))))

tmp <- out <- list()
tmp <- lapply(fau, function(x) rep(x$word, x$freq))
out <- lapply(tmp, function(x) table(factor(x, levels=u, labels=u)))
tab <- do.call("rbind", out)
sim <- c(1 - vegdist(tab, method="morisita")) #兩兩相似矩陣
k <- 0
edges <- data.frame()
for(i in 1:(nrow(tab)-1)){
  for(j in (i+1):nrow(tab)){
    k <- k +1
    tmp <- data.frame("Source"=i, "Target"=j, Type="Undirected", "Weight"=sim[k])
    edges <- rbind(edges, tmp)
  }
}
edges



node <- data.frame("id"=1:nrow(tab), "x"=rep(1/nrow(tab),nrow(tab)))

write.csv(node, "node.csv")
write.csv(edges, "edges.csv")
