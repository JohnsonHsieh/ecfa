library(XML)
library(RCurl)
library(tm)
library(tmcn)
library(Rwordseg)
library(wordcloud)
library(RColorBrewer)
library(vegetarian)
#-----------------------------------------
# 處理字詞庫
#-----------------------------------------

# 匯入sogou字庫
words1 <- toTrad(readLines("http://wubi.sogou.com/dict/download_txt.php?id=9182")) # ptt字庫
words2 <- toTrad(readLines("http://wubi.sogou.com/dict/download_txt.php?id=9912")) # 繁體字庫
words <- c(words1,words2)
insertWords(words)

# 自建字庫
strwords <-  c("服貿", "服贸", "馬英九", "江宜樺", "立法院", "國會", "行政院", "魏揚", "林飛帆", "陳為廷", "台灣", 
               "警察", "暴力", "鎮暴警察", "學運", "黑色島國", "清大", "台大", "鎮壓", "後退", "張慶忠", "王金平",
               "蘋果", "陪審團", "粉絲團", "蘋論", "陣線", "最新", "評論", "獨立", "媒體", "每日", "總覽", "有話", "要說" ,"即時", "論壇")
insertWords(strwords, strtype=rep("n", length(strwords)), numfreq=rep(1000, length(strwords)))

# 定義停詞
myStopWords <- c(toTrad(stopwordsCN()), "編輯", "時間", "標題", "發信", "實業", "作者", "要聞", "即時新聞", "聯合新聞網", "全文網址", "全文", "網址", 
                 "大家", "今天", "知道", "非常", "很多", "現在", "希望", "不要", "已經", "看到", "謝謝", "其實", "事情",
                 "蘋果", "陪審團", "粉絲團", "蘋論", "陣線", "最新", "評論", "獨立", "媒體", "每日", "總覽", "有話", "要說" ,"即時", "論壇",
                 "投稿", "報導", "新聞", "表示", "粉絲", "沒有", "青島", "院內", "濟南", "現場", "主持人", "場內", "一起", "出來", "一下", "裡面", "可能", "需要",
                 "應該", "覺得", "繼續", "告訴", "不能", "剛剛", "接下來", "下去", "廣播", "訊息", "可能", )

myText <- function(input, method="vec"){
  if(method=="vec"){
    d.corpus <- Corpus(VectorSource(input)) # 建立語料庫
    
    d.corpus <- tm_map(d.corpus, removePunctuation) #清除標點符號
    d.corpus <- tm_map(d.corpus, removeNumbers) #清除數字
    d.corpus <- tm_map(d.corpus, function(word) { #清除英文字母
      gsub("[A-Za-z0-9]", "", word)
    })
    d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
    d.corpus <- lapply(d.corpus, function(sent) sent[names(sent)=="n"])
    d.corpus <- Corpus(VectorSource(d.corpus))
    d.corpus <- tm_map(d.corpus, removeWords, myStopWords)
  }else if(method=="dir"){
    d.corpus <- Corpus(DirSource(input)) # 建立語料庫
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
    d.corpus <- tm_map(d.corpus, removeWords, myStopWords)    
  }
  
  tdm <- TermDocumentMatrix(d.corpus, control = list(wordLengths = c(2, Inf)))
  m1 <- as.matrix(tdm)
  v <- sort(rowSums(m1), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  d <- d[d$freq>2,]
  d
}

#----------------------------------------------------------
# 爬ptt 服貿版文章 http://www.ptt.cc/bbs/FuMouDiscuss/
#----------------------------------------------------------
data <- list()
for( i in 1:565){ 
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

if(length(dir("ptt"))==0){
  setwd("ptt")
  sapply(data, getdoc)  # 爬服貿版文章內容
  setwd("../")
}
feq <- list()
feq[[1]] <- myText(input="live/", method="dir") 
feq[[2]] <- myText(input="ptt/", method="dir") 



#-----------------------------------------
# 整理各家新聞資料
#-----------------------------------------
raw <- read.csv("news/cleaned-output.csv")
news <- raw[!duplicated(raw$標題),] # 只擷取第一次刊登的新聞
news$新聞來源 <- factor(news$新聞來源, levels=c(1:10,12:14), 
                    labels=c("蘋果", "中時", "中央社", "東森", "自由", "新頭殼", "NowNews", 
                             "聯合", "TVBS", "中廣", "台視", "華視", "民視"))
content <- news[,c("新聞來源", "內容")]

# 計算各家媒體的關鍵字頻
for(i in 1:13){
  id <- which(content$新聞來源==levels(content$新聞來源)[i])
  if(length(id)!=0){
    input <- as.character(content[id,"內容"])
    feq[[i+2]] <- myText(input=input, sep=""))
  }
}


# 計算關鍵字頻的相似度
tmp <- out <- list()
u <- as.character(unique(unlist(lapply(feq, function(x)as.character(x$word)))))
tmp <- lapply(feq, function(x) rep(as.character(x$word), x$freq))
out <- lapply(tmp, function(x) table(factor(x, levels=u, labels=u)))
tab <- do.call("rbind", out)
# tab <- sweep(tab,MARGIN=1,STATS=rowSums(tab),FUN="/")
rownames(tab) <- c("Live", "PTT", levels(content$新聞來源))
sim <- sim.table(tab,q=2,half = FALSE)
sim <- sim[lower.tri(sim)]
k <- 0
edges <- data.frame()
for(i in 1:(nrow(tab)-1)){
  for(j in (i+1):nrow(tab)){
    k <- k + 1
    tmp <- data.frame("Source"=i, "Target"=j, Type="Undirected", "Weight"=sim[k])
    edges <- rbind(edges, tmp)
  }
}

node <- data.frame("id"=1:nrow(tab), "label"=c("Live", "PTT", levels(content$新聞來源)), "x"=rep(1/nrow(tab),nrow(tab)))
write.csv(node, paste(Sys.Date(),"-","node.csv",sep=""))
write.csv(edges, paste(Sys.Date(),"-","edges.csv",sep=""))
a <- edges$Source 
b <- edges$Target
id1 <- which(a==1 | a==2 | a==3 | a==4 | a==7 | a==10)
id2 <- which(b==1 | b==2 | b==4 | b==4 | b==7 | b==10)
write.csv(edges[intersect(id1,id2),], paste(Sys.Date(),"-","edges(紙本新聞).csv",sep=""))

write.csv(t(tab), paste(Sys.Date(),"-","新聞字頻.csv",sep=""))


