library(XML)
library(RCurl)
library(tm)
library(tmcn)
library(Rwordseg)
library(wordcloud)
library(RColorBrewer)
library(vegan)

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


#-----------------------------------------
# 整理各家新聞資料
#-----------------------------------------
raw <- read.csv("0325/0325-output.csv")
news <- raw[!duplicated(raw$標題),] # 只擷取第一次刊登的新聞
news$新聞來源 <- factor(news$新聞來源, levels=c(1:10,12:14), 
                    labels=c("蘋果", "中時", "中央社", "東森", "自由", "新頭殼", "NowNews", 
                             "聯合", "TVBS", "中廣", "台視", "華視", "民視"))
content <- news[,c("新聞來源", "內容")]

# 計算各家媒體的關鍵字頻
feq <- list()
for(i in 1:14){
  id <- which(content$新聞來源==levels(content$新聞來源)[i])
  if(length(id)!=0){
    input <- as.character(content[id,"內容"])
    feq[[i]] <- myText(input=input, output=paste(Sys.Date(),"-",levels(content$新聞來源)[i],"字頻.txt", sep=""))
  }
}


# 計算關鍵字頻的相似度
tmp <- out <- list()
u <- as.character(unique(unlist(lapply(feq, function(x)as.character(x$word)))))
tmp <- lapply(feq, function(x) rep(as.character(x$word), x$freq))
out <- lapply(tmp, function(x) table(factor(x, levels=u, labels=u)))
tab <- do.call("rbind", out)
tab <- sweep(tab,MARGIN=1,STATS=rowSums(tab),FUN="/")
tab[11,] <- NA
sim <- c(1 - vegdist(tab[-11,], method="morisita")) #兩兩相似矩陣
k <- 0
edges <- data.frame()
for(i in 1:(nrow(tab)-1)){
  for(j in (i+1):nrow(tab)){
    k <- k + 1
    tmp <- data.frame("Source"=i, "Target"=j, Type="Undirected", "Weight"=sim[k])
    edges <- rbind(edges, tmp)
  }
}
edges <- na.omit(edges)

node <- data.frame("id"=1:nrow(tab), "label"=c(levels(content$新聞來源),"PTT","Live"), "x"=rep(1/nrow(tab),nrow(tab)))
write.csv(node, paste(Sys.Date(),"-","node.csv",sep=""))
write.csv(edges, paste(Sys.Date(),"-","edges.csv",sep=""))

edges <- data.frame()
for(i in 1:(nrow(tab)-1)){
  for(j in (i+1):nrow(tab)){
    k <- k + 1
    tmp <- data.frame("Source"=i, "Target"=j, Type="Undirected", "Weight"=sim[k])
    edges <- rbind(edges, tmp)
  }
}

a <- edges$Source 
b <- edges$Target
id1  <- which(a==1 | a==2 | a==5 | a==8 | a==15 | a==16)
id2 <- which(b==1 | b==2 | b==5 | b==8 | b==15 | b==16)
write.csv(edges[intersect(id1,id2),], paste(Sys.Date(),"-","edges(紙本新聞).csv",sep=""))


