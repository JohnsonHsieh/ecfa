myText <- function(input, output="g0v-freq.txt", method="vec"){
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
  write.table(d, file=output,quote=FALSE,sep="\t",row.names=FALSE, col.names=FALSE)
  d
}