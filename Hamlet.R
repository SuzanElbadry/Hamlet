library(tm)
library(wordcloud)
library(sentimentr)
library(data.table)
library(ggplot2)
library(SnowballC)

ReadFile <- function(Path){
  if (file.exists(Path))
    Hamlet <- readtext::readtext(Path)
  else
    print("File not found")
}

ParagrapgScenceMap <- function(x) {is_na<-is.na(x); x[Reduce(function(i,j) if (is_na[j]) i else j, seq_len(length(x)), accumulate=T)]}

PreprocessText <- function(HamletText){
  if(HamletText!=""){
    HamletText <-  gsub("\\n\\t\\[(.*?)\\]","",HamletText)
    HamletText <- gsub("\n\n\tHAMLET\n\n","",HamletText)
    HamletText <- gsub("\n\n[\n]+\t","\n\t",HamletText)
    SCENEIDX <-  gregexpr("SCENE", HamletText)
    HamletText <- substr(HamletText,SCENEIDX[[1]][1],nchar(HamletText))
    NewLineIDX <-  gregexpr("\n\n[A-Z]+", HamletText)
    DTFinal <- data.table(matrix(nrow = lengths(NewLineIDX),ncol=2))
    DTFinal$V1 <- as.character(DTFinal$V1)
    DTFinal$V2 <- as.character(DTFinal$V2)
    setnames(DTFinal,old=c("V1","V2"),new=c("Text","Scence"))
    cnt=1
    for (i in 1:lengths(NewLineIDX)){
      if (i==1)
        DTFinal[i,1] <- substr(HamletText,0,NewLineIDX[[1]][i]-1)
      else{
        DTFinal[i,1] <- substr(HamletText,NewLineIDX[[1]][i],NewLineIDX[[1]][i+1]-1)
        if (grepl("SCENE",DTFinal[i,1])){
          DTFinal[i,2] <- paste("SCENE",as.character(cnt),sep=" ")
          cnt <- cnt+1
        }
      }
    }
    DTFinal[lengths(NewLineIDX),1] <- substr(HamletText,NewLineIDX[[1]][lengths(NewLineIDX)],nchar(HamletText))
    DTFinal$Scence <- ParagrapgScenceMap(DTFinal$Scence)
  }else{print("Empty File")}
  
  return(DTFinal)
}

GetMultiSpeakerCount <- function(HamletTextDT){
  SpeakersCountSameTime <-  lapply(HamletTextDT$Text,function (x) if(grepl("|",x,fixed =TRUE))  lengths(regmatches(x, gregexpr("[A-Z]+\t", removePunctuation(x)))) else 0)
}

GetSpeakers <- function(HamletTextDT){
  Speakers <-  lapply(HamletTextDT$Text,function (x) if(grepl("|",x,fixed =TRUE)) unlist(strsplit(gsub("([A-Z][A-Z]+)|.", "\\1 ", x), "\\s+"))[2:3] else
    gsub("\\n\\n","",strsplit(x, split="\t")[[1]][1]))
}

GetMultSentCount <- function(HamletTextDT){
  MultiSentCount <- lapply(HamletTextDT$Text,function (x)  lengths(regmatches(x, gregexpr("[a-z|A-Z]+\\n\\t", x))))
}

GetSingleSentCount <- function(HamletTextDT){
  SingleSentCount <- lapply(HamletTextDT$Text, function (x) if (grepl("\\t", x) ) 1 else 0 )
}

HamletsentStats <- function(HamletTextDT,HamletCountList){
  SSentCount <- as.data.table(HamletCountList[1])
  setnames(SSentCount,old=c("V1"),new=c("SSentCount"))
  MSentCount <- as.data.table(HamletCountList[2])
  setnames(MSentCount,old=c("V1"),new=c("MSentCount"))
  Speakers <- transpose(as.data.table(HamletCountList[3][[1]]))
  Speakers$V1 <- removePunctuation(Speakers$V1)
  setnames(Speakers,old=c("V1"),new=c("Speakers"))
  
  MSpeakersCount <- as.data.table(HamletCountList[4])
  setnames(MSpeakersCount,old=c("V1"),new=c("MSpeakersCount"))
  
  StatsDT <- as.data.table(cbind(HamletTextDT,SSentCount,MSentCount,Speakers,MSpeakersCount))
  
  StatsDT$Speakers <- removePunctuation((StatsDT$Speakers))
  StatsDT$V2 <- removePunctuation((StatsDT$V2))
  
  MspeakersDT <- as.data.table(StatsDT[MSpeakersCount>1,c(Speakers,V2)])
  MspeakersDT <- MspeakersDT[,.N,by=c("V1")]
  setnames(MspeakersDT,old=c("V1"),new=c("Speakers"))
  StatsDT$V2 <- NULL
  StatsDT$SSentCount <- as.double(StatsDT$SSentCount)
  StatsDT$MSentCount <- as.double(StatsDT$MSentCount)
  StatsDT$Speakers <- as.character(StatsDT$Speakers)
  
  StatsDT[MSpeakersCount>0, MSentCount:=0]
  StatsDT[MSpeakersCount>0, SSentCount:=0]
  StatsDT[MSpeakersCount>1,Speakers:=""]
  
  
  StatsDT[grepl("SCENE",Speakers),SSentCount:=0]
  StatsDT[grepl("SCENE",Speakers),Speakers:=""]
  
  StatsDT[MSentCount>0,MSentCount:=MSentCount+1]
  StatsDT[,SentSpoken:=max(MSentCount,SSentCount),by=c("Text")]
  
  StatsDT$Speakers <- trimws(StatsDT$Speakers)
  MspeakersDT$Speakers <- trimws(MspeakersDT$Speakers)
  
  StatsDT$SSentCount <- NULL
  StatsDT$MSentCount <- NULL
  StatsDT$MSpeakersCount <- NULL
  
  return(list(StatsDT,MspeakersDT))
}

AggregateCounts <- function(SentStatsList){
  StatsDT <- SentStatsList[[1]]
  MspeakersDT <- SentStatsList[[2]]
  TotalCount <- StatsDT[(Speakers!="" | !is.na(Speakers)),sum(SentSpoken),by=c("Speakers")]
  TotalCount <- merge(TotalCount,MspeakersDT,by= c("Speakers"),all=TRUE)
  TotalCount[is.na(V1),V1:=0]
  TotalCount[is.na(N),N:=0]
  TotalCount[,Total:=V1+N]
  
  TotalCount$V1 <- NULL
  TotalCount$N <- NULL
  TotalCount <- TotalCount[Total>0]
  
  return(TotalCount)
}

PreprocessTextWordCloud <- function(Hamlet,frequency){
  HamletDT <- as.data.table(PreprocessText(Hamlet$text))
  HamletDT$Text <- removePunctuation(HamletDT$Text)
  docs <- Corpus(VectorSource(HamletDT$Text))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "\t")
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, stemDocument, language = "english")
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <-sort(rowSums(m),decreasing=FALSE)
  WordFreq <- data.frame(word = names(v),freq=v)
  WordFreq <-  subset(WordFreq,freq<=frequency)
  return(WordFreq)
}


PreprocessTextSentiment <- function(Hamlet){
  HamletDT <- as.data.table(PreprocessText(Hamlet$text))
  HamletDT$Text <-trimws(gsub("[A-Z]+\t","",HamletDT$Tex))
  HamletDT$Text <- removePunctuation(HamletDT$Text)
  HamletDT$Text <- gsub("\n\t",".",HamletDT$Text)
  HamletDT$Text <- gsub("\n+"," ",HamletDT$Text)
  HamletDT$Text <- gsub("\t+"," ",HamletDT$Text)
  return(HamletDT)
}

GetSceneSentiment <- function(HamletDT){
  
  SentimentList <-lapply(HamletDT$Text,function(x) sentiment(x))
  HamletDT$SentimentScore <- 0.0
  SentIDX <- dim(HamletDT)[2]
  for (i in 1:length(SentimentList)){
    HamletDT[i,SentIDX] <- mean(SentimentList[[i]]$sentiment)
  }
  AverageSceneSentimnet <- HamletDT[!is.na(Scence),mean(SentimentScore),by=c("Scence")]
  setnames(AverageSceneSentimnet,old = c("V1"),new = c("AvgScore"))
  
  return(AverageSceneSentimnet)
}

Task1 <- function(Hamlet){
  HamletDT <- PreprocessText(Hamlet$text)
  MSpeakersCount <-  GetMultiSpeakerCount(HamletDT)
  Speakers <-  GetSpeakers(HamletDT)
  HamletDT$Text <- removePunctuation(HamletDT$Text)
  MSentCount <- GetMultSentCount(HamletDT)
  SSentCount <- GetSingleSentCount(HamletDT)
  HAmletCountList <-  list(SSentCount,MSentCount,Speakers,MSpeakersCount)
  SentStats <- HamletsentStats(HamletDT,HAmletCountList)
  SpeakersLinesCount <- AggregateCounts(SentStats)
  return(SpeakersLinesCount)
}

Task2 <- function(Hamlet,freq){
  WordFreq <- PreprocessTextWordCloud(Hamlet,freq)
  set.seed(1234)
  wordcloud(words = WordFreq$word, freq = WordFreq$freq,
            max.words=200,random.order = FALSE, 
            olors=brewer.pal(8, "Dark2"), scale=c(.8,.2), rot.per=.15)  
}

Task3 <- function(Hamlet){
  SentimentPreProcess <- GetSceneSentiment(PreprocessTextSentiment(Hamlet))
}

Hamlet <- ReadFile("hamlet.txt")
SpeakersLinesCount <- Task1(Hamlet)
p<-ggplot(data=SpeakersLinesCount, aes(x=reorder(Speakers, -Total), y=Total)) +geom_text(aes(label = Total), position=position_dodge(width=0.9), vjust=-1)+
  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(x="Speakers")
p

Task2(Hamlet,1)

SceneSentiment <- Task3(Hamlet)
