setwd("/mnt/Storage/WorkSpace/Course") # 設定工作目錄

library(curl);   library(xml2)
library(stringr); library(readxl)
library(RCurl);   library(XML)
library(plyr); library(igraph)
library(arules); library(data.table)
library(text2vec); library(jiebaR);
library(doMC);library(ggplot2) 
library(gcookbook);library(plotrix)
library(rgr); library(qdap)

registerDoMC()

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

articleList = unique(read_excel("articleDF.xlsx"))

# Data Type Convert
articleList$author = trim(articleList$author)
articleList$popular = gsub(",","",articleList$popular)
articleList$popular = as.numeric(articleList$popular)
articleList$postDate = as.POSIXct(articleList$postDate, format="%Y-%m-%d %H:%M")

#作者模型Author Model
#library(openxlsx)
authorModel = ddply( articleList, c("author"), summarise, memberLev=max(authorLevel),personalStanding=max(standing),firstPost=min(postDate),lastPost=max(postDate),postNum=length(topic),.parallel=TRUE)
#write.xlsx(authorModel, "authorModel.xlsx", sheetName="Sheet1" ) 
#主題模型 Topic Model
topicModel = ddply( articleList, c("topic"), summarise, popular=max(popular),replyNum=max(replyNo), author=author[1], postTime=postDate[1],.parallel=TRUE)
#write.xlsx(topicModel, "topicModel.xlsx", sheetName="Sheet1" ) 
#文章模型 Article Model
articleModel = ddply( articleList, c("content"), summarise, topic=topic,replyNum=replyNo, author=author, postTime=postDate,.parallel=TRUE)
#write.xlsx(articleModel, "articleModel.xlsx", sheetName="Sheet1" ) 

# 主題價值模型
Pv = ddply( articleList, c("topic"), summarise, popular=max(popular), replyNum=max(as.numeric(replyNo)), authorNum=length(unique(author)), firstDate=min(postDate), recentDate=max(postDate), wordCount=wc(paste(content,collapse="")),.parallel=TRUE)
Pv$postNo0 = cut(Pv$replyNum, breaks=c(0,1,9,99,999,7999));   table(Pv$postNo0)
Pv$chCount0 = cut(Pv$wordCount, breaks=c(-1,0,9,99,999,9999,99999,999999));    #-- table(Pv$chCount0)
Pv$nDay = as.Date(Pv$recentDate)-as.Date(Pv$firstDate);   range(Pv$nDay)   #-- 0 3931
Pv$nDay0 = cut(as.numeric(Pv$nDay), breaks=c(-1,0,7,30,100,400,1200,4300));  #-- table(Pv$nDay0)
dim(Pv);   head(Pv,3)
table(Pv$nDay0, Pv$chCount0)

# 作者價值模型
Cv = ddply( articleList, c("author"), summarise, D0=min(postDate), Df=max(postDate), Tcount=length(as.numeric(replyNo)==1), TRCount=length(postDate), chCount=wc(paste(content,collapse="")),.parallel=TRUE)
Cv$Rcount = Cv$TRCount - Cv$Tcount
Cv$TRcount0 = cut(Cv$TRCount,breaks=c(-1,0,1,9,99,999,9999)); 
Cv$chCount0 = cut(Cv$chCount, breaks=c(-1,0,9,99,999,9999,99999,999999));   table(Cv$chCount0)
Cv$nDay = as.Date(Cv$Df)-as.Date(Cv$D0);   range(Cv$nDay)   #-- 0 3886
Cv$nDay0 = cut(as.numeric(Cv$nDay), breaks=c(-1,0,7,30,100,400,1200,4600));   table(Cv$nDay0)
dim(Cv);   head(Cv,3)
table(Cv$nDay0, Cv$chCount0)

#回文
replyandpostFreq = ddply(articleList, c("author"), summarise,FF = length(topic))
table(cut(replyandpostFreq$FF, breaks=c(1,20,40,80,100,200,400,800,1600,3000), dig.lab=5))
#發文
postFreq = ddply(topicModel, c("author"), summarise,FF = length(topic))
table(cut(postFreq$FF, breaks=c(1,2,4,8,16,32,64, 128,200), dig.lab=5))


# 各年主題數
yearTopic = table(format(topicModel$postTime,'%Y'))
yearTopic.df = as.data.frame(yearTopic) 
xx = barplot(yearTopic, xlab="Year", ylab="Number of topic",ylim=c(0,4500))
text(x = xx, y=yearTopic.df$Freq, label = yearTopic.df$Freq, pos = 3, cex = 0.8,  col="red")

# Article Year Over Month
table(format(articleModel$postTime,'%Y'),format(articleModel$postTime,'%m'))

# Topic Year Over Month
table(format(topicModel$postTime,'%Y'),format(topicModel$postTime,'%m'))

# 各月主題數
monthoTopic = table(format(topicModel$postTime,'%m'))

# 各年新作者
yearAuthor = table(format(authorModel$firstPost,'%Y'))
yearAuthor.df = as.data.frame(yearAuthor)

ggplot(yearAuthor.df, aes(x = Var1, y = as.integer(Freq),group=1)) + geom_line() + geom_point(size = 3, shape = 22, colour = "darkred", fill = "yellow") + geom_text(aes(label = Freq),position = position_dodge(0.9),vjust = -1) + ylim(10, max(yearAuthor.df$Freq))+
  labs(x = "Year", 
       y = "New Author")

#品牌分析( Topic )
Xtext = str_replace_all( paste(topicModel$topic ,collapse=""),"\n","" )  #-- 合併為一字串，並移除換行碼
cutter = worker()   #-- cutter 為jiebaR的工作引擎
Xword   = segment(Xtext, cutter)       #-- Xword: 用jieba切出詞語
ncharXW = sapply(1:length(Xword), function (k) nchar(Xword[k]))  
Xword2  = Xword[which(ncharXW>=2)]     #-- Xword2: 選出2個字以上(長度大於等於2)的詞語
bands = c("NVIDIA", "AMD")
bands_keyword_count = c(length(Xword2[which(tolower(Xword2) == "nvidia")]), length((Xword2[which(tolower(Xword2) == "ati")])) + length((Xword2[which(tolower(Xword2) == "amd")])))
band_keyword_comp = data.frame(bands, bands_count)
nvidia_count <- 0
amd_count <- 0
for ( t in Xword2[which(tolower(Xword2) == "nvidia")] ){
  nvidia_count = nvidia_count + length(grep(t, topicModel$topic))
}

for ( t in Xword2[which(tolower(Xword2) == "amd")] ){
  amd_count = amd_count + length(grep(t, topicModel$topic))
}

for ( t in Xword2[which(tolower(Xword2) == "ati")] ){
  amd_count = amd_count + length(grep(t, topicModel$topic))
}

#關聯圖
Xtext = str_replace_all( paste(articleModel$content ,collapse=""),"\n","" )  #-- 合併為一字串，並移除換行碼
cutter = worker()   #-- cutter 為jiebaR的工作引擎
Xword   = segment(Xtext, cutter)       #-- Xword: 用jieba切出詞語
ncharXW = sapply(1:length(Xword), function (k) nchar(Xword[k]))  
Xword3  = Xword[which(ncharXW>=3)]     #-- Xword2: 選出3個字以上(長度大於等於2)的詞語
setDT(list(Xword3))
trans <- as(strsplit(Xword3, " "), "transactions")
summary(trans)
itemLabels(trans)
rulesSp = apriori( trans, parameter = list(supp=0.005, conf=0.008))
#detach(package:tm, unload=TRUE)
inspect(rulesSp)  
library(igraph)  #-- 社群網路分析(Social Network Analysis, SNA) 的強大軟件包
rulesSP.df = data.frame( lhs=labels(lhs(rulesSp)), rhs=labels(rhs(rulesSp)), rulesSp@quality)
gR = graph.edgelist(cbind(as.character(rulesSP.df$lhs),as.character(rulesSP.df$rhs)))
E.gR = format(as.numeric(rulesSP.df$confidence), digits=2)
plot(gR, edge.curved=0.7, edge.label=E.gR, edge.label.color="#FF5555", edge.arrow.size=0.1)

# 詞庫詞性
wkr = worker(type="tag")
term = segment(topicModel$topic, wkr)
nterm = unique(term[grep("[n]+",names(term))]);   length(nterm); nterm
# write.table(nterm,"/mnt/Storage/WorkSpace/Course/video0.txt",quote=FALSE, row.names=FALSE, col.names=FALSE )
aterm = unique(term[grep("[a]+",names(term))]);   length(aterm); aterm
# write.table(aterm,"/mnt/Storage/WorkSpace/Course/aterm.txt",quote=FALSE, row.names=FALSE, col.names=FALSE )

# 關鍵字頻率
Fword2 = freq(Xword2)
sorted_Fword2 = Fword2[order(Fword2$freq, decreasing = TRUE),]
library(arulesViz)
plot(rulesSp, method = "graph",control = list(type="rulesSp", main="",cex=2, nodeCol =c("green")))
# 會員等級發文關鍵字

# 發文時間
table(format(topicModel$postTime,'%A')) # 主題(星期)
table(format(articleModel$postTime,'%A')) # 文章(星期)

table(format(topicModel$postTime,'%m')) # 主題(月份)
table(format(articleModel$postTime,'%m')) # 主題(月份)

# 型號與時間
keywordYear <- function(year) {
  Xtext = str_replace_all( paste(topicModel[which(factor(format(topicModel$postTime,'%Y'))== year),]$topic ,collapse=""),"\n","" )  #-- 合併為一字串，並移除換行碼
  library(jiebaR);   cutter = worker()   #-- cutter 為jiebaR的工作引擎
  Xword   = segment(Xtext, cutter)       #-- Xword: 用jieba切出詞語
  ncharXW = sapply(1:length(Xword), function (k) nchar(Xword[k]))  
  Xword2  = Xword[which(ncharXW>=2)]     #-- Xword2: 選出2個字以上(長度大於等於2)的詞語
  Fword2 = freq(Xword2)
  sort_Fword2 = Fword2[order(Fword2$freq,decreasing = TRUE),]; sort_Fword2
  write.table(sort_Fword2,paste0("/mnt/Storage/WorkSpace/Course/",year,"_keyword.txt"),quote=FALSE, row.names=FALSE, col.names=FALSE )
}

for(i in 2004:2017) {
  keywordYear(i)
}

# 風向羅盤(Not Work)
plotWindCompass <- function (content,Wkey,gMat,aMat,pPower,nPower,topic) {
  library(plotrix)
  par( family="STKaiti" )
  plot(x=NULL, y=NULL, xlim=c(-5,5), ylim=c(-5,5), 
       main=paste0(topic," 網路風向羅盤圖 ","(貼文數=",as.character(length(content)),")"), 
       xlab="",  ylab="")
  abline(v=0);         abline(h=0);                
  draw.circle(0,0,  1,lty=2,border="orange");     
  draw.circle(0,0,  2,lty=2,border="orange");     
  # draw.circle(0,0, 2.5,lty=1,border="brown")
  draw.circle(0,0,  3,lty=1,border="brown");     
  draw.circle(0,0,  4,lty=2,border="orange")
  draw.circle(0,0,  5,lty=2,border="orange")
  quadText <- c("應用", "品牌/型號", "記憶體", "週邊")
  for (j in 1:4) text( 5.5*cos(2*pi*(j-0.5)/4), 5.5*sin(2*pi*(j-0.5)/4), quadText[j], col="brown" )
  for (j in 1:4) {
    dt = 90/(length(Wkey[[j]])+1) 
    for (i in 1:length(Wkey[[j]])) {
      theta = ((j-1)*90+dt*i) * pi / 180;       
      kContent = grep(gMat[which(gMat!=""),Wkey[[j]][i]], content)
      print(gMat[,Wkey[[j]][i]!=""])
      if (length(kContent)!=0) {
        rPos = max(as.numeric(pPower[kContent]),na.rm=T)
        if (rPos > 0.5) {
          boxed.labels( (3+2*rPos)*cos(theta), (3+2*rPos)*sin(theta), labels=Wkey[[j]][i], bg="#FFFFFF", col="blue", cex=0.9, xpad=0.2, ypad=0.4, border=FALSE)
        } else {
          boxed.labels( (3+2*rPos)*cos(theta), (3+2*rPos)*sin(theta), labels=Wkey[[j]][i], bg="#FFFFFF", col="green", cex=0.9, xpad=0.2, ypad=0.4, border=FALSE)
        }   
        rNeg = max(as.numeric(nPower[kContent]),na.rm=T)
        if (rNeg < 0.5) {
          boxed.labels( (3-2*rNeg)*cos(theta), (3-2*rNeg)*sin(theta), labels=Wkey[[j]][i], bg="#FFFFFF", col="orange", cex=0.9, xpad=0.2, ypad=0.4, border=FALSE)
        } else {
          boxed.labels( (3-2*rNeg)*cos(theta), (3-2*rNeg)*sin(theta), labels=Wkey[[j]][i], bg="#FFFFFF", col="red", cex=0.9, xpad=0.2, ypad=0.4, border=FALSE)
        }   
        lines( c( (2*(rPos+1)+1)*cos(theta), (2*(1-rNeg)+1)*cos(theta)), c( (2*(rPos+1)+1)*sin(theta), (2*(1-rNeg)+1)*sin(theta)))
      }
    }  
  }
}


wkGraphic = worker(user="/mnt/Storage/WorkSpace/Course/video0.txt", type="tag")
wkAdj = worker(user="/mnt/Storage/WorkSpace/Course/aterm.txt", type="tag")
graphicList = c("nMem", "nBand", "nExtend", "nModel", "nPort", "nSoftware", "nCardType")
graphicDegree = c(0,25,50,75,100,  120,150,180,  210,240,270,  300,330)
adjList = c("ap","a0","an")
graphicMat = matrix(0, nrow=dim(topicModel)[1], ncol=length(graphicList))
adjMat   = matrix(0, nrow=dim(topicModel)[1], ncol=length(adjList))

for (i in 1:length(topicModel$topic)) { 
  graphicKW = segment(topicModel$topic[i], wkGraphic);
  for (j in 1:length(graphicList)) { IND = grep(graphicList[j], names(graphicKW)); graphicMat[i,j] = paste(graphicKW[IND],collapse="/") }
  adjKW = segment(topicModel$topic[i], wkAdj)
  for (j in 1:length(adjList)) { IND = grep(adjList[j], names(adjKW)); adjMat[i,j] = paste(adjKW[IND],collapse="/") }
}

colnames(graphicMat) = graphicList;  colnames(adjMat) = adjList

Wkey = list(c("nSoftware", "nCardType"),c("nBand", "nModel"), "nMem", c("nExtend", "nPort")) #, c("nMain","nVer","nRank","nOS")
length(adjMat[1:333,1])
plotWindCompass(topicModel$topic,Wkey,graphicMat,adjMat,as.numeric(adjMat[1:333,1]!=""),as.numeric(adjMat[1:333,3]!=""),"mobile01顯示卡")
