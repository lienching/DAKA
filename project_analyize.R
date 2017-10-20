setwd("/mnt/Storage/WorkSpace/Course") # 設定工作目錄

library(curl);   library(xml2)
library(stringr); library(readxl)
library(RCurl);   library(XML);
library(plyr)

articleList = unique(read_excel("articleDF.xlsx"))
attach(articleList)
detach(articleList)
topic = length(unique(articleList$topic)); topic ## 討論串主題

author = length(unique(articleList$author)); author ## 作者
poster = unique(articleList$author[which(articleList$isReply==0)]); length(poster) ## 樓主

Cv = ddply(articleList, c("authorLevel"), summarise,FF = length(topic))
Cv = ddply(articleList, c("topic"), summarise, popular=popular)
arrange(articleList)
sortedData[[100,8]]



table(Cv$author, Cv$FF)
table(Cv$FF)
