setwd("/mnt/Storage/WorkSpace/Course") # 設定工作目錄

library(curl);   library(xml2)
library(stringr); library(readxl)
library(RCurl);   library(XML);
library(plyr)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

articleList = unique(read_excel("articleDF.xlsx"))

# Data Type Convert
articleList$author = trim(articleList$author)
articleList$popular = gsub(",","",articleList$popular)
articleList$popular = as.numeric(articleList$popular)
articleList$postDate = as.POSIXct(articleList$postDate, format="%Y-%m-%d %H:%M")

#作者模型Author Model
authorModel = ddply( articleList, c("author"), summarise, memberLev=max(authorLevel),personalStanding=max(standing),firstPost=min(postDate),lastPost=max(postDate),postNum=length(topic))

#文章模型Article Model
articleModel = ddply( articleList, c("topic"), summarise, popular=max(popular),replyNum=max(replyNo), author=author[1])

