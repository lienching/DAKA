
setwd("/mnt/Storage/WorkSpace/Course") # 設定工作目錄

library(curl);   library(xml2)
library(stringr); library(openxlsx)
library(RCurl);   library(XML)

dataDir =  paste0(getwd(), "/crawling_page/DAKA-H1.dir/")
dataMenu = paste0(getwd(), "/crawling_page/Menu1.csv")


data_size = 1
## Initialization Variables
menu_index = 1

## Looper
repeat {
  ## Article Data Frame Item
  topic = NULL   # 文章標題
  isReply = NULL # 主文/回文
  postDate = NULL # 張貼時間
  content = NULL # 文章內容
  replyNo = NULL # 回覆編號
  authorLevel = NULL # 作者等級( e.g 高級會員, 新進人員 etc...)
  author = NULL # 作者
  popular = NULL # 文章人氣
  standing = NULL # 個人積分
  article_standing = NULL # 文章積分
  articleID = NULL # 回文編號
  item_index = 1
  item_page = 1
  targetDir = str_replace(dataDir, "H1", paste0("H", as.character(menu_index)))
  if ( !dir.exists(targetDir)) {
    break
  }
  
  repeat {
    target = NULL
    exitLoop = FALSE
    targetFile = paste0(targetDir,paste0("DAKA-I", as.character(item_index), "-", as.character(item_page), ".html"))
    
    while( !file.exists(targetFile)) {
      item_index = item_index + 1
      item_page = 1
      if ( item_index > 30) {
        exitLoop = TRUE
        break
      }
      targetFile = paste0(targetDir,paste0("DAKA-I", as.character(item_index), "-", as.character(item_page), ".html"))
    }
    
    if ( exitLoop ) {
      break
    }
    
    print(paste0(">>Parsing ", targetFile) )
    
    target = paste(readLines(targetFile), collapse="\n")
    
    
    targetX = htmlParse(target)
    tmptopic <- unlist(lapply( xpathSApply(targetX, "//main/h1"), saveXML))
    tmptopic <- gsub("<.*?>", "",tmptopic)
    tmpisReply = NULL # 主文/回文
    tmppostDate = NULL # 張貼時間
    tmpcontent = NULL # 文章內容
    tmpreplyNo = NULL # 回覆編號
    tmpauthorLevel = NULL # 作者等級( e.g 高級會員, 新進人員 etc...)
    tmpauthor = NULL # 作者
    tmppopular = NULL # 文章人氣
    tmpstanding = NULL # 個人積分
    tmparticle_standing = NULL # 文章積分
    tmparticleID = NULL # 回文編號
    targetList = unlist(lapply(xpathSApply(targetX, "//main/article/div[@class='single-post']"),saveXML))
    if ( item_page == 1 ) {
      for(k in 1:length(targetList)) {
        tr <- xmlRoot(xmlParse(targetList[k]))
        if ( k == 1 ) {
          tmpisReply = 0
          tmppostDate <- unlist(lapply( xpathSApply(tr, "//div[@class='date']"), saveXML))
          tmppostDate <- gsub("<.*?>", "", tmppostDate)
          tmpreplyNo <- unlist(strsplit(unlist(strsplit(tmppostDate,"\\s+")),"#")[2])[2]
          tmppostDate <- paste0(strsplit(unlist(strsplit(tmppostDate,"\\s+")),"#")[1], " ", unlist(strsplit(unlist(strsplit(tmppostDate,"\\s+")),"#")[2])[1])
          tmppostDate <- gsub("\".*$", "", tmppostDate)
          tmpcontent <- unlist(lapply( xpathSApply(tr, "//div[@class='single-post-content']"), saveXML))
          tmpcontent <- gsub("<.*?>", "", tmpcontent)
          tmpcontent <- gsub("\\n[0-9]*", "", tmpcontent)
          tmparticleID <- unlist(lapply( xpathSApply(tr, "//ul[@class='author-detail']/li/span"),  saveXML))
          tmparticle_standing <-gsub("<.*?>","",tmparticleID[4])
          tmpstanding <- gsub("<.*?>","",tmparticleID[6])
          tmparticleID <- gsub("<.*?>","",tmparticleID[2])
          tmpauthorLevel <- xpathSApply( tr, "//div[@class='fn']/a")
          tmpauthorLevel <- xmlGetAttr(tmpauthorLevel[[1]], 'title')
          tmpauthor <- unlist(lapply( xpathSApply( tr, "//div[@class='fn']/a"),  saveXML))
          tmpauthor <- gsub("<.*?>","",tmpauthor)
          tmppopular <- unlist(lapply( xpathSApply( tr, "//div[@class='info']"),  saveXML))
          tmppopular <- unlist(strsplit(gsub("<.*?>","",tmppopular ), "\\s+"))[2]
        }
        else {
          tmpisReply = 1
          tmppostDate <- unlist(lapply( xpathSApply(tr, "//div[@class='date']"), saveXML))
          tmppostDate <- gsub("<.*?>", "", tmppostDate)
          tmpreplyNo <- unlist(strsplit(unlist(strsplit(tmppostDate,"\\s+")),"#")[2])[2]
          tmppostDate <- paste0(strsplit(unlist(strsplit(tmppostDate,"\\s+")),"#")[1], " ", unlist(strsplit(unlist(strsplit(tmppostDate,"\\s+")),"#")[2])[1])
          tmppostDate <- gsub("\".*$", "", tmppostDate)
          tmpcontent <- unlist(lapply( xpathSApply(tr, "//div[@class='single-post-content']"), saveXML))
          tmpcontent <- gsub("<.*?>", "", tmpcontent)
          tmpcontent <- gsub("\\n[0-9]*", "", tmpcontent)
          tmparticleID <- unlist(lapply( xpathSApply(tr, "//ul[@class='author-detail']/li/span"),  saveXML))
          tmparticle_standing <- -1
          tmpstanding <- gsub("<.*?>","",tmparticleID[4])
          tmparticleID <- gsub("<.*?>","",tmparticleID[2])
          tmpauthorLevel <- xpathSApply( tr, "//div[@class='fn']/a")
          tmpauthorLevel <- xmlGetAttr(tmpauthorLevel[[1]], 'title')
          tmpauthor <- unlist(lapply( xpathSApply( tr, "//div[@class='fn']/a"),  saveXML))
          tmpauthor <- gsub("<.*?>","",tmpauthor)
          tmppopular <- -1
          
        }
        
        topic = c(topic, tmptopic)
        isReply = c(isReply, tmpisReply)
        postDate = c(postDate, tmppostDate)
        content = c(content, tmpcontent)
        replyNo = c(replyNo, tmpreplyNo)
        authorLevel = c(authorLevel, tmpauthorLevel)
        author = c(author, tmpauthor)
        popular = c(popular, tmppopular)
        standing = c(standing, tmpstanding)
        article_standing = c(article_standing, tmparticle_standing)
        articleID = c(articleID, tmparticleID)
      }
    }
    else {
      for(k in 1:length(targetList)) {
        tr <- xmlRoot(xmlParse(targetList[k]))
        tmpisReply = 1
        tmppostDate <- unlist(lapply( xpathSApply(tr, "//div[@class='date']"), saveXML))
        tmppostDate <- gsub("<.*?>", "", tmppostDate)
        tmpreplyNo <- unlist(strsplit(unlist(strsplit(tmppostDate,"\\s+")),"#")[2])[2]
        tmppostDate <- paste0(strsplit(unlist(strsplit(tmppostDate,"\\s+")),"#")[1], " ", unlist(strsplit(unlist(strsplit(tmppostDate,"\\s+")),"#")[2])[1])
        tmppostDate <- gsub("\".*$", "", tmppostDate)
        tmpcontent <- unlist(lapply( xpathSApply(tr, "//div[@class='single-post-content']"), saveXML))
        tmpcontent <- gsub("<.*?>", "", tmpcontent)
        tmpcontent <- gsub("\\n[0-9]*", "", tmpcontent)
        tmparticleID <- unlist(lapply( xpathSApply(tr, "//ul[@class='author-detail']/li/span"),  saveXML))
        tmparticle_standing <- -1
        tmpstanding <- gsub("<.*?>","",tmparticleID[4])
        tmparticleID <- gsub("<.*?>","",tmparticleID[2])
        tmpauthorLevel <- xpathSApply( tr, "//div[@class='fn']/a")
        tmpauthorLevel <- xmlGetAttr(tmpauthorLevel[[1]], 'title')
        tmpauthor <- unlist(lapply( xpathSApply( tr, "//div[@class='fn']/a"),  saveXML))
        tmpauthor <- gsub("<.*?>","",tmpauthor)
        tmppopular <- -1
        
        topic = c(topic, tmptopic)
        isReply = c(isReply, tmpisReply)
        postDate = c(postDate, tmppostDate)
        content = c(content, tmpcontent)
        replyNo = c(replyNo, tmpreplyNo)
        authorLevel = c(authorLevel, tmpauthorLevel)
        author = c(author, tmpauthor)
        popular = c(popular, tmppopular)
        standing = c(standing, tmpstanding)
        article_standing = c(article_standing, tmparticle_standing)
        articleID = c(articleID, tmparticleID) 
      }
    }
    item_page = item_page + 1
  }
  
  menu_index = menu_index + 1;
  resultDF = data.frame(topic, isReply, postDate, content, replyNo, authorLevel, author, popular, standing, article_standing, articleID )
  if ( !file.exists("articleDF.xlsx") ) {
    write.xlsx(resultDF, "articleDF.xlsx", sheetName="Sheet1" ) 
  }
  else {
    wb <- loadWorkbook("articleDF.xlsx")
    writeData(wb, sheet = 1, resultDF, startRow=data_size, colNames = FALSE)
    saveWorkbook(wb,"articleDF.xlsx", overwrite = TRUE)
  }
  data_size = data_size + nrow(resultDF)
}
## End Looper


