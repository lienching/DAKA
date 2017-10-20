#install.packages("curl") 
#install.packages("xml2")
#install.packages("stringr")

setwd("/mnt/Storage/WorkSpace/Course") # 設定工作目錄

library(curl);   library(xml2)
library(stringr); library(xlsx)
library(RCurl);   library(XML)

url = "https://www.mobile01.com/topiclist.php?f=298&p=1"
dataDir = paste0(getwd(), "/crawling_page/")

for(k in 841:934) { # 1 ~ 100 頁 (Max:934)
  urlH = str_replace(url, "=1", paste0("=", as.character(k))) # 當前要爬的網址
  print(paste0(">> Crawling for page - ", k, " with urlH = ", urlH, "...")) # 提示訊息
  Hk <- read_html(curl(urlH, handle = new_handle("useragent" = "Mozilla/5.0"))) # 將檔案解析後放入 Hk ( handler 用來解決 SSL 問題)
  as.character(Hk) # Hk 型態轉換
  menuFile = paste0(dataDir, paste0("DAKA-H", as.character(k),".html"))
  write(as.character(Hk), menuFile) # 寫檔
  artDir = paste0(dataDir, paste0("DAKA-H", as.character(k),".dir/"))
  if ( !dir.exists(artDir)) {
    dir.create(artDir)
  }
  target = paste(readLines(menuFile), collapse="\n")
  targetX = htmlParse(target)
  targetList = xpathSApply(targetX, "//table/tbody/tr")
  targetXList = unlist(lapply(targetList, saveXML))
  
  ## For TA ( 2017/09/27) : make discussion list in to dataframe
  menuList = NULL
  topics = NULL
  reply_nums = NULL
  authors = NULL
  author_post_times = NULL
  new_respon_times = NULL
  new_respon_users = NULL
  for ( i in 1:length(targetXList)) {
    tr <- xmlRoot(xmlParse(targetXList[i]))
    topic <- unlist(lapply( xpathSApply(tr,"//td[@class='subject']/span/a"), saveXML))
    topic <- gsub("<.*?>", "",topic)
    topics <- c(topics, topic)
    reply_num <- unlist(lapply( xpathSApply(tr,"//td[@class='reply']"), saveXML))
    reply_num <- gsub("<.*?>", "", reply_num)
    reply_nums <- c(reply_nums, reply_num)
    author <- lapply( xpathSApply(tr,"//td[@class='authur']/a/p"), saveXML)
    author_post_time <- gsub("<.*?>", "", author[1])
    author_post_times <- c(author_post_times, author_post_time)
    author <- gsub("<.*?>", "", author[2])
    authors <- c(authors, author)
    news_respon <- lapply( xpathSApply(tr,"//td[@class='latestreply']/a/p"), saveXML)
    news_respon_time <- gsub("<.*?>", "", news_respon[1])
    new_respon_times <- c(new_respon_times, news_respon_time)
    news_respon_user <- gsub("<.*?>", "", news_respon[2])
    new_respon_users <- c(new_respon_users, news_respon_user)
  }
  
  menuList = data.frame(topics, reply_nums, authors, author_post_times, new_respon_users, new_respon_times)
  write.csv(menuList, paste0(dataDir,"Menu",k,".csv"))
  ##
  
  href = NULL
  for ( i in 1:length(targetXList)) {
    tr <- xmlRoot(xmlParse(targetXList[i]))
    a <- xpathSApply(tr, "//a")
    tmp <- xmlGetAttr(a[[1]], 'href')
    href = c(href, paste0("https://www.mobile01.com/",tmp))
  }
  
  for( j in 1: length(href)) {
    print(paste0(">>>> Crawling for page - ", k, ", item ", j, " - 1 with href = ", href[j], "...")) # 提示訊息
    item_Hk <- read_html(curl(href[j], handle = new_handle("useragent" = "Mozilla/5.0"))) # 將檔案解析後放入 item_Hk ( handler 用來解決 SSL 問題)
    itemFile = paste0(artDir, "DAKA-I", as.character(j), "-1.html" )
    write(as.character(item_Hk), itemFile)
    target = paste(readLines(itemFile), collapse="\n")
    targetX = htmlParse(target)
    present_page = 1
    
    haveNext = TRUE
    while(haveNext) {
      tryHk <- read_html(curl(paste0(href[j], "&p=", present_page + 1), handle = new_handle("useragent" = "Mozilla/5.0")))
      itemFile = paste0(artDir, "DAKA-I", as.character(j), "-", present_page + 1,".html" )
      print(paste0(">>>> Try crawling for page - ", k, ", item ", j, " - ", present_page + 1," with href = ", paste0(href[j], "&p=", present_page + 1), "...")) # 提示訊息
      targetX = htmlParse(tryHk) 
      targetNum = xpathSApply(targetX, "//div[@class='contentfoot']/p[@class='numbers']/span") 
      targetNum <- unlist(lapply( targetNum, saveXML))
      targetNum <- gsub("<.*?>","",targetNum)
      if ( as.integer(targetNum) > present_page ) {
        present_page = present_page + 1;
        write(as.character(tryHk), itemFile)
      }
      else {
        haveNext = FALSE
      }
      Sys.sleep(1)
    }
    
    Sys.sleep(1)
  }
  
  Sys.sleep(1) # 休息 1 秒，避免觸發網站反爬蟲機制
}
