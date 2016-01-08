#创建于2014-06-30
# zhouwy


#预处理数据
preprocessData2 <- function(data) {
  
  #1 过滤    
  # "intrial", "intrial2", "ucrelease", "ucpatch", "inrelease2", "inrelease3", "ucrelease2", "ucrelease3"
  #  "9.8.5.442","9.7.8.425", "9.8.9.457"
  ver_t <- c("9.8.9.457")
  subVer_t <- c("ucrelease")  
  langs_t <- c("en-us", "zh-cn")
  fields_t <-c('phit','prqst','bf','ccf','nch',
               'unm','ihmt','add','dft','dcwr',
               'delt','mctt','umt','mtc','mhit',
               'cct','mtt','tt','fne','nc',
               'cos','cfs','cis','cjs','ccs',
               'co','cf','ci','cj','cc',
               'CC','nlc','nlt','clt','qit',
               'qst','qdt','qut','F','C',
               'J','I','O','h3','hc',
               'hj','hi','hf','ho','T0',
               'T1','T2','chs','jhs','ihs',
               'fhs','ohs','P','pd','pvs',
               'hit0to3','hit3to7','hit7to8','hit8to10','hit10to12')
    
  #数据过滤  
  data <- subset(data, (data$ver %in% ver_t) & (data$subver %in% subVer_t) & (data$lang %in% langs_t))
  
  if (nrow(data) == 0)
    return(data)
  #2 数据格式化
  for(f in fields_t){
    data[c(f)]<-as.numeric(as.matrix(data[c(f)]))
    data[c(f)][is.na(data[c(f)]),1]<-0
  }
  
  #3 添加字段
  data$hit <- data$h3+data$hc+data$hj+data$hi+data$hf+data$ho;
  data$request <- data$C+data$J+data$I+data$O+data$F;
  
  #data$ver_subver <- paste(as.character(data$ver), as.character(data$subver))  
  #data <- subset(data, data$request>0)  
  #data$hit_rate <- round(data$hit/data$request*100, 2)  
  #data$fne_rate <- round(data$file_not_exist_count/data$request*100, 2)  
  
  return (data)
}

httpRate <- function(file) {
  
  data <- read.table(file, sep='\t', header=T)
  
  data <- preprocessData2(data)
  
  print(sum(data$hit)/sum(data$request))
  return (data)
}


#预处理数据
preprocessData <- function(data) {
  
  #1 过滤    
  # "intrial", "intrial2", "ucrelease", "ucpatch", "inrelease2", "inrelease3", "ucrelease2", "ucrelease3"
  #  "9.8.5.442","9.7.8.425","ucrelease","ucrelease2","uctrial2","uctrial3", "uctrialmtopt", "ucpatch"
  # "inrelease", "inrelease2", "9.8.0.435", "9.7.5.418",
  #"9.8.5.442","9.8.9.457", "9.9.0.459", "9.8.0.435", "9.7.8.425", "9.9.0.459", "9.8.0.435", "9.7.8.425"
  #"9.9.0.459", "9.8.9.457", 
  #"9.9.2.467", "9.8.9.457", "9.9.0.459", , "9.9.0.459", "9.9.3.478", "9.8.9.457"
  #, "9.9.5.489", "9.9.6.495" "9.9.4.484", "9.9.3.478", "9.9.7.500"
  # "10.0.0.488", "10.0.1.512", "10.0.2.523"
  # '9.9.8.511', '10.0.0.488', '9.9.7.500'
  ver_t <- c("9.9.2.467")
  #,  "9.9.0.459", "9.8.0.435", "9.7.8.425", "9.9.0.459", ")
  #"inapprelease3", "inapprelease2", "inapprelease", "inapprelease4", "ucrelease"
  #"inrelease", "inrelease2", "inrelease3", "inapprelease3", "inapprelease2", "inapprelease", "inapprelease4"
  #"ucpatch", "ucrelease","ucrelease2", "ucrelease3", "inapprelease"
  #"uctrial2", "ucrelease2", "uctrialjslopt"
  #"uctrialcco", "intrialcco", 
  #, "uctrial", "ucrelease", "inapprelease", "uctrial2", "ucrelease2"
  #c("inapprelease","inrelease", "ucrelease", "uctrialcco", "intrialcco","uctrial", "intrial3", "uctrial2", "intrial", "intrial2", "uctrial3", "uctrial4")
  subVer_t <- c('inprerelease', 'intrialcco')
  
  userMin_t <- 1000
  langs_t <- c( "en-us")
  fields_t <-c('css','js','image','other','mainframe1','mainframe2','subframe','X3xx_hit','css_hit',
               'js_hit','image_hit','frame_hit','other_hit','users', 't0', 't1', 't2', 'cc','fne',
               'phit', 'prqst', 'nlc')
  
  date_min <- min(as.character(data$pt))
  #date_min <- c("2014-09-20")
  date_max <- max(as.character(data$pt))
  #date_max <- c("2014-07-11")
  #数据过滤  
  data <- subset(data, (data$ver %in% ver_t) & (data$subver %in% subVer_t) & (data$lang %in% langs_t) &
                   (as.numeric(data$users)>=userMin_t) & (as.character(data$pt)>=date_min) & 
                   (as.character(data$pt)<=date_max) )
  
  
  #其它过滤
  #data <- subset(data, !((data$ver %in% c("9.8.0.435")) & (data$subver %in% c("uctrial"))))
  
  if (nrow(data) == 0)
    return(data)
  #return (data)
  #2 数据格式化
  for(f in fields_t){
    #print(f)
    data[c(f)]<-as.numeric(as.matrix(data[c(f)]))
    data[c(f)][is.na(data[c(f)]),1]<-0
  }
  
  #3 添加字段
  #data$X3xx_hit+
  data$hit <- data$X3xx_hit+data$css_hit+data$js_hit+data$image_hit+data$frame_hit+data$other_hit;
  data$request <- data$css+data$js+data$image+data$other+data$mainframe1+data$mainframe2+data$subframe;
  
  data$ver_subver <- paste(as.character(data$ver), as.character(data$subver))  
  data <- subset(data, data$request>0)  
  data$hit_rate <- round(data$hit/data$request*100, 2)
  
  data$fne_rate <- round(data$fne/data$request*100, 2)
 
  data$pt <- formatDate(data)
  return (data)
}

formatDate <- function(data) {
  s1 <- data$pt
  s2 <- strsplit(as.character(s1), '-')
  v <- c()
  for (i in 1:length(s2)) {
    s <- unlist(s2[i])
    v <- c(v, paste(s[2], s[3], sep='/'))    
  }
  return (v)
}

showGraph <- function(data) {
  #按版本显示命中率
  require("ggplot2")    
  #命中率
  #facet_grid(ver~.)+
  #pch=ver_subver, lty=ver_subver
  sp <- qplot(pt, hit_rate, data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", 
              ylab="命中率", color=ver_subver, main="命中率走势图")+facet_grid(.~lang)
  #+
  #            theme(axis.text.x=element_text(angle=45, hjust=1, size=9, colour="black"))
  #  scale_y_continuous(limits=c(32.6,36),breaks=seq(30,36,0.2))
  print(sp)
  
  
  dd <- data.frame(data$hit_rate)
  dd$pt <- data$pt
  
  #return (data)
  #scale_x_continuous(limits=c(1950,2000),breaks=seq(1950,2000,5)) +
  #  theme(axis.text.x=theme_text(angle=45,size=8))
  #p <- ggplot(data, aes(pt, hit_rate, group=interaction(ver,subver), colour=interaction(ver,subver)))
  #p <- p + geom_line()+geom_point()
  #p <- p + geom_line(aes(group=1))
  #print(p)
  
  #用户数
  sp <- qplot(pt, users, data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", ylab="用户数", colour=ver_subver, main="用户数走势图")+facet_grid(.~lang)
  print(sp)
  
  
  #return (data)
  #data <- subset(data, data$t0<100 & data$t0>0 & data$t1>0 & data$t1<100 & data$t2<100)
  #缓存文件数
  sp <- qplot(pt, cc, data=data, geom=c("line"), group=ver_subver, xlab="日期", ylab="缓存文件数", 
              colour=ver_subver)+facet_grid(.~lang)
  print(sp)
  #dd$cc = round(data$cc, 0)
  #缓存大小
  sp <- qplot(pt, t0+t1+t2, data=data, geom=c("line"), group=ver_subver, xlab="日期", ylab="缓存大小", 
              colour=ver_subver)+facet_grid(.~lang)
  print(sp)
  #dd$T <- round(data$t0+data$t1+data$t2, 2)
  #文件缺失比例
  sp <- qplot(pt, fne_rate, data=data, geom=c("line"), group=ver_subver, xlab="日期", ylab="文件缺失占比", 
              colour=ver_subver)
  print(sp)
  #dd$ver <- data$ver
  #dd$subver <- data$subver
  #文件缺失与命中率
  #sp <- qplot(fne_rate,hit_rate,  data=data, geom=c("point", "line"), group=ver_subver, xlab="文件缺失", ylab="命中率", 
  #            colour=ver_subver)
  #print(sp)
  
  #中转请求与命中
  sp <- qplot(pt, phit/request*100,  data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", ylab="中转命中比例", 
              colour=ver_subver)+facet_grid(.~lang)
  print(sp)
  sp <- qplot(pt, prqst/request*100,  data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", ylab="中转请求比例", 
              colour=ver_subver)+facet_grid(.~lang)
  print(sp)
  sp <- qplot(pt, phit/prqst*100,  data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", ylab="中转命中率", 
              colour=ver_subver)+facet_grid(.~lang)
  print(sp)
  #dd$phit_rate<- round(data$phit/data$prqst*100, 1)
  sp <- qplot(pt, (hit-phit)/(request-prqst)*100,  data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", ylab="直连命中率", 
              colour=ver_subver)+facet_grid(.~lang)
  print(sp)
  #dd$dhit_rate <- round((data$hit-data$phit)/(data$request-data$prqst)*100, 1)
  #网络加载次数
  #sp <- qplot(pt, nlc/request,  data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", ylab="网络加载占比", 
  #            colour=ver_subver)
  #print(sp)
  
 # sp <- qplot(pt, bf/request,  data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", ylab="大文件未入缓存占比", 
 #             colour=ver_subver)
 # print(sp)
 # sp <- qplot(pt, nch/request,  data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", ylab="符合缓存规则未入缓存占比", 
 #             colour=ver_subver)
  #print(sp)
  
  #sp <- qplot(pt, nlt,  data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", ylab="网络平均加载时间", 
  #            colour=ver_subver)
  #print(sp)
  sp <- qplot(pt, clt,  data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", ylab="缓存平均加载时间", 
              colour=ver_subver)
  print(sp)
  #sp <- qplot(pt, X3xx_hit*100/hit,  data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", ylab="网络平均加载时间", 
  #            colour=ver_subver)
  #print(sp)
  return (data)
  
}

showHttpCacheSummaryData <- function(file) {
  data <- read.table(file, sep=",", header=T)  
  
  
  data <- preprocessData(data)
  
  if (nrow(data) == 0)
    return (data)
  data <- showGraph(data)
  
  return (data)
}
showRelationshipOfCCAndHitRate <- function(file) {
  data <- read.table(file, sep=",", header=T)
  #1 过滤    
  ver_t <- c("9.8.1.447")
  subVer_t <- c("ucrelease3")      
  langs_t <- c("zh-cn")
  fields_t <-c('css','js','image','other','mainframe1','mainframe2','subframe','X3xx_hit','css_hit',
               'js_hit','image_hit','frame_hit','other_hit','cc')
  
  #数据过滤  
  data <- subset(data, (data$ver %in% ver_t) & (data$subver %in% subVer_t) &
                   as.numeric(data$cc)<30000 )
  
  
  if (nrow(data) == 0)
    return(data)
  #2 数据格式化
  for(f in fields_t){
    data[c(f)]<-as.numeric(as.matrix(data[c(f)]))
    data[c(f)][is.na(data[c(f)]),1]<-0
  }
    
  #3 添加字段
  data$hit <- data$X3xx_hit+data$css_hit+data$js_hit+data$image_hit+data$frame_hit+data$other_hit;
  data$request <- data$css+data$js+data$image+data$other+data$mainframe1+data$mainframe2+data$subframe;
  
  data$ver_subver <- paste(as.character(data$ver), as.character(data$subver))  
  data <- subset(data, data$request>0)  
    
  require("plyr")
  data <- ddply(data, "sn", summarize, hit_rate=sum(hit)/sum(request), cc=mean(cc), ver=ver)
  data$cc <- round(data$cc/10)*10
  data <- ddply(data, "cc", summarize, hit_rate=mean(hit_rate))
    
  require("ggplot2")    
  sp <- qplot(cc, hit_rate, data=data, geom=c("point", "smooth"),  xlab="文件数", ylab="命中率", main="文件数与命中率")  
  print(sp)
  
  return (data)
 
}

#vm wa 曲线
vmstat <- function(file980, file985) {
  dd1 <- read.table(file980, sep='\t', header=T)
  dd2 <- read.table(file985, sep='\t', header=T)
  
  plot(dd1$wa, xlim=c(0, 150), ylab="io wait time", ylim=c(0, 100), type="l")
  lines(dd2$wa, type="l", lty=2)
}

showPerf <- function(file) {
  data <- read.table(file, sep=",", header=T)
  #1 过滤    
  ver_t <- c("9.7.8.425")
  subVer_t <- c("ucrelease2")      
  langs_t <- c("zh-cn")
  fields_t <-c('clt','qit', 'qst', 'qdt','qut')
  
  #数据过滤  
  data <- subset(data, (data$ver %in% ver_t) & (data$subver %in% subVer_t) )
  
  
  if (nrow(data) == 0)
    return(data)
  #2 数据格式化
  for(f in fields_t){
    data[c(f)]<-as.numeric(as.matrix(data[c(f)]))
    data[c(f)][is.na(data[c(f)]),1]<-0
  }
  require("ggplot2")    
  thisTitle = genTitle(ver_t, subVer_t)
  sp <- qplot(pt, clt, data=data, geom=c("point", "smooth"), group="clt", ylim=c(0, 100), xlab="日期", ylab="缓存平均加载时间", main=thisTitle)  
  print(sp)
  
  sp <- qplot(pt, qit, data=data, geom=c("point", "smooth"), group="qit", ylim=c(0, 20), xlab="日期", ylab="insert平均时间", main=thisTitle)  
  print(sp)
  
  sp <- qplot(pt, qst, data=data, geom=c("point", "smooth"), group="qst", ylim=c(0, 20), xlab="日期", ylab="select平均时间", main=thisTitle)  
  print(sp)
  
  sp <- qplot(pt, qut, data=data, geom=c("point", "smooth"), group="qut", ylim=c(0, 10), xlab="日期", ylab="update平均时间", main=thisTitle)  
  print(sp)
  
  sp <- qplot(pt, qdt, data=data, geom=c("point", "smooth"), group="qdt", ylim=c(0, 5), xlab="日期", ylab="delete平均时间", main=thisTitle)  
  print(sp)
}

genTitle <- function(ver_t, subver_t) {
  subTitle_t <- "("  
  for (x in ver_t) {
    subTitle_t <- paste(subTitle_t, x, sep=" ")
  }
  subTitle_t <- paste(subTitle_t, " )(", sep="")
  for (x in subver_t) {
    subTitle_t <- paste(subTitle_t, x, sep=" ")
  }
  subTitle_t <- paste(subTitle_t, " )", sep="")
  return (subTitle_t)
}

#统计用户数
userStat <- function(file) {
  #1 过滤      
  #userMin_t <- 500000
  langs_t <- c("zh-cn", "en-us", "es-la", "zh-tw", "vi", "pt-br", "ru", "id")
  fields_t <-c('users')
  
  data <- read.table(file, sep=",", header=T)
  #date_min <- min(as.character(data$pt))
  date_min <- c("2014-07-01")
  date_max <- max(as.character(data$pt))
  #date_max <- c("2014-07-03")
  #数据过滤  
  
  data <- subset(data, (data$lang %in% langs_t) & (as.character(data$pt)>=date_min) & 
                   (as.character(data$pt)<=date_max) )
  
  if (nrow(data) == 0)
    return(data)
  #2 数据格式化
  for(f in fields_t){
    data[c(f)]<-as.numeric(as.matrix(data[c(f)]))
    data[c(f)][is.na(data[c(f)]),1]<-0
  }
  
  require("ggplot2")
  require("plyr")
  #3 添加字段  
  data$ver_subver <- paste(as.character(data$ver), as.character(data$subver))
  data <- data[order(data$pt), ]
  data <- ddply(data, c("pt"), summarize, users=sum(users))
  sp <- qplot(pt, users, data=data, geom=c("point", "smooth"), xlab="日期", ylab="各版本用户数", group=1)  
  
  
  #data <- ddply(data, c("pt", "lang"), summarize, users=sum(users))
  #sp <- qplot(pt, users, data=data, geom=c("point", "line"), group=lang, colour=lang, 
  #                        xlab="日期", ylab="中文版用户数")
  print(sp)
  
  #return (data)
  
}

clientCache <- function(file) {
  data <- read.table(file, sep=",", header=T)
  require("ggplot2")
  
  #sp <- qplot(order, init, data=data, geom=c("point", "line"), group=phone, ylim=c(0, 2500), xlim=c(0, 12), ylab="中转缓存初始化耗时", colour=phone)  
  #print(sp)
  #sp <- qplot(order, update, data=data, geom=c("point", "line"), group=phone, ylim=c(0, 1200), xlim=c(0, 12),ylab="整理文件耗时", colour=phone)  
  #print(sp)
  #sp <- qplot(order, size, data=data, geom=c("point", "line"), group=version,  ylab="历史数据文件size", colour="black")  
  #print(sp)
  #plot(data$size2/1000, type="l", ylim=c(0, 2), ann=FALSE)
  #lines(data$size1/1000, type="l", lty=2)
  #title(ylab= "上传的资源列表大小")
  #sp <- qplot(query, data=data, group=website, linetype=website, geom=c("point", "line"), ylab="查询次数", main="原方案查询存储后端次数变化趋势")
  #sp <- qplot(query, ..density.., data=data,linetype=website)
  #sp <- qplot(order, size, data=data, group=version, linetype=version, ylab="上传资源列表大小(B)", main="访问新浪网数据")+geom_line()
  #sp <- qplot(order, size/1000, data=data, group=version, colour=I("red"), linetype=version, shape=version, ylab="文件大小（KB）", main="存储拓扑关系的数据文件大小", facets=order~version)+geom_line(colour=I("red"))
  sp <- ggplot(data, aes(order, query, colour=website))+geom_line()+geom_point()
  
  sp
  data2 <- transform(data, query = query+10)
  sp %+% data2
  
  #print(sp)
  #sp <- ggplot(data, )
  
  
}

test <- function() {
  #xgrid <- width(df, seq(min(x), max(x), length=50))
  #interp <- data.frame(x=xgrid, y=approx(df$x, df$y, df))
}

versionHitRate <- function(file) {
  
  data <- read.table(file, sep=",", header=T)
  #1 过滤    
  # "intrial", "intrial2", "ucrelease", "ucpatch", "inrelease2", "inrelease3", "ucrelease2", "ucrelease3"
  #  "9.8.5.442","9.7.8.425","ucrelease","ucrelease2","uctrial2","uctrial3", "uctrialmtopt", "ucpatch"
  # "inrelease", "inrelease2", "9.8.0.435", "9.7.5.418",
  #"9.8.5.442","9.8.9.457", "9.9.0.459", "9.8.0.435", "9.7.8.425", "9.9.0.459", "9.8.0.435", "9.7.8.425"
  #"9.9.0.459", "9.8.9.457", 
  #"9.9.2.467", "9.8.9.457", "9.9.0.459", 
  #, "9.7.8.425", "9.8.0.435", 
  #"9.7.5.418", "9.8.5.442", "9.7.6.428", "9.6.0.378"
  #"9.6.3.413", "9.9.2.467", "9.9.0.459", "9.9.3.478", "9.9.2.479", "9.8.9.457", "9.7.5.418", "9.7.6.428"
  ver_t <- c( "9.6.3.413", "9.9.0.459", "9.8.9.457", "9.9.2.479", "9.9.5.489", "9.9.6.495", "9.9.4.484", "9.9.3.478", "9.9.7.500")
  #,  "9.9.0.459", "9.8.0.435", "9.7.8.425", "9.9.0.459")
  #"inapprelease3", "inapprelease2", "inapprelease", "inapprelease4", "ucrelease"
  #"inrelease", "inrelease2", "inrelease3", "inapprelease3", "inapprelease2", "inapprelease", "inapprelease4"
  #"ucpatch", "ucrelease","ucrelease2", "ucrelease3", "inapprelease"
  #"uctrial2", "ucrelease2", "uctrialjslopt"
  #"uctrialcco", "intrialcco", 
  #, "uctrial", "ucrelease", "inapprelease", "uctrial2", "ucrelease2"
  #subVer_t <- c("uctrialcco", "intrialcco","intrial3", "uctrial")
  userMin_t <- 1000
  #zh-cn  en-us
  langs_t <- c("zh-cn")
  fields_t <-c('css','js','image','other','mainframe1','mainframe2','subframe','X3xx_hit','css_hit',
               'js_hit','image_hit','frame_hit','other_hit','users', 't0', 't1', 't2', 'cc','fne',
               'phit', 'prqst', 'nlc', 'qst')
  
  #date_min <- min(as.character(data$pt))
  date_min <- c("2014-08-03")
  date_max <- max(as.character(data$pt))
  #date_max <- c("2014-07-11")
  #数据过滤  
  data <- subset(data, (data$ver %in% ver_t) & (data$lang %in% langs_t) &
                   (as.numeric(data$users)>=userMin_t) & (as.character(data$pt)>=date_min) & 
                   (as.character(data$pt)<=date_max) )
  
  
  #其它过滤
  #data <- subset(data, !((data$ver %in% c("9.8.0.435")) & (data$subver %in% c("uctrial"))))
  
  if (nrow(data) == 0)
    return(data)
  #return (data)
  #2 数据格式化
  for(f in fields_t){
    #print(f)
    data[c(f)]<-as.numeric(as.matrix(data[c(f)]))
    data[c(f)][is.na(data[c(f)]),1]<-0
  }
  
  #3 添加字段
  #data$X3xx_hit+
  data$hit <- data$X3xx_hit+data$css_hit+data$js_hit+data$image_hit+data$frame_hit+data$other_hit;
  data$request <- data$css+data$js+data$image+data$other+data$mainframe1+data$mainframe2+data$subframe;  
  require("plyr")
  data <- ddply(data, c("pt","ver", "lang"), summarise, hit=sum(hit), prqst=sum(prqst), request=sum(request), users=sum(users), cc=mean(cc), 
                qst=mean(qst), qut=mean(qut), qit=mean(qit), qdt=mean(qdt), clt=mean(clt), umt=mean(umt))
  
  data <- subset(data, data$request>0)  
  data$hit_rate <- round(data$hit/data$request*100, 2)
  
  #data$fne_rate <- round(data$fne/data$request*100, 2)
  
  data$pt <- formatDate(data)
  require("ggplot2")
  sp <- qplot(pt, hit_rate, data=data, group=ver, xlab="日期", ylab="命中率", colour=ver, main="命中率走势图")+facet_grid(lang~.)+geom_line()+facet_grid(.~lang)+
    theme(axis.text.x=element_text(angle=75, hjust=1, size=9, colour="black"))
  #  scale_y_continuous(limits=c(32.6,36),breaks=seq(30,36,0.2))
  print(sp)
  
  sp <- qplot(pt, prqst/request*100,  data=data, geom=c("point", "line"), group=ver, xlab="日期", ylab="中转请求比例", 
              colour=ver)+facet_grid(.~lang)
  print(sp)
  return (data)
  #用户数
  sp <- qplot(pt, users, data=data, geom=c("point", "line"), group=ver, xlab="日期", ylab="用户数", colour=ver, main="用户数走势图")+facet_grid(.~lang)  +
    theme(axis.text.x=element_text(angle=75, hjust=1, size=9, colour="black"))
  print(sp)
  
  sp <- qplot(pt, cc, data=data, geom=c("point", "line"), group=ver, xlab="日期", ylab="缓存文件数", colour=ver, main="缓存文件数")+facet_grid(.~lang)  +
    theme(axis.text.x=element_text(angle=75, hjust=1, size=9, colour="black"))
  print(sp)
  
  sp <- qplot(pt, hit_rate/cc, data=data, geom=c("point", "line"), group=ver, xlab="日期", ylab="命中率/缓存文件数", colour=ver, main="缓存空间利用率")+facet_grid(.~lang)  +
    theme(axis.text.x=element_text(angle=75, hjust=1, size=9, colour="black"))
  print(sp)
  
 
  sp <- qplot(pt, clt, data=data, geom=c("point", "line"), group=ver, xlab="日期", ylab="缓存平均加载时间", colour=ver, main="clt")+facet_grid(.~lang)  +
    theme(axis.text.x=element_text(angle=75, hjust=1, size=9, colour="black"))
  print(sp)
  
  sp <- qplot(pt, umt, data=data, geom=c("point", "line"), group=ver, xlab="日期", ylab="缓存平均加载时间", colour=ver, main="clt")+facet_grid(.~lang)  +
    theme(axis.text.x=element_text(angle=75, hjust=1, size=9, colour="black"))
  print(sp)
  return (data)
}

versionUser <- function(file) {
  langs_t <- c("zh-cn")
  fields_t <-c('users')
  
  data <- read.table(file, sep=",", header=T)
  date_min <- c("2014-08-22")
  date_max <- c("2014-08-25")
  #date_max <- c("2014-07-03")
  #数据过滤  
  #ver_t <- c("9.9.3.478", "9.9.0.459", "9.8.0.435", "9.7.8.425", "9.9.0.459")
  #,  "9.9.0.459", "9.8.0.435", "9.7.8.425", "9.9.0.459")
  #"inapprelease3", "inapprelease2", "inapprelease", "inapprelease4", "ucrelease"
  #"inrelease", "inrelease2", "inrelease3", "inapprelease3", "inapprelease2", "inapprelease", "inapprelease4"
  #"ucpatch", "ucrelease","ucrelease2", "ucrelease3", "inapprelease"
  #"uctrial2", "ucrelease2", "uctrialjslopt"
  #"uctrialcco", "intrialcco", 
 
  userMin_t <- 100
  #return (data)
  for(f in fields_t){
    data[c(f)]<-as.numeric(as.matrix(data[c(f)]))
    data[c(f)][is.na(data[c(f)]),1]<-0
  }
  #&(as.numeric(data$users)>=userMin_t) 
  #数据过滤  
  data <- subset(data, (data$lang %in% langs_t) & (as.character(data$pt)>=date_min) & 
                   (as.character(data$pt)<=date_max) & (data$users>=userMin_t) )
  #return (data)
  if (nrow(data) == 0)
    return(data)
  #2 数据格式化
    
  require("ggplot2")
  data$ver_subver <- paste(as.character(data$ver), as.character(data$subver))
  data <- ddply(data, c("pt","ver", "lang"), summarise, users=sum(users))
  #sp <- qplot(ver_subver, users, data=data, geom=("point")) + theme(axis.text.x=element_text(angle=90, hjust=1, size=8, colour="grey50"))
  #print(sp)
  data <- subset(data, data$users>300000)
  sp <- ggplot(data, aes(x = ver, y = users)) + geom_bar(stat = "identity",aes(fill=ver), position="dodge") +
    theme(axis.text.x=element_text(angle=90, hjust=1, size=10, colour="black")) + facet_grid(pt~.)+xlab("版本")+
    ylab("用户")
  print(sp)
  
  return (data)
}

formatDate2 <- function(data) {
  s1 <- data$pt
  s2 <- strsplit(as.character(s1), '/')
  v <- c()
  for (i in 1:length(s2)) {
    s <- unlist(s2[i])
    v <- c(v, paste(s[2], s[3], sep='/'))    
  }
  return (v)
}

ins <- function(file) {
  data <- read.table(file, sep=',', header=T)
  #return (data)
  data$pt <- formatDate2(data)
  data['ins']<-as.numeric(as.matrix(data['ins']))
  data <- subset(data, as.numeric(data$users)>=5000 & data$country %in% c('China'))
  require("ggplot2")
  data$ver_subver <- paste(as.character(data$ver), as.character(data$subver))
  sp <- qplot(pt, ins,  data=data, geom=c("point", "line"), group=ver_subver, xlab="日期", ylab="ins", 
              lty=ver_subver, pch=ver_subver)
  print(sp)
}

cctStat <- function(file) {
  #1 过滤      
  #userMin_t <- 500000
  langs_t <- c("zh-cn", "en-us")
  fields_t <-c("cct")
  
  data <- read.table(file, sep=",", header=T)
  date_min <- min(as.character(data$pt))
  date_max <- max(as.character(data$pt))
  #date_max <- c("2014-07-03")
  #数据过滤  
  
  ver_t <- c("9.9.2.467", "9.9.4.484", "9.9.0.459", "9.8.9.457")
  #,  "9.9.0.459", "9.8.0.435", "9.7.8.425", "9.9.0.459", ")
  #"inapprelease3", "inapprelease2", "inapprelease", "inapprelease4", "ucrelease"
  #"inrelease", "inrelease2", "inrelease3", "inapprelease3", "inapprelease2", "inapprelease", "inapprelease4"
  #"ucpatch", "ucrelease","ucrelease2", "ucrelease3", "inapprelease"
  #"uctrial2", "ucrelease2", "uctrialjslopt"
  #"uctrialcco", "intrialcco", 
  #, "uctrial", "ucrelease", "inapprelease", "uctrial2", "ucrelease2"
  subVer_t <- c("uctrialcco", "intrialcco","uctrial", "intrial3")
  data <- subset(data, (data$ver %in% ver_t) & (data$lang %in% langs_t) & (as.character(data$pt)>=date_min) & 
                   (as.character(data$pt)<=date_max) )
  
  #return (data)
  if (nrow(data) == 0)
    return(data)
  #2 数据格式化
  for(f in fields_t){
    data[c(f)]<-as.numeric(as.matrix(data[c(f)]))
    data[c(f)][is.na(data[c(f)]),1]<-0
  }
  
  require("ggplot2")
  require("plyr")
  #3 添加字段  
  data$ver_subver <- paste(as.character(data$ver), as.character(data$subver))
  data <- data[order(data$pt), ]
  data <- ddply(data, c("sn", "pt","ver", "lang"), summarise, cct=sum(cct))
  userClean <- length(subset(data,data$cct>0))
  print(userClean/nrow(data))
  #sp <- qplot(pt, cct/users, data=data, geom=c("point", "line"), xlab="日期", 
  #            ylab="缓存清理平均次数", group=ver, colour=ver) + facet_grid(.~lang)  
  
  
  #data <- ddply(data, c("pt", "lang"), summarize, users=sum(users))
  #sp <- qplot(pt, users, data=data, geom=c("point", "line"), group=lang, colour=lang, 
  #                        xlab="日期", ylab="中文版用户数")
  #print(sp)
  
  return (data)
}
