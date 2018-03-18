library(xlsx)
library(reshape)
library(ggplot2)
library(plyr)
library(dplyr)
library(sqldf)
library(knitr)

ThisYear<-"17/18" #school year
OddSemester<-TRUE
semester<-1
OddSems<-seq(1,9,by=2)
EvenSems<-seq(2,10,by=2)

file <- loadWorkbook("/Users/hendrik/OneDrive - Aalborg Universitet/semester coordination/grades exam Over The years Mea Med1 and 2 Medialogy.xlsx")
fileName<-"/Users/hendrik/OneDrive - Aalborg Universitet/semester coordination/grades exam Over The years Mea Med1 and 2 Medialogy.xlsx"
df1 <- readColumns(getSheets(file)[[1]], startColumn = 1, endColumn = 24, startRow = 1, endRow = 78, header = T)
df<-read.xlsx(fileName, sheetIndex=1, sheetName=NULL, rowIndex=NULL,
              startRow=1, endRow=78, colIndex=seq(1:17),
              as.data.frame=TRUE, header=TRUE, colClasses=NA,
              keepFormulas=FALSE, encoding="unknown",check.names=FALSE)

dfm<-melt(df,id=c("location","education","year","sem","course","exam"))
#identify courses that are pass/fail in a given year / careful later on if switches occur in study plan
coursesWithPassFail<-sqldf("select distinct year, education, course from dfm where variable='I' and value>=-1")
coursesWithPassFail$isPassFail<-TRUE
dfm<-merge(dfm,coursesWithPassFail,all.x = TRUE)
#remove pass/fails from grades courses
dfm<-dfm[!(is.na(dfm$isPassFail) & dfm$variable %in% c('B','I')),]
#remove grades from pass/fail courses
dfm<-dfm[is.na(dfm$isPassFail) | dfm$variable %in% c('U','EB','B','I'),]

dfm<-dfm[(dfm$isPassFail==TRUE & (dfm$variable %in% c(-5,-4,0,1))) | (is.na(dfm$isPassFail) & !dfm$variable==1),]

#dfm<-dfm[!is.na(dfm$value),]

dfm[is.na(dfm$value),]$value<-0
dfm$variable<-as.character(dfm$variable)
dfm[dfm$variable=="EB",]$variable<-"-5"
dfm[dfm$variable=="U",]$variable<-"-4"


dfm[dfm$variable=="I",]$variable<-"0"
dfm[dfm$variable=="B",]$variable<-"1"


dfm<-dfm[dfm$variable!="pass" & dfm$variable!="fail",]
#remove all grades from passfail courses that are not pass/fail grades
# dfm[dfm$variable=="pass",]$variable<-"2"
# dfm[dfm$variable=="fail",]$variable<-"0"

dfm$variable<-as.numeric(dfm$variable)


df[df$sem==semester & df$exam=="exam" & df$course=="PI",]

columns = names(dfm)[seq(1:6)]

#colu = names(data)[-3]

dfx <- dfm %>%
  group_by_at(vars(one_of(columns))) %>%
  mutate(percent=value/sum(value))

dfmp<-sqldf("select location, education, year, sem, course, exam,  sum(value) as total from dfm group by  location, education, year, sem, course, exam")
dfm<-merge(dfm,dfmp,all.x = TRUE)
dfm$percent<-dfm$value/dfm$total
dfm<-dfm[!is.na(dfm$variable),]

ggplot(dfm[dfm$sem==semester & dfm$exam=="exam" & !(dfm$variable %in% c("pass", "fail")) ,],aes(x=variable,y=percent,color=course))+
  theme_bw()+ ggtitle(paste("Comparison of modules on Med",semester,ThisYear,sep="")) +
  geom_line(data=dfm[dfm$sem==semester & dfm$exam=="exam" &  !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,])+ 
  scale_y_continuous(labels = scales::percent)+
  geom_point(data=dfm[dfm$sem==semester & dfm$exam=="exam" &  !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,])+
  #geom_text(data=dfm[dfm$sem==2 & dfm$exam=="exam" &  !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,],aes(label=value),hjust=.5, vjust=1.5)+ 
  scale_x_continuous(labels=c("EB","U","-3","0/I","B","2","4","7","10","12"), breaks = c(-5,-4,-3,0,1,2,4,7,10,12))+xlab("grades")
