df <- read.table(pipe('pbpaste'), header = T) 
ThisYear<-"16/17"
library(xlsx)
library(reshape)
library(ggplot2)
library(plyr)
library(dplyr)
library(sqldf)
file <- loadWorkbook("/Users/hendrik/OneDrive - Aalborg Universitet/semester coordination/Med1/grades exam Over The years Mea Med1 and 2 Medialogy.xlsx")
fileName<-"/Users/hendrik/OneDrive - Aalborg Universitet/semester coordination/Med1/grades exam Over The years Mea Med1 and 2 Medialogy.xlsx"
df1 <- readColumns(getSheets(file)[[1]], startColumn = 1, endColumn = 24, startRow = 1, endRow = 78, header = T)
df<-read.xlsx(fileName, sheetIndex=1, sheetName=NULL, rowIndex=NULL,
          startRow=1, endRow=78, colIndex=seq(1:17),
          as.data.frame=TRUE, header=TRUE, colClasses=NA,
          keepFormulas=FALSE, encoding="unknown",check.names=FALSE)

dfm<-melt(df,id=c("location","education","year","sem","course","exam"))
dfm[is.na(dfm$value),]$value<-0
dfm$variable<-as.character(dfm$variable)
dfm[dfm$variable=="EB not allowed to Exam (EB)",]$variable<-"-5"
dfm[dfm$variable=="U noshow (U)",]$variable<-"-4"
dfm<-dfm[dfm$variable!="pass" & dfm$variable!="fail",]

# dfm[dfm$variable=="pass",]$variable<-"2"
# dfm[dfm$variable=="fail",]$variable<-"0"

dfm$variable<-as.numeric(dfm$variable)


df[df$sem==2 & df$exam=="exam" & df$course=="PI",]

columns = names(dfm)[seq(1:6)]

#colu = names(data)[-3]

dfx <- dfm %>%
  group_by_at(vars(one_of(columns))) %>%
  mutate(percent=value/sum(value))

dfmp<-sqldf("select location, education, year, sem, course, exam,  sum(value) as total from dfm group by  location, education, year, sem, course, exam")
dfm<-merge(dfm,dfmp,all.x = TRUE)
dfm$percent<-dfm$value/dfm$total

#course comparison
ggplot(dfm[dfm$sem==2 & dfm$exam=="exam" & !(dfm$variable %in% c("pass", "fail")) ,],aes(x=variable,y=percent,color=course))+
  theme_bw()+ ggtitle(paste("Comparison of modules on Med2",ThisYear)) +
  geom_line(data=dfm[dfm$sem==2 & dfm$exam=="exam" &  !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,])+ 
  scale_y_continuous(labels = scales::percent)+
  geom_point(data=dfm[dfm$sem==2 & dfm$exam=="exam" &  !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,])+
  #geom_text(data=dfm[dfm$sem==2 & dfm$exam=="exam" &  !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,],aes(label=value),hjust=.5, vjust=1.5)+ 
  scale_x_continuous(labels=c("EB","U","-3","0","2","4","7","10","12"), breaks = c(-5,-4,-3,0,2,4,7,10,12))+xlab("grades")


ggplot(dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="PI" & !(dfm$variable %in% c("pass", "fail")) ,],aes(x=variable,y=percent))+
  theme_bw()+ ggtitle("Programming for Interaction") +
  geom_smooth(se=TRUE)+ 
  scale_y_continuous(labels = scales::percent)+
  geom_point(data=dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="PI" & !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,])+
  geom_text(data=dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="PI" & !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,],aes(label=value),hjust=.5, vjust=1.5)+ 
  scale_x_continuous(labels=c("EB","U","-3","0","2","4","7","10","12"), breaks = c(-5,-4,-3,0,2,4,7,10,12))+xlab("grades")


ggplot(dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="MMA" & !(dfm$variable %in% c("pass", "fail")) ,],aes(x=variable,y=percent))+
  theme_bw()+ ggtitle("Mathematics for Multimedia Applications (MMA)") +
  geom_smooth(se=TRUE)+scale_x_continuous(labels=c("EB","U","-3","0","2","4","7","10","12"), breaks = c(-5,-4,-3,0,2,4,7,10,12))+ 
  scale_y_continuous(labels = scales::percent)+
  geom_point(data=dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="MMA" & !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,])+
  geom_text(data=dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="MMA" & !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,],aes(label=value),hjust=.5, vjust=1.5)+xlab("grades")

 ggplot(dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="ID" & !(dfm$variable %in% c("pass", "fail")) ,],aes(x=variable,y=percent))+
  theme_bw()+ ggtitle("Interaction Design (ID)") +
  geom_smooth(se=TRUE)+scale_x_continuous(labels=c("EB","U","-3","0","2","4","7","10","12"), breaks = c(-5,-4,-3,0,2,4,7,10,12))+ 
  scale_y_continuous(labels = scales::percent)+
  geom_point(data=dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="ID" & !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,])+
  geom_text(data=dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="ID" & !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,],aes(label=value),hjust=.5, vjust=1.5)+xlab("grades")

ggplot(dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="P2" & !(dfm$variable %in% c("pass", "fail")) ,],aes(x=variable,y=percent))+
  theme_bw()+ ggtitle("P2 Project") +
  geom_smooth(se=TRUE)+scale_x_continuous(labels=c("EB","U","-3","0","2","4","7","10","12"), breaks = c(-5,-4,-3,0,2,4,7,10,12))+ 
  scale_y_continuous(labels = scales::percent)+
  geom_point(data=dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="P2" & !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,])+
  geom_text(data=dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="P2" & !(dfm$variable %in% c("pass", "fail")) & dfm$year  ==ThisYear ,],aes(label=value),hjust=.5, vjust=1.5)+xlab("grades")

str(dfm)


                         
