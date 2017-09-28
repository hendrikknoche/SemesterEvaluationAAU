df <- read.table(pipe('pbpaste'), header = T) 
library(xlsx)
library(reshape)
library(ggplot2)
library(plyr)

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
dfm[dfm$variable=="pass",]$variable<-"2"
dfm[dfm$variable=="fail",]$variable<-"0"

dfm$variable<-as.numeric(dfm$variable)

df[df$sem==2 & df$exam=="exam" & df$course=="PI",]

columns = names(dfm)[seq(1:6)]

# plyr - works
dfm<-ddply(dfm, columns, summarize, value=mean(value))

ggplot(dfm[dfm$sem==2 & dfm$exam=="exam" & dfm$course=="PI" & !(dfm$variable %in% c("pass", "fail")) ,],aes(x=variable,y=value))+theme_bw()+geom_point()+geom_smooth(se=TRUE)+scale_x_continuous(breaks = c(-5,-4,-3,0,2,4,7,10,12))
str(dfm)
