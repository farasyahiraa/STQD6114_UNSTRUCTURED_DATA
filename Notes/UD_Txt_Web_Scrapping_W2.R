eg1<-read.table(file.choose(),fill=T,header=F) #Data CG.txt
eg1[1,]
eg2<-read.csv(file.choose(),header=F) #Data CG.csv
eg2[1,]
#Using tm package
Library(tm)
eg3<-c("Hi!","Welcome to STQD6114","Tuesday, 11-1pm")
mytext<-VectorSource(eg3)
mycorpus<-VCorpus(mytext) #volatilecorpus
inspect(mycorpus)
as.character(mycorpus[[1]])

#Example using VectorSource
eg4<-t(eg1) #From example 1
a<-sapply(1:7,function(x)
  trimws(paste(eg4[,x],collapse=" "),"right"))
mytext<-VectorSource(a)
mycorpus<-VCorpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]])

#Example using DirSource
mytext<-DirSource("movies")
mycorpus<-VCorpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]])

#Example using DataFrameSource
eg5<-read.csv(file.choose(),header=F) #Using doc6.csv
docs<-data.frame(doc_id=c("doc_1","doc_2"),
                 text=c(as.character(eg5[1,]),as.character(eg5[2,])),
                 dmeta1=1:2,dmeta2=letters[1:2],stringsAsFactors=F)
mytext<-DataframeSource(docs)
mycorpus<-VCorpus(mytext)
inspect(mycorpus)
as.character(mycorpus[[1]])

### 07/04/2025 Monday ####
eg6<-readLines("https://en.wikipedia.org/wiki/Data_science")
eg6[grep("\\h2",eg6)] #header
eg6[grep("\\p",eg6)] #nak exctract paragraph
#Using library XML

 # to use XML paCKAGE (give you a clear web scrapping) we need to use readline func
library(XML)
eg6<-readLines("https://en.wikipedia.org/wiki/Data_science")
doc<-htmlParse(eg6) #convert to R memory
doc.text<-unlist(xpathApply(doc,'//p',xmlValue)) #unlist by paraghraph
doc.text
unlist(xpathApply(doc,'//h2',xmlValue)) #unlist by header

 # use GET func and paste any website you want to have
eg7<-GET("https://en.wikipedia.org/wiki/Data_science")
#Using library httr
library(httr)
doc<-htmlParse(eg7)
doc.text<-unlist(xpathApply(doc,'//p',xmlValue))

# use read_html, use Selector Gadget 
#Using library rvest
library(rvest)
eg8<-read_html("https://en.wikipedia.org/wiki/Data_science")
# dari website guna selector gadget, pick any data, copy bawah paste dkt func
nodes<-html_nodes(eg8,'figcaption') #referring to htmlparse we want to save wht we want to have in R memory
texts<-html_text(nodes) #similar to unlist
        
#Selecting multiple pages
# using rvest package
pages<-paste0('https://www.amazon.com/s?k=skincare&crid=249U9LJFNUS0P&sprefix=skincare%2Caps%2C310&ref=nb_sb_noss_2&page=',0:6)
#pages<- paste0('website                 &page=',0:6) #include page start from 0 as first page
eg10<-read_html(pages[1]) #1 refer to 1st page
# use selector gadget, se
nodes<-html_nodes(eg10,'.a-text-normal , .a-price-whole')
texts<-html_text(nodes)

Price<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url ,'.a-text-normal , .a-price-whole')
  html_text(nodes)}

# read the pages
sapply(pages,Price)
do.call("c",lapply(pages,Price)) #lapply giving you longlist information pages by pages

