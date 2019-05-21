#Diniz chupa rola

inst_pkgs = load_pkgs = c("chron","tm","SnowballC","wordcloud","RColorBrewer","rvest","gdata","data.table")

inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]

if(length(inst_pkgs)){
  install.packages(inst_pkgs)
} 

lapply(load_pkgs, require, character.only=T)

setwd(" ")
dados=fread("train.csv",header = TRUE)
head(dados)
#creat corpus of Ingredients(TEXT)
corpus=Corpus(VectorSource(dados$comment_text))
#convert text lowercase
corpus=tm_map(corpus,tolower)
corpus[[1]]
#remove Punctuation
corpus=tm_map(corpus,removePunctuation)
#Remove Stopwords
corpus=tm_map(corpus,removeWords,c(stopwords('english')))
#Remove Whitespaces
corpus=tm_map(corpus,stripWhitespace)
#Perform Stemming
corpus=tm_map(corpus,stemDocument)
#transformando em texto (nao sei se é necessário)
corpus=tm_map(corpus,PlainTextDocument)


###document matrix
frequencies = DocumentTermMatrix(corpus)
frequencies


#organizando frequency of terms
freq=colSums(as.matrix(frequencies))
length(freq)
ord=order(freq)
ord

#if wish to export the matrix (to see how it looks) to an excel file
m=as.matrix(frequencies)
dim(m)
write.csv(m,file='matrix.csv')

#check most and least frequent words
freq[head(ord)]
freq[tail(ord)] 
#check our table of 20 frequencies
head(table(freq),20)
tail(table(freq),20)

#remove sparse terms

sparse= removeSparseTerms(frequencies,1-3/nrow(frequencies))
dim(sparse)

#creat data frame for visualization

wf =data.frame(word=names(freq),freq=freq)
head(wf)

#plot terms witch appear atleast 10000 times
library(ggplot2)
chart= ggplot(subset(wf,freq>10000),aes(x=word,y=freq))
chart=chart+geom_bar(stat='identity',color='black',fill='white')
chat=chart+theme(axis.text=element_text(angle=45,hjust=1))
chart

#find associated terms
findAssocs(frequencies,c('salt','oil'),corlimit=0.3)

#cread wordcloud
set.seed(57)

#plot word clould

wordcloud(names(freq),freq,min.freq = 2500,scale=c(6,.1),colors=brewer.pal(4,"BuPu"))

#plot 5000 most used words
wordcloud(names(freq),freq,max.words = 5000,scale=c(6,.1),colors=brewer.pal(6,'Dark2'))

#creat sparse as data frame
newsparse=as.data.frame(as.matrix(sparse))
dim(newsparse)

#check if all words are appropriate
colnames(newsparse)=make.names(colnames(newsparse))

#check for the dominant dependent variable
table(dados$cuisine)