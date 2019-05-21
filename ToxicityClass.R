inst_pkgs = load_pkgs = c("chron","tm","SnowballC","wordcloud","RColorBrewer","rvest","gdata","data.table")

inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]

if(length(inst_pkgs)){
  install.packages(inst_pkgs)
} 

lapply(load_pkgs, require, character.only=T)

setwd("E:\\1 - Estudos\\1 - Projetos\\Kaggle\\Analise de sentimento\\Dados")
train = fread("train.csv",header = TRUE, encoding = "UTF-8")
test = fread("test.csv",header = TRUE, encoding = "UTF-8")

#######################################################################










############# LIMPANDO OS train #############

#creat corpus_train of Ingredients(TEXT)
corpus_train = Corpus(VectorSource(train$comment_text))
corpus_test  = Corpus(VectorSource(test$comment_text))

writeLines(as.character(corpus_train[[1]])) # Verificando os comentarios carregados
writeLines(as.character(corpus_test[[1]])) # Verificando os comentarios carregados

#######################################################################










############# TRATANDO OS DADOS DE TREINAMENTO

#convert text lowercase
corpus_train = tm_map(corpus_train,content_transformer(tolower))
#remove Punctuation
corpus_train = tm_map(corpus_train,removePunctuation)
#Remove Stopwords
corpus_train = tm_map(corpus_train,removeWords,c(stopwords('english')))
#Remove Whitespaces
corpus_train = tm_map(corpus_train,stripWhitespace)
#Removendo numeros
corpus_train = tm_map(corpus_train, removeNumbers) 
#Perform Stemming
#corpus_train = tm_map(corpus_train,stemDocument)

#######################################################################










############# TRATANDO OS DADOS DE TESTE

#convert text lowercase
corpus_test = tm_map(corpus_test,content_transformer(tolower))
#remove Punctuation
corpus_test = tm_map(corpus_test,removePunctuation)
#Remove Stopwords
corpus_test = tm_map(corpus_test,removeWords,c(stopwords('english')))
#Remove Whitespaces
corpus_test = tm_map(corpus_test,stripWhitespace)
#Removendo numeros
corpus_test = tm_map(corpus_test, removeNumbers)

#######################################################################










############# TRANSFORMANDO A MATRIZ EM DOCUMENTO TERMO

#frequencies = DocumentTermMatrix(corpus_train)
#frequencies

dtm_train = DocumentTermMatrix(corpus_train)
inspect(dtm_train[1:100, 1:50])

dtm_test = DocumentTermMatrix(corpus_test)
inspect(dtm_test[1:5, 1:20])

#######################################################################










############# VERIFICANDO A FREQUÊNCIA DAS PALAVRAS 2 (Quando não conseguimos transformar em Matrix)

#freq = findFreqTerms(dtm_train,1000) #Encontrando termos que aparecem mais que 1000 vezes

comentarios = train$comment_text
avisos1 = tolower(comentarios)
avisos1 = removePunctuation(avisos1)
avisos1 = removeNumbers(avisos1)
avisos1 = removeWords(avisos1, stopwords("english"))
avisos1 = stripWhitespace(avisos1)
avisos1 = trimws(avisos1)
avisos1 = strsplit(avisos1, " ")
avisos1 = unlist(avisos1)


freq  = table(avisos1)
freq1 = sort(freq, decreasing=TRUE)
temple.sorted.table = data.frame(Freq = freq1)

teste = head(temple.sorted.table, 30) # 30 palavras mais frequentes


##### Nuvem palavras 

pal1 <- brewer.pal(30,"Paired")
pal3 <- brewer.pal(30,"Set3")

## Nuvem de palavras mais frequentes
WC <- wordcloud(temple.sorted.table$Freq.avisos1, 
                temple.sorted.table$Freq.Freq,
                min.freq=1,
                max.words=30, 
                random.order=FALSE, 
                rot.per=.15,colors=pal1)

#######################################################################









############# PALAVRAS TÓXICAS

vetor_toxic = c("shit","fuck","pussy","dick","damn","bitch","crap","piss","darn","cock","asshole","bastard","fag","Arse","Arsehole","slut","douche","Ass","Berk","Crickhold")
















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
table(train$cuisine)