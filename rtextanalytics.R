#Packages
install.packages("tm")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("ggplot2")
install.packages("corpustools")
library(tm)
library(stringr)
library(stringi)
require(qdap)
require(qdapDictionaries)
require(tm)
require(wordcloud)
require(wordcloud2)


#Import of Data 
folder="Data Collection"
files=list.files(folder)
x=paste(folder,"\\",files, sep="")
x
text = lapply(x, readLines)
text[5]
t=lapply(text, FUN=paste, collapse="")
t
#Corpus Conversion
c=VCorpus(VectorSource(t))
c
#Text cleaning 
c=tm_map(c, content_transformer(tolower))
c=tm_map(c,removePunctuation)
c=tm_map(c, removeNumbers)
c=tm_map(c, removeWords, stopwords("en"))
c=tm_map(c, stripWhitespace)
c=tm_map(c, stemDocument)

#Word Cloud
wordcloud(c, min.freq = 20, colors = brewer.pal(8, "Dark2"),rot.per = .4, random.order = F)


#Term Documentation Matrix
tdm = TermDocumentMatrix(c)
tdm
inspect(tdm)

tt=removeSparseTerms(tdm, .4)
inspect(tt)
tt

#TD Matrix
tdmat=as.matrix(tdm)
tdmat
dim(tdmat)

#comparasion Cloud
comparison.cloud(tdmat, colors = brewer.pal(12, "Paired"), rot.per = .25)


#Term Frequency 

freq.terms=findFreqTerms(tt, lowfreq = 20, highfreq = 80)
freq.terms

#Word Association

findAssocs(tt, "blockchain", 0.3)
findAssocs(tt, c("blockchain","finance"),c(0.3,0.2))

#text plot network 
set.seed(144)
textplot_network(tdm,  min_freq = 10)
