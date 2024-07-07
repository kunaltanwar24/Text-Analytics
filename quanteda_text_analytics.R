require(quanteda)
require(dplyr)
require(readtext)





t1=readtext("*.txt")
t1

t2=t1$text
t2


c=corpus(t1)


summary(c)

#Detailed Summary
require(quanteda.textstats)
textstat_summary(c)
textstat_readability(c)
 
#Text cleaning using tokens

t=tokens(c,remove_punct = T,
         remove_symbols = T,
         remove_numbers = T,
         remove_url = T,
         remove_separators = T)

t=tokens_tolower(t)
t=tokens_remove(t, pattern = stopwords("english"))


#clean corpus created by OCR(Optical Character Recognition)
t=tokens_select(t,
                c("[\\d-]","[[:punct:]]","^.{1,2}$"),
                selection = "remove",
                valuetype = "regex",
                verbose = TRUE
                )


#converting tokens to dfm(document frequency matrix)

d=dfm(t)
d
head(dfm_sort(d, decreasing = TRUE, 
     margin="both"),
     n = 10)


#Textstats Functions

topfeatures(d)
textstat_frequency(d)


#count specific word in corpus
mydict = dictionary(list(all_terms = c("blockchain")))

tokens_select(t, mydict)
tokens_select(t, mydict) %>% dfm()



#wordcloud
require(RColorBrewer)
require(quanteda.textplots)

textplot_wordcloud(d,
                   min_count = 20,
                   rotation = .25,
                   color = brewer.pal(8, "Dark2"))



#combine two words
text2 = textstat_collocations(c, size=2,
                              min_count = 40)


arrange(text2, desc(count))


#Keyness, Similarity & Dissimilarity
require(quanteda.textstats)
d
ky=textstat_keyness(d,target = 6L )
ky



textstat_simil(d)
dis = textstat_dist(d)
h=hclust(as.dist(dis))
plot(h)




#Sentimental Analysis
install.packages("syuzhet")
require(syuzhet)

a=get_nrc_sentiment(t2)
a


#calculating total score for each sentiment

a_score = data.frame(colSums(a[,]))
names(a_score)<-"Score"
a_score<-cbind("sentiment"=rownames(a_score),a_score)
rownames(a_score)<-NULL

install.packages("ggplot2")
require(ggplot2)
ggplot(data=a_score, aes(x=sentiment, y=Score))+
  geom_bar(aes(fill=sentiment),stat="identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+
  ylab("Scores")+
  ggtitle("Sentiments for Finance & Blockchain News Article")





# using Sentiment analysis package
install.packages("SentimentAnalysis")
require(SentimentAnalysis)
senti = analyzeSentiment(c)
senti

#GI Dictionary (Harvard Dictionary as used inn General Inquirer)
x1 = senti[,2:4]
c1=colSums(x1, na.rm=T)
c2=round(c1,2)
c2

bb=barplot(c2, las=1, ylim=c(0,30),
           col= rainbow(3), main="Sentiment Score")
text(bb, c2+1, c2, pos=3)
box()

#Henry's Finance Specific Dictionary
x2=senti[,5:7]
c1=colSums(x2, na.rm = T)
c2=round(co1,2)
c2

bb=barplot(c1, las=1, ylim=c(0,2.0),
           col= rainbow(3), main="Sentiment Score")
text(bb, c2+.1, c2, pos=3)
box()


#LM Dictionary(Loughran-McDonald Master Dictionary)
x1=senti[, 8:11]
c1=colSums(x1, na.rm =T)
c2=round(c1,2)
c2

bb=barplot(c1, las=1, ylim=c(0,4.0),
           col= rainbow(3), main="Sentiment Score")
text(bb, c2+.1, c2, pos=3)
box()

#QDAP Dictionary
x1=senti[, 12:14]
c1=colSums(x1, na.rm = T)
c2=round(c1,2)
c2

bb=barplot(c1, las=1, ylim=c(0,15),
           col= rainbow(3), main="Sentiment Score")
text(bb, c2+.5, c2, pos=3)
box()
