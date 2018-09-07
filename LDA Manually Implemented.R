library(dplyr)
library(tidyr)
library(tm)
#library(magrittr)

rawdocs <- c(
  "eat turkey on turkey day holiday",
  "i like to eat cake on holiday",
  "turkey trot race on thanksgiving holiday",
  "snail race the turtle",
  "time travel space race",
  "movie on thanksgiving",
  "movie at air and space museum is cool movie",
  "aspiring movie star"
)

Create.DTM_DF<- function(text)
{
  docs = VCorpus(VectorSource(as.character(text)))
  docs = tm_map(docs, content_transformer(tolower))
  dtm <<- DocumentTermMatrix(docs,control = list(wordLengths = c(1,Inf)))
  df_dtm = as.data.frame(as.matrix(dtm))
  return(df_dtm)
}

df_dtm <- Create.DTM_DF(rawdocs)
colnames(df_dtm)


#===================================

gen_dict <- function(df_dtm,n.topics = 2)
{
  words <- c()
  docs <- c()
  for(i in 1:nrow(df_dtm))
  {
    val <- df_dtm[i,which(df_dtm[i,]!=0)]
    word <- rep(names(val),val)
    doc <- rep(paste0("Doc",i),length(word))
    words <- c(words,word)
    docs <- c(docs,doc)
  }
  df <- data.frame("Docs" = docs,"Words" = words)
  df$Topic <- paste0("T",sample(1:n.topics,nrow(df),replace = T))
  return(df)
}

dict <- gen_dict(df_dtm,3)
View(head(dict,10))

wtopic <- function(dict)
{
  df <- dict %>% group_by(Topic,Words) %>% summarise("Value" = n())
  df <- spread(df,Words,Value,fill = 0) %>% as.data.frame()
  rownames(df) <- df$Topic
  df$Topic <- NULL
  return(df)
}
wt <- wtopic(dict)
#View(head(wt))


dtopic <- function(dict)
{
  df <- dict %>% group_by(Topic,Docs) %>% summarise("Value" = n())
  df <- spread(df,Topic,Value,fill = 0) %>% as.data.frame()
  rownames(df) <- df$Docs
  df$Docs <- NULL
  return(df)
}

dt <- dtopic(dict)
View(dt)



eta <- 1
alpha <- 1
K <- 3
W <- length(names(wt))

for (j in seq(1, 1000)){
  for (i in seq(1, nrow(dict))){
    doc_id = as.numeric(gsub('Doc', '', dict$Docs[i]))
    left  <- (wt[,dict$Words[i]]+eta)/(rowSums(wt) + W*eta)
    right <- (dt[doc_id,]+alpha)/(sum(dt[doc_id,])+ K * alpha)
    new_prob <- left * right
    new_topic = sample(seq(1,3), 1, prob=new_prob)
    dict$Topic[i] = paste0('T', new_topic)
    dt = dtopic(dict)
    wt = wtopic(dict)
  }  
}

wt
View(wt/rowSums(wt))

dt
View(dt/rowSums(dt))


library(tidytext)
lda.out <- LDA(x=dtm,k=3, method="Gibbs")
wtopic <- tidy(lda.out, matrix = "beta")
dtopic <- tidy(lda.out,matrix = "gamma")
View(wtopic)
View(dtopic)






