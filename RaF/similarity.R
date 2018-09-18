### Clustering and Classification ###

library("tm")
library("DTK")
library("SnowballC")
library("proxy")
library("cluster")
library("dendextend")
library("splitstackshape")

#set working directory
setwd("~/Desktop/ANNE/collocate")


# pulled all_words_as_vec from typetoken.R, but until clean up don't source it here
# it has too much junk 

all_words_as_vec<-function(group){
  fullpath<-paste("collocate/",group,"_all_dialogue", sep="")
  setwd("~/Desktop/ANNE")
  filenames<-list.files(fullpath, pattern="*.txt", full.names=FALSE)
  
  setwd(paste("~/Desktop/ANNE/",fullpath,sep=""))
  collapsed<-NULL
  for (i in 1:length(filenames)) {
    work<-scan(filenames[i], what="character", quote="") # load novel, separate chunks by line breaks
    work.clean<-gsub("\\d", "", work)       # remove numbers
    work.clean<-tolower(work.clean)
    work.clean<-subset(work.clean, !(work.clean %in% stopwords("English")))
    work.clean<-gsub("[[:punct:]]","", work.clean)    # remove punctuation
    work.clean<-trimws(work.clean)
    work.clean<-work.clean[which(work.clean!="")]
    work.clean<-wordStem(work.clean, language = "english")
    
    collapsed<-c(collapsed, work.clean)
  }
  return(collapsed)
}

sim_sample<-function(word_vec, size){
  a<-sample(word_vec, size, replace=TRUE)
  b<-sample(word_vec, size, replace=TRUE)
  return(cbind(a,b))
}

jac<-function(word_df){
  a<-word_df[,1]
  b<-word_df[,2]
  itrsct<-length(intersect(a,b))
  size<-nrow(word_df)
  jcrd<-(itrsct/((2*size)-itrsct))
  return(jcrd)
}

jac2<-function(word_df){
  a<-word_df[,1]
  b<-word_df[,2]
  itr<-length(intersect(a,b))
  return(itr)
}

mult_jac<-function(word_vec, size=100){
  output<-NULL
  for (i in 1:1000){
    df<-sim_sample(word_vec, size)
    jcrd<-jac(df)
    output<-c(output, jcrd)
  }
  return(output)
}

removeWords<-function(str, sim) {
  x<-unlist(strsplit(str, " "))
  strsplit(paste(x[!x %in% sim], collapse = " "), " ")
}

kl_div<-function(word_vec,df){
  box<-NULL
  for(i in 1:1000){
    samp<-sim_sample(word_vec, 1500)
    samp1<-samp[,1]
    samp2<-samp[,2]
    
    tab1<-table(removeWords(samp1, intersect(samp1,samp2)))
    tab2<-table(removeWords(samp2, intersect(samp1,samp2)))
    box<-c(box,nrow(tab1),nrow(tab2))
  }
  return(cbind(df, box))
}

#finds expected number of unique words across both of the samples
#then calculates what fraction of the words were not repeated 
adjustment_score<-function(word_vec, sample_size){
  n<-length(word_vec)
  expect.uniques<-n*(dbinom(1,2*sample_size,(1/n)))
  return(expect.uniques/(2*sample_size))
}

## must be at the collocate dics in question
sim_matrix<-function(folder){
  #read in corpus1
  corpus1 <- VCorpus(DirSource(folder, encoding = "UTF-8"), readerControl=list(language="English"))
  
  #clean your data
  corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
  corpus1 <- tm_map(corpus1, content_transformer(tolower))
  corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
  corpus1 <- tm_map(corpus1, removeWords, stopwords("English")) ## inspect your stopword lists
  corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
  corpus1 <- tm_map(corpus1, removeWords, stopwords("English")) ## inspect your stopword lists
  corpus1 <- tm_map(corpus1, stemDocument, language = "english") # stem your words
  
  #create your document term matrix
  corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
  corpus1.matrix<-as.matrix(corpus1.dtm, stringsAsFactors=F)
  
  #perform transformations on the raw counts
  
  #Method 0: Remove sparse terms
  corpus1.dtm.sparse<-removeSparseTerms(corpus1.dtm, 0.9) #lower number = requiring that a word be in more documents
  corpus1.matrix.sparse<-as.matrix(corpus1.dtm.sparse, stringsAsFactors=F)
  
  #Method 1: Scaling
  scaling1<-rowSums(corpus1.matrix) #get total word counts for each work
  corpus1.scaled<-corpus1.matrix.sparse/scaling1 #turn counts into percentages 
  
  
  sim.m<-simil(corpus1.scaled, method = "cosine") #cosine or Jaccard or any other measure
  dist.m<-pr_simil2dist(sim.m)
  
  return(as.matrix(sim.m))
}

sample_matrix<-function(matrix){
  new<-NULL
  for (i in 1:100){
    smpl<-sample(matrix, 1, replace=TRUE)
    while (is.na(smpl)){
      smpl<-sample(matrix,1)
    }
    new<-c(new, smpl)
  }
  return(as.vector(na.omit(new)))
}

build_anova<-function(dataframe){
  stacked.x<-stack(dataframe)
  return(aov(values~ind, data=stacked.x))
}
build_dtk<-function(dataframe){
  stacked<-stack(dataframe)
  attach(stacked)
  return(DTK.test(values, ind, 0.10))
}






### similarity part 3 functions ###
sample_as_vec<-function(group){
  fullpath<-paste("collocate/",group,"_all_dialogue", sep="")
  setwd("~/Desktop/ANNE")
  filenames<-list.files(fullpath, pattern="*.txt", full.names=FALSE)
  samp<-sample(filenames, 20, replace=FALSE)
  
  setwd(paste("~/Desktop/ANNE/",fullpath,sep=""))
  collapsed<-NULL
  for (i in 1:length(samp)) {
    work<-scan(samp[i], what="character", quote="") # load dialogue, separate chunks by line breaks
    work.clean<-gsub("\\d", "", work)       # remove numbers
    work.clean<-tolower(work.clean)
    work.clean<-subset(work.clean, !(work.clean %in% stopwords("English")))
    work.clean<-gsub("[[:punct:]]","", work.clean)    # remove punctuation
    work.clean<-trimws(work.clean)
    work.clean<-work.clean[which(work.clean!="")]
    work.clean<-wordStem(work.clean, language = "english")
    
    text_sample<-sample(work.clean, 100, replace=TRUE)
    collapsed<-c(collapsed, text_sample)
  }
  return(collapsed)
}

samp_jac<-function(word_vec){
  output<-NULL
  for (i in 1:50){
    df<-sim_sample(word_vec, 500)
    jcrd<-jac(df)
    output<-c(output, jcrd)
  }
  return(output)
}

build_many_samples<-function(){
  asian_sample<-sample_as_vec("east_asian")
  black_sample<-sample_as_vec("black")
  latinx_sample<-sample_as_vec("latinx")
  white_sample<-sample_as_vec("white")
  
  samp_jacall<-NULL
  samp_jacall<-samp_jac(asian_sample)
  samp_jacall<-cbind(samp_jacall,samp_jac(black_sample))
  samp_jacall<-cbind(samp_jacall,samp_jac(latinx_sample))
  samp_jacall<-cbind(samp_jacall,samp_jac(white_sample))
  
  colnames(samp_jacall)<-c("asian","black", "latinx", "white")
  
  mean1<-mean(samp_jacall[,1])
  mean2<-mean(samp_jacall[,2])
  mean3<-mean(samp_jacall[,3])
  mean4<-mean(samp_jacall[,4])
  
  return(c(mean1,mean2,mean3,mean4))
}
