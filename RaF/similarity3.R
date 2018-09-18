### Similarity part 3: the sampling jubilee ### 
# Choose 20 characters and take a 100 word sample from each one to create our word vector 
# Then randomly sample 50 jac. from each one and take the mean
# repeat 1000 times ad nauseum 

#setwd("~/Desktop/ANNE")
#filenames<-list.files("all_characters_dialogue", pattern="*.txt", full.names=FALSE)

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

output<-NULL
for (i in 1:1000){
  output<-rbind(output,build_many_samples())
}