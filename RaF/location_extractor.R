### adapted from A. Piper's tutorial on POS extraction. 

require("NLP")
library("openNLP")
library("openNLPdata")
options(java.parameters = "- Xmx1024m")

# want to transform the words in a directory of documents into their parts of speech for further analysis
##Loop for extracting and writing POS for directory of works
#this takes as input a directory of .txt files
#it requires an empty directory to put the POS of files

#load your annotators
sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
word_token_annotator <- Maxent_Word_Token_Annotator(language = "en")
pos_tag_annotator <- Maxent_POS_Tag_Annotator(language = "en")


#define your directory where your files are stored
direct.name<-c("New_All_Characters_Dialogue/New_All_Characters_Dialogue")

#get a list of all files in your text directory
filenames<-list.files(direct.name, pattern="*.txt", full.names=FALSE)

#establish your root directory
#this is the directory where your files are stored
root.dir<-"C:/Users/Anne/Desktop/New_All_Characters_Dialogue/New_All_Characters_Dialogue"

#difine your POS directory
dir.pos<-"C:/Users/Anne/Desktop/loc"

#run loop
#NLP is very slow, be patient
for (i in 1:length(filenames)) {
  setwd(root.dir)
  work <-scan(filenames[i], what="character", quote="")
  if (length(work)>0){
    work.clean <- gsub("\\d", "", work)
    text.whole<-paste(work.clean, collapse=" ") # collapse into single chunk
    text.char<-as.String(text.whole)
  
    a1 <- annotate(text.char, sent_token_annotator)
    a2 <- annotate(text.char, word_token_annotator, a1)
  
  #entity tokenization
  #change the annotator based on type of entity "location" or "person"
    entity_annotator <- Maxent_Entity_Annotator(language="en", kind="location", probs = FALSE)
  #run annotation
    a4<-annotate(text.char, entity_annotator, a2)
    gc()
  #subset by entities
    a4w <- subset(a4, type == "entity")
  #extract entity names
    entities<-text.char[a4w]
  #extract unique entities
    entities.unique<-unique(entities)
  
    setwd(dir.pos)
    if(length(entities.unique)!=0){
      write(entities.unique, file = filenames[i])
    }
  }
}
