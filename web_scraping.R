
## P2
## a)

library(XML)
library(curl)
library(stringr)
library(RCurl)

index <- htmlParse("http://www.debates.org/index.php?page=debate-transcripts")
listOfANodes <- getNodeSet(index, "//a[@href]")
listOfURLs = sapply(listOfANodes, xmlGetAttr, "href")
nameOfURLs = sapply(listOfANodes, xmlValue)

yearWanted = grep("1996|2000|2004|2008|2012", nameOfURLs)
listOfURLs = listOfURLs[yearWanted]
nameOfURLs = nameOfURLs[yearWanted]

firstOnly <- grep("First", nameOfURLs)
listOfURLs = listOfURLs[firstOnly]
nameOfURLs = nameOfURLs[firstOnly]

dates <- str_extract(nameOfURLs, "(September|October) \\d+, \\d{4}")
URLandDates <- cbind(listOfURLs, dates)
URLandDates <- URLandDates[nrow(URLandDates):1,]


## Year xxx

getBody <- function(year) {
  URL <- URLandDates[((year-1992)/4),1]
  content <- htmlParse(URL,useInternalNodes = TRUE)
  body <- xpathSApply(content, "//p/text()", xmlValue)
  lines <- grep("Transcription by|COPYRIGHT", body)
  body <- body[-lines]
  end <- grep("END", body)
  if (is.na(end[1])==FALSE) {
    body <- body[-c(end[1]:length(body))]
  }
  
  speaker <- str_match(body, "^(\\[\\*\\] )?[A-Z]+:")
  data <- cbind(speaker[,1], body)
  index = 1
  for (n in 2:(nrow(data))) {
    if (is.na(data[n,1])==FALSE) {
      index <- n
    }
    else {
      data[index,2] <- paste(data[index,2], data[n,2], collapse="\n")
    }
  }
  chunkBySpeaker <- data[is.na(data[,1])==FALSE,]
  
  index = 1
  list <- 1
  for (n in 2:(nrow(chunkBySpeaker))) {
    if (chunkBySpeaker[n,1]!=chunkBySpeaker[n-1,1]) {
      index <- n
      list <- c(list,index)
    }
    else {
      chunkBySpeaker[n,2] <- str_replace_all(chunkBySpeaker[n,2], "^[A-Z]+: ", "")
      chunkBySpeaker[index,2] <- paste(chunkBySpeaker[index,2], 
                                       chunkBySpeaker[n,2], collapse="\n")
    }
  }
  
  chunkBySpeakerNew <- chunkBySpeaker[list,]
  chunkBySpeakerNew <- chunkBySpeakerNew[chunkBySpeakerNew[,1]!="SPEAKERS:",]
  if (chunkBySpeakerNew[1,1]=="[*] LEHRER:") {
    chunkBySpeakerNew[1,1]="LEHRER:"
  }
  return(chunkBySpeakerNew)
}

orgData1996 <- getBody(1996)
orgData2000 <- getBody(2000)
orgData2004 <- getBody(2004)
orgData2008 <- getBody(2008)
orgData2012 <- getBody(2012)


# Count number of laughter and applause before stripping them.

colNames <- c("1996 MODERATOR", "1996 CLINTON", "1996 DOLE", "2000 MODERATOR", "2000 GORE", 
              "2000 BUSH", "2004 MODERATOR", "2004 KERRY", "2004 BUSH", 
              "2008 MODERATOR", "2008 OBAMA", "2008 MCCAIN", 
              "2012 MODERATOR", "2012 OBAMA", "2012 ROMNEY")
rowNames <- c("LAUGHTER", "APPLAUSE")
LaughApplause <- data.frame(matrix(numeric(0), nrow=2, ncol=15), stringsAsFactors=FALSE)
colnames(LaughApplause) <- colNames
row.names(LaughApplause) <- rowNames

getLaughAppl <- function(LaughApplause, orgData, year) {
  speakername <- unique(orgData[,1])
  speaker1 <- orgData[(orgData[,1]==speakername[1]),]
  speaker2 <- orgData[(orgData[,1]==speakername[2]),]
  speaker3 <- orgData[(orgData[,1]==speakername[3]),]
  
  # Laughter
  speaker1laugh <- lapply(speaker1[,2], function(x){str_count(x, "(LAUGHTER)")})
  sumlaughter1 <- sum(unlist(speaker1laugh))
  
  speaker2laugh <- lapply(speaker2[,2], function(x){str_count(x, "(LAUGHTER)")})
  sumlaughter2 <- sum(unlist(speaker2laugh))
  
  speaker3laugh <- lapply(speaker3[,2], function(x){str_count(x, "(LAUGHTER)")})
  sumlaughter3 <- sum(unlist(speaker3laugh))
  
  LaughApplause[1,(3*(year-1992)/4 - 2)] <- sumlaughter1
  LaughApplause[1,(3*(year-1992)/4 - 1)] <- sumlaughter2
  LaughApplause[1,(3*(year-1992)/4)] <- sumlaughter3
  
  # Applause
  speaker1applause <- lapply(speaker1[,2], function(x){str_count(x, "(APPLAUSE)")})
  sumappl1 <- sum(unlist(speaker1applause))
  
  speaker2applause <- lapply(speaker2[,2], function(x){str_count(x, "(APPLAUSE)")})
  sumappl2 <- sum(unlist(speaker2applause))
  
  speaker3applause <- lapply(speaker3[,2], function(x){str_count(x, "(APPLAUSE)")})
  sumappl3 <- sum(unlist(speaker3applause))
  
  LaughApplause[2,(3*(year-1992)/4 - 2)] <- sumappl1
  LaughApplause[2,(3*(year-1992)/4 - 1)] <- sumappl2
  LaughApplause[2,(3*(year-1992)/4)] <- sumappl3
  
  return(LaughApplause)
  
}

LaughApplause <- getLaughAppl(LaughApplause, orgData1996, 1996)
LaughApplause <- getLaughAppl(LaughApplause, orgData2000, 2000)
LaughApplause <- getLaughAppl(LaughApplause, orgData2004, 2004)
LaughApplause <- getLaughAppl(LaughApplause, orgData2008, 2008)
LaughApplause <- getLaughAppl(LaughApplause, orgData2012, 2012)

# end of getting laughter/applause numbers


# strip out unnecessary formatting (speaker names) and non-spoken text
# such as (LAUGHTER), (APPLAUSE), (CROSSTALK)

stripUnnecessary <- function(orgData) {
  stripname <- lapply(orgData[,2], 
                      function(x){str_replace_all(x, "^(\\[\\*\\] )?[A-Z]+: ", "")})
  stripNonSpoken <- lapply(stripname, 
                           function(x){str_replace_all(x, " \\([A-Z]+\\)", "")})
  # data2012new is one person per chunk, name stripped
  dataNew <- cbind(orgData[,1], unlist(stripNonSpoken))
  return(dataNew)
}

finalData1996 <- stripUnnecessary(orgData1996)
finalData2000 <- stripUnnecessary(orgData2000)
finalData2004 <- stripUnnecessary(orgData2004)
finalData2008 <- stripUnnecessary(orgData2008)
finalData2012 <- stripUnnecessary(orgData2012)

# Call cat function now to output nicely-formatted spoken transcript
niceFormat1996 <- paste(finalData1996[,1], finalData1996[,2], collapse="\n\n")
cat(niceFormat1996)

# split word function
wordSplit <- function(speaker) {
  # delete all punctuations
  nopunc <- lapply(speaker[,2], function(x){str_replace_all(x, "[!@$%^&*()\\.,\\?\":;-]+", "")})
  # handle extra spaces due to deletion of multiple punctations
  nopunc <- lapply(nopunc, function(x){str_replace_all(x, "[ ]{2,}", " ")})
  # split words by using space as delimiter
  wordsSpeaker <- lapply(nopunc, function(x){str_split(x, " ")})
  return(wordsSpeaker)
}

wordCount <- function(speaker) {
  wordsSpeaker <- wordSplit(speaker)
  wordsSpeaker <- unlist(wordsSpeaker)
  charSpeaker <- sum(nchar(wordsSpeaker))
  avgSpeaker <- round(charSpeaker / length(wordsSpeaker), digits=2)
  return(c(length(wordsSpeaker), charSpeaker, avgSpeaker))
}


# data frame
colNames <- c("1996 MODERATOR", "1996 CLINTON", "1996 DOLE", "2000 MODERATOR", "2000 GORE", 
              "2000 BUSH", "2004 MODERATOR", "2004 KERRY", "2004 BUSH", 
              "2008 MODERATOR", "2008 OBAMA", "2008 MCCAIN", 
              "2012 MODERATOR", "2012 OBAMA", "2012 ROMNEY")
rowNames <- c("Number of words", "Number of chars", "Average word length",
              "I", "we", "America{n}", "democra{cy,tic}", "republic", 
              "Democrat{ic}", "Republican", "free{dom}", "war", "God",
              "God Bless", "Jesus, Christ, Christian")
dataOverall <- data.frame(matrix(numeric(0), nrow=15, ncol=15), stringsAsFactors=FALSE)
colnames(dataOverall) <- colNames
row.names(dataOverall) <- rowNames

getOverallData <- function(overallData, finalDataYear, year) {
  speakername <- unique(finalDataYear[,1])
  speaker1 <- finalDataYear[(finalDataYear[,1]==speakername[1]),]
  speaker2 <- finalDataYear[(finalDataYear[,1]==speakername[2]),]
  speaker3 <- finalDataYear[(finalDataYear[,1]==speakername[3]),]
  
  pattern <- c("I[^A-Za-z]", "[Ww]e[^A-Za-z]", "American?", "democracy|democratic",
               "republic", "Democrat(ic)?", "Republican", "[Ff]ree(dom)?[^A-z]", "[Ww]ar[^A-z]",
               "[Gg]od(?! [Bb]less)", "[Gg]od [Bb]less", "Jesus|Christ[^A-z]|Christian")
  
  
  for (n in 1:length(pattern)) {
    wordSpeaker1 <- lapply(speaker1[,2], function(x){str_count(x, pattern[n])})
    numSpeaker1 <- sum(unlist(wordSpeaker1))
    wordSpeaker2 <- lapply(speaker2[,2], function(x){str_count(x, pattern[n])})
    numSpeaker2 <- sum(unlist(wordSpeaker2))
    wordSpeaker3 <- lapply(speaker3[,2], function(x){str_count(x, pattern[n])})
    numSpeaker3 <- sum(unlist(wordSpeaker3))
    overallData[n+3,(3*(year-1992)/4 - 2)] <- numSpeaker1
    overallData[n+3,(3*(year-1992)/4 - 1)] <- numSpeaker2
    overallData[n+3,(3*(year-1992)/4)] <- numSpeaker3
  }
  
  # Speaker1 words
  # call wordSplit fuction
  overallData[1:3,(3*(year-1992)/4 - 2)] <- wordCount(speaker1)
  # Speaker2 words
  overallData[1:3,(3*(year-1992)/4 - 1)] <- wordCount(speaker2)
  # Speaker3 words
  overallData[1:3,(3*(year-1992)/4)] <- wordCount(speaker3)
  
  return(overallData)
}

dataOverall <- getOverallData(dataOverall, finalData1996, 1996)
dataOverall <- getOverallData(dataOverall, finalData2000, 2000)
dataOverall <- getOverallData(dataOverall, finalData2004, 2004)
dataOverall <- getOverallData(dataOverall, finalData2008, 2008)
dataOverall <- getOverallData(dataOverall, finalData2012, 2012)
View(dataOverall)

FullData <- rbind(dataOverall, LaughApplause)

# Comments on average word length
# Average number of words candidates speak is 6000-8000.
# Average word length is between 4.3 and 4.5 for every candidate.
# In the debate in 2004, both Kerry and Bush spoke less words than the
# candidates from other years.

# Comments on word choices
# Almost all candidates use the word "I" more than the word "we". 
# Unlike any of the other candidates, Obama likes to use the word "we" much
# more than the word "I". This is consistent with his idea of facing
# difficulties as a whole community.
# In the debate in 2004, the word "war" and "freedom" is used in a higher frequency 
# than other years. This is also consistent with the background that the war in
# Iraq and Afghanistan was going on.

# sentence split
sentenceSplit <- function(finalDataYear) {  
  listspeech <- as.list(finalDataYear[,2])
  pattern <- c("Mr.", "Mrs.", "Ms.", "Dr.", "U.S.")
  replace <- c("Mr", "Mrs", "Ms", "Dr", "US")
  for (i in 1:length(pattern)) {
    listspeech <- lapply(listspeech, function(x){str_replace_all(x, pattern[i], replace[i])})
  }
  sentSplit <- lapply(listspeech, function(x){str_match_all(x, "[^.!?]+[?!\\.]{1,}")})
  return(sentSplit)
}
head(unlist(sentenceSplit(finalData1996)))

# create full speech data frame with speaker, speech, sentence and words.
speechDataFrame <- function(finalData) {
  word <- unlist(wordSplit(finalData), recursive=FALSE)
  sent <- unlist(sentenceSplit(finalData), recursive=FALSE)
  speech <- cbind(finalData, sent, word)
  colnames(speech) <- c("speaker", "speech", "sentence", "word")
  return(speech)
}

speech1996 <- speechDataFrame(finalData1996)



