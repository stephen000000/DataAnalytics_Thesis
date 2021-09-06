library(tidyverse)
library("stringr")  

mainData <- read.csv(file.choose())

songList <- mainData[,1]
length(songList)
for(i in 1:length(songList)){
  song <- songList[i]
  splitSong <- unlist(strsplit(song,"resource/"))
  songTitle <- splitSong[2]
  songTitle <- str_replace_all(songTitle,"_"," ")
  mainData[i,1] <- songTitle
}


artistList <- mainData[,4]
length(artistList)
for(i in 1:length(artistList)){
  artist <- artistList[i]
  splitArtist <- unlist(strsplit(artist,"resource/"))
  artistName <- splitArtist[2]
  check <- grepl("[(]",artistName)
  if(check== T){
    artistName <- unlist(strsplit(artistName,"[(]"))
    artistName <- artistName[1]
  }
  artistName <- str_replace_all(artistName,"_"," ")
  lastChar <- substr(artistName,nchar(artistName),nchar(artistName))

  if(lastChar == " "){
    artistName <-substr(artistName,1,nchar(artistName)-1)
  }
  

  mainData[i,4] <- artistName
}

mainDataCopy <- mainData
mainData <- mainDataCopy

songList2 <- mainData[,1]
songList2 <-unique(unlist(songList2))  
for(i in 1:length(songList2)){
  songName <- songList2[i]
  songData <- mainDataCopy  %>% filter(Song==songName) %>% filter(Sales == max(Sales))
  if(i == 1){
    mainData <- songData
  }else{
    mainData <- rbind(mainData,songData)
  }
}



## -- Genre -- ##
genreData <- read.csv(file.choose())
copyGenreData <- genreData


gSongList <- genreData[,1]
length(gSongList)
for(i in 1:length(gSongList)){

  songGenre <- gSongList[i]
  splitSongGenre <- unlist(strsplit(songGenre,"resource/"))
  songTitleGenre <- splitSongGenre[2]
  
  songTitleGenre <- str_replace_all(songTitleGenre,"_"," ")
  
  genreData[i,1] <- songTitleGenre
}


copyGenreData <- genreData


songGenre <- genreData[,2]
length(songGenre)
for(i in 1:length(songGenre)){
  genre <- songGenre[i]
  splitGenre<- unlist(strsplit(genre,"resource/"))
  genre <- splitGenre[2]

  genre <- str_replace_all(genre,"_"," ")

  genreData[i,2] <- genre
}

copyGenreData <- genreData

gSongList2 <- genreData[,1]
gSongList2 <-unique(unlist(gSongList2))  
for(i in 1:length(gSongList2)){
  songName <- gSongList2[i]
  gSongData <- copyGenreData %>% filter(x==songName)
  gSongData <- gSongData[1,]
  if(i == 1){
    genreData <- gSongData
  }else{
    genreData <- rbind(genreData,gSongData)
  }
}


## -- Release Date -- ##
releaseDateData <- read.csv(file.choose())
copyReleaseDateData <- releaseDateData 

rdSongList <- releaseDateData[,1]
for(i in 1:length(rdSongList)){
  songRD <- rdSongList[i]
  splitSongRD <- unlist(strsplit(songRD,"resource/"))
  songTitleRD <- splitSongRD [2]

  songTitleRD<- str_replace_all(songTitleRD,"_"," ")

  releaseDateData[i,1] <- songTitleRD
}

copyReleaseDateData <- releaseDateData 

rdSongList2 <- releaseDateData[,1]
rdSongList2 <-unique(unlist(rdSongList2)) 

for(i in 1:length(rdSongList2)){
  songName <- rdSongList2 [i]

  rdSongData <- copyReleaseDateData %>%  filter(x==songName)

  rdSongData <- rdSongData[1,]

  if(i == 1){
    releaseDateData <- rdSongData
  }else{
    releaseDateData <- rbind(releaseDateData,rdSongData)
  }

}


## -- Runtime Data -- ##
runtimeData <- read.csv(file.choose())
copyRuntimeData <- runtimeData

rtSongList <- runtimeData[,1]
for(i in 1:length(rdSongList)){
  songRT <- rdSongList[i]
  splitSongRT <- unlist(strsplit(songRT,"resource/"))
  songTitleRT <- splitSongRT [2]

  songTitleRT<- str_replace_all(songTitleRT,"_"," ")

  runtimeData[i,1] <- songTitleRT
}

copyRuntimeData <- runtimeData

rtSongList2 <- runtimeData[,1]
rtSongList2 <-unique(unlist(rtSongList2)) 

for(i in 1:length(rtSongList2)){
  songName <- rtSongList2[i]

  rtSongData <- copyRuntimeData %>% filter(x==songName)

  rtSongData <- rtSongData[nrow(rtSongData),]

  if(i == 1){
    runtimeData <- rtSongData
  }else{
    runtimeData <- rbind(runtimeData,rtSongData)
  }
}

names(mainData)[1] <- "x"


mainData <- mainData %>% left_join(genreData,by="x")
mainData <- mainData %>% left_join(releaseDateData,by="x")
mainData <- mainData %>% left_join(runtimeData,by="x")

write.csv(mainData,"Thesis_dbpedia_Dataset.csv",)
