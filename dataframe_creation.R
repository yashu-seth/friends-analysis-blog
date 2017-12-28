#load library 
if(!require("XML")) install.packages("XML")

# set the working directory accordingly
# make sure the directory has the html files of all
# the episodes
setwd("C:\\work\\blogging\\friends\\friends\\season")

# Helper Functions

# function to split a list on a given set of indices
splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

removeTextInsideBracket <- function(x){
  x <- gsub("\\[", "\\(", x)
  x <- gsub("\\]", "\\)", x)
  x <- gsub("\\([^\\)]*\\)", " ", x)
  x
}

removePunctuations <- function(x) {
  gsub("[[:punct:][:blank:]]+", " ", x)
}

removeWeirdChars <- function(x) {
  x <- gsub("[^[:alpha:][:blank:][:punct:]0-9]", "", x)
  gsub("Â|â", "", x)
}

cleanText <- function(x){
  x <- removeTextInsideBracket(x)
  x <- removePunctuations(x)
  x <- gsub("\r\n", " ", x)
  trimws(tolower(x))
}

cleanTitle <- function(x){
  x <- removeTextInsideBracket(x)
  x <- substring(x, gregexpr("The|THE", x)[[1]][1])
  x <- trimws(removeWeirdChars(x[length(x):length(x)]))
  x
}

getSceneLoc <- function(x){
  loc <- substr(x,
                gregexpr(":", x)[[1]][1] + 1,
                gregexpr("\\]|[\\.|,|;]", x)[[1]][1] - 1)
  loc <- removeWeirdChars(loc)
  if(grepl("\\[Scene", loc)){
    tmp <- unlist(strsplit(loc, "\\[Scene"))
    if(length(tmp) == 1) loc <- ""
    else loc <- tmp[2]
  }
  trimws(loc)
}

# Create an empty data frame
# Each row of the data denotes a separate scene

data <- as.data.frame(matrix(ncol = 17, nrow = 0),
                      colClasses = c(rep("character", 4), rep("integer", 12), "character"))
colnames(data) <- c("season", "episode", "episodeTitle", "scene",
                    "noLinesRoss", "noLinesChandler", "noLinesMonica",
                    "noLinesJoey", "noLinesRachel", "noLinesPhoebe",
                    "noWordsRoss", "noWordsChandler", "noWordsMonica",
                    "noLinesJoey", "noWordsRachel", "noWordsPhoebe",
                    "speakers")

files <- list.files()

for(file in files){

  episodeHtml <-  htmlTreeParse(file, useInternal = TRUE)

  season <- substr(file, 1, 2)
  
  # If there was a two part episode
  episode <- substr(file, 3, 4)
  if(grepl("-", file)) {
    episode <- paste(substr(file, 3, 4), substr(file, 8, 9), sep=",")
  } 

  episodeText <- trimws(unlist(xpathApply(episodeHtml, '//p', xmlValue)))
  
  if(length(episodeText) <= 10){
    episodeText <- trimws(unlist(xpathApply(episodeHtml,
          '//p/text()[preceding-sibling::br and following-sibling::br]',
          xmlValue)))
  }

  title <- unlist(xpathApply(episodeHtml, '//title', xmlValue))
  if(file == "0912.html") title <- "The One With Phoebe's Rats"
  if(file == "0909.html") title <- "The One With Rachel's Phone Number"
  if(file == "0906.html") title <- "The One With The Male Nanny"
  
  title <- cleanTitle(title)

  sceneIdx <- which(grepl("\\(At|\\(at|\\[At|\\[at|\\[Scene",
                          episodeText))
  if(length(sceneIdx) == 0) sceneIdx <- c(2)
  
  scenes <- splitAt(episodeText, sceneIdx)
  
  for(scene in scenes[-1]){
    sceneLoc <- getSceneLoc(scene[1])
    # number of dialogues
    nd <- vector("integer")
    # number of words
    nw <- vector("integer")
    for(line in scene[-1]){
      
      if(length(unlist(strsplit(removeTextInsideBracket(line), ":"))) == 0) next()
      
      # The speaker of this line
      speaker <- cleanText(unlist(strsplit(line, ":"))[[1]])
      
      if(length(unlist(strsplit(speaker, " "))) > 2) next()
      
      lineDet <- unlist(strsplit(cleanText(line), " "))
      
      if(any(grepl(tolower(speaker), c("", "commercial", "end", "opening",
                                   "closing", "html")))) next()
      
      
      if(is.na(nd[speaker])) nd[speaker] <- 0
      if(is.na(nw[speaker])) nw[speaker] <- 0
      
      nd[speaker] <- nd[speaker] + 1
      nw[speaker] <- nw[speaker] + length(lineDet) - 1
    }
    
    speakers <- removeWeirdChars(names(nd)[!is.na(names(nd))])
    row <- c(season = season, episode = episode, episodeTitle = title,
             scene = sceneLoc,
             noLinesRoss = unname(nd["ross"]), noLinesChandler = unname(nd["chandler"]),
             noLinesMonica = unname(nd["monica"]), noLinesJoey = unname(nd["joey"]),
             noLinesRachel = unname(nd["rachel"]), noLinesPhoebe = unname(nd["phoebe"]),
             noWordsRoss = unname(nw["ross"]), noWordsChandler = unname(nw["chandler"]),
             noWordsMonica = unname(nw["monica"]), noWordsJoey = unname(nw["joey"]),
             noWordsRachel = unname(nw["rachel"]), noWordsPhoebe = unname(nw["phoebe"]),
             speakers = paste(speakers, collapse = ","))
    data <- rbind(data, t(row))
  }
  print(file)
}

data <- as.data.frame(apply(data, 2, function(x) as.character(x)), stringsAsFactors = F)
data[is.na(data)] <- 0

write.csv(data, "friends.csv", row.names = FALSE)
