# load libraries
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("dplyr")) install.packages("dplyr")

# set the working directory accordingly
# make sure the directory has the data created
# from the dataframe_creation.R script.

setwd("C:\\work\\blogging\\friends\\friends\\season")

data <- read.csv("friends2.csv")

friends <- c("Ross", "Chandler", "Monica", "Joey", "Rachel", "Phoebe")

# Total Number of Lines, Scenes and Words
lines <- colSums(data[,5:10])
scenes <- apply(data[,5:10], 2, function(x) sum(x!=0))
words <- colSums(data[,11:16])

friendsData <- as.data.frame(cbind(friends, lines, scenes, words),
                            row.names = F)
colnames(friendsData) <- c("friends", "lines", "scenes", "words")

# Numer of total lines 
g <- ggplot() +
  geom_bar(stat = "identity",
           aes(x=friends, y=lines, fill=friends)) +
  xlab("Friends") + ylab("Lines") +
  ggtitle("Total Number of Lines")
g 

# Number of Lines Per Season
perSeason <- data %>%
  group_by(season) %>%
  summarise(Chandler = sum(noLinesChandler),
            Joey = sum(noLinesJoey),
            Monica = sum(noLinesMonica),
            Phoebe = sum(noLinesPhoebe),
            Rachel = sum(noLinesRachel),
            Ross = sum(noLinesRoss))%>%
  mutate(Total = Chandler+Ross+Rachel+Monica+Phoebe+Joey) %>%
  mutate(ChandlerPer = Chandler*100/Total,
         JoeyPer = Joey*100/Total,
         MonicaPer = Monica*100/Total,
         PhoebePer = Phoebe*100/Total,
         RachelPer = Rachel*100/Total,
         RossPer = Ross*100/Total)

pos <- numeric()
for(i in 1:10){
  x <- cumsum(as.numeric(perSeason[i,9:14])) - as.numeric(perSeason[i,9:14]) / 2
  pos <- rbind(pos, x)
}

perSeasonLines <- c(perSeason$ChandlerPer, perSeason$JoeyPer,
                    perSeason$MonicaPer, perSeason$PhoebePer,
                    perSeason$RachelPer, perSeason$RossPer)

type <- sort(unlist(lapply(friends, function(x) rep(x, 10))))

pos <- as.vector(pos)
  
seasons <- rep(perSeason$season, 6)

perSeason2 <- data.frame(type, seasons, perSeasonLines, pos) %>%
  mutate(seasons = as.factor(seasons),
         type = as.factor(type))
# plot
g <- ggplot(perSeason2) +
  geom_bar(stat = "identity",
           aes(x=seasons, y=perSeasonLines, fill=type)) +
  scale_fill_brewer(palette="Set2") +
  geom_text(data=perSeason2, aes(x = seasons, y=pos,
                            label = paste0(round(perSeasonLines, 1), "%"))) +
  ggtitle("Percentage of Lines Spoken Per Season") +
  labs(x="Seasons", y="Lines %") +
  theme_bw() + 
  theme(axis.title.x = element_text(color="black", size=11, face="bold"),
        axis.title.y = element_text(color="black", size=11, face="bold"))
g

# Numer of total words
g <- ggplot() +
  geom_bar(stat = "identity",
           aes(x=friends, y=words, fill=friends)) +
  xlab("Friends") + ylab("Lines") +
  ggtitle("Total Number of Words")
g 

# Words per season
perSeason <- data %>%
  group_by(season) %>%
  summarise(Chandler = sum(noWordsChandler),
            Joey = sum(noWordsJoey),
            Monica = sum(noWordsMonica),
            Phoebe = sum(noWordsPhoebe),
            Rachel = sum(noWordsRachel),
            Ross = sum(noWordsRoss))%>%
  mutate(Total = Chandler+Ross+Rachel+Monica+Phoebe+Joey) %>%
  mutate(ChandlerPer = Chandler*100/Total,
         JoeyPer = Joey*100/Total,
         MonicaPer = Monica*100/Total,
         PhoebePer = Phoebe*100/Total,
         RachelPer = Rachel*100/Total,
         RossPer = Ross*100/Total)

pos <- numeric()
for(i in 1:10){
  x <- cumsum(as.numeric(perSeason[i,9:14])) - as.numeric(perSeason[i,9:14]) / 2
  pos <- rbind(pos, x)
}

perSeasonWords <- c(perSeason$ChandlerPer, perSeason$JoeyPer,
                    perSeason$MonicaPer, perSeason$PhoebePer,
                    perSeason$RachelPer, perSeason$RossPer)

type <- sort(unlist(lapply(friends, function(x) rep(x, 10))))

pos <- as.vector(pos)

seasons <- rep(perSeason$season, 6)

perSeason2 <- data.frame(type, seasons, perSeasonLines, pos) %>%
  mutate(seasons = as.factor(seasons),
         type = as.factor(type))

# plot
g <- ggplot(perSeason2) +
  geom_bar(stat = "identity",
           aes(x=seasons, y=perSeasonWords, fill=type)) +
  scale_fill_brewer(palette="Set2") +
  geom_text(data=perSeason2, aes(x = seasons, y=pos,
                                 label = paste0(round(perSeasonWords, 1), "%"))) +
  ggtitle("Percentage of Words Spoken Per Season") +
  labs(x="Seasons", y="Words %") +
  theme_bw() + 
  theme(axis.title.x = element_text(color="black", size=11, face="bold"),
        axis.title.y = element_text(color="black", size=11, face="bold"))
g


# Numer of total scenes   
g <- ggplot() +
  geom_bar(stat = "identity",
           aes(x=friends, y=scenes, fill=friends)) +
  xlab("Friends") + ylab("No of Screen Appearances") +
  ggtitle("Total Number of Screen Appearances")
g

# Number of individual scenes
checkIndvidual <- function(x, i){
  if(all(x[-i]==0) & x[i] != 0) TRUE
  else FALSE
} 

individualScenes <- integer()

for(i in 1:6){
  t <- sum(apply(data[,5:10], 1,
                 function(x) checkIndvidual(x, i)))
  individualScenes <- c(individualScenes, t)   
}

# plot
g <- ggplot() +
  geom_bar(stat = "identity",
           aes(x=friends,
               y=individualScenes,
               fill=friends)) +
  xlab("Friends") + ylab("Individual Screen Appearances") +
  ggtitle("Total Number of Individual Screen Appearances")
g

# Number of mentions in episode title
mentions <- rep(0, 6)
names(mentions) <- friends

episodeTitles <- data %>%
  distinct(episodeTitle) %>%
  select(episodeTitle) %>%
  mutate(episodeTitle = tolower(episodeTitle))


for(friend in friends){
  mentions[friend] <- sum(grepl(tolower(friend),
                                tolower(episodeTitles$episodeTitle)))
}

# plot
g <- ggplot() +
  geom_bar(stat = "identity",
           aes(x=friends,
               y=mentions,
               fill=friends)) +
  xlab("Friends") + ylab("Mentions in the Episode Title") +
  ggtitle("Number of Mentions in the Episode Title")
g

# Central Perk Screen Appearances
centralPerkData <- data %>% 
  mutate(scene = tolower(scene)) %>%
  filter(grepl("central perk", scene) | grepl("perk", scene))

# plot
g <- ggplot() +
  geom_bar(stat = "identity",
           aes(x=friends,
               y=apply(centralPerkData[,5:10], 2, function(x) sum(x!=0)),
               fill=friends)) +
  xlab("Friends") + ylab("No of Screen Appearances") +
  ggtitle("Total Number of Screen Appearances at Central Perk")
g


# Monica's Apartment Screen Appearances
monicasApartment <- data %>%
  filter(grepl("monica", tolower(scene)))

scenes <- apply(monicasApartment[,5:10], 2, function(x) sum(x!=0))

# plot
g <- ggplot() +
  geom_bar(stat = "identity",
           aes(x=friends,
               y=scenes,
               fill=friends)) +
  xlab("Friends") + ylab("No of Screen Appearances") +
  ggtitle("Total Number of Screen Appearances at Monica's Apartment")
g

# Other Speakers
speakers <- unlist(strsplit(as.character(data$speakers), ","))
count <- rep(1, length(speakers))

speakersData <- as.data.frame(cbind(speakers = tolower(speakers),
                                    count = count)) %>%
  mutate(count = as.integer(count)) %>%
  group_by(speakers) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))
  
others <- c("Gunther", "Mike", "janice")
scenes <- unname(unlist(c(speakersData[speakersData[,1] == "gunther", 2],
            speakersData[speakersData[,1] == "mike", 2],
            speakersData[speakersData[,1] == "janice", 2])))

# plot
g <- ggplot() +
  geom_bar(stat = "identity",
           aes(x=others,
               y=scenes,
               fill=others)) +
  xlab("Friends") + ylab("No of Screen Appearances") +
  ggtitle("Total Number of Screen Appearances at Monica's Apartment")
g
