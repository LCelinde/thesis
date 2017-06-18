
### MASTER THESIS CODE - LIEKE TEL ###

#-------------------------------------------------------------------------------

## DATA FROM HTML FILES 

#--------------------------------------------------------------------------------
setwd("E:/Kickstarter")
project_files <- list.files("Technology/", 
                            pattern = "1469786401_description.html", 
                            full.names = TRUE, recursive = TRUE)


library("XML")
library("stringr")
library("knitr")

#First, loop over all the HTML files, parsing each one in turn and store 
#the resulting parsed HTML in the corresponding entry of a list

nprojects <- length(project_files)
projects_parsed <- vector(mode = "list", length = nprojects)
for (i in seq_along(project_files)) {
  tmp <- readLines(project_files[i])
  projects_parsed[[i]] <- htmlParse(tmp)    
}

#define Xpaths for different columns 

#project descriptions
xdescriptions <- "//*[@id='content-wrap']/div[2]/section[1]/div/div/div/div/div[1]/div[2]"

#percentagepledged
xpercentagepledged <- "//*[@id='pledged']/@data-percent-raised"

#project duration 
xduration <- "//*[@id='project_duration_data']/@data-duration"

#video present"
xvideo <- "//*[@id='video-section']/@data-has-video"

#number of updates
xupdates <- "//*[@id='content-wrap']/div[2]/div/div/div/div[2]/a[3]/span"

#number of comments
xcomments <- "//*[@id='content-wrap']/div[2]/div/div/div/div[2]/a[4]/span/data"

# create empty data frame 
data_projects <- data.frame(Filename = dirname(project_files), Category = basename(project_files), 
                            Success = NA, Prototypicality = NA, Description = NA,
                            Duration = NA, Video = NA, Numberofupdates = NA, Numberofcomments = NA, 
                            stringsAsFactors = FALSE)

data_projects$Filename <- gsub("Technology//", "", data_projects$Filename )

for (i in seq_along(projects_parsed)) {
val <- xpathSApply(projects_parsed[[i]], path = xdescriptions, xmlValue)
data_projects [i, "Description"] <- if (length(val) == 0) NA else val

val <- xpathSApply(projects_parsed[[i]], path = xvideo )
data_projects [i, "Video"] <- if (length(val) == 0) NA else val 

val <- xpathSApply(projects_parsed[[i]], path = xupdates, xmlValue)
data_projects [i, "Numberofupdates"] <- if (length(val) == 0) NA else val

val <- xpathSApply(projects_parsed[[i]], path = xcomments, xmlValue)
data_projects [i, "Numberofcomments"] <- if (length(val) == 0) NA else val

val <- xpathSApply(projects_parsed[[i]], path = xpercentagepledged)
data_projects [i, "Percentagepledged"] <- if (length(val) == 0) NA else val

val <- xpathSApply(projects_parsed[[i]], path = xduration)
data_projects [i, "Duration"] <- if (length(val) == 0) NA else val} 


#delete projects in other languages than english (from 297 to 287 projects)
data_projects <- data_projects[-c(19, 46, 61, 85, 150, 183, 240, 251, 280, 286), ]

#----------------------------------------------------------------------------
#SAME ANALYSIS FOR CATEGORY 'DESIGN'

setwd("E:/Kickstarter")
project_files <- list.files("Design/", 
                            pattern = "1469764801_description.html", 
                            full.names = TRUE, recursive = TRUE)

#First, loop over all the HTML files, parsing each one in turn and store 
#the resulting parsed HTML in the corresponding entry of a list

nprojects <- length(project_files)
projects_parsed <- vector(mode = "list", length = nprojects)
for (i in seq_along(project_files)) {
  tmp <- readLines(project_files[i])
  projects_parsed[[i]] <- htmlParse(tmp)    
}

# create empty data frame
data_projects2 <- data.frame(Filename = dirname(project_files), Category = basename(project_files),
                             Success = NA, Prototypicality = NA, Description = NA,
                             Duration = NA, Video = NA, Numberofupdates = NA, Numberofcomments = NA, 
                              stringsAsFactors = FALSE)

data_projects2$Filename <- gsub("Design//", "", data_projects2$Filename )


for (i in seq_along(projects_parsed)) {
  val <- xpathSApply(projects_parsed[[i]], path = xdescriptions, xmlValue)
  data_projects2 [i, "Description"] <- if (length(val) == 0) NA else val
  
  val <- xpathSApply(projects_parsed[[i]], path = xvideo )
  data_projects2 [i, "Video"] <- if (length(val) == 0) NA else val 
  
  val <- xpathSApply(projects_parsed[[i]], path = xupdates, xmlValue)
  data_projects2 [i, "Numberofupdates"] <- if (length(val) == 0) NA else val
  
  val <- xpathSApply(projects_parsed[[i]], path = xcomments, xmlValue)
  data_projects2 [i, "Numberofcomments"] <- if (length(val) == 0) NA else val
  
  val <- xpathSApply(projects_parsed[[i]], path = xpercentagepledged)
  data_projects2 [i, "Percentagepledged"] <- if (length(val) == 0) NA else val
  
  val <- xpathSApply(projects_parsed[[i]], path = xduration)
  data_projects2 [i, "Duration"] <- if (length(val) == 0) NA else val} 


#delete projects in other languages than english (from 241 to 230 projects)
data_projects2 <- data_projects2[-c(17, 26, 32, 70, 78, 115, 145, 151, 166, 199, 220), ]


#bind the two dataframes
data_projects_new <- rbind(data_projects, data_projects2)

#-------------------------------------------------------------------------------

#DATA FROM JSON FILES

#--------------------------------------------------------------------------------
##JSON from Technology
setwd("E:/Kickstarter")
json_files <- list.files("Technology", 
                         pattern = '1469786401.json', 
                         full.names = TRUE, recursive = TRUE)
nprojects <- length(json_files)
library("jsonlite")
library("utils")
library("dplyr")

JSON_data <- data.frame(Filename = NA, Goal = NA, Static_usd_rate = NA,  
                        Staff_pick = NA, stringsAsFactors = FALSE )

for(i in 1:nprojects) {
  wf <- fromJSON(json_files[i])
  data_flat <- data.frame(wf$id, wf$goal, wf$static_usd_rate, wf$staff_pick,
                          stringsAsFactors = FALSE )
  JSON_data[i,] <- data_flat
}


#----------------------------------------------------------------------------
#SAME ANALYSIS FOR CATEGORY 'DESIGN'

##JSON from Design
setwd("E:/Kickstarter")
json_files2 <- list.files("Design", 
                         pattern = '1469786401.json', 
                         full.names = TRUE, recursive = TRUE)
nprojects <- length(json_files2)
library("jsonlite")
library("utils")
library("dplyr")

JSON_data2 <- data.frame(Filename = NA, Goal = NA, Static_usd_rate = NA, Staff_pick = NA, 
                        stringsAsFactors = FALSE )

for(i in 1:nprojects) {
  wf <- fromJSON(json_files2[i])
  data_flat2 <- data.frame(wf$id, wf$goal, wf$static_usd_rate, wf$staff_pick,
                          stringsAsFactors = FALSE )
  JSON_data2[i,] <- data_flat2
}

#bind json data
JSON_data_new <- rbind(JSON_data, JSON_data2)

#----------------------------------------------------------------------------
#MERGE DATA
#---------------------------------------------------------------------------

#merge html and json data
data_projects_new <- merge(data_projects_new, JSON_data_new, by = "Filename")

#-------------------------------------------------------------------------------
#change numbers in numeric, %pledged

options(scipen=999)
data_projects_new$Percentagepledged <- as.numeric(data_projects_new$Percentagepledged)
data_projects_new$Percentagepledged100 <- data_projects_new$Percentagepledged * 100
data_projects_new$Percentagepledged100 <- round(data_projects_new$Percentagepledged100, digits = 2)

data_projects_new$Duration <- as.numeric(data_projects_new$Duration)

data_projects_new$Numberofupdates <- as.numeric(data_projects_new$Numberofupdates)

data_projects_new$Numberofcomments <- as.numeric(data_projects_new$Numberofcomments)
data_projects_new$Numberofcomments[is.na(data_projects_new$Numberofcomments)] <- 0

#succesfully funded dummy
data_projects_new$Success <- as.numeric(data_projects_new$Percentagepledged100 >= 100)
data_projects_new$Success <- factor(data_projects_new$Success, labels = c("No", "Yes"))

#Goal in USD
data_projects_new$USD_goal <- data_projects_new$Goal * data_projects_new$Static_usd_rate

#count number of words in description
split <- str_split(data_projects_new$Description, " ")
data_projects_new$Descriptionlength <- sapply(split,length)

#make video dummy and staff pick dummy
data_projects_new$Video <- as.integer(data_projects_new$Video == "true")
data_projects_new$Video <- factor(data_projects_new$Video, labels = c("No", "Yes"))

data_projects_new$Staff_pick <- as.integer(data_projects_new$Staff_pick == "TRUE")
data_projects_new$Staff_pick <- factor(data_projects_new$Staff_pick, labels = c("No", "Yes"))

#make category variables
data_projects_new$Category <- gsub("1469786401_description.html", "Technology", data_projects_new$Category)
data_projects_new$Category <- gsub("1469764801_description.html", "Design", data_projects_new$Category)
data_projects_new$Category <- factor(data_projects_new$Category)

#-------------------------------------------------------------------------------

## readability ##

#------------------------------------------------------------------------
library("readability")

data_projects_new$nr <- c(1:nrow(data_projects_new))
read <- with(data_projects_new, readability(Description, list(nr), order.by.readability = FALSE))

data_projects_new$FOGindex <- read$Gunning_Fog_Index
data_projects_new$FOGindex <- round(data_projects_new$FOGindex, digits = 2)
data_projects_new$FOGindex <- as.numeric(data_projects_new$FOGindex)

#spelling
library(hunspell)
misspelled <- hunspell(data_projects_new$Description, 
                                    ignore = c("Â", "app", "smartphone", "smartphones", 
                                               "www", "TEG", "iOS", "UX", "DT", "colour", 
                                               "apps", "eBook", "programme", "Facebook", 
                                               "hashtags", "hashtag", "iPhone", "offline", 
                                               "usernames", "Instagram", "GoogleMaps", "IOS"
                                               , "Guidester", "hardcopy", "Youtube", "USB", 
                                               "crowdfunding", "Kickstarter", "Tanky", "CCD", 
                                               "FOV", "FPV", "Ghz", "GoPro", "Timeline", "AEMX", 
                                               "https", "youtube", "png", "http", "PlayStation"
                                               , "facebook", "iPad", "Powerleaf", "GPIO", "retweets"
                                               , "Coldplay", "VWO", "Akido","TAXZI", "texting", "Ø",
                                               "MKD", "SEO", "Bluetooth", "nixie", "htm", "google", "dL"
                                               , "wikipedia", "Firebase", "ANE", "SMK", "GPS", "Javascript"
                                               , "EdCar", "WiFi", "Zoose"))

data_projects_new$misspelledwords <- misspelled
split <- str_split(data_projects_new$misspelledwords, " ")
data_projects_new$misspelledwords <- sapply(split,length)


#delete rows without discriptions or too much spelling errors (probably other language)
data_projects_new <-data_projects_new[!(data_projects_new$Descriptionlength <=10),]
data_projects_new <-data_projects_new[!(data_projects_new$misspelledwords >=160),]

#-------------------------------------------------------------------------------

## COMPUTING PROTOTYPICALITY VARIABLE ##

#-----------------------------------------------------------------------------
library("tm")
library("SnowballC")
require("proxy")
library("kSamples")
library("plyr")
library("dplyr")

k = 5 
set.seed(1)
data_projects_new$id <- sample(1:k, nrow(data_projects_new), replace = TRUE)
list <- 1:k
table(data_projects_new$id)

fold_ind <- split(seq_len(nrow(data_projects_new)), data_projects_new$id)
data_AD <- data.frame(AD = rep(NA, nrow(data_projects_new)))
pts <- 0:1000/1000

progress.bar <- create_progress_bar("text")
progress.bar$init(k)


for (i in 1:k){
  
  trainingset <- subset(data_projects_new, id %in% list[-i])
  testset <- subset(data_projects_new, id %in% c(i))
  
  ## Create a text corpus based on text files
  docstrain <- Corpus(VectorSource(trainingset$Description)) 
  
  ## Custom transformation to change a pattern to a space
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  docstrain <- tm_map(docstrain, toSpace, "-")
  docstrain <- tm_map(docstrain, toSpace, ":")
  docstrain <- tm_map(docstrain, toSpace, "'")
  docstrain <- tm_map(docstrain, toSpace, "'")
  docstrain <- tm_map(docstrain, toSpace, " -")
  docstrain <- tm_map(docstrain, toSpace, "/")
  
  docstrain <- tm_map(docstrain, removePunctuation)
  docstrain <- tm_map(docstrain, content_transformer(tolower))
  docstrain <- tm_map(docstrain, removeNumbers)
  docstrain <- tm_map(docstrain, removeWords, stopwords("english"))
  docstrain <- tm_map(docstrain, stripWhitespace)
  
  ## stemming
  docstrain <- tm_map(docstrain, stemDocument)
  
  ## Some clean up for specific issues
  docstrain <- tm_map(docstrain, content_transformer(gsub),
                      pattern = "organiz", replacement = "organ")
  docstrain <- tm_map(docstrain, content_transformer(gsub),
                      pattern = "organis", replacement = "organ")
  docstrain <- tm_map(docstrain, content_transformer(gsub),
                      pattern = "andgovern", replacement = "govern")
  docstrain <- tm_map(docstrain, content_transformer(gsub),
                      pattern = "inenterpris", replacement = "enterpris")
  docstrain <- tm_map(docstrain, content_transformer(gsub),
                      pattern = "team-", replacement = "team")
  
  #make tdm for training data
  tdm <- TermDocumentMatrix(docstrain, control = list(minWordLength = 3, weighting = weightBin, bounds = list(global=c(20, Inf))))
  tdm <- as.matrix(tdm)
  
  #make jaccard matrix with words of baseline
  jaccard_dist_mat <- proxy::dist(as.matrix(tdm), method = 'jaccard', by_rows = TRUE, convert_similarities = FALSE)
  jaccard_dist <- as.matrix(jaccard_dist_mat)
  
  #take subset of similarity matrix for each project in baseline
  submatsbase <- vector(mode = "list", length = nrow(trainingset))
  for (j in 1:nrow(trainingset)) {
    nms <- names(tdm[tdm[, j] == 1, j])
    ind <- match(x = nms, table = rownames(jaccard_dist))
    submatsbase[[j]] <- jaccard_dist[ind, ind] 
  }
  
  #make baseline distribution (4/5 of data)
  baselinedistributions <- vector(mode = "list", length = nrow(trainingset))
  for (l in seq_along(submatsbase)) {
    baselinedistributions[[l]]  <- ecdf(submatsbase[[l]]) (pts)  } 
  
  distributions_baseline <- do.call(cbind, baselinedistributions)
  average_baseline <- rowMeans(distributions_baseline)
  plot(average_baseline)
  
  
  #take subset of similarity matrix for each project in TEST DATA
  
  ## Create a text corpus based on text files
  docstest <- Corpus(VectorSource(testset$Description)) 
  
  ## Custom transformation to change a pattern to a space
  docstest <- tm_map(docstest, toSpace, "-")
  docstest <- tm_map(docstest, toSpace, ":")
  docstest <- tm_map(docstest, toSpace, "'")
  docstest <- tm_map(docstest, toSpace, "'")
  docstest <- tm_map(docstest, toSpace, " -")
  docstest <- tm_map(docstest, toSpace, "/")
  
  docstest <- tm_map(docstest, removePunctuation)
  docstest <- tm_map(docstest, content_transformer(tolower))
  docstest <- tm_map(docstest, removeNumbers)
  docstest <- tm_map(docstest, removeWords, stopwords("english"))
  docstest <- tm_map(docstest, stripWhitespace)
  
  ## stemming
  docstest <- tm_map(docstest, stemDocument)
  
  ## Some clean up for specific issues
  docstest <- tm_map(docstest, content_transformer(gsub),
                     pattern = "organiz", replacement = "organ")
  docstest <- tm_map(docstest, content_transformer(gsub),
                     pattern = "organis", replacement = "organ")
  docstest <- tm_map(docstest, content_transformer(gsub),
                     pattern = "andgovern", replacement = "govern")
  docstest <- tm_map(docstest, content_transformer(gsub),
                     pattern = "inenterpris", replacement = "enterpris")
  docstest <- tm_map(docstest, content_transformer(gsub),
                     pattern = "team-", replacement = "team")
  
  #make tdm for testset
  tdmtest <- TermDocumentMatrix(docstest, control = list(minWordLength = 3, weighting = weightBin, bounds = list(global=c(5, Inf))))
  tdmtest <- as.matrix(tdmtest)
  
  #make submats for projects in testset
  submatstest <- vector(mode = "list", length = nrow(testset))
  for (j in 1:nrow(testset)) {
    nms <- names(tdmtest[tdmtest[, j] == 1, j])
    ind <- match(x = nms, table = rownames(jaccard_dist))
    submatstest[[j]] <- jaccard_dist[ind, ind] }
  
  #make distribution of edge weights for each project (1/5 of data)
  distributions <- vector(mode = "list", length = nrow(testset))
  for (l in seq_along(submatstest)) {
    distributions[[l]]  <- ecdf(submatstest[[l]]) (pts)  } 
  
  #compare each distribution of edge weight of each project to the prototypical 
  #distribution of baseline with KS test
  ad_test <- vector(mode = "list", length = nrow(testset))
  for (j in seq_along (distributions)) {
    ad_test[[j]] <- ad.test(distributions[[j]], average_baseline, method = "exact", dist = FALSE, Nsim = 1000)
    
    sapply(ad_test, function(x) x$ad[1, 1])
    data_AD$AD[fold_ind[[i]]] <- sapply(ad_test, function(x) x$ad[1, 1])
    
    
  }
  
  progress.bar$step()
  
}

data_projects_new$Prototypicality <- data_AD$AD
data_projects_new$Prototypicality <- as.numeric(data_projects_new$Prototypicality)


#-------------------------------------------------------------------------------

#EXPLORATORY ANALYSIS

#------------------------------------------------------------------------


#STATISTICS OF NUMERIC VARIABLES
summary_numeric <- function(x) {
  c(N = sum(!is.na(x)),
    Mean = mean(x, na.rm = TRUE), 
    Std.Dev = sd(x, na.rm = TRUE), 
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE)
  )
}

col_num <- sapply(data_projects_new, is.numeric)
data_projects_num <- data_projects_new[, col_num]

statistics <- data.frame(t(sapply(data_projects_num, summary_numeric)))


#STATISTICS OF CATEGORICAL VARIABLES
col_fac <- sapply(data_projects_new, is.factor)
data_projects_fac <- data_projects_new[, col_fac]

summary_categorical <- function(x) {
  ## Tabulate the variable, sort from largest to smallest
  tab <- sort(table(x), decreasing = TRUE)
  levs <- names(tab)
  
  ## Return a vector of summary statistics
  data.frame(
    N = sum(!is.na(x)),
    NLevels = nlevels(x),
    Mode = levs[1], 
    Mode_Proportion = tab[1] / sum(tab),
    Mode2 = levs[2], 
    Mode2_Proportion = tab[2] / sum(tab),
    stringsAsFactors = FALSE
  )
}

statistics <- data.frame(t(sapply(data_projects_fac, summary_categorical)))


#Statistics of success
summary(data_projects_new$Success)
by(data_projects_new$Success, data_projects_new$Category, summary)


#DISTRIBUTIONS
ggplot(data_projects_new, aes(x = USD_goal)) + geom_histogram()+ theme_minimal() + labs(title = "Distribution USD_goal")

ggplot(data_projects_new, aes(x = Prototypicality)) + geom_histogram()+ theme_minimal() + labs(title = "Distribution Prototypicality")

ggplot(data_projects_new, aes(x = FOGindex)) + geom_histogram()+ theme_minimal() + labs(title = "Distribution FOG-index")

ggplot(data_projects_new, aes(x = misspelledwords)) + geom_histogram()+ theme_minimal() + labs(title = "Distribution spelling errors")

ggplot(data_projects_new, aes(x = Numberofupdates)) + geom_histogram()+ theme_minimal() + labs(title = "Distribution Number of updates")

ggplot(data_projects_new, aes(x = Numberofcomments)) + geom_histogram()+ theme_minimal() + labs(title = "Distribution Number of comments")

ggplot(data_projects_new, aes(x = Descriptionlength)) + geom_histogram()+ theme_minimal() + labs(title = "Distribution Description length")

ggplot(data_projects_new, aes(x = Video)) + geom_bar() + theme_minimal() + labs(title = "Distribution Video")

ggplot(data_projects_new, aes(x = Staff_pick)) + geom_bar() + theme_minimal() + labs(title = "Distribution Staff-pick")

ggplot(data_projects_new, aes(x = Category)) + geom_bar() + theme_minimal() + labs(title = "Distribution Category")

#---------------------------------------------
##TEXT ANALYSIS
#---------------------------------------------------
library("tm")

docs <- Corpus(VectorSource(data_projects_new$Description)) 

## Custom transformation to change a pattern to a space
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, toSpace, "/")

## Removing punctuation: replace a space
docs <- tm_map(docs, removePunctuation)

## Transform to lower case
docs <- tm_map(docs, content_transformer(tolower))

## Strip out numbers
docs <- tm_map(docs, removeNumbers)

## Remove stopwords using standard stopword list
docs <- tm_map(docs, removeWords, stopwords("english"))

## Strip out whitespace 
docs <- tm_map(docs, stripWhitespace)

## Need SnowballC library for stemming
library("SnowballC")

## Stem document
docs <- tm_map(docs, stemDocument)

## Some clean up for specific issues
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "inenterpris", replacement = "enterpris")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "team-", replacement = "team")

## Create document-term matrix
dtm <- DocumentTermMatrix(docs)
dim(dtm)

## Word stem frequencies
freq <- colSums(as.matrix(dtm))

## length should be total number of terms
length(freq)

## Create sort order
ord <- order(freq, decreasing = TRUE)

## Inspect most / least frequently occurring terms
freq[head(ord)]
freq[tail(ord)]


#make TERMDOCUMENT MATRIX with only 1 and 0
tdm <- TermDocumentMatrix(docs, control = list(minWordLength = 3, weighting = weightBin, bounds = list(global=c(20, Inf))))
tdm <- as.matrix(tdm)

## Word stem frequencies
freq <- rowSums(as.matrix(tdm))

## length should be total number of terms
length(freq)

## Create sort order
ord <- order(freq, decreasing = TRUE)

## Inspect most / least frequently occurring terms in docs
freq[head(ord)]
freq[tail(ord)]

#make jaccard matrix
jaccard_dist <- proxy::dist(as.matrix(tdm), method = 'jaccard', by_rows = TRUE, convert_similarities = FALSE)
jaccard_dist <- as.matrix(jaccard_dist)
diag(jaccard_dist) <- NA
jaccard_dist[lower.tri(jaccard_dist)] <- NA

head(sort(jaccard_dist, decreasing = TRUE, na.last = TRUE))

x <- which(jaccard_dist>=sort(jaccard_dist, decreasing = T)[6], arr.ind = T)

rnames = rownames(jaccard_dist)[x[,1]]
cnames = rownames(jaccard_dist)[x[,2]]

dd <- data.frame(rnames = rnames, cnames = cnames)
dd

jaccard_dist["html", "replay"]
jaccard_dist["browser", "replay"]
jaccard_dist["sound", "replay"]
jaccard_dist["browser", "html"]
jaccard_dist["sound", "html"]
jaccard_dist["sound", "browser"]


#lowest jaccard indices

system.time( 
  idx <- which( 
    matrix(jaccard_dist %in% head(sort(jaccard_dist, decreasing = FALSE, na.last = NA), 10), 
           nr = nrow(jaccard_dist)), arr.ind = TRUE, useNames = TRUE))
idx

idx <- idx[1:6,]

rnames = rownames(jaccard_dist)[idx[,1]]
cnames = rownames(jaccard_dist)[idx[,2]]

dd <- data.frame(rnames = rnames, cnames = cnames)
dd

jaccard_dist["basi", "band"]
jaccard_dist["basi", "cash"]
jaccard_dist["mail", "cash"]
jaccard_dist["mail", "clean"]
jaccard_dist["highest", "clip"]
jaccard_dist["recycl", "clip"]


## make wordcloud for title page
library("wordcloud2")

v <- sort(rowSums(tdm),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

wordcloud(names(freqr), freqr, min.freq = 80, random.order=FALSE, rot.per=0.29, colors = brewer.pal(3, "Set2"), shape = "circle")
wordcloud <- wordcloud2(d, color = "random-light", size = 0.2, gridSize = 0, shape = "circle", maxRotation = pi/2, rotateRatio = 0.3)

letterCloud(d, "K", color = "random-light", size = 0.2)

#-------------------------------------------------------------------------------

#EXPLORATION PLOTS

#-------------------------------------------------------------------
#plot every test variable to the response variable
library("ggplot2")

#numeric variables
ggplot(data_projects_new, aes(x = USD_goal, fill = Success)) + 
  geom_density(alpha = 0.5) + scale_x_log10() + theme_minimal() + 
  labs(x = "Goal in USD (log)", y = "Density", title = "Distribution of the funding goal (log)")

ggplot(data_projects_new, aes(x = Prototypicality, fill = Success)) + 
  geom_density(alpha = 0.5) + scale_x_log10() + theme_minimal() + 
  labs(x = "Prototypicality (log)", y = "Density", title = "Distribution of the prototypicality (log)") 
  
ggplot(data_projects_new, aes(x = FOGindex, fill = Success)) + 
  geom_density(alpha = 0.5) + theme_minimal() + 
  labs(x = "FOGindex - Lexical complexity", y = "Density", title = "Distribution of the FOGindex - lexical complexity") 

ggplot(data_projects_new, aes(x = misspelledwords, fill = Success)) + 
  geom_density(alpha = 0.5) + theme_minimal() + 
  labs(x = "Number of spelling errors", y = "Density", title = "Distribution of the spelling errors") 

ggplot(data_projects_new, aes(x = Duration, fill = Success)) + 
  geom_density(alpha = 0.5) + theme_minimal() + 
  labs(x = "Duration", y = "Density", title = "Distribution of the duration") 

ggplot(data_projects_new, aes(x = Numberofupdates, fill = Success)) + 
  geom_density(alpha = 0.5) + scale_x_log10() + theme_minimal() + 
  labs(x = "Number of updates (log)", y = "Density", title = "Distribution of the number of updates (log)") 

ggplot(data_projects_new, aes(x = Numberofcomments, fill = Success)) + 
  geom_density(alpha = 0.5) + scale_x_log10() + theme_minimal() + 
  labs(x = "Number of comments (log)", y = "Density", title = "Distribution of the number of comments (log)") 

ggplot(data_projects_new, aes(x = Descriptionlength, fill = Success)) + 
  geom_density(alpha = 0.5) + theme_minimal() + 
  labs(x = "Amount of words in description", y = "Density", title = "Distribution of the description length") 

#categorical plots
ggplot(data_projects_new, aes(x = Video, fill = Success)) + 
  geom_bar() + theme_minimal() + 
  labs(x = "Video", y = "Frequency", title = "Distribution of the presence of a video") 

ggplot(data_projects_new, aes(x = Staff_pick, fill = Success)) + 
  geom_bar() + theme_minimal() + 
  labs(x = "Staff-pick", y = "Frequency", title = "Distribution of Staff-pick") 

ggplot(data_projects_new, aes(x = Category, fill = Success)) + 
  geom_bar() + theme_minimal() + 
  labs(x = "Category", y = "Frequency", title = "Distribution of category") 


#associations
pairs(data_projects_new[ , c("USD_goal", "Success", "Duration", "Prototypicality", "Video", "Staff_pick", "Category", "Numberofupdates", "Numberofcomments", "misspelledwords", "FOGindex", "Descriptionlength")])


#-------------------------------------------------------------------------------
#change success to 0 (= fail) and 1 (= success)
data_projects_new$Success <- as.integer(data_projects_new$Success == "Yes")
data_projects_new$Success <- as.factor(data_projects_new$Success)

#copy data frame, just in case
data_copy <- data_projects_new
###############################

#-------------------------------------------------------------

## LOGISTIC REGRESSIONS ##

#--------------------------------------------------------

#transform variables that cause separation problems
ggplot(data_projects_new, aes(x = Numberofcomments)) + geom_bar()
sum(data_projects_new$Numberofcomments>100)
data_projects_new <- subset(data_projects_new, !data_projects_new$Numberofcomments>100)

data_projects_new$log_USD_Goal <- log10(data_projects_new$USD_goal)

#---------------------------------------------------------------------------------------------

## MODEL 1 = only control ## 


library("boot")

#logistic regression with 10-fold cross validation
fit_glm <- glm(Success ~ log_USD_Goal +  Duration  + Staff_pick
                + Video + Numberofupdates + Numberofcomments + Category, 
               data = data_projects_new, family = binomial(link = "logit"))
summary(fit_glm)
fit_glm$coefficients

library("car")
vif(fit_glm)


# Cross validation (customized)


library(plyr)   # progress bar
library(caret)  # confusion matrix

fpr <- NULL
fnr <- NULL
pbar <- create_progress_bar('text')
pbar$init(10)

acc <- NULL
mce <- NULL

## 10-fold CV 
library("boot")
data_projects_new$pred_prob <- 0
data_projects_new$pred <- 0
K <- 10

## Permutation of the row indices
set.seed(1)
n <- nrow(data_projects_new)
perm <- sample.int(n)

## Vector identifying the folds, randomly reordered
folds <- rep_len(1:K, length.out = n)[perm]

for(fold in 1:K){
  
  train <- data_projects_new[folds != fold, ]
  test <- data_projects_new[folds == fold, ]
  
  
  # Fitting
  model <- glm(Success ~ log_USD_Goal + Duration  + Staff_pick
               + Video + Numberofupdates + Numberofcomments + Category, 
               data = train, family = binomial(link = "logit"))
  
  # Predict results
  results_prob <- predict(model, test, type='response')
  data_projects_new[folds == fold,  "pred_prob"] <- results_prob
  
  # If prob > 0.5 then 1, else 0
  results <- ifelse(results_prob > 0.5,1,0)
  data_projects_new[folds == fold,  "pred"] <- results
  
  # Actual answers
  answers <- test$Success
  
  # Accuracy calculation
  misClasificError <- mean(answers != results)
  
  # Collecting results
  acc[fold] <- 1-misClasificError
  mce[fold] <- misClasificError
  
  pbar$step()
}


# Average accuracy of the model
mean(acc)
mean(mce)

#confusion matrix
table(data_projects_new$Success, data_projects_new$pred)


#----------------------------------------------------------------------------------------------------

## MODEL 2 - WITH TEXTUAL FEATURES ##


fit_glm2 <- glm(Success ~ Prototypicality + misspelledwords + FOGindex 
               + log_USD_Goal + Duration + Staff_pick
               + Video + Numberofupdates + Numberofcomments + Category
               + Descriptionlength, 
               data = data_projects_new, family = binomial(link = "logit"))
summary(fit_glm2)
fit_glm2$coefficients

vif(fit_glm2)


# Cross validation (customized)
fpr <- NULL
fnr <- NULL
pbar <- create_progress_bar('text')
pbar$init(10)

acc <- NULL
mce <- NULL

data_projects_new$pred_prob <- 0
data_projects_new$pred <- 0
K <- 10

set.seed(1)
n <- nrow(data_projects_new)
perm <- sample.int(n)

folds <- rep_len(1:K, length.out = n)[perm]

for(fold in 1:K){
  
  train <- data_projects_new[folds != fold, ]
  test <- data_projects_new[folds == fold, ]
  
  
  # Fitting
  model <- glm(Success ~ Prototypicality + misspelledwords + FOGindex 
               + log_USD_Goal + Duration + Staff_pick
               + Video + Numberofupdates + Numberofcomments + Category
               + Descriptionlength, 
               data = train, family = binomial(link = "logit"))
  
  
  # Predict results
  results_prob <- predict(model, test, type='response')
  data_projects_new[folds == fold,  "pred_prob"] <- results_prob
  
  # If prob > 0.5 then 1, else 0
  results <- ifelse(results_prob > 0.5,1,0)
  data_projects_new[folds == fold,  "pred"] <- results
  
  # Actual answers
  answers <- test$Success
  
  # Accuracy calculation
  misClasificError <- mean(answers != results)
  
  # Collecting results
  acc[fold] <- 1-misClasificError
  mce[fold] <- misClasificError
  
  pbar$step()
}


# Average accuracy of the model
mean(acc)
mean(mce)

#confusion matrix
table(data_projects_new$Success, data_projects_new$pred)



#-----------------------------------------------------------------------------------------

## MODEL 3 - best subset ##

library("bestglm")
data_for_best_subset <- data_projects_new[ , c("Prototypicality", "misspelledwords", 
                                               "FOGindex", "log_USD_Goal", "Duration",
                                               "Numberofcomments", "Numberofupdates",
                                               "Descriptionlength", "Video", "Staff_pick",
                                               "Category", "Success")]



#coefficients
fit_glm3 <- glm(Success ~ Prototypicality + log_USD_Goal + Numberofcomments + Video + Staff_pick + Category, 
             data = data_projects_new, family = binomial(link = "logit"))
summary(fit_glm3)
fit_glm3$aic


vif(fit_glm3)

#subset selection

combnall <- function(x, m = x) {
  do.call(c, lapply(1:m, combn, x = x, simplify = FALSE))
}

set.seed(1)
K = 10
library("plyr")
pbar <- create_progress_bar('text')
pbar$init(10)

## Split into K folds
perm <- sample.int(nrow(data_for_best_subset))
folds <- rep_len(1:K, length.out = nrow(data_for_best_subset))[perm]

## Matrix to store the acc stin
acc <- NULL
ind_best_acc <- NULL
ind_best <- NULL
best_subset <- NULL
accuracy <- NULL

nms <- names(data_for_best_subset)
x_cand <- setdiff(nms, "Success")

## Combinations to try as columns
subsets <- combnall(length(x_cand))
n_subsets <- length(subsets)

## Vector to store the log-likelihoods in
results_prob <- numeric(n_subsets)

set.seed(1)
## Loop over folds
for (j in 1:K) {
  
  ## Try each model 
  for (i in 1:n_subsets) {
    subset_formula <- reformulate(termlabels = x_cand[subsets[[i]]], 
                                  response = "Success", intercept = TRUE)
    
    for (fold in 1:K) {
      train <- data_for_best_subset[folds != fold, ]
      test <- data_for_best_subset[folds == fold, ]
      
      fit <- glm(subset_formula, data = train, family = binomial(link = "logit"))
      
      # Predict results
      results_prob <- predict(fit, newdata = test, type='response')
      data_projects_new[folds == fold,  "pred_prob"] <- results_prob
      
      # If prob > 0.5 then 1, else 0
      results <- ifelse(results_prob > 0.5,1,0)
      data_projects_new[folds == fold,  "pred"] <- results
      
      # Actual answers
      answers <- test$Success
      
      # Accuracy calculation
      misClasificError <- mean(answers != results)
      
      # Collecting results
      acc[fold] <- 1-misClasificError
    }
    
    accuracy[i] <- mean(acc)
    
  }
  
  ## Get and return the best subset
  ind_best_acc[[j]] <- max(accuracy)
  ind_best[[j]] <- which.max(accuracy)
  best_subset[[j]] <- x_cand[subsets[[ind_best[[j]]]]]
  
  pbar$step()
}

ind_best_acc
best_subset

plot(accuracy, main = "Classification accuracy of all possible subsets", xlab = "Subset", ylab = "Classification accuracy")
points(accuracy, col = "salmon")


#prediction with this model

pbar <- create_progress_bar('text')
pbar$init(10)

acc <- NULL
mce <- NULL

## 10-fold CV 
library("boot")
data_projects_new$pred_prob <- 0
data_projects_new$pred <- 0
K <- 10

## Permutation of the row indices
set.seed(1)
n <- nrow(data_projects_new)
perm <- sample.int(n)

## Vector identifying the folds, randomly reordered
folds <- rep_len(1:K, length.out = n)[perm]

for(fold in 1:K){
  
  train <- data_projects_new[folds != fold, ]
  test <- data_projects_new[folds == fold, ]
  
  # Fitting
  model <- glm(Success ~ Prototypicality + log_USD_Goal + Numberofcomments + Video + Staff_pick + Category, 
               data = train, family = binomial(link = "logit"))
  
  # Predict results
  results_prob <- predict(model, test, type='response')
  data_projects_new[folds == fold,  "pred_prob"] <- results_prob
  
  # If prob > 0.5 then 1, else 0
  results <- ifelse(results_prob > 0.5,1,0)
  data_projects_new[folds == fold,  "pred"] <- results
  
  # Actual answers
  answers <- test$Success
  
  # Accuracy calculation
  misClasificError <- mean(answers != results)
  
  # Collecting results
  acc[fold] <- 1-misClasificError
  mce[fold] <- misClasificError
  
  pbar$step()
}


# Average accuracy of the model
mean(acc)
mean(mce)

#confusion matrix
table(data_projects_new$Success, data_projects_new$pred)

## -----------------------------------------------------------------------------------------

## DECISION TREE ##

#---------------------------------------------------------------------------------------



library("rpart")
library("rpart.plot") 


data_projects_new$Success <- factor(data_projects_new$Success, labels = c("No", "Yes"))

controlpar <- rpart.control(minsplit = 2, minbucket = 1, cp = 0, 
                            maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
                            xval = 0, surrogatestyle = 0, maxdepth = 30)

set.seed(1)
fit_kick <- rpart(Success ~ Prototypicality + misspelledwords + FOGindex 
                  + log_USD_Goal +  Duration +  Staff_pick
                  + Video + Numberofupdates + Numberofcomments + Category
                  + Descriptionlength , data = data_projects_new, 
                  control = list(rpart.control))


## CV results
printcp(fit_kick)
plotcp(fit_kick)
prp(fit_kick)
rpart.plot(fit_kick)


## 1-standard error rule tree
indMin <- which.min(fit_kick$cptable[, "xerror"])
err1sd <- fit_kick$cptable[indMin, "xerror"] + fit_kick$cptable[indMin, "xstd"]
ind1sd <- which(fit_kick$cptable[, "xerror"] < err1sd)[1]
fit_kick_pruned <- prune(fit_kick, cp = fit_kick$cptable[ind1sd, "CP"])
prp(fit_kick_pruned)
rpart.plot(fit_kick_pruned)

## Predicted values
predictions <- predict(fit_kick_pruned, type = "class")

#confusion matrix
table(predictions, data_projects_new$Success)

#classification accuracy
misClasificError <- mean(data_projects_new$Success != predictions)
1-misClasificError


#cross-validation with this model

pbar <- create_progress_bar('text')
pbar$init(10)

acc <- NULL
mce <- NULL
fit_kick_pruned <- NULL

## 10-fold CV 
library("boot")
data_projects_new$pred_prob <- 0
data_projects_new$pred <- 0
K <- 10

## Permutation of the row indices
set.seed(1)
n <- nrow(data_projects_new)
perm <- sample.int(n)

## Vector identifying the folds, randomly reordered
folds <- rep_len(1:K, length.out = n)[perm]

for(fold in 1:K){
  
  train <- data_projects_new[folds != fold, ]
  test <- data_projects_new[folds == fold, ]
  
  # Fitting
  fit_kick <- rpart(Success ~ Prototypicality + misspelledwords + FOGindex 
                    + log_USD_Goal +  Duration +  Staff_pick
                    + Video + Numberofupdates + Numberofcomments + Category
                    + Descriptionlength , data = train, 
                    control = list(rpart.control))
  indMin <- which.min(fit_kick$cptable[, "xerror"])
  err1sd <- fit_kick$cptable[indMin, "xerror"] + fit_kick$cptable[indMin, "xstd"]
  ind1sd <- which(fit_kick$cptable[, "xerror"] < err1sd)[1]
  fit_kick_pruned[[fold]]<- prune(fit_kick, cp = fit_kick$cptable[ind1sd, "CP"])
  
  # Predict results
  results <- predict(fit_kick_pruned[[fold]], test, type='class')
  data_projects_new[folds == fold,  "pred"] <- results
  
  # Actual answers
  answers <- test$Success
  
  # Accuracy calculation
  misClasificError <- mean(answers != results)
  
  # Collecting results
  acc[fold] <- 1-misClasificError
  mce[fold] <- misClasificError
  
  pbar$step()
}


# Average accuracy of the model
mean(acc)
mean(mce)

#confusion matrix
table(data_projects_new$Success, data_projects_new$pred)


#------------------------------------------------

## THRESHOLD ANALYSIS ##

#-------------------------------------------------------------------------------
## Function for calculating error rates for specified threshold p0
class_error <- function(p0 = 0.5, prob, actual) {
  
  ## Create factor using p0, calculate confusion matrix
  pred <- cut(prob, breaks = c(0, p0, 1), labels = c("No", "Yes"))
  confmat <- table(actual = actual, predicted = pred)
  
  ## Calculate error rates
  error <- 1 - sum(diag(confmat)) / sum(confmat)
  fpr <- confmat[1, 2] / sum(confmat[1, ])
  fnr <- confmat[2, 1] / sum(confmat[2, ])
  
  ## Return a vector
  c(error_rate = error, false_positive_rate = fpr, 
    false_negative_rate = fnr)
}


#----------------------------------------------------------------------

## MODEL 1 = only control

#10-fold cross-validation 
library(plyr)   
library(caret)
pbar <- create_progress_bar('text')
pbar$init(10)

## 10-fold CV 
library("boot")
data_projects_new$pred_prob <- 0
K <- 10

## Permutation of the row indices
set.seed(1)
n <- nrow(data_projects_new)
perm <- sample.int(n)

## Vector identifying the folds, randomly reordered
folds <- rep_len(1:K, length.out = n)[perm]

for(fold in 1:K){
  
  train <- data_projects_new[folds != fold, ]
  test <- data_projects_new[folds == fold, ]
  
  # Fitting
  model <- glm(Success ~ log_USD_Goal + Duration  + Staff_pick
               + Video + Numberofupdates + Numberofcomments + Category, 
               data = train, family = binomial(link = "logit"))
  
  # Predict results
  results_prob <- predict(model, test, type='response')
  data_projects_new[folds == fold,  "pred_prob"] <- results_prob
  
  pbar$step()
}

## Apply the function to a grid of thresholds
p0_seq <- seq(from = 0.001, to = 0.999, length.out = 50)
error_mat <- sapply(p0_seq, class_error, 
                    prob = data_projects_new$pred_prob, 
                    actual = data_projects_new$Success)

## Create data frame for plotting
error_df1 <- as.data.frame(t(error_mat))
error_df1$Threshold <- p0_seq


#--------------------------------------------------------------------------------

## MODEL 2

#10-fold cross-validation 
library(plyr)   
library(caret)
pbar <- create_progress_bar('text')
pbar$init(10)

## 10-fold CV 
library("boot")
data_projects_new$pred_prob <- 0
K <- 10

## Permutation of the row indices
set.seed(1)
n <- nrow(data_projects_new)
perm <- sample.int(n)

## Vector identifying the folds, randomly reordered
folds <- rep_len(1:K, length.out = n)[perm]

for(fold in 1:K){
  
  train <- data_projects_new[folds != fold, ]
  test <- data_projects_new[folds == fold, ]
  
  # Fitting
  model <- glm(Success ~ Prototypicality + misspelledwords + FOGindex 
               + log_USD_Goal + Duration + Staff_pick
               + Video + Numberofupdates + Numberofcomments + Category
               + Descriptionlength, 
               data = train, family = binomial(link = "logit"))
  
  # Predict results
  results_prob <- predict(model, test, type='response')
  data_projects_new[folds == fold,  "pred_prob"] <- results_prob
  
  pbar$step()
}

## Apply the function to a grid of thresholds
p0_seq <- seq(from = 0.001, to = 0.999, length.out = 50)
error_mat <- sapply(p0_seq, class_error, 
                    prob = data_projects_new$pred_prob, 
                    actual = data_projects_new$Success)

## Create data frame for plotting
error_df2 <- as.data.frame(t(error_mat))
error_df2$Threshold <- p0_seq



#------------------------------------------
#model 3 

#10-fold cross-validation 
library(plyr)   
library(caret)
pbar <- create_progress_bar('text')
pbar$init(10)

## 10-fold CV 
library("boot")
data_projects_new$pred_prob <- 0
K <- 10

## Permutation of the row indices
set.seed(1)
n <- nrow(data_projects_new)
perm <- sample.int(n)

## Vector identifying the folds, randomly reordered
folds <- rep_len(1:K, length.out = n)[perm]

for(fold in 1:K){
  
  train <- data_projects_new[folds != fold, ]
  test <- data_projects_new[folds == fold, ]
  
  # Fitting
  model <- glm(Success ~ Prototypicality + log_USD_Goal +  Staff_pick
               + Video + Numberofcomments + Category, 
               data = train, family = binomial(link = "logit"))
  
  # Predict results
  results_prob <- predict(model, test, type='response')
  data_projects_new[folds == fold,  "pred_prob"] <- results_prob
  
  pbar$step()
}

## Apply the function to a grid of thresholds
p0_seq <- seq(from = 0.001, to = 0.999, length.out = 50)
error_mat <- sapply(p0_seq, class_error, 
                    prob = data_projects_new$pred_prob, 
                    actual = data_projects_new$Success)

## Create data frame for plotting
error_df3 <- as.data.frame(t(error_mat))
error_df3$Threshold <- p0_seq

#------------------------------------------

#bind together for plotting
error_dfs <- rbind(error_df1, error_df2, error_df3)
error_dfs$Model <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                     2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                     3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)
error_dfs$Model <- as.factor(error_dfs$Model)

## Plot error rates
library("ggplot2")
ggplot(data = error_dfs, aes(x = Threshold, y = error_rate, colour = Model)) + geom_line() +
  ylim(c(0, 1)) + xlim(c(0, 1)) + theme_minimal() + labs(title = "Plot error rates different thresholds", x = "Classification error")

## Plot a rough ROC curve
ggplot(data = error_dfs, aes(x = false_positive_rate, y = 1 - false_negative_rate, colour = Model)) + geom_line() +
  ylim(c(0, 1)) + xlim(c(0, 1)) + xlab("True Positive Rate") + ylab("False Positive Rate") + labs(title = "ROC curves of models")+ theme_minimal()


