#Have R request no memory limit for windows
memory.size(max = FALSE)
memory.limit(size = NA)




#load in the tables

questions <- read.table("../Questions.csv",
                        sep=',',
                        header=T,
                        na.strings="NA",
                        comment.char="",
                        allowEscapes = F,
                        quote = '"')

answers <- read.table("../Answers.csv",
                    sep=',',
                    header=T,
                    na.strings = "NA",
                    comment.char = "",
                    allowEscapes = F,
                    fill = T,
                    quote = '"')

# tags <- read.table("../Tags.csv",
#                    sep=',',
#                    header = T,
#                    na.strings = "NA",
#                    allowEscapes = F,
                   # nrows = 30)

require(tm)
require(SnowballC)

#Change the questions and body into a corpus
questionsCorp <- Corpus(VectorSource(questions$Body))

#ftp://cran.r-project.org/pub/R/web/packages/tm/vignettes/tm.pdf

#Data cleaning for the questions corpus
#First, remove any whitespace within or around words
questionsCorp <- tm_map(questionsCorp, stripWhitespace)

#transform all content to lowercase, to prevent casing from causing duplicate features
questionsCorp <- tm_map(questionsCorp, content_transformer(tolower))

#remove stopwords such as the, and, a, etc.
questionsCorp <- tm_map(questionsCorp, removeWords, stopwords("english"))

#remove punctuation. This removes not only the periods from being at the end of a sentence,
#but in this case, periods within library names are remeoved (such as java.util.scanner)
questionsCorp <- tm_map(questionsCorp, removePunctuation)

#now, we stem the terms so that terms such as run and running are treated as the same term
questionsCorp <- tm_map(questionsCorp, stemDocument)

#create the bag-of-words form, or document term matrix.
questionsDTM <- DocumentTermMatrix(questionsCorp, control=list(removePunctuation=TRUE))
rm(questionsCorp)
gc()
questionsDTM <- removeSparseTerms(questionsDTM, 0.95)
questionsFrame <- data.frame(data.matrix(questionsDTM))
rm(questionsDTM)
gc()
#Repeating the above steps, now with the titles corpus.
titlesCorp <- Corpus(VectorSource(questions$Title))
titlesCorp <- tm_map(titlesCorp, content_transformer(tolower))
titlesCorp <- tm_map(titlesCorp, removeWords, stopwords("english"))
titlesCorp <- tm_map(titlesCorp, removePunctuation)
titlesCorp <- tm_map(titlesCorp, stemDocument)
titlesDTM <- DocumentTermMatrix(titlesCorp, control=list(removePunctuation=TRUE))
rm(titlesCorp)
gc()
# #removing sparse terms


titlesDTM <- removeSparseTerms(titlesDTM, 0.99)

titlesFrame <- data.frame(data.matrix(titlesDTM))
rm(titlesDTM)
gc()
#pasting the title and body text together
names(titlesFrame) <- paste(names(titlesFrame),"titles")
questionsFrame$OwnerUserIdNB <- questions$OwnerUserId
questionsFrame$IdNB <- questions$Id
questionsFrame$ScoreNB <- questions$Score
#questionsFrame$creation <- as.Date.character(questions$CreationDate, format = "%Y-%m-%dT%H:%M:%SZ")
questionsFrame <- cbind (questionsFrame, titlesFrame)



require(e1071)
#creating the closed/open results and turning it to a factor
questionsFrame$classData[is.na(questions$ClosedDate)] <- "CLOSED"
questionsFrame$classData[!is.na(questions$ClosedDate)] <- "OPEN"
questionsFrame$classData <- as.factor(questionsFrame$classData)

#creating the is/is not answered results
questionsFrame$isAnswered <-questions$Id %in% answers$ParentId
gc()

results <- sapply (1:5, function (x){
  #create 5 way cv split
  divide <- sample(1:nrow(questionsFrame),nrow(questionsFrame)/5, replace= FALSE)
  
  #separate training/test data
  training <- questionsFrame[-divide,]
  test <- questionsFrame[divide,]
  
  #train the learners
  nb <- naiveBayes(classData ~ . , data = training)
  an <- naiveBayes(as.factor(isAnswered) ~ ., data = training)
  
  #run predictions
  pred <- predict(nb,test)
  pred2 <- predict(an, test) 
  
  isClosed <- sum(pred == test$classData) / (nrow(questionsFrame)/5)
  isAnswered <- sum (pred2 == test$isAnswered) / (nrow(questionsFrame)/5)
  
  return (list("isClosed" = isClosed, "isAnswered" = isAnswered ) )}
)

sink("nb.txt")
print ("Percentage Correctly Predicted Using Naive Bayes")
print (results)
sink()
gc()

results <- sapply (1:5, function (x){
  #create 5 way cv split
  divide <- sample(1:nrow(questionsFrame),nrow(questionsFrame)/5, replace= FALSE)
  
  #separate training/test data
  training <- questionsFrame[-divide,]
  test <- questionsFrame[divide,]
  
  #train the learners
  nb <- svm(classData ~ . , data = training)
  an <- svm(as.factor(isAnswered) ~ ., data = training)
  
  #run predictions
  pred <- predict(nb,test)
  pred2 <- predict(an, test) 
  
  isClosed <- sum(pred == test$classData) / (nrow(questionsFrame)/5)
  isAnswered <- sum (pred2 == test$isAnswered) / (nrow(questionsFrame)/5)
  
  return (list("isClosed" = isClosed, "isAnswered" = isAnswered ) )}
)

sink("svm.txt")
print ("Percentage Correctly Predicted Using SVM")
print (results)
sink()
gc()


