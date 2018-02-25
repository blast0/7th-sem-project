library(tm)
library(RTextTools)
library(dplyr)
library(caret)

nnpn<- read.csv(file.choose(), stringsAsFactors = FALSE)
glimpse(nnpn)
nnpn<-nnpn[1:2]
nnpn <- nnpn[sample(nrow(nnpn)), ]
nnpn <- nnpn[sample(nrow(nnpn)), ]
glimpse(nnpn)


nnpn$class <- as.factor(nnpn$class)


corpus <- Corpus(VectorSource(nnpn$text))
corpus

inspect(corpus[1:3])

# Use dplyr's  %>% (pipe) utility to do this neatly.
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)


dtm <- DocumentTermMatrix(corpus.clean)
# Inspect the dtm
inspect(dtm[40:50, 10:15])
dim(dtm)

nnpn.train1 <- nnpn[1:1000,]
nnpn.test1 <- nnpn[1001:10662,]
nnpn.train2 <- nnpn[1:2000,]
nnpn.test2 <- nnpn[2001:10662,]
nnpn.train3 <- nnpn[1:3000,]
nnpn.test3 <- nnpn[3001:10662,]
nnpn.train4 <- nnpn[1:4000,]
nnpn.test4 <- nnpn[4001:10662,]
nnpn.train5 <- nnpn[1:5000,]
nnpn.test5 <- nnpn[5001:10662,]
nnpn.train6 <- nnpn[1:6000,]
nnpn.test6 <- nnpn[6001:10662,]
nnpn.train7 <- nnpn[1:7000,]
nnpn.test7 <- nnpn[7001:10662,]
nnpn.train8 <- nnpn[1:8000,]
nnpn.test8 <- nnpn[8001:10662,]
nnpn.train9 <- nnpn[1:9000,]
nnpn.test9 <- nnpn[9001:10662,]
nnpn.train10 <- nnpn[1:10000,]
nnpn.test10 <- nnpn[1:10662,]

dtm.train1 <- dtm[1:1000,]
dtm.test1 <- dtm[1001:10662,]
dtm.train2 <- dtm[1:2000,]
dtm.test2 <- dtm[2001:10662,]
dtm.train3 <- dtm[1:3000,]
dtm.test3 <- dtm[3001:10662,]
dtm.train4 <- dtm[1:4000,]
dtm.test4 <- dtm[4001:10662,]
dtm.train5 <- dtm[1:5000,]
dtm.test5 <- dtm[5001:10662,]
dtm.train6 <- dtm[1:6000,]
dtm.test6 <- dtm[6001:10662,]
dtm.train7 <- dtm[1:7000,]
dtm.test7 <- dtm[7001:10662,]
dtm.train8 <- dtm[1:8000,]
dtm.test8 <- dtm[8001:10662,]
dtm.train9 <- dtm[1:9000,]
dtm.test9 <- dtm[9001:10662,]
dtm.train10 <- dtm[1:10000,]
dtm.test10 <- dtm[1:10662,]

corpus.clean.train1 <- corpus.clean[1:1000]
corpus.clean.test1 <- corpus.clean[1001:10662]
corpus.clean.train2 <- corpus.clean[1:2000]
corpus.clean.test2 <- corpus.clean[2001:10662]
corpus.clean.train3 <- corpus.clean[1:3000]
corpus.clean.test3 <- corpus.clean[3001:10662]
corpus.clean.train4 <- corpus.clean[1:4000]
corpus.clean.test4 <- corpus.clean[4001:10662]
corpus.clean.train5 <- corpus.clean[1:5000]
corpus.clean.test5 <- corpus.clean[5001:10662]
corpus.clean.train6 <- corpus.clean[1:6000]
corpus.clean.test6 <- corpus.clean[6001:10662]
corpus.clean.train7 <- corpus.clean[1:7000]
corpus.clean.test7 <- corpus.clean[7001:10662]
corpus.clean.train8 <- corpus.clean[1:8000]
corpus.clean.test8 <- corpus.clean[8001:10662]
corpus.clean.train9 <- corpus.clean[1:9000]
corpus.clean.test9 <- corpus.clean[9001:10662]
corpus.clean.train10 <- corpus.clean[1:10000]
corpus.clean.test10 <- corpus.clean[1:10662]


dim(dtm.train1)


fivefreq1 <- findFreqTerms(dtm.train1, 5)
fivefreq2 <- findFreqTerms(dtm.train2, 5)
fivefreq3 <- findFreqTerms(dtm.train3, 5)
fivefreq4 <- findFreqTerms(dtm.train4, 5)
fivefreq5 <- findFreqTerms(dtm.train5, 5)
fivefreq6 <- findFreqTerms(dtm.train6, 5)
fivefreq7 <- findFreqTerms(dtm.train7, 5)
fivefreq8 <- findFreqTerms(dtm.train8, 5)
fivefreq9 <- findFreqTerms(dtm.train9, 5)
fivefreq10 <- findFreqTerms(dtm.train10, 5)

length((fivefreq1))


dtm.train.nb1 <- DocumentTermMatrix(corpus.clean.train1, control=list(dictionary = fivefreq1))
dtm.train.nb2 <- DocumentTermMatrix(corpus.clean.train2, control=list(dictionary = fivefreq2))
dtm.train.nb3 <- DocumentTermMatrix(corpus.clean.train3, control=list(dictionary = fivefreq3))
dtm.train.nb4 <- DocumentTermMatrix(corpus.clean.train4, control=list(dictionary = fivefreq4))
dtm.train.nb5 <- DocumentTermMatrix(corpus.clean.train5, control=list(dictionary = fivefreq5))
dtm.train.nb6 <- DocumentTermMatrix(corpus.clean.train6, control=list(dictionary = fivefreq6))
dtm.train.nb7 <- DocumentTermMatrix(corpus.clean.train7, control=list(dictionary = fivefreq7))
dtm.train.nb8 <- DocumentTermMatrix(corpus.clean.train8, control=list(dictionary = fivefreq8))
dtm.train.nb9 <- DocumentTermMatrix(corpus.clean.train9, control=list(dictionary = fivefreq9))
dtm.train.nb10 <- DocumentTermMatrix(corpus.clean.train10, control=list(dictionary = fivefreq10))


dim(dtm.train.nb1)


dtm.test.nb1 <- DocumentTermMatrix(corpus.clean.test1, control=list(dictionary = fivefreq1))
dtm.test.nb2 <- DocumentTermMatrix(corpus.clean.test2, control=list(dictionary = fivefreq2))
dtm.test.nb3 <- DocumentTermMatrix(corpus.clean.test3, control=list(dictionary = fivefreq3))
dtm.test.nb4 <- DocumentTermMatrix(corpus.clean.test4, control=list(dictionary = fivefreq4))
dtm.test.nb5 <- DocumentTermMatrix(corpus.clean.test5, control=list(dictionary = fivefreq5))
dtm.test.nb6 <- DocumentTermMatrix(corpus.clean.test6, control=list(dictionary = fivefreq6))
dtm.test.nb7 <- DocumentTermMatrix(corpus.clean.test7, control=list(dictionary = fivefreq7))
dtm.test.nb8 <- DocumentTermMatrix(corpus.clean.test8, control=list(dictionary = fivefreq8))
dtm.test.nb9 <- DocumentTermMatrix(corpus.clean.test9, control=list(dictionary = fivefreq9))
dtm.test.nb10 <- DocumentTermMatrix(corpus.clean.test10, control=list(dictionary = fivefreq))


dim(dtm.test.nb1)
    
    
# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}


trainNB1 <- apply(dtm.train.nb1, 2, convert_count)
testNB1 <- apply(dtm.test.nb1, 2, convert_count)
trainNB2 <- apply(dtm.train.nb2, 2, convert_count)
testNB2 <- apply(dtm.test.nb2, 2, convert_count)
trainNB3 <- apply(dtm.train.nb3, 2, convert_count)
testNB3 <- apply(dtm.test.nb3, 2, convert_count)
trainNB4 <- apply(dtm.train.nb4, 2, convert_count)
testNB4 <- apply(dtm.test.nb4, 2, convert_count)
trainNB5 <- apply(dtm.train.nb5, 2, convert_count)
testNB5 <- apply(dtm.test.nb5, 2, convert_count)
trainNB6 <- apply(dtm.train.nb6, 2, convert_count)
testNB6 <- apply(dtm.test.nb6, 2, convert_count)
trainNB7 <- apply(dtm.train.nb7, 2, convert_count)
testNB7 <- apply(dtm.test.nb7, 2, convert_count)
trainNB8 <- apply(dtm.train.nb8, 2, convert_count)
testNB8 <- apply(dtm.test.nb8, 2, convert_count)
trainNB9 <- apply(dtm.train.nb9, 2, convert_count)
testNB9 <- apply(dtm.test.nb9, 2, convert_count)
trainNB10 <- apply(dtm.train.nb10, 2, convert_count)
testNB10 <- apply(dtm.test.nb10, 2, convert_count)



naive <- function(x, ...)
  UseMethod("naiveBayes")

naiveBayes.default <- function(x, y, laplace = 0, ...) {
  call <- match.call()
  Yname <- deparse(substitute(y))
  x <- as.data.frame(x)
  
  ## estimation-function
  est <- function(var)
    if (is.numeric(var)) {
      cbind(tapply(var, y, mean, na.rm = TRUE),
            tapply(var, y, sd, na.rm = TRUE))
    } else {
      tab <- table(y, var)
      (tab + laplace) / (rowSums(tab) + laplace * nlevels(var))
    }
  
  ## create tables
  apriori <- table(y)
  tables <- lapply(x, est)
  
  ## fix dimname names
  for (i in 1:length(tables))
    names(dimnames(tables[[i]])) <- c(Yname, colnames(x)[i])
  names(dimnames(apriori)) <- Yname
  
  structure(list(apriori = apriori,
                 tables = tables,
                 levels = if (is.logical(y)) c(FALSE, TRUE) else levels(y),
                 call   = call
  ),
  
  class = "naiveBayes"
  )
}

naiveBayes.formula <- function(formula, data, laplace = 0, ...,
                               subset, na.action = na.pass) {
  call <- match.call()
  Yname <- as.character(formula[[2]])
  
  if (is.data.frame(data)) {
    ## handle formula
    m <- match.call(expand.dots = FALSE)
    m$... <- NULL
    m$laplace = NULL
    m$na.action <- na.action
    m[[1L]] <- quote(stats::model.frame)
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    if (any(attr(Terms, "order") > 1))
      stop("naiveBayes cannot handle interaction terms")
    Y <- model.extract(m, "response")
    X <- m[,-attr(Terms, "response"), drop = FALSE]
    
    return(naiveBayes(X, Y, laplace = laplace, ...))
  } else if (is.array(data)) {
    nam <- names(dimnames(data))
    ## Find Class dimension
    Yind <- which(nam == Yname)
    
    ## Create Variable index
    deps <- strsplit(as.character(formula)[3], ".[+].")[[1]]
    if (length(deps) == 1 && deps == ".")
      deps <- nam[-Yind]
    Vind <- which(nam %in% deps)
    
    ## create tables
    apriori <- margin.table(data, Yind)
    tables <- lapply(Vind,
                     function(i) (margin.table(data, c(Yind, i)) + laplace) /
                       (as.numeric(apriori) + laplace * dim(data)[i]))
    names(tables) <- nam[Vind]
    
    structure(list(apriori = apriori,
                   tables = tables,
                   levels = names(apriori),
                   call   = call
    ),
    
    class = "naiveBayes"
    )
  } else stop("naiveBayes formula interface handles data frames or arrays only")
  
}


print.naiveBayes <- function(x, ...) {
  cat("\nNaive Bayes Classifier for Discrete Predictors\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\nA-priori probabilities:\n")
  print(x$apriori / sum(x$apriori))
  
  cat("\nConditional probabilities:\n")
  for (i in x$tables) {print(i); cat("\n")}
  
}

predict.naiveBayes <- function(object,
                               newdata,
                               type = c("class", "raw"),
                               threshold = 0.001,
                               eps = 0,
                               ...) {
  type <- match.arg(type)
  newdata <- as.data.frame(newdata)
  attribs <- match(names(object$tables), names(newdata))
  isnumeric <- sapply(newdata, is.numeric)
  islogical <- sapply(newdata, is.logical)
  newdata <- data.matrix(newdata)
  L <- sapply(1:nrow(newdata), function(i) {
    ndata <- newdata[i, ]
    L <- log(object$apriori) + apply(log(sapply(seq_along(attribs),
                                                function(v) {
                                                  nd <- ndata[attribs[v]]
                                                  if (is.na(nd)) rep(1, length(object$apriori)) else {
                                                    prob <- if (isnumeric[attribs[v]]) {
                                                      msd <- object$tables[[v]]
                                                      msd[, 2][msd[, 2] <= eps] <- threshold
                                                      dnorm(nd, msd[, 1], msd[, 2])
                                                    } else object$tables[[v]][, nd + islogical[attribs[v]]]
                                                    prob[prob <= eps] <- threshold
                                                    prob
                                                  }
                                                })), 1, sum)
    if (type == "class")
      L
    else {
      ## Numerically unstable:
      ##            L <- exp(L)
      ##            L / sum(L)
      ## instead, we use:
      sapply(L, function(lp) {
        1/sum(exp(L - lp))
      })
    }
  })
  if (type == "class") {
    if (is.logical(object$levels))
      L[2,] > L[1,]
    else
      factor(object$levels[apply(L, 2, which.max)], levels = object$levels)
  } else t(L)
}



classifier1 <- naive(trainNB1, nnpn.train1$class, laplace = 1) 
classifier2 <- naive(trainNB2, nnpn.train2$class, laplace = 1) 
classifier3 <- naive(trainNB3, nnpn.train3$class, laplace = 1) 
classifier4 <- naive(trainNB4, nnpn.train4$class, laplace = 1) 
classifier5 <- naive(trainNB5, nnpn.train5$class, laplace = 1) 
classifier6 <- naive(trainNB6, nnpn.train6$class, laplace = 1) 
classifier7 <- naive(trainNB7, nnpn.train7$class, laplace = 1) 
classifier8 <- naive(trainNB8, nnpn.train8$class, laplace = 1) 
classifier9 <- naive(trainNB9, nnpn.train9$class, laplace = 1) 
classifier10 <- naive(trainNB10, nnpn.train10$class, laplace = 1)

pred1 <- predict(classifier1, newdata=testNB1)
pred2 <- predict(classifier2, newdata=testNB2)
pred3 <- predict(classifier3, newdata=testNB3)
pred4 <- predict(classifier4, newdata=testNB4)
pred5 <- predict(classifier5, newdata=testNB5)
pred6 <- predict(classifier6, newdata=testNB6)
pred7 <- predict(classifier7, newdata=testNB7)
pred8 <- predict(classifier8, newdata=testNB8)
pred9 <- predict(classifier9, newdata=testNB9)
pred10 <- predict(classifier10, newdata=testNB10)


tp1 <- table("Predictions"= pred1,  "Actual" = nnpn.test1$class )
tp2 <- table("Predictions"= pred2,  "Actual" = nnpn.test2$class )
tp3 <- table("Predictions"= pred3,  "Actual" = nnpn.test3$class )
tp4 <- table("Predictions"= pred4,  "Actual" = nnpn.test4$class )
tp5 <- table("Predictions"= pred5,  "Actual" = nnpn.test5$class )
tp6 <- table("Predictions"= pred6,  "Actual" = nnpn.test6$class )
tp7 <- table("Predictions"= pred7,  "Actual" = nnpn.test7$class )
tp8 <- table("Predictions"= pred8,  "Actual" = nnpn.test8$class )
tp9 <- table("Predictions"= pred9,  "Actual" = nnpn.test9$class )
tp10 <- table("Predictions"= pred10,  "Actual" = nnpn.test10$class )

round(100*prop.table(tp1,1),2)

conf.mat1 <- confusionMatrix(pred1, nnpn.test1$class)
conf.mat2 <- confusionMatrix(pred2, nnpn.test2$class)
conf.mat3 <- confusionMatrix(pred3, nnpn.test3$class)
conf.mat4 <- confusionMatrix(pred4, nnpn.test4$class)
conf.mat5 <- confusionMatrix(pred5, nnpn.test5$class)
conf.mat6 <- confusionMatrix(pred6, nnpn.test6$class)
conf.mat7 <- confusionMatrix(pred7, nnpn.test7$class)
conf.mat8 <- confusionMatrix(pred8, nnpn.test8$class)
conf.mat9 <- confusionMatrix(pred9, nnpn.test9$class)
conf.mat10 <- confusionMatrix(pred10, nnpn.test10$class)



conf.mat1

conf.mat1$byClass

conf.mat1$overall

conf.mat1$overall['Accuracy']



a<-conf.mat1$overall['Accuracy']

y<-array(c(conf.mat1$overall['Accuracy'],conf.mat2$overall['Accuracy'],
           conf.mat3$overall['Accuracy'],conf.mat4$overall['Accuracy'],
           conf.mat5$overall['Accuracy'],conf.mat6$overall['Accuracy'],
           conf.mat7$overall['Accuracy'],conf.mat8$overall['Accuracy'],
           conf.mat9$overall['Accuracy'],conf.mat10$overall['Accuracy']))

x<-array(c(10,20,30,40,50,60,70,80,90,100))
plot(x,y,xlab = "Percentage of training dataset",ylab = "Accuracy", main="Accuracy of prediction")
plot(x,y,xlab = "Percentage of training dataset",ylab = "Accuracy", main="Accuracy of prediction", col="red", type ="l" )