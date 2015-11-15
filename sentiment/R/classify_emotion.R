classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,
                             verbose=FALSE,verbosestep=FALSE,...) {
    fname <- tempfile("sentiment_emotion", fileext=".csv")
    if (verbose) print(paste("TEMP FILE : ",fname))
    zz <- file(fname, "w")
    
    matrix <- create_matrix(textColumns,...)
#    lexicon <- read.csv(system.file("data/emotions.csv.gz",package="sentiment"),header=FALSE)
    lexicon <- read.csv("emotions.csv.gz",header=FALSE)
    
    counts <- list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
    #documents <- c()
    if(verbosestep==F) verbosestep=100
    stepval <- round(seq(from=1,to=nrow(matrix), length.out=verbosestep),0)
    www<-1;
    nrm <- nrow(matrix)
    for (i in 1:nrm) {
        if (verbose & i==stepval[www]) {
            print(paste("DOCUMENT",i, "of", nrm))
            www<-www+1
        }
        scores <- list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
        doc <- matrix[i,]
        words <- findFreqTerms(doc,lowfreq=1)
        
        for (word in words) {
            for (key in names(scores)) {
                emotions <- lexicon[which(lexicon[,2]==key),]
                index <- pmatch(word,emotions[,1],nomatch=0)
                if (index > 0) {
                    entry <- emotions[index,]
                    
                    category <- as.character(entry[[2]])
                    count <- counts[[category]]
                    
                    score <- 1.0
                    if (algorithm=="bayes") score <- abs(log(score*prior/count))
                    
                    if (verbose==2) {
                        print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
                    }
                    
                    scores[[category]] <- scores[[category]]+score
                }
            }
        }
        
        if (algorithm=="bayes") {
            for (key in names(scores)) {
                count <- counts[[key]]
                total <- counts[["total"]]
                score <- abs(log(count/total))
                scores[[key]] <- scores[[key]]+score
            }
        } else {
            for (key in names(scores)) {
                scores[[key]] <- scores[[key]]+0.000001
            }
        }
        
        best_fit <- names(scores)[which.max(unlist(scores))]
        if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
        #documents <- rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
        q <- paste(i,scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit , sep=",")
        cat(q, file=zz,sep="\n")
    }
    close(zz)
    documents <- read.csv(fname, header=F)
    unlink(fname)
    colnames(documents) <- c("LINENUMBER","ANGER","DISGUST","FEAR","JOY","SADNESS","SURPRISE","BEST_FIT")
    return(documents)
}