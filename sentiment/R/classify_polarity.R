classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,
                              verbose=FALSE,verbosestep=FALSE,...) {
    fname <- tempfile("sentiment_polarity", fileext=".csv")
    zz <- file(fname, "w")
    matrix <- create_matrix(textColumns,...)
#    lexicon <- read.csv(system.file("data/subjectivity.csv.gz",package="sentiment"),header=FALSE)
    lexicon <- read.csv("subjectivity.csv.gz",header=FALSE)
    
    counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
    documents <- c()

    if(verbosestep==F) verbosestep=100
    stepval <- round(seq(from=1,to=nrow(matrix), length.out=verbosestep),0)
    www<-1;
    
    nrm<-nrow(matrix)
    for (i in 1:nrm) {
        if (verbose && i==stepval[www]) {
            print(paste("DOCUMENT",i))
            www<-www+1;
        }
        scores <- list(positive=0,negative=0)
        doc <- matrix[i,]
        words <- findFreqTerms(doc,lowfreq=1)
        
        for (word in words) {
            index <- pmatch(word,lexicon[,1],nomatch=0)
            if (index > 0) {
                entry <- lexicon[index,]
                
                polarity <- as.character(entry[[2]])
                category <- as.character(entry[[3]])
                count <- counts[[category]]
                
                score <- pweak
                if (polarity == "strongsubj") score <- pstrong
                if (algorithm=="bayes") score <- abs(log(score*prior/count))
                
                if (verbose==3) {
                    print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
                }
                
                scores[[category]] <- scores[[category]]+score
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
        ratio <- as.integer(abs(scores$positive/scores$negative))
        if (ratio==1) best_fit <- "neutral"
#        documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
        q <- paste(i,scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit, sep=",")
        cat(q, file=zz,sep="\n")
        if (verbose>=2) {
            print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
            cat("\n")
        }
    }
    close(zz)
    documents <- read.csv(fname,header=F)
    #unlink(fname)
    colnames(documents) <- c("LINENUMBER","POS","NEG","POS/NEG","BEST_FIT")
    return(documents)
}