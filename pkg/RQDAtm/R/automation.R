codingTermDF <- function(code,var.name,n=30){
    cp <- RQDA2tm(code ,byFile = FALSE)
    cp <- tm_map(cp, stripWhitespace)
    txt <- prescindMeta(cp,c("ID"))
    re <- vector()
    for (i in 1:nrow(txt)) {
	re[i]<- CWSimdict(PlainTextDocument(cp)[[i]], TRUE)
    }
    cp <- Corpus(VectorSource(re))
    dtm <- DocumentTermMatrix(cp,control = list(minWordLength=2,removeNumbers=TRUE))
    top_index <- findFreqTerms(dtm,sort(dtm$v ,decreasing = TRUE)[n])
    mat <- as.data.frame(as.matrix(dtm[,top_index]))
    mat <- cbind(code=var.name,mat)
}

vectorTermDF <- function(text,var){
    text <- Corpus(VectorSource(text))
    text<- tm_map(text, stripWhitespace)
    re <- vector()
    for (i in 1:length(text)) {
        re[i]<- CWSimdict(PlainTextDocument(text)[[i]], TRUE)
    }
    text <- Corpus(VectorSource(re))
    dtm <- DocumentTermMatrix(text,control = list(minWordLength=2,removeNumbers=TRUE))
    preData <- as.data.frame(as.matrix(dtm))
    ndata <- as.data.frame(matrix(data = 0, nrow = nrow(preData), ncol = length(var)))
    names(ndata) <- var
    idx <- match(names(preData),names(ndata))
    ndata[,idx[!is.na(idx)]] <- preData[,which(!is.na(idx))]
    ndata
}

## po <- codingTermDF('political opportunity','po')
## smi <- codingTermDF('state-movement intersection','smi')
## nw <- codingTermDF('network','nw')
## comb <- gtools:::smartbind(po,smi,nw)
## comb[is.na(comb)] <- 0
## comb$code <- as.factor(comb$code=="po")
## library(e1071)
## model <- svm(code ~ ., data = comb, kernel = "sigmoid")
## test <- "���ǲ�����ҵί���Ǯ���������ǻ�������ǽ��ж����޷������ƣ���ӯ��������ȡһЩ�����ѣ��������ǽ������⡢ˮ�硢��ӡ��ֽ�ŷ��ã����Ǻܶ࿪֧��Ҫ�ǿ��Һ�Ҷ��ʦ���Լ�һЩҵί�����γе��ġ�"
## dat<- vectorTermDF(test,names(comb)[-1])
## predict(model,dat)