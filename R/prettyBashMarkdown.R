formatBash <- function(code,width.cutoff = getOption("width"),...){
    if (is.null(code)) {
        if (source == "clipboard" && Sys.info()["sysname"] == "Darwin") {
            source = pipe("pbpaste")
        }
    }
    else {
        source = textConnection(code)
        on.exit(close(source), add = TRUE) ## afareinafa
    }
    text = readLines(source, warn = FALSE)
    print(text)
    split.chr <- c("\\s","\\/","\\.",",","\\|\\|","\\|")
    out <- vector()
    for(i in 1:length(text)){
        k  <- 1
        code  <- text[i]
        len.idx =1
        while(length(len.idx)!=0L ){
            code <- unlist(lapply(code,function(x){if(nchar(x)>width.cutoff){strsplit(x,paste0("(?<=",split.chr[k],")"),perl=T)}else{x}}))
            tmp.len = unlist(lapply(code,nchar))
            len.idx = which(tmp.len > width.cutoff)
            k = k+1
        }
        grp.idx <- findInterval(1:length(tmp.len),grp.point(tmp.len,width.cutoff),left.open=T)
        grp.tb  <- data.table(code,grp.idx)
        out <- c(out,grp.tb[,paste0(.SD$code,collapse=""),by=grp.idx]$V1)
     }
    out
}

grp.point <- function(x,max_len){
    print(x)
    print(max_len)
    sum_len <- 0
    cum_sum <- vector()
    idx <- vector()
    for(i in 1:length(x)){
        sum_len = sum_len + x[i]
        if(sum_len > max_len){
            sum_len <- x[i]
            idx <- c(idx,i-1)
        }
        cum_sum <- c(cum_sum,sum_len)
    }
    
    if(length(idx)!=0 && idx[length(idx)]!=length(x)){
        idx <- c(idx,length(x))
    }else{
        idx <- 1
    }
    idx
}

