#' This is used to format bash chunk code in rmarkdown.
#'
#' @param
#' code: input code
#' @return 
#' Output a vector of codes. Each element is a line in the md file.
#' #' @export
#' formatBash
#' @examples
#' ```{bash,tidy=formatBash } echo $PATH```
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
    
    ## split chr using the follow delimiter sequentially until the max len for a word is shorter than width.cutoff 
    split.chr <- c("\\s","\\/","\\.",",","\\|\\|","\\|")

    ## split the text by each line and by each delimiter.
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

        ## Group codes parts in the manner that the total length of a group is shortter than threshold
        grp.idx <- findInterval(1:length(tmp.len),grp.point(tmp.len,width.cutoff),left.open=T)

        ## Paste the code parts in each same group
        for(j in unique(grp.idx)){
            idx <- which(grp.idx==j)
            out <- c(out,paste0(code[idx],collapse=""))
        }
    }
    out
}


## code parts is a vection including all the small pieces of codes. grp.point function is used to find the dividing points of the code parts to concatenate.
grp.point <- function(x,max_len){
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

