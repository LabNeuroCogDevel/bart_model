# http://en.wikipedia.org/wiki/Rescorla%E2%80%93Wagner_model
# dataframe with useful info
builddf <- function(xmpl) {
  #xmpl <- subset(bart,subject==10665)
  
  # best reward
  xmpl$maxrew <- xmpl$pumps
  xmpl$maxrew[xmpl$status=='popped'] <- xmpl$maxrew[xmpl$status=='popped']  -1
  #xmpl$maxrew <- c(0,cummax(xmpl$maxrew)[-1])
  xmpl$maxrew <- cummax(xmpl$maxrew)
  xmpl$maxrew <- c(0,xmpl$maxrew[-nrow(xmpl)])
  
  # pumps to a pops happen
  xmpl$lastpop <- xmpl$pumps
  xmpl$lastpop[xmpl$status!='popped'] <- 0
  xmpl$lastpop <- c(0,xmpl$lastpop[-nrow(xmpl)])
  
  # distance from pop
  uniquepop <- as.character(xmpl$status)
  uniquepop[uniquepop=='popped'] <- sequence(length(which(uniquepop=='popped')))
  xmpl$popdist <- sequence(rle(uniquepop)$length)
  
  # distance from cash
  uniquecash <- as.character(xmpl$status)
  uniquecash[uniquecash!='popped'] <- sequence(length(which(uniquecash!='popped')))
  xmpl$cashdist <- sequence(rle(uniquecash)$length)

  xmpl$deltCash <- xmpl$pumps - xmpl$lastrew
  xmpl$deltPop  <- xmpl$pumps - xmpl$lastpop
  return(xmpl) 
} 
