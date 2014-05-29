library(ggplot2)
library(plyr)
## Models and functions
source('model.R')
#
#
# read in data
bartraw <- read.csv('Bart.csv',header=T)
# get only subject,trial,status(pop,timeout,inflating),num pump
bart <- bartraw[,c('Subject','Trial','Status.SubTrial.', 'Numpumps.SubTrial.')]
names(bart) <- c('subject','trial','status','pumps')
# timeout is the same as inflating
bart$status[bart$status=='timeout'] <- 'inflating'

bartdf <- ddply(bart,.(subject),builddf) # use builddf from model.R
# inflating is cashing out
#bart$Status.SubTrial.[bart$Status.SubTrial.=='inflating'] <- 'cashout'



# see all subjects
p<- ggplot(bartdf,aes(x=trial,y=pumps))+
   geom_point(aes(color=status)) +
   geom_line(aes(y=deltCash,color=I('deltCash'))) +
   geom_line(aes(y=deltPop,color=I('deltPop'))) +
   facet_wrap(~subject) +
   theme_bw()
# we are only going to look at a few example subjects
examples <- subset(bart,subject==10665|subject==10653|subject==10772)

xmpl <- builddf(subset(bart,subject==10665) )

print(p)

