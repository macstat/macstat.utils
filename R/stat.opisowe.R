# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



stat.opisowe <- function(data,x,g="") {

    require(plyr) #biblioteka do

    if(is.factor(x)==TRUE)
    # tabela jakościowa
        if ( missing(g)==FALSE )  {
            temp <- ddply(data, .(g), transform, sum.n = length(g))
            ddply(temp, .(g,x), summarise, n = length(x), proporcja.grup = n / sum.n[1] * 100, .drop=FALSE)
        }
    else{
        t1 <- table(x)
        t.finished <-cbind(t1, round(prop.table(t1)*100,2))
        colnames(t.finished) <- c('n','procent')
        t.finished
    }

    else
        #tabela ilościowa
        ddply(data, .(g), summarise, mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE),  min=min(x, na.rm=TRUE), q1=quantile(x,.25, na.rm=TRUE), median=median(x, na.rm=TRUE), q3=quantile(x,.75, na.rm=TRUE), max=max(x, na.rm=TRUE))

}



