\name{CS.GXcurve}
\alias{CS.GXcurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CS.GXcurve(table, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
%%     ~~Describe \code{table} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (table, ...) 
{
    datalist = list(table, ...)
    out = rbind(table, ...)
    GXrecordinglength = table(table(out$name))
    toobig = GXrecordinglength[which(as.numeric(names((GXrecordinglength))) > 
        58)]
    toobig_names = names(which(table(out$name) > 58))
    toosmall = GXrecordinglength[which(as.numeric(names((GXrecordinglength))) < 
        57)]
    toosmall_names = names(which(table(out$name) < 57))
    countlist = list()
    fullname = vector()
    for (i in 1:length(datalist)) {
        tem = 1
        fullname = c(fullname, datalist[[i]]$name[1])
        for (j in 2:length(datalist[[i]]$name)) {
            if (datalist[[i]]$name[j] != datalist[[i]]$name[j - 
                1]) {
                tem = tem + 1
                fullname = c(fullname, datalist[[i]]$name[j])
            }
        }
        countlist[[i]] = tem
    }
    total = 0
    for (k in 1:length(countlist)) {
        cat("input", k, "/", unique(datalist[[k]]$tag), ":", 
            countlist[[k]], "entries\n")
        total = total + countlist[[k]]
    }
    cat("overall combining", total, "entries\n\n")
    duplicatelist = names(which(table(fullname) > 1))
    missrecording = toobig_names[!(toobig_names \%in\% duplicatelist)]
    if (length(duplicatelist) != 0) {
        print(paste(">>> Walch out! You have", length(duplicatelist), 
            "case(s) in which you have duplicate entry names.", 
            "Better check out raw data:"))
        print(duplicatelist)
    }
    if (length(toosmall_names) != 0) {
        print(paste(">>> Walch out! You have", length(toosmall_names), 
            "case(s) in which you have too few(", names(toosmall), 
            ") gas exchange recordings.", "Better check out raw data:"))
        print(toosmall_names)
    }
    if (length(missrecording) != 0) {
        print(paste(">>> Walch out! You have", length(missrecording), 
            "case(s) in which you have missing name recording between two seperate measurements", 
            "Better check out raw data:"))
        print(missrecording)
    }
    attr(out, "measurement_time") <- NULL
    attr(out, "machine") <- NULL
    attr(out, "lightsource") <- NULL
    attr(out, "AD.avgtime") <- NULL
    attr(out, "flow") <- NULL
    attr(out, "par") <- NULL
    attr(out, "CO2_mixer") <- NULL
    attr(out, "Tblock") <- NULL
    attr(out, "duplicatelist") <- duplicatelist
    attr(out, "toosmall_names") <- toosmall_names
    attr(out, "missrecording") <- missrecording
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
