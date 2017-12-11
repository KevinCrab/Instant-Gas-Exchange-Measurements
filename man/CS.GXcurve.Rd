\name{CS.GXcurve}
\alias{CS.GXcurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combine and Screen
%%  ~~function to do ... ~~
}
\description{
This function merges GXcurve class data from seperate machines into one file. It screens the recording numbers in each measurement and reports the following three human errors: 1. Duplicate names. Ideally the name recording for leaves are unique. But it is possible that during operation people accidentally put in a wrong name, resulting to a name duplication for another leaf. 2. Miss name recording. As mentioned before names are recorded by inserting "remarks" in between two measurements. Sometimes people forgot to add remark. 3. Recording Interruption. When recording, errors might occur or the position of leaf needs to be adjusted, thus leading to too few recording in that measurement. 

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CS.GXcurve(table, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
  GXcurve class table. If inputting single file, then this function does only screening.
%%     ~~Describe \code{table} here~~
}
  \item{\dots}{
  If inputing more than one GXcurve class table, these tables will first be merged and then screened.
%%     ~~Describe \code{\dots} here~~
}
}

\value{
Returns a GXcurve class table. Three types of human errors are summarized and saved in table attributes. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Jiayang Xie <kevinxie@illinois.edu>
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
Allcurve = CS.GXcurve(GXcurve_table1, GXcurve_table2, GXcurve_table3, GXcurve_table4)
}
