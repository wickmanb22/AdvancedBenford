\name{benford_categorization}
\alias{Benford Categorization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Benford Categorization
%%  ~~function to do ... ~~
}
\description{
This function takes a midi file and returns its Benford classification as described in the paper "A Classification System for Assessing a Data Set’s Adherence to Benford’s Law"" by Sybil Prince-Nelson (2021).}
\usage{
Benford Categorization(midi_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{midi file}{
A valid midi file in the form "filename.mid"
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
Brian Wickman
}
\note{
Github is @wickmanb22
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
For a file named "AnotherBrickInTheWall", the function should look like:
benford_categorization("AnotherBrickInTheWall.mid")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
