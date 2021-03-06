% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collect_corona_jp.R
\name{collect_corona_go_jp}
\alias{collect_corona_go_jp}
\title{Collect mobility data}
\usage{
collect_corona_go_jp(source)
}
\arguments{
\item{source}{Data source. Choose between "agoop", "docomo",
"kddi" or "yahoo".}
}
\description{
\Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
}
\details{
Data on the rate of decline in human mobility from
two data sources ("Agoop", "Docomo", "KDDI" and "Yahoo") will be obtained from the website.
The data is updated on a daily basis and shows the values
for each of several areas, compared to the state before the declaration.
}
\seealso{
\url{https://corona.go.jp/dashboard/}
}
