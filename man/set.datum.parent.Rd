% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set.datum.parent.R
\name{set.datum.parent}
\alias{set.datum.parent}
\title{Set Datum Parent}
\usage{
set.datum.parent(x)
}
\arguments{
\item{x}{A syntactically correct XML file with datumEntity set.}
}
\value{
A syntactically correct XML file with datumParent set.
}
\description{
Each element of an XML file has a single parent which is identified by datumEntity. Setting
the datumParent attribute preserves the XML structure through transformations.
}
