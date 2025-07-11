% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_utils.R
\name{get_banff_dictionary}
\alias{get_banff_dictionary}
\title{Get the Banff data dictionary}
\usage{
get_banff_dictionary(
  version = NULL,
  which = NULL,
  language = "label:en",
  detail = FALSE
)
}
\arguments{
\item{version}{A character string referring the version of Banff classification.
The most recent classification is the default.
Options are "2022" (default), "2017".}

\item{which}{Indicates which variables to get from the Banff data dictionary.
If NULL both input and output variables are provided.*}

\item{language}{Optional input allowing the user to get the diagnoses in a
specific language. Options are "label:en" (default), "label:fr", "label:de",
"label:sp", "label:nl", "label:jp", "label:in".}

\item{detail}{Optional argument indicating whether the data dictionary should
include temporary variables generated in the process or not.
FALSE by default.}
}
\value{
A list of tibbles representing meta data used in the process. The metadata
are the list of variables used, and their associated categories, if any.
}
\description{
This function gets the data dictionary used to control the consistency of
the input dataset.
}
\examples{
{

  get_banff_dictionary()

}

}
