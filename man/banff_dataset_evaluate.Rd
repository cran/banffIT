% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_assessment.R
\name{banff_dataset_evaluate}
\alias{banff_dataset_evaluate}
\title{Evaluates the format and content of the input dataset}
\usage{
banff_dataset_evaluate(banff_dataset, version = NULL)
}
\arguments{
\item{banff_dataset}{A tibble object.}

\item{version}{A character string referring the version of Banff classification.
The most recent classification is the default.
Options are "2022" (default), "2017".}
}
\value{
A list of tibble objects giving information on the assessment of the dataset.
}
\description{
This function takes a dataset and evaluates its format and content based on
the accepted format specified in the data dictionary. It applies a series of
checks to make sure the dataset is ready to be processed by the
\code{\link[=add_diagnoses]{add_diagnoses()}} function which assigns diagnoses to each observation of the
dataset. The function evaluates whether:
\itemize{
\item The input file is a dataset
\item All mandatory variables are present in the dataset
\item Missing values (NA) are present in variables where they are not allowed
\item Data types are correct.
\item The combination of ID, center, and biopsy date is unique
\item There are duplicated variable in the dataset
\item Dates are valid
\item Content values follow the category values as specified in the data dictionary
\item Constraints specified in the data dictionary are respected
}
}
\examples{
{

banff_dataset <- get_banff_template()
banff_dataset_evaluate(banff_dataset)

}

}
