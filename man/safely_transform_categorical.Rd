% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safely_transform_categorical.R
\name{safely_transform_categorical}
\alias{safely_transform_categorical}
\title{Calculating a Transformation of Categorical Feature Using Hierarchical Clustering}
\usage{
safely_transform_categorical(
  explainer,
  variable,
  method = "complete",
  B = 500,
  collapse = "_"
)
}
\arguments{
\item{explainer}{DALEX explainer created with explain() function}

\item{variable}{a feature for which the transformation function is to be computed}

\item{method}{the agglomeration method to be used in hierarchical clustering, one of:
"ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"}

\item{B}{number of reference datasets used to calculate gap statistics}

\item{collapse}{a character string to separate original levels while combining them to the new one}
}
\value{
list of information on the transformation of given variable
}
\description{
The safely_transform_categorical() function calculates a transformation function
for the categorical variable using predictions obtained from black box model and hierarchical clustering.
The gap statistic criterion is used to determine the optimal number of clusters.
}
\examples{

library(DALEX)
library(randomForest)
library(rSAFE)

data <- apartments[1:500,]
set.seed(111)
model_rf <- randomForest(m2.price ~ construction.year + surface + floor +
                           no.rooms + district, data = data)
explainer_rf <- explain(model_rf, data = data[,2:6], y = data[,1])
safely_transform_categorical(explainer_rf, "district")

}
\seealso{
\code{\link{safe_extraction}}
}
