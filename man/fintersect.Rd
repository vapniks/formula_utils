% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formula_utils.R
\name{fintersect}
\alias{fintersect}
\title{Create formula from intersection of independent variables of other formulae.}
\usage{
fintersect(..., asString = FALSE)
}
\arguments{
\item{...}{Formula objects or strings representing formulas or their right hand sides.}

\item{asString}{If TRUE then return formula as a string, otherwise as a formula object.}
}
\value{
A formula object or a string representing a formula object.
}
\details{
The first formula/string argument should be either the name of the dependent variable or a formula object or
string representation of a formula containing a dependent variable (and a ~).
This will be used as the dependent variable of the result. The independent variables of the returned result will
consist the intersection of all independent variables in the function arguments.
}
\author{
Ben Veal
}
