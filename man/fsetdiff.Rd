\name{fsetdiff}
\alias{fsetdiff}
\title{Remove common independent variables from formulas.}
\usage{
  fsetdiff(..., asString = FALSE, diffUnion = FALSE)
}
\arguments{
  \item{...}{Formula objects or strings representing
  formulas or their right hand sides.}

  \item{asString}{If TRUE then return formula as a string,
  otherwise as a formula object.}

  \item{diffUnion}{If TRUE then remove from each formula
  any terms that are in ANY of the other formulas
  (otherwise only terms that are in ALL of the other
  formulas are removed).}
}
\value{
  A formula object or a string representing a formula
  object.
}
\description{
  Remove common independent variables from formulas.
}
\details{
  The first formula/string argument should be either the
  name of the dependent variable or a formula object or
  string representation of a formula containing a dependent
  variable (and a ~). This will be used as the dependent
  variable of the result. By default any independent
  variables that are in ALL the formula/string arguments to
  the function are removed from each of those formulas and
  the results returned as either a list of formula
  arguments (asString=FALSE, default), or a character
  vector of strings representing those formulas
  (asString=TRUE). If diffUnion==TRUE then instead all
  independent variables that are in ANY of the formula
  arguments will be removed.
}
\author{
  Ben Veal
}

