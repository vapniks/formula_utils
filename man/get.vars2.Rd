\name{get.vars2}
\alias{get.vars2}
\title{Return the variables in a formula as a character list.}
\usage{
  get.vars2(form, strip = FALSE)
}
\arguments{
  \item{form}{a formula object.}

  \item{strip}{if TRUE then transformation functions will
  be removed from variable names.}
}
\value{
  Character vector containing names of all variables in
  formula 'form'. The first element in the vector
  corresponds to the right hand side variable of 'form'.
}
\description{
  Return the variables in a formula as a character list.
}
\author{
  Ben Veal
}

