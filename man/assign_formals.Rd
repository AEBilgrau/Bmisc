% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_formals.R
\name{assign_formals}
\alias{assign_formals}
\title{Assign the formals of a function}
\usage{
assign_formals(fun)
}
\arguments{
\item{fun}{A function.}
}
\value{
Assigns the default values of the arguments of a function to
  variables of the argument name.
  Invisibly returns the assigned variable names.
}
\description{
Assign the 'formals' of a function to the global enviroment. Helpful when
debugging or prototyping.
}
\examples{
assigned_vars <- assign_formals(stats::binom.test)
sort(assigned_vars)
ls()
alternative
eval(alternative)
}
