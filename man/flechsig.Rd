% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{flechsig}
\alias{flechsig}
\alias{flechsig_3d}
\title{flechsig atlas}
\format{
An object of class \code{brain_atlas} of length 4.

An object of class \code{ggseg3d_atlas} (inherits from \code{tbl_df}, \code{tbl}, \code{data.frame}) with 4 rows and 4 columns.
}
\usage{
flechsig

flechsig_3d
}
\description{
A historical Flechsig 1920 atlas based on files
released with  Pijnenburg et al., NeuroImage,
239, 2021, (\href{https://doi.org/10.1016/j.neuroimage.2021.118274}{DOI})
Version 1; 15-01-2021.
}
\examples{
data(flechsig)
data(flechsig_3d)
}
\references{
P.E. Flechsig, Anatomie Des Menschlichen Gehirns und Rückenmarks auf Myelogenetischer Grundlage, G. Thieme (1920)
\itemize{
 \item{flechsig}{ - flechsig atlas}
 \item{flechsig_3d}{ - flechsig 3d mesh atlas}
}
}
\concept{ggseg_atlases ggseg3d_atlases}
\keyword{datasets}
