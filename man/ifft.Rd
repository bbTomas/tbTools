% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbTools.R
\name{ifft}
\alias{ifft}
\title{ifft}
\usage{
ifft(sig)
}
\arguments{
\item{sig}{input vector}
}
\value{
output vector of the same length as the input vector
}
\description{
Inverse Fast Fourier Transform (discrete FT), Matlab-like behavior.
}
\details{
This is really the inverse of the fft function, so ifft(fft(x)) == x.
}
\examples{
ifft(fft(1:5))
}
\seealso{
\code{\link[stats]{fft}}, \code{\link[base]{Re}}, \code{\link[base]{Im}}, \code{\link[base]{Mod}}, \code{\link[base]{Conj}}
}
