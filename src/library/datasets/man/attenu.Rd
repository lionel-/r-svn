% File src/library/datasets/man/attenu.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{attenu}
\docType{data}
\alias{attenu}
\title{The \I{Joyner}--\I{Boore} Attenuation Data}
\description{
  This data gives peak accelerations measured at various observation
  stations for 23 earthquakes in California.  The data have been used
  by various workers to estimate the attenuating affect of distance
  on ground acceleration.
}
\usage{attenu}
\format{
  A data frame with 182 observations on 5 variables.
  \tabular{rlll}{
    [,1] \tab \code{event}   \tab numeric \tab Event Number\cr
    [,2] \tab \code{mag}     \tab numeric \tab Moment Magnitude\cr
    [,3] \tab \code{station} \tab factor  \tab Station Number\cr
    [,4] \tab \code{dist}    \tab numeric \tab Station-hypocenter distance (km)\cr
    [,5] \tab \code{accel}   \tab numeric \tab Peak acceleration (g)}
}
\source{
  Joyner, W.B., D.M. Boore and R.D. Porcella (1981).  Peak horizontal
  acceleration and velocity from strong-motion records including
  records from the 1979 Imperial Valley, California earthquake.  \abbr{USGS}
  Open File report 81-365. Menlo Park, Ca.
}
\references{
  Boore, D. M. and Joyner, W. B.(1982).
  The empirical prediction of ground motion,
  \emph{Bulletin of the Seismological Society of America}, \bold{72},
  S269--S268.

  Bolt, B. A. and Abrahamson, N. A. (1982).
  New attenuation relations for peak and expected accelerations of
  strong ground motion.
  \emph{Bulletin of the Seismological Society of America}, \bold{72},
  2307--2321.

  Bolt B. A. and Abrahamson, N. A. (1983).
  Reply to W. B. Joyner & D. M. Boore's \dQuote{Comments on: New
    attenuation relations for peak and expected accelerations for peak
    and expected accelerations of strong ground motion},
  \emph{Bulletin of the Seismological Society of America}, \bold{73},
  1481--1483.

  Brillinger, D. R. and Preisler, H. K. (1984).
  An exploratory analysis of the Joyner-Boore attenuation data,
  \emph{Bulletin of the Seismological Society of America}, \bold{74},
  1441--1449.

  Brillinger, D. R. and Preisler, H. K. (1984).
  \emph{Further analysis of the Joyner-Boore attenuation data}.
  Manuscript.
}
\examples{
require(graphics)
## check the data class of the variables
sapply(attenu, data.class)
summary(attenu)
pairs(attenu, main = "attenu data")
coplot(accel ~ dist | as.factor(event), data = attenu, show.given = FALSE)
coplot(log(accel) ~ log(dist) | as.factor(event),
       data = attenu, panel = panel.smooth, show.given = FALSE)
}
\keyword{datasets}
