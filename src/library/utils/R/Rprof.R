#  File src/library/utils/R/Rprof.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

Rprof <- function(filename = "Rprof.out", append = FALSE, interval =  0.02,
                  memory.profiling = FALSE, gc.profiling = FALSE,
                  line.profiling = FALSE, filter.callframes = FALSE,
                  numfiles = 100L, bufsize = 10000L,
                  event = c("default", "cpu", "elapsed"))
{
    event <- match.arg(event)
    if(is.null(filename)) filename <- ""
    invisible(.External(C_Rprof, filename, append, interval, memory.profiling,
                        gc.profiling, line.profiling, filter.callframes,
                        numfiles, bufsize, event))
}

Rprofmem <- function(filename = "Rprofmem.out", append = FALSE, threshold = 0)
{
    if(is.null(filename)) filename <- ""
    invisible(.External(C_Rprofmem, filename, append, as.double(threshold)))
}
