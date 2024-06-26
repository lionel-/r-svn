##  File src/library/utils/R/strcapture.R
##  Part of the R package, https://www.R-project.org
##
##  Copyright (C) 1995-2023 The R Core Team
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  https://www.R-project.org/Licenses/

strcapture <- function(pattern, x, proto, perl = FALSE, useBytes = FALSE) {
    m <- regexec(pattern, x, perl=perl, useBytes=useBytes)
    str <- regmatches(x, m)
    ntokens <- length(proto) + 1L
    nomatch <- lengths(str) == 0L
    str[nomatch] <- list(rep.int(NA_character_, ntokens))
    if (length(str) > 0L && length(str[[1L]]) != ntokens) {
### FIXME: this will not always detect an error when there are no matches
        stop("The number of captures in 'pattern' != 'length(proto)'")
    }
    mat <- matrix(as.character(unlist(str)), ncol=ntokens,
                  byrow=TRUE)[,-1L,drop=FALSE]
    conformToProto(mat, proto)
}

## Not yet exported
strextract <- function(pattern, x, perl = FALSE, useBytes = FALSE) {
    m <- regexec(pattern, x, perl=perl, useBytes=useBytes)
    unlist(regmatches(x, m))
}

conformToProto <- function(mat, proto) {
    ans <- lapply(seq_along(proto), function(i) {
        if (isS4(proto[[i]])) {
            methods::as(mat[,i], class(proto[[i]]))
        } else {
            fun <- match.fun(paste0("as.", class(proto[[i]])))
            fun(mat[,i])
        }
    })
    names(ans) <- names(proto)
    if (isS4(proto)) {
        methods::as(ans, class(proto))
    } else {
        as.data.frame(ans, optional=TRUE, stringsAsFactors=FALSE)
    }
}

## Not yet exported
strslice <- function(x, split, proto, fixed = FALSE, perl = FALSE,
                     useBytes = FALSE)
{
    str <- strsplit(x, split, fixed=fixed, perl=perl, useBytes=useBytes)
    ntokens <- length(proto)
    if (length(str) > 0L) {
        if (length(str[[1L]]) != ntokens) {
            stop("The number of tokens != 'length(proto)'")
        } else if (length(unique(lengths(str))) > 1L) {
            stop("The number of tokens is not consistent across 'x'")
        }
    }
    mat <- matrix(as.character(unlist(str)), ncol=ntokens, byrow=TRUE)
    conformToProto(mat, proto)
}

## not yet exported; called from tools:::config_val_to_logical()
str2logical <- function(x, na.ok=TRUE) {
    if(!is.character(x)) x <- as.character(x)
    stopifnot(length(x) == 1L)
    if(na.ok && (is.na(x) || x == "NA")) return(NA)
    if(!is.na(v <- as.logical(x))) # via fast C code, e.g. for "True"
        return(v)
    v <- tolower(x)
    if      (v %in% c("1", "yes")) TRUE
    else if (v %in% c("0", "no")) FALSE
    else {
        warning(gettextf("cannot coerce %s to TRUE or FALSE", sQuote(x)),
                domain = NA)
        NA
    }
}
