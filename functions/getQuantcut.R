getQuantcut <- function (score, quantiles = 0:10/10) {
    score <- na.omit(score)
    if (all(grepl("[.]", score[!is.na(score)]))) {
        tmp.digits <- 2
    } else {
        tmp.digits <- 4
    }
    tmp.cuts <-
        .quantcut(
            x = score,
            q = quantiles,
            right = FALSE,
            dig.lab = min(tmp.digits, max(nchar(score)))
        )

    levs <- levels(tmp.cuts)
    
    mkpairs <- function(x) {
        sapply(x, function(y) {
                if (length(y) == 1) return(y[c(1, 1)])
                if (length(y) == 2) y[c(2, 2)] else y[2:3]
            }
        )
    }
    pairs <- t(mkpairs(strsplit(levs, "[^0-9+\\.\\-]+")))
    tmp.lookup <-
      data.table(seq(length(levs)), t(mkpairs(strsplit(levs, "[^0-9+\\.\\-]+"))))
    setnames(tmp.lookup, c("Qx", "LOW", "HIGH"))
    tmp.lookup
}

###   .quantcut Utility from `gofSGP` (which is based on the `quantcut` package)

.quantcut <- function (x, q = seq(0, 1, by = 0.25), na.rm = TRUE, dig.lab, ...) {
    x <- round(x, digits = dig.lab)
    quant <- quantile(x, q, na.rm = na.rm) # Deal with dups better.
    dups <- duplicated(quant)
    if (any(dups)) {
        flag <- x %in% unique(quant[dups])
        retval <- ifelse(flag, paste("[", as.character(x), "]", sep = ""), NA)
        uniqs <- unique(quant)
        reposition <- function(ct) {
            flag <- x >= ct
            if (sum(flag) == 0) return(ct) else return(min(x[flag], na.rm = na.rm))
        }

        newquant <- sapply(uniqs, reposition)
        if (any(duplicated(newquant))) {
            newquant <- sort(uniqs)
        }
        retval[!flag] <-
          as.character(cut(x[!flag], breaks = newquant, include.lowest = TRUE, ...))
        levs <- na.omit(unique(retval[order(x)]))
        retval <- factor(retval, levels = levs)
        mkpairs <- function(x) sapply(x, function(y) if (length(y) == 2) y[c(2, 2)] else y[2:3])
        pairs <- mkpairs(strsplit(levs, "[^0-9+\\.\\-]+"))
        rownames(pairs) <- c("lower.bound", "upper.bound")
        colnames(pairs) <- levs
        closed.lower <- rep(FALSE, ncol(pairs))
        closed.upper <- rep(TRUE, ncol(pairs))
        closed.lower[1] <- TRUE
        for (i in 2:ncol(pairs)) {
            if (pairs[1, i] == pairs[1, i - 1] && pairs[1, i] == pairs[2, i - 1]) {
                closed.lower[i] <- FALSE
            }
        }
        for (i in 1:(ncol(pairs) - 1)) {
            if (pairs[2, i] == pairs[1, i + 1] && pairs[2, i] == pairs[2, i + 1]) {
                closed.upper[i] <- FALSE
            }
        }
        levs <-
            ifelse(pairs[1, ] == pairs[2, ],
                   pairs[1, ],
                   paste(
                     ifelse(closed.lower, "[", "("), pairs[1, ], ",", pairs[2, ],
                     ifelse(closed.upper, "]", ")"), sep = ""
                   )
            )
        levels(retval) <- levs
    } else {
        retval <- cut(x, quant, include.lowest = TRUE, dig.lab = dig.lab, ...)
    }
    return(retval)
} ## END .quantcut function


getFCASE <- function(variable, lookup) {
    max.q <- max(lookup$Qx)
    getCondition <- function(r) {
        precond <- paste0(
        if("CONTENT_AREA" %in% names(lookup)) paste0("CONTENT_AREA=='", lookup[r, CONTENT_AREA], "' & "),
        if("GRADE" %in% names(lookup)) paste0("GRADE=='", lookup[r, GRADE], "' & "),
        variable)
        if (lookup[r, LOW] == lookup[r, HIGH]) {
            return(paste0(precond, " == ", lookup[r, LOW], ", 'Q", lookup[r, Qx], "'"))
        }
        if (lookup[r, Qx]==1) {
            return(paste0(precond, " < ", lookup[r, HIGH], ", 'Q1'"))
        }
        if (lookup[r, Qx] != max.q) {
            paste0(
                precond, " >= ", lookup[r, LOW], " & ",
                variable, " < ", lookup[r, HIGH], ", 'Q", lookup[r, Qx], "'") |> return()
            # data.table %between% doesn't apply the inclusive/exclusive the way we want:
            # return(paste0(precond, " %between% c(", paste(lookup[r, .(LOW, HIGH)], collapse = ", "), "), 'Q", lookup[r, Qx], "'"))
        } else {
            return(paste0(precond, " >= ", lookup[r, LOW], ", 'Q", lookup[r, Qx], "'"))
        }
    }
    tmp.fcase <- unlist(lapply(seq(nrow(lookup)), getCondition))
    return(paste0("fcase(", paste(tmp.fcase, collapse = ","), ")"))
}
