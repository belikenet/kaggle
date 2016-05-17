inferSideProb <- function(cabin) {
    if (!is.array(cabin) && is.na(cabin))
        return(NA_real_)
    cabins <- unique(unlist(strsplit(cabin, " ")))
    a <- mean(as.numeric(str_sub(cabins, -1)) %% 2, na.rm = T)
    return (a)
}

