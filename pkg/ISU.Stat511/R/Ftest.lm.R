Ftest.lm <-
function(object, Cmat, d = 0, conf.level = 0.95) {
    b = coef(object)
    V = vcov(object)
    if (is.null(dim(Cmat))) 
        Cmat = matrix(Cmat, nrow = 1)
    dfn = nrow(Cmat)
    dfd = object$df
    if (ncol(Cmat) != length(b)) 
        stop("Number of columns in 'Cmat' (", ncol(Cmat), ") does not match the length of coefficients (", 
            length(b), ")")
    Cb.d = (Cb <- Cmat %*% b) - d
    Fstat = drop(t(Cb.d) %*% solve(Cmat %*% V %*% t(Cmat)) %*% 
        Cb.d/dfn)
    pvalue = 1 - pf(Fstat, dfn, dfd)
    names(Fstat) = "F"
    PARAMETER = c(`num df` = dfn, `denom df` = dfd)
    se = sqrt(diag(Cmat %*% V %*% t(Cmat)))
    alpha = 1 - conf.level
    tval = qt(1 - alpha/2, dfd)
    cint = cbind(Cb - tval * se, Cb + tval * se)
    dimnames(cint) = list(rownames(Cmat), sprintf("%.1f%%",
        100 * c(alpha/2, 1 - alpha/2)))
    attr(cint, "conf.level") = conf.level
    res = list(statistic = Fstat, parameter = PARAMETER, p.value = pvalue, 
        conf.int = cint, method = "F test for Cmat %*% beta = d", 
        data.name = deparse(substitute(object)))
    attr(res, "class") = "htest"
    res
}

