# make sure htmlwidgets:::toJSON() turns list() to {} in JSON, instead of []
.emptyList = setNames(list(), character())
emptyList = function() .emptyList

# evaluate a formula using `data` as the environment, e.g. evalFormula(~ z + 1,
# data = data.frame(z = 1:10))
evalFormula = function(x, data) {
  if (!inherits(x, 'formula')) return(x)
  if (length(x) != 2) stop('The formula must be one-sided: ', deparse(x))
  eval(x[[2]], data, environment(x))
}

evalVar <- function(var, data){
    stopifnot(inherits(var, 'formula'))
    if (var != ~NULL){
        evalFormula(var, data=data)
    }
}

evalVarArg <- function(x, data, simplify=FALSE, eval=TRUE){
    # eval var list to a data.frame
    # E.g.
    ## evalVarArg(Species, iris)
    ## evalVarArg(~Species, iris)
    ## evalVarArg("Species", iris)
    ## evalVarArg(as.numeric(Species), iris)
    ## evalVarArg(~as.numeric(Species), iris)
    ## evalVarArg("as.numeric(Species)", iris)
    ## evalVarArg(c(Species, Sepal.Width), iris)
    ## evalVarArg(c(as.numeric(Species), Sepal.Width), iris)
    ## evalVarArg(c(as.numeric(Species)+1, Sepal.Width), iris)

    # coerce x to formula
    x <- arg1 <- deparse(substitute(x))
    x <- gsub('^\\"(.*)\\"$' , '\\1', x)
    x <- as.formula(ifelse(grepl("^~", x), x, paste('~', x)))

    # split x if it is packed in a list/ vector
    if (! grepl('^(c|list|data\\.frame)\\(', deparse(substitute(x)[[2]])))
        x <- deparse(substitute(x)[[2]])  # a string
    else
        x <- as.character(parse(text=x[[2]]))  # a vector
      ## each element in x --> formula
    #browser()
    x[! grepl("^~", x)] <- paste('~', x[! grepl("^~", x)])
    x <- as.vector(sapply(x, as.formula))

    # loop evalVar() and filter valid data.frame
    out <- sapply(x, evalVar, data=data, simplify=FALSE)
    nrows <- sapply(out, length)
    out <- out[names(nrows)[nrows==nrow(data)]]
    names <- names(x)
    names <- names[nrows==nrow(data)]
    names <- gsub("^ *~ *(.*)$|^c\\((.*)\\)$", "\\1", names)
    out <- as.data.frame(out, stringsAsFactors=FALSE)
    names(out) <- names
    if (length(out) == 0){
        warning(paste("You yielded nothing by requiring", deparse(substitute(arg1)),
                      "out of", deparse(substitute(data))))
        return(NULL)
    }else{
        if (eval){
            if (simplify) if (ncol(out) == 1) return(out[,1])
            return(out)
        }else{
            return(names)
        }
    }
}

# merge two lists by names, e.g. x = list(a = 1, b = 2), mergeList(x, list(b =
# 3)) => list(a = 1, b = 3)
mergeList = function(x, y) {
  if (!is.list(y) || length(y) == 0) return(x)
  yn = names(y)
  if (length(yn) == 0 || any(yn == '')) {
    warning('The second list to be merged into the first must be named')
    return(x)
  }
  for (i in yn) {
    xi = x[[i]]
    yi = y[[i]]
    if (is.list(xi)) {
      if (is.list(yi)) x[[i]] = mergeList(xi, yi)
    } else x[[i]] = yi
  }
  x
}

# automatic labels from function arguments
autoArgLabel = function(arg, auto) {
    if (inherits(try(arg, TRUE), 'try-error')) arg <- deparse(substitute(arg))
    if (! inherits(arg, 'formula') && ! is.null(arg)) {
        if (! grepl("^~", arg, ''))  arg <- as.formula(paste('~', arg))
    }
    if (is.null(arg)) return('')
    if (inherits(arg, 'formula')) return(deparse(arg[[2]]))
    auto
}

#' @export
#' @exportMethod + echarts
"+.echarts" <- function(e1, e2){
    stopifnot(inherits(e1, 'echarts'))
    browser()
    e1 <- deparse(substitute(e1))
    e2 <- deparse(substitute(e2))
    return(paste(e1,'%>%',e2))
}

# -------------Lazy functions to judge class-------------------
isDate <- function(x, format=NULL){
    if (!is.null(format)){
        if (!is(try(as.Date(x),TRUE),"try-error")) TRUE else FALSE
    }else{
        if (!is(try(as.Date(x,format=format),TRUE),"try-error")) TRUE else FALSE
    }
}
isTime <- function(x, origin=NULL, tz='CST'){
    if (is.null(origin)){
        return(FALSE)
    }else{
        if (!is(try(as.POSIXct,T),"try-error")) TRUE else FALSE
    }
}
isLatin <- function(x){
    if (is.factor(x)) x <- as.character(x)
    return(all(grepl("^[[:alnum:][:space:][:punct:]]+$", x, perl=TRUE)))
}
isFormula <- function(x){
    return(inherits(x, 'formula'))
}

ifnull <- function(x, y)  if (is.null(x)) return(y) else return(x)
ifna <- function(x, y)  if (is.na(x)) return(y) else return(x)
ifnan <- function(x, y)  if (is.nan(x)) return(y) else return(x)
ifblank <- function(x, y)  if (length(x) == 0) return(y) else return(x)
ifzero <- function(x, y)  if (x==0) return(y) else return(x)
ifmissing <- function(x, y) if (missing(x)) return(y) else return(x)

convTimestamp <- function(time, from='R', to='JS'){
    stopifnot(inherits(time, c("numeric", "Date", "POSIXct", "POSIXlt")))
    if (from=='R' && to=='JS')
        return(as.numeric(as.POSIXct(time, orig="1970-01-01")) * 1000)
    if (from=='JS' && to=='R')
        return(as.POSIXct(time/1000, orig="1970-01-01"))
}
#--------Other functions for position, color, HTML table conversion------------

#' Get A String Containing 'rgba' Function
#'
#' Echarts uses rgba function heavily. You can convert color vectors into rgba function
#' in string form to pass to an echarts object.
#' @param vecrgb A vector of RGB elements, or simply red int.
#' @param ... If vecrgb is simply red int, you can pass green, blue, alpha int here.
#'
#' @return A character string. E.g, 'rgba(125, 125, 125, 0.6)' or '#FFFFFF'
#' @export
#'
#' @examples
#' \dontrun{
#' rgba(c(123, 123, 124, 125))  # return 'rgba(123,123,124,0.490196078431373)'
#' rgba(123, 123, 124, 0.5) # return 'rgba(123,123,124,0.5)'
#' rgba(123, 123, 124)  # return '#7B7B7C'
#' }
rgba <- function(vecrgb, ...){
    if (is.matrix(vecrgb) && dim(vecrgb) == c(3,1)) vecrgb <- vecrgb[,1]
    ## vecrgb is yielded from col2rgb()

    if (is.list(vecrgb)) rgb <- as.vector(unlist(vecrgb))
    if (length(vecrgb) == 1) vecrgb <- c(vecrgb, unlist(list(...)))
    if (min(vecrgb, na.rm=TRUE)<0 || max(vecrgb, na.rm=TRUE)>255) {
        stop("All elements should be numeric 0-255!")
    }
    if (length(vecrgb[!is.na(vecrgb)]) == 3){
        return(rgb(red=vecrgb[1], green=vecrgb[2], blue=vecrgb[3], max=255))
    }else if (length(vecrgb[!is.na(vecrgb)])==4){
        #return(rgb(red=vecrgb[1],green=vecrgb[2],blue=vecrgb[3],alpha=vecrgb[4],
        #           max=255))
        return(paste0('rgba(',vecrgb[1],',',vecrgb[2],',',vecrgb[3],',',
                      as.numeric(ifelse(vecrgb[4]<=1, vecrgb[4],
                                        round(vecrgb[4]/255, 4))),
                      ')'))
    }else{
        stop("Must be of length 3 or 4!")
    }
}

checkColorDiff <- function(col1, col2, ...){
    stopifnot((col1 %in% colors() || grepl("#[[:xdigit:]]{6}", col1) ||
                   grepl("^rgba\\(", col1)) &&
              (col2 %in% colors() || grepl("#[[:xdigit:]]{6}", col2) ||
                   grepl("^rgba\\(", col2)))
    if (grepl("^rgba\\(", col1)){
        col1 <- as.numeric(unlist(strsplit(col1, "[\\(,\\)]")[[1]][2:5]))
        col1 <- rgb(col1[1], col1[2], col1[3], col1[4]*255, max=255)
    }else{
        col1 <- getColors(col1)
    }
    if (grepl("^rgba\\(", col2)){
        col2 <- as.numeric(unlist(strsplit(col2, "[\\(,\\)]")[[1]][2:5]))
        col2 <- rgb(col2[1], col2[2], col2[3], col2[4]*255, max=255)
    }else{
        col2 <- getColors(col2)
    }

    bright1 <- sum(c(299, 587, 114) * col2rgb(col1))/1000
    bright2 <- sum(c(299, 587, 114) * col2rgb(col2))/1000
    brightDiff <- abs(bright1 - bright2)
    hueDiff <- sum(abs(col2rgb(col1, TRUE) - col2rgb(col2, TRUE)))
    return(data.frame('Diff' = c(brightDiff, hueDiff),
                      'Suffiecient'=c(brightDiff >= 125, hueDiff >= 500),
                      row.names=c('Bright', 'Hue')))
}

#' Invert A Color to Its Conplementary Color
#'
#' @param color A hex or named color, or color in 'rgba(R, G, B, A)' string.
#' @param mode One or a vector of modes combined. You can only input the first letter.
#' Default 'bw', which is most useful in textStyles.
#' \describe{
#'  \item{\code{bw}}{black and white invertion}
#'  \item{\code{opposite}}{complete invertion to get an opposite color}
#'  \item{\code{hue}}{only invert hue in terms of \code{\link{hsv}}}
#'  \item{\code{saturation}}{only invert saturation in terms of \code{\link{hsv}}}
#'  \item{\code{lumination}}{only invert lumination in terms of \code{\link{hsv}}}
#' }
#' @param ... Elipsis
#'
#' @return Inverted hex color
#' @export
#'
#' @seealso \code{\link{hsv}}, \code{\link{rgb2hsv}}, \code{\link{rgb}},
#' @examples
#' col <- sapply(list('o', 'h', 'l', 's', 'b', c('h', 'l'), c('h', 's'),
#'               c('l', 's'), c('h', 's', 'l')), function(mode) {
#'               return(invertColor('darkred', mode))
#'         })
#' library(scales)
#' show_col(c('darkred', unlist(col)))
#'
invertColor <- function(color, mode=c('bw', 'opposite', 'hue', 'saturation',
                                      'lumination', ''),
                        ...){
    if (! grepl("^rgba\\(", color)) col <- color <- getColors(color)
    if (grepl("^rgba\\(", color)){
        col <- as.numeric(unlist(strsplit(col, "[\\(,\\)]")[[1]][2:5]))
        col <- rgb(col[1], col[2], col[3], col[4]*255, max=255)
    }
    modeAbbrev <- tolower(substr(mode, 1, 1))
    rgb <- col2rgb(col)
    hsv <- rgb2hsv(rgb)

    if ('b' %in% modeAbbrev){  # black and white invert
        bright <- sum(c(299, 587, 114) * rgb) / 1000
        if (bright >= 128) return("#000000")
        else return("#FFFFFF")
    }else if ('o' %in% modeAbbrev) {
        rgb_neg <- rep(255, 3) - rgb
        return(rgb(rgb_neg[1], rgb_neg[2], rgb_neg[3], max=255))
    }else{
        if ('h' %in% modeAbbrev)
            hsv[1] <- ifelse(hsv[1] > 0.5, hsv[1] - 0.5, hsv[1] + 0.5)
        if ('s' %in% modeAbbrev)
            hsv[2] <- 1 - hsv[2]
        if ('l' %in% modeAbbrev)
            hsv[3] <- 1 - hsv[3]
        return(hsv(hsv[1], hsv[2], hsv[3]))
    }
}

#' Text Position and Direction
#'
#' Converts text postion from clock digits to c(x, y, direction) vector
#' @param pos 1-12, clock digits.
#'
#' @return A vector of x-alignment, y-alignment and direction.
#' @export
#'
#' @examples
#' \dontrun{
#' vecPos(2) ## returns c("right", "top", "vertical")
#' }
#' @note
#' # Postion of Clock Numbers 1-12 \cr
#' \tabular{lllll}{
#'  10(l, t, v) \tab 11(l, t, h) \tab 12(c, t, h) \tab 1(r, t, h) \tab 2(r, t, v) \cr
#'  9(l, c, v) \tab \tab \tab \tab 3(r, c, v) \cr
#'  8(l, b, v) \tab 7(l, b, h) \tab 6(c, b, h) \tab 5(r, b, h) \tab 4(r, b, v)
#' }
#'
vecPos <- function(pos){
    TblPos=as.data.frame(rbind(c("right",  "top",    "horizontal"),
                               c("right",  "top",    "vertical"),
                               c("right",  "center", "vertical"),
                               c("right",  "bottom", "vertical"),
                               c("right",  "bottom", "horizontal"),
                               c("center", "bottom", "horizontal"),
                               c("left",   "bottom", "horizontal"),
                               c("left",   "bottom", "vertical"),
                               c("left",   "center", "vertical"),
                               c("left",   "top",    "vertical"),
                               c("left",   "top",    "horizontal"),
                               c("center", "top",    "horizontal")
                               ),
                         stringsAsFactors=FALSE)
    names(TblPos) <- c("x","y","z")
    return(as.vector(unlist(TblPos[pos,])))
}

exchange <- function(x, y){
    a <- x
    x <- y
    y <- a
    return(list(x, y))
}
