#' @importFrom data.table melt
series_scatter <- function(lst, type, return=NULL, ...){
    # g = echartr(mtcars, wt, mpg, am)
    lst <- mergeList(list(weight=NULL, series=NULL), lst)
    if (!is.numeric(lst$x[,1])) stop('x and y must be numeric')
    data <- cbind(lst$y[,1], lst$x[,1])

    if (!is.null(lst$weight)){  # weight as symbolSize
        data <- cbind(data, lst$weight[,1])
        minWeight <- min(abs(lst$weight[,1]), na.rm=TRUE)
        maxWeight <- max(abs(lst$weight[,1]), na.rm=TRUE)
        range <- maxWeight - minWeight
        folds <- maxWeight / minWeight
        if (abs(folds) < 50){  # max/min < 50, linear
            jsSymbolSize <- JS(paste0('function (value){
                return ', switch(ceiling(abs(folds)/10), 6,5,4,3,2),
                '*Math.round(Math.abs(value[2]/', minWeight,'));
                }'))
        }else{  # max/min >= 50, normalize
            jsSymbolSize <- JS(paste0('function (value){
                return Math.round(1+29*(Math.abs(value[2])-', minWeight,')/', range, ');
            }'))
        }
    }
    obj <- list()
    if (is.null(lst$series)) {  # no series
        if (is.null(lst$weight))
            obj <- list(list(type=type$type[1], data=asEchartData(data[,2:1])))
        else
            obj <- list(list(type=type$type[1], data=asEchartData(data[,c(2:1,3)]),
                             symbolSize=jsSymbolSize))
    }else{  # series-specific
        data <- cbind(data, lst$series[,1])
        data <- split(as.data.frame(data), lst$series[,1])
        if (is.null(lst$weight)){
            obj <- lapply(seq_along(data), function(i){
                list(name = names(data)[i], type = type$type[i],
                     data = asEchartData(data[[i]][,2:1]))
            })  ## only fetch col 1-2 of data, col 3 is series
        }else{
            obj <- lapply(seq_along(data), function(i){
                list(name = names(data)[i], type = type$type[i],
                     data = asEchartData(data[[i]][,c(2:1, 3)]),
                     symbolSize=jsSymbolSize)
            })  ## fetch col 1-2 and 3 (x, y, weight)
        }
    }

    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }
}

series_bar <- function(lst, type, return=NULL, ...){
    # example:
    # echartr(mtcars, row.names(mtcars), mpg,
    #     series=factor(am,labels=c('Manual','Automatic')),
    #     type=c('hbar','scatter'))
    lst <- mergeList(list(series=NULL), lst)
    data <- cbind(lst$y[,1], lst$x[,1])

    if (!'y' %in% names(lst)) {  # y is null, then...
        if (grepl('hist', type$misc)){  # histogram
            hist <- hist(data[,1], plot=FALSE)
            if (grepl('density', type$misc)){
                data <- as.matrix(cbind(hist$density, hist$mids))  # y, x
            }else{
                data <- as.matrix(cbind(hist$counts, hist$mids))  # y, x
            }
        }else{ # simply run freq of x
            if (is.numeric(data[,1])){
                data <- as.matrix(as.data.frame(table(data[,1])))
            }else{
                data <- as.matrix(table(data[,1]))
            }
        }
    }

    obj <- list()
    if (is.null(lst$series)) {  # no series
        if (is.numeric(lst$x[,1])){
            obj <- list(list(type=type$type[1], data=asEchartData(data[,2:1])))
            if (any(grepl("flip", type$misc))) obj[[1]]$barHeight=10
            if (grepl('hist',type$misc)) {
                obj[[1]]$barGap = '1%'
                obj[[1]]$barWidth = floor((dev.size('px')[1]-200) / length(hist$breaks))
                obj[[1]]$barMaxWidth = floor(820 / length(hist$breaks))
            }
        }else{
            obj <- list(list(type=type$type[1], data=asEchartData(data[,1])))
        }
    }else{  # series-specific
        dataCross <- tapply(data[,1], list(data[,2], lst$series[,1]), function(x) {
            if (length(x) == 1) return(x)
            stop('y must only have one value corresponding to each combination of x and series')
        })
        idx <- match(unique(data[,2]),rownames(dataCross))
        dataCross <- dataCross[idx,]
        #rownames(dataCross) <- data[,2]
        data <- dataCross

        obj <- lapply(seq_len(ncol(data)), function(i){
            if (is.numeric(lst$x[,1])){
                o = list(name = colnames(data)[i], type = type$type[i],
                         data = asEchartData(cbind(as.numeric(rownames(data)),
                                                        data[,i])))
                if (any(grepl("flip", type$misc)))
                    o <- mergeList(o, list(barHeight=10))
            }else{
                o = list(name = colnames(data)[i], type = type$type[i],
                         data = asEchartData(data[,i]))
            }
            if (type$stack[i]) o[['stack']] = 'Group'
            return(o)
        })
    }

    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }
}

series_line = function(lst, type, return=NULL, ...) {
    # Example:
    # g=echartr(airquality, as.character(Day), Temp,z= Month, type='curve')
    # g=echartr(airquality, as.character(Day), Temp,z= Month, type='area_smooth')
    lst <- mergeList(list(series=NULL), lst)
    data <- cbind(lst$y[,1], lst$x[,1])

    if (is.null(lst$x[,1]) && is.ts(lst$y[,1])) {
        lst$x[,1] = as.numeric(time(lst$y[,1]))
        lst$y[,1] = as.numeric(lst$y[,1])
    }
    obj <- list()

    if (is.numeric(lst$x[,1])) {
        obj = series_scatter(lst, type = type)
    }else{
        if (is.null(lst$series[,1])) {
            obj = list(list(type = 'line', data = asEchartData(lst$y[,1])))
        }
    }
    if (length(obj) == 0) obj = series_bar(lst, type = type)

    # area / stack / smooth
    areaIdx <- which(grepl("fill", type$misc))
    stackIdx <- which(type$stack)
    smoothIdx <- which(type$smooth)
    if (length(areaIdx) > 0){
        for (i in areaIdx)  obj[[i]][['itemStyle']] <-
                list(normal=list(areaStyle=list(
                    type='default')))
    }
    if (length(stackIdx) > 0) {
        for (i in stackIdx) obj[[i]][['stack']] <- 'Group'
    }
    if (length(smoothIdx) > 0) {
        for (i in smoothIdx) obj[[i]][['smooth']] <- TRUE
    }

    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }

}

series_k <- function(lst, type, return=NULL, ...){
    # g=echartr(stock, date, c(open, close, low, high), type='k')

    data <- cbind(lst$y[,1], lst$x[,1])
    obj <- list(list(name='Stock', type=type$type[1], data=asEchartData(lst$y[,1:4])))
    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }
}

series_pie <- function(lst, type, return=NULL, ...){
    # g=echartr(stock, date, c(open, close, low, high), type='k')

    data <- cbind(lst$y[,1], lst$series[,1])
    data <- dcast()
    obj <- list(list(name='Stock', type=type$type[1], data=asEchartData(lst$y[,1:4])))
    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }
}
#---------------------------legacy functions-----------------------------------
# split the data matrix for a scatterplot by series
data_scatter = function(x, y, series = NULL, type = 'scatter') {
  xy = unname(cbind(x, y))
  if (is.null(series)) return(list(list(type = type, data = xy)))
  xy = split(as.data.frame(xy), series)
  nms = names(xy)
  obj = list()
  for (i in seq_along(xy)) {
    obj[[i]] = list(name = nms[i], type = type, data = unname(as.matrix(xy[[i]])))
  }
  obj
}

data_bar = function(x, y, series = NULL, type = 'bar') {

  # plot the frequencies of x when y is not provided
  if (is.null(y)) {

    if (is.null(series)) {
      y = table(x)
      return(list(list(type = type, data = unname(c(y)))))
    }

    y = table(x, series)
    nms = colnames(y)
    obj = list()
    for (i in seq_len(ncol(y))) {
      obj[[i]] = list(name = nms[i], type = type, data = unname(y[, i]))
    }
    return(obj)

  }

  # when y is provided, use y as the height of bars
  if (is.null(series)) {
    return(list(list(type = type, data = y)))
  }

  xy = tapply(y, list(x, series), function(z) {
    if (length(z) == 1) return(z)
    stop('y must only have one value corresponding to each combination of x and series')
  })
  xy[is.na(xy)] = 0
  nms = colnames(xy)
  obj = list()
  for (i in seq_len(ncol(xy))) {
    obj[[i]] = list(name = nms[i], type = type, data = unname(xy[, i]))
  }
  obj

}

data_line = function(x, y, series = NULL) {
  if (is.null(x) && is.ts(y)) {
    x = as.numeric(time(y))
    y = as.numeric(y)
  }
  if (is.numeric(x)) {
    return(data_scatter(x, y, series, type = 'line'))
  }
  if (is.null(series)) {
    return(list(list(type = 'line', data = y)))
  }
  data_bar(x, y, series, type = 'line')
}
