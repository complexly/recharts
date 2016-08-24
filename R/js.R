
tooltipJS <- function(type) {
    # tooltipJS templates for echarts
    js <- list()
    js[['time']] <- JS('function (params) {
    var date = new Date(params.value[0]);
    data = date.getFullYear() + "-"
        + (date.getMonth() + 1) + "-"
        + date.getDate() + " "
        + date.getHours() + ":"
        + date.getMinutes();
    if (param.value.length > 2) {
        return data + "<br/>"
        + params.value[1] + ", "
        + params.value[2];
    } else {
        return data + "<br/>"
        + params.value[1];
    }
    }')

    js[['scatter']] <- JS('function (params) {
    var i;
    var text;
    if (params.seriesName === null || params.seriesName === ""){
        if (params.value.length > 1) {
            text = params.value[0];
            for (i = 1; i < params.value.length; i++){
                text += " ,    " + params.value[i];
            }
            return text;
        } else {
            return params.name + " : " + params.value;
        }
    } else {
        if (params.value.length > 1) {
            text = params.seriesName + " :<br/>" + params.value[0];
            for (i = 1; i < params.value.length; i++) {
                text += " ,    " + params.value[i];
            }
            return text;
        } else {
            return params.seriesName + " :<br/>"
            + params.name + " : " + params.value;
        }
    }
    }')

    js[['chord_mono']] <- JS('function (params) {
    if (params.name && params.name.indexOf("-") != -1) {
    return params.name.replace("-", " " + params.seriesName + " ")
    }
    else {
    return params.name ? params.name : params.data.id
    }
    }')

    js[['chord_multi']] <- JS('function (params) {
    if (params.indicator2) {    // is edge
    return params.indicator2 + " " +
    params.name + " " + params.indicator + " : " +
    params.value.weight;
    } else {    // is node
    return params.name
    }
    }')

    js[['pie']] <- '{a} <br/>{b} : {c} ({d}%)'

    js[['k']] <- JS('function (params) {
        var res = params[0].name;
        res += "<br/>  Open : " + params[0].value[0] +
        "  High : " + params[0].value[3];
        res += "<br/>  Close : " + params[0].value[1] +
        "  Low : " + params[0].value[2];
        return res;
    }')

    js[['hist']] <- JS('function (params){
        return params.value[2] + "<br/>Count:" +
        params.value[1];
    }')
    switch(type,
           time = js$time, scatter = js$scatter, chord_mono = js$chord_mono,
           chord_multi = js$chord_multi, pie = js$pie, k = js$k,
           hist = js$hist)
    }

convFormat2JS <- function(fmt, type=c("value", "category", "time")){
    # fmt is fmt char in sprintf()
    type <- match.arg(type)
    js <- NULL
    if (type %in% c('value', 'log')){
        if (grepl("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[dfGg](.*)$", fmt)){
            pre = gsub("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[dfGg](.*)$","\\1", fmt)
            n = as.numeric(gsub("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[dfGg](.*)$",
                                "\\3", fmt))
            unit = gsub("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[dfGg](.*)$","\\4", fmt)
            n = ifna(n, 0)
            js = JS(paste0("function(x){return '", pre, "' + x.toFixed(", n,
                           ") + '", unit, "';}"))
        }else if (grepl("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[Ee](.*)$", fmt)){
            pre = gsub("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[Ee](.*)$","\\1", fmt)
            n = as.numeric(gsub("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[Ee](.*)$",
                                "\\3", fmt))
            unit = gsub("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[Ee](.*)$","\\4", fmt)
            n = ifna(n, 0)
            js = JS(paste0("function(x){return '", pre, "' + x.toExponential(",
                           n, ") + '", unit, "';}"))
        }
    }else if (type == 'category'){
        if (grepl("^(.*)%s(.*)$", fmt)){
            pre = gsub("^(.*)%s(.*)$","\\1", fmt)
            unit = gsub("^(.*)%s(.*)$","\\2", fmt)
            js = JS(paste0("function(x){return '", pre, "' + x + '", unit, "';}"))
        }
    ## for time, fmt is a strftime format
    }else if (type == 'time'){
        if (fmt == '%F')  # %Y-%m-%d
            js = JS(paste0("function(x) {return x.getFullYear() + '-' + ",
                           "x.getMonth() + '-' + x.getDate();}"))
        if (fmt == '%D')  # %m/%d/%y
            js = JS(paste0("function(x) {return x.getMonth() + '/' + ",
                           "x.getDate() + '/' + x.getYear();}"))
        if (fmt == '%c')  # %a %b %e %H:%M:%S %Y
            js = JS(paste0("function(x) {
                           var monthNames = ['January', 'February', 'March',
                           'April', 'May', 'June', 'July', 'August', 'September',
                           'October', 'November', 'December'];",
                           "var weekNames = ['Monday', 'Tuesday', 'Wednesday',
                           'Thursday', 'Friday', 'Saturday', 'Sunday'];",
                           "return weekNames[x.getDay()] + ' ' + ",
                           "monthNames[x.getMonth()] + ' ' + x.getDate() + ' ' + ",
                           "x.getHours() + ':' + x.getMinutes() + ':' + ",
                           "x.getSeconds() + ' ' + x.getFullYear();}"))
        if (fmt %in% c('%T','%X'))  # %H:%M:%S
            js = JS(paste0("function(x) {return x.getHours() + ':' + ",
                           "x.getMinutes() + ':' + x.getSeconds();}"))
        if (fmt %in% c('%R'))  # %H:%M
            js = JS(paste0("function(x) {return x.getHours() + ':' + ",
                           "x.getMinutes();}"))
        if (fmt %in% c('%x'))  # %y/%m/%d
            js = JS(paste0("function(x) {return x.getYear() + '/' + ",
                           "x.getMonth() + '/' + x.getDate();}"))
        if (is.null(js)){
            js = fmt
            js = gsub("%[Aa]", "' + weekNames[x.getDay()] + '", js)
            js = gsub("%[Bbh]", "' + monthNames[x.getMonth()] + '", js)
            js = gsub("%C", "' + floor(x.getFullYear() / 100) + '", js)
            js = gsub("%[de]", "' + x.getDate() + '", js)
            js = gsub("%[gy]", "' + x.getYear() + '", js)
            js = gsub("%[GY]", "' + x.getFullYear() + '", js)
            js = gsub("%H", "' + x.getHours() + '", js)
            js = gsub("%I", paste0("' + x.getHours()>12 ? x.getHours()-12 : '",
                                   "x.getHours() + "), js)
            js = gsub("%M", "' + x.getMinutes() + '", js)
            js = gsub("%m", "' + x.getMonth() + '", js)
            js = gsub("%p", "' + x.getHours()>12 ? 'PM' : 'AM' + '", js)
            js = gsub("%S", "' + x.getSeconds() + '", js)
            js = gsub("%u", "' + weekNames[x.getDay()] + '", js)
            js = gsub("%w", "' + x.getDay() + '", js)
            js = paste0("function(x) {",
                        "var monthNames = ['January', 'February', 'March',
                        'April', 'May', 'June', 'July', 'August', 'September',
                        'October', 'November', 'December'];",
                        "var weekNames = ['Monday', 'Tuesday', 'Wednesday',
                        'Thursday', 'Friday', 'Saturday', 'Sunday'];",
                        "return ", substr(js, 4, nchar(js)))
            js = gsub("^(.+) \\+ ' *$", "\\1;\\}", js)
            js = JS(gsub("\\+ '' \\+", "+", js))
        }
    }
    if (is.null(js)) {
        if (grepl("^\\{.+\\}.*$", fmt)) js <- fmt
        else js <- JS("function (x) {return x;}")
    }
    return(js)
}

