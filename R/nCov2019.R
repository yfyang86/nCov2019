#' download statistical numbers of the wuhan 2019-nCov 
#'
#' @title get_nCov2019
#' @return nCov2019 object
#' @export
#' @importFrom jsonlite fromJSON
#' @author Guangchuang Yu
get_nCov2019 <- function() {
  structure(jsonlite::fromJSON(.get_json()),
            class = 'nCov2019')
}

##' @method print nCov2019
##' @export
print.nCov2019 <- function(x, ...) {
  cat("China (total confirmed cases):", x$chinaTotal$confirm)
  cat("\nlast update:", time(x), "\n")
}

##' @importFrom stats time
##' @method time nCov2019
##' @export
time.nCov2019 <- function(x, ...) {
  x$lastUpdateTime
}

##' @method open nCov2019
##' @export
open.nCov2019 <- function(con, ...) {
  url <- 'https://news.qq.com/zt2020/page/feiyan.htm'
  utils::browseURL(url)
  invisible(url)
}

##' @method [ nCov2019
##' @export
`[.nCov2019` <- function(object, i, j, by="total", ...) {
  by <- match.arg(by, c("total", "today"))
  d <- object$areaTree[1,2][[1]]
  name = d[[1]]
  if (missing(i)) {
    res <- cbind(name=name, d[[by]])
  } else if (length(i) == 1) {
    res <- extract_province(object, i, by)
  } else {
    res <- do.call("rbind",
                   lapply(i, function(ii) {
                     extract_province(object, ii, by)
                    })
                   )
  }
    
  res[1:nrow(res), j, drop=F]
}

##' @method summary nCov2019
##' @export
summary.nCov2019 <- function(object, by = "total", ...) {
  by <- match.arg(by, c("total", "today"))
  if (by == "total") {
    return(object$chinaDayList)
  }
  return(object$chinaDayAddList)
}


extract_province <- function(object, i, by) {
  if (i == 'global') {
    res <- cbind(name = object$areaTree[[1]], object$areaTree[[by]])
    return(res)
  } 
  
  d <- object$areaTree[1,2][[1]]
  name = d[[1]]
  if (is.character(i)) {
    i <- which(name == i)
  }
  stats <- d[i, 2][[1]]
  cbind(name=stats$name, stats[[by]])
}

.get_json <- function() {
  url <- 'https://view.inews.qq.com/g2/getOnsInfo?name=disease_h5&callback=1580373566110'
  x <- suppressWarnings(readLines(url, encoding="UTF-8"))
  x <- sub("^\\d+\\(", "", x)
  x <- sub("\\)$", "", x)
  y <- jsonlite::fromJSON(x)
  return(y$data)  
}

#' This is data to be included in the nCov2019 package
#'
#' @name chinamaps
#' @docType data
#' @author Yifan Yang \email{yfyang.86@gmail.com}
#' @references \url{www.stats.gov.cn}
#' @keywords data
NULL



#' @importFrom magrittr %>%
#' @importFrom data.table data.table
#' @importFrom data.table merge
#' @import ggplot2
#' @import wesanderson
#' @import mapdata
#' @method plot nCov2019
#' @return nCov2019 list
#' @export
#' @param x nCov2019 Object
#' @param chinamaps Data Frame
#' @examples
#' # y = plot(get_nCov2019(), chinamaps)
#' # print(y$val)
plot.nCov2019 <-function(x, chinamaps){
  udf_1<-Vectorize(
    function(x,val=2) {
      x %>% strsplit(split = '') %>% unlist %>% length() > val}
    ,vectorize.args = 'x'
  )
  
  
  chinamaps.db <- data.table(chinamaps)
  #cat("PHASE 1") 
  chinamaps.db[LevelType==1,]$"ShortName" %>% as.character() -> prov_list
  
  city_db <- data.table(names="", lat=0, long=0, confirmed=0)
  
  u <- NULL
  
  for (i in prov_list){
    tryCatch( (u = rbind(u,cbind(i[1],x[i,]))), error=function(cond) {warning('no data')})
  }
  
  #cat("PHASE 2")
  u <- data.table(u)
  names(u)[1] = "ProvinceName"
  names(u)[2] = "ShortName"
  u$ShortName<- as.character(u$ShortName)
  u$ShortName<-sub('*区$','',u$ShortName)
  u$ShortName[u$ShortName %>% udf_1() %>% as.vector()] <- sub('*县$','',u[u$ShortName %>% udf_1() %>% as.vector(),ShortName])
  u$ShortName<-sub('*自治州$','',u$ShortName)
  u$ShortName[u$ShortName %>% udf_1() %>% as.vector()] <-  sub('*州$','',u[u$ShortName %>% udf_1() %>% as.vector(),ShortName])
  u$ShortName[u$ShortName %>% udf_1() %>% as.vector()] <-  sub('*市$','',u[u$ShortName %>% udf_1() %>% as.vector(),ShortName])
  
  z <- merge(u, chinamaps.db, by = "ShortName", all = TRUE)[confirm>0,]
  
  mat.cities = z[!is.na(lng),c("ShortName", "Lat", "lng", "confirm", "ProvinceName","ID")]
  
  mat.cities_db = mat.cities[mat.cities[,max(as.integer(ID)), by=ShortName], nomatch=0L, on = c(ID="V1")][,1:5]
  
  china <- map("china", plot = F)
  
  a<-list(date=format(Sys.time(), "%Y%b%d"), val=NULL, val2=NULL)
  
  a$val2 = ggplot() + 
    geom_path(data = china, aes(long, lat, group = group), color = '#3161A4', show.legend = F) +
    geom_point(data = mat.cities_db
               , aes(x = lng, y = Lat
                     , size = confirm
                     , color=ceiling(log(mat.cities_db$confirm))+1
               ), alpha = 0.8) + 
    scale_color_continuous( limits=c(1, max(ceiling(log(mat.cities_db$confirm))+1))
                            , breaks=seq(1, max(ceiling(log(mat.cities_db$confirm))+1), by=1)
                            , low = 'blue', high = 'red'
    ) +
    guides(color= guide_legend(), size=guide_legend()) + 
    theme_bw() + 
    theme(legend.position = "none", 
          axis.line = element_blank(),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +  
    ggtitle(paste("Date",format(Sys.time())))
  
  a$val = ggplot() + 
    geom_path(data = china, aes(long, lat, group = group), color = '#3161A4', show.legend = F) +
    geom_point(data = mat.cities_db
               , aes(x = lng, y = Lat
                     , size = confirm
                     , color= ProvinceName
               ), alpha = 0.8) + 
    guides(color= guide_legend(), size=guide_legend()) + 
    scale_fill_manual(values = wes_palette("Zissou1", 36, type = "continuous")) +
    theme_bw() + 
    theme(legend.position = "none", 
          axis.line = element_blank(),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +  
    ggtitle(paste("Date",format(Sys.time())))
  return(a);
}



