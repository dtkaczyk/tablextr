parse <- function(url) {
    data <- RCurl::getURL(url)
    XML::htmlParse(data)
}

getNodes <- function(xmlData, xpath=NULL, children=c()) {
    if (!is.null(xpath)) {
        XML::getNodeSet(xmlData, xpath)
    } else if (length(children) == 0) {
        XML::xmlChildren(xmlData)
    } else {
        child <- XML::xmlChildren(xmlData)
        child[names(child) %in% children]
    }
}

getValue <- function(node) {
    stringr::str_trim(XML::xmlValue(node))
}

getName <- XML::xmlName

getAttrNames <- function(node) {
    names(XML::xmlAttrs(node))
}

getAttrValue <- XML::xmlGetAttr
