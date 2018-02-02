#################################################################
#' Functions to count figures and include cross-reference
#'
#' @param string label for cross-reference
#' @param string caption for figure
#' @param string prefix
#'
#' @return Captions and cross-reference in rendered markdown document
#'
#' @export
#' @docType methods
#' @rdname figRef-method
# @examples
# needs examples
# options(figcap.prefix = "Figure", figcap.sep = ":", figcap.prefix.highlight = "**")
# figRef("label", "A caption")
# `r figRef("label")`
# Cross-referencing: http://galahad.well.ox.ac.uk/repro/
figRef <- local({
    tag <- numeric()
    created <- logical()
    used <- logical()
    function(label, caption, prefix = options("figcap.prefix"), 
        sep = options("figcap.sep"), prefix.highlight = options("figcap.prefix.highlight")) {
        i <- which(names(tag) == label)
        if (length(i) == 0) {
            i <- length(tag) + 1
            tag <<- c(tag, i)
            names(tag)[length(tag)] <<- label
            used <<- c(used, FALSE)
            names(used)[length(used)] <<- label
            created <<- c(created, FALSE)
            names(created)[length(created)] <<- label
        }
        if (!missing(caption)) {
            created[label] <<- TRUE
# original version that included 'Figure 1:'
#        paste0(prefix.highlight, prefix, " ", i, sep, prefix.highlight, 
#                " ", caption)
# modified version that just keeps count and includes the caption
            paste0(" ", caption)
        } else {
            used[label] <<- TRUE
            paste(prefix, tag[label])
        }
    }
})


#################################################################
#' Functions to count tables and include cross-reference
#'
#' @param string label for cross-reference
#' @param string caption for figure
#' @param string prefix
#'
#' @return Captions and cross-reference in rendered markdown document
#'
#' @export
#' @docType methods
#' @rdname tabRef-method
# @examples
# needs examples
# options(tabcap.prefix = "Table", tabcap.sep = ":", tabcap.prefix.highlight = "**")
# tabRef("label", "A caption")
# `r tabRef("label")`
# Cross-referencing: http://galahad.well.ox.ac.uk/repro/
tabRef <- local({
    tag <- numeric()
    created <- logical()
    used <- logical()
    function(label, caption, prefix = options("tabcap.prefix"), 
        sep = options("tabcap.sep"), prefix.highlight = options("tabcap.prefix.highlight")) {
        i <- which(names(tag) == label)
        if (length(i) == 0) {
            i <- length(tag) + 1
            tag <<- c(tag, i)
            names(tag)[length(tag)] <<- label
            used <<- c(used, FALSE)
            names(used)[length(used)] <<- label
            created <<- c(created, FALSE)
            names(created)[length(created)] <<- label
        }
        if (!missing(caption)) {
            created[label] <<- TRUE
            paste0(prefix.highlight, prefix, " ", i, sep, prefix.highlight, 
                " ", caption)
        } else {
            used[label] <<- TRUE
            paste(prefix, tag[label])
        }
    }
})