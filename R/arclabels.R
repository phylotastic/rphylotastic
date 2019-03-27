# modified from phytools::arc.cladelabels

#' Add arc labels to tips of a phylogeny; works for non-monophyletic groups and single tip lineages.
#'
#' @inheritParams phytools::arc.cladelabels
#' @param phy An object of class phylo.
#' @param tips A character vector (or a list ?) with the names of the tips that belong to the clade or group. If multiple groups are going to be plotted, tips must be given in the form of a list.
#' @param text A character vector indicating the desired text to label the arcs.
#' @param plot_singletons Boolean. If TRUE (default), it will add arcs (and labels) to single tip lineages too. If FALSE, no arc or labels will be plotted over that tip..
# #' @param ln.offset
# #' @param lab.offset
# #' @param cex
# #' @param orientation
#' @export
#' @importFrom ape .PlotPhyloEnv
arclabels <- function(phy, tips, ...) {
    UseMethod("arclabels", tips)
}

#' @return \code{NULL}
#'
#' @rdname arclabels
#' @method arclabels default
#' @export
arclabels.default <- function (phy = NULL, tips, text, plot_singletons = TRUE,
    ln.offset = 1.02, lab.offset = 1.06,
    cex = 1, orientation = "horizontal", ...){

    # tips <- tipsies[[7]]
    obj <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    if (obj$type != "fan")
        stop("method works only for type=\"fan\"")
    h <- max(sqrt(obj$xx^2 + obj$yy^2))
    if (methods::hasArg(lwd))
        lwd <- list(...)$lwd
    else lwd <- graphics::par()$lwd
    if (methods::hasArg(col))
        col <- list(...)$col
    else col <- graphics::par()$col
    if (methods::hasArg(lend))
        lend <- list(...)$lend
    else lend <- graphics::par()$lend
    if (methods::hasArg(clockwise))
        clockwise <- list(...)$clockwise
    else clockwise <- TRUE
    if (methods::hasArg(n))
        n <- list(...)$n
    else n <- 0.05
    if (is.null(phy)) {
        phy <- list(edge = obj$edge, tip.label = 1:obj$Ntip,
            Nnode = obj$Nnode)
        class(phy) <- "phylo"
    }
    # d <- getDescendants(phy, node)
    # tips <- tipsies[[9]]
    if(is.character(tips)){
        d <- which(phy$tip.label%in%tips)
    }
    d <- sort(d[d <= ape::Ntip(phy)])
    breaks <- c(0, which(diff(d) != 1), length(d))
    breaks <- lapply(seq(length(breaks) - 1),
        function(i) d[(breaks[i] + 1):breaks[i+1]])
    for (i in seq(length(breaks))){
        if(!plot_singletons & length(breaks[[i]]) > 1) {
            next }
        d <- breaks[[i]]
        deg <- atan(obj$yy[d]/obj$xx[d]) * 180/pi
        # print(deg)
        ii <- intersect(which(obj$yy[d] >= 0), which(obj$xx[d] <
            0))
        deg[ii] <- 180 + deg[ii]
        ii <- intersect(which(obj$yy[d] < 0), which(obj$xx[d] < 0))
        deg[ii] <- 180 + deg[ii]
        ii <- intersect(which(obj$yy[d] < 0), which(obj$xx[d] >=
            0))
        deg[ii] <- 360 + deg[ii]
        # ln.offset <- 1.16
        plotrix::draw.arc(x = 0, y = 0, radius = ln.offset * h, deg1 = min(deg),
            deg2 = max(deg), lwd = lwd, col = col, lend = lend, n = n)
        if(!is.null(text)){
            if (orientation == "curved")
                plotrix::arctext(text, radius = lab.offset * h, middle = stats::median(range(deg *
                    pi/180)), cex = cex, clockwise = clockwise)
            else if (orientation == "horizontal") {
                x0 <- lab.offset * cos(stats::median(deg) * pi/180) * h
                y0 <- lab.offset * sin(stats::median(deg) * pi/180) * h
                graphics::text(x = x0, y = y0, label = text, adj = c(if (x0 >=
                    0) 0 else 1, if (y0 >= 0) 0 else 1), offset = 0,
                    cex = cex)
                }
        }
    }
}


# #' @return \code{NULL}
# #'
# #' @rdname arclabels
# #' @method arclabels list
# #' @export
# arclabels.list <- function (phy = NULL, text, tips, plot_singletons = TRUE, ln.offset = 1.02, lab.offset = 1.06,
#     cex = 1, orientation = "horizontal", ...){
