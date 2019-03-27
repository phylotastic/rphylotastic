# modified version of phytools::arc.cladelabels
#' Add arc labels to tips of a phylogeny; works for non-monophyletic groups and single tip lineages.
#'
#' @inheritParams phytools::arc.cladelabels
# #' @param tree An object of class phylo.
# #' @param text A character vector indicating the desired text to label the arcs.
#' @param tips A character vector with the names of the tips that belong to the clade or group
#' @param plot_singletons Boolean. If TRUE (default), it will add arcs (and labels) to single tip lineages too. If FALSE, no arc or labels will be plotted over that tip..
# #' @param ln.offset
# #' @param lab.offset
# #' @param cex
# #' @param orientation
#' @export
arclabels <- function (tree = NULL, text, tips = NULL, plot_singletons = TRUE, ln.offset = 1.02, lab.offset = 1.06,
    cex = 1, orientation = "horizontal", ...){

        # tips <- tipsies[[7]]
    obj <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    if (obj$type != "fan")
        stop("method works only for type=\"fan\"")
    h <- max(sqrt(obj$xx^2 + obj$yy^2))
    if (hasArg(lwd))
        lwd <- list(...)$lwd
    else lwd <- par()$lwd
    if (hasArg(col))
        col <- list(...)$col
    else col <- par()$col
    if (hasArg(lend))
        lend <- list(...)$lend
    else lend <- par()$lend
    if (hasArg(clockwise))
        clockwise <- list(...)$clockwise
    else clockwise <- TRUE
    if (hasArg(n))
        n <- list(...)$n
    else n <- 0.05
    # if (mark.node)
    #     points(obj$xx[node], obj$yy[node], pch = 21, bg = "red")
    if (is.null(tree)) {
        tree <- list(edge = obj$edge, tip.label = 1:obj$Ntip,
            Nnode = obj$Nnode)
        class(tree) <- "phylo"
    }
    # d <- getDescendants(tree, node)
    # tips <- tipsies[[9]]
    if(is.character(tips)){
        d <- which(tree$tip.label%in%tips)
    }
    d <- sort(d[d <= Ntip(tree)])
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
                plotrix::arctext(text, radius = lab.offset * h, middle = median(range(deg *
                    pi/180)), cex = cex, clockwise = clockwise)
                #     text = "Passerilidae"
                #     lab.offset <- 1.2
                # plotrix::arctext(text, radius = lab.offset * h, middle = mean(range(deg)),
                # cex = cex, clockwise = clockwise)
            else if (orientation == "horizontal") {
                x0 <- lab.offset * cos(median(deg) * pi/180) * h
                y0 <- lab.offset * sin(median(deg) * pi/180) * h
                text(x = x0, y = y0, label = text, adj = c(if (x0 >=
                    0) 0 else 1, if (y0 >= 0) 0 else 1), offset = 0,
                    cex = cex)
                }
        }
    }
}
