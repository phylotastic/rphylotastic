get_fams_on_tips <- function(yellowstone_bird_tree, yellowstone_bird_fams){
  yellowstone_bird_tree$tip.label = names(yellowstone_bird_fams$family)
  return(yellowstone_bird_tree)
}

write_plot <- function(phy = NULL, file = "1", height = 5.5, width = 5.5,
    mai = c(0.7,0.1,0,0), tip.color =
    ifelse(tree$tip.label%in%birds_I_saw, "red", "black"), ...,
  arclabelspars = NULL){

    filename <- paste0("data-raw/yellowstone_bird_tree_plot", file, ".pdf")
    pdf(filename, width, height)
    par(xpd = TRUE)
    par(mai = mai)
    ape::plot.phylo(tree, tip.color = tip.color, ...)
    if(!methods::hasArg(type)){
        ape::axisPhylo(cex.axis = 0.5)
        mtext("Time (MYA)", at = (max(get("last_plot.phylo",envir =
        .PlotPhyloEnv)$xx) * 0.5), side = 1, line = 2, cex = 0.5)
    }
    if(inherits(arclabelspars, "list")){
      tips = arclabelspars$tips
      text = arclabelspars$text
      arc.cols = arclabelspars$arc.cols
      arc.label.offset = arclabelspars$arc.label.offset
      arc.line.offset= arclabelspars$arc.line.offset
      for(i in seq(length(tipsies))){
        cat(i, families[i], "\n")
        arclabels(phy = yellowstone_bird_tree, text = text[i], tips = tips[[i]],
            orientation = "horizontal", col = arc.cols[i], lwd = 4,
            lab.offset = arc.label.offset[i], ln.offset=arc.line.offset[i], cex = 0.5)
      }

    # nulo <- mapply(arclabels, tree, text = text, tips = tips,
    #     col = arc.cols, lab.offset = arc.label.offset, ln.offset= arc.line.offset,
    #     MoreArgs=list(mark.node=FALSE, lwd = 4, cex = 0.5, orientation = "horizontal"))
    }
    dev.off()
}

graph2_tests <- function(tree, birds_I_saw, families, tipsies){
    for(i in seq(length(tipsies))){
        pdf(paste0("data-raw/yellowstone_bird_tree_plot2_test", i, ".pdf"), height = 5.5, width = 5.5)
        par(xpd = TRUE)
        par(mai = rep(1, 4))
        ape::plot.phylo(tree,
            tip.color = ifelse(tree$tip.label%in%birds_I_saw, "red", "black"),
            cex=0.3, type = "fan", edge.width = 0.45, label.offset = 1.5)
        arclabels(phy = tree, text = families[i], tips = tipsies[[i]],
                orientation = "horizontal", lab.offset = 1.3, ln.offset=1.2, cex = 0.5)
        dev.off()
    }
}

albo <- function(arc.line.offset){
    arc_label_offset <- arc.line.offset+0.05
    arc_label_offset[29] <- arc_label_offset[29]-0.01
    arc_label_offset[30] <- arc_label_offset[30]+0.05
    arc_label_offset[31] <- arc_label_offset[31]+0.02
    return(arc_label_offset)
}

albo2 <- function(arc.line.offset){
  arc_label_offset <- arc.line.offset+0.05
  base <- arc_label_offset[1]
  arc_label_offset[c(10, 15, 28, 30)] <- base - 0.05
  arc_label_offset[c(1:4, 11, 19, 21, 23, 29, 34)] <- base - 0.1
  arc_label_offset[c(22, 26, 38)] <- base - 0.15
  arc_label_offset[c(12)] <- base + 0.5
  return(arc_label_offset)
}
