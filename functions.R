library(grid)

group <- function(src, op, dst) {
    grid.rect()
    grid.segments(0, 1, 1, 0)
    grid.group(src, op, dst)
    grid.text(op, x=.1, y=.1, just=c("left", "bottom"),
              gp=gpar(fontfamily="HersheySans"))
}
porterDuff <- function(src, dst) {
    grid.newpage()
    pushViewport(viewport(width=.95, height=.95,
                          layout=grid.layout(3, 4, respect=TRUE)))
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1),
                 viewport(width=.95, height=.95))
    group(src, "over", dst)
    popViewport(2)
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=2),
                 viewport(width=.95, height=.95))
    group(src, "clear", dst)
    popViewport(2)
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=3),
                 viewport(width=.95, height=.95))
    group(src, "source", dst)
    popViewport(2)
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=4),
                 viewport(width=.95, height=.95))
    group(src, "in", dst)
    popViewport(2)
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1),
                 viewport(width=.95, height=.95))
    group(src, "out", dst)
    popViewport(2)
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=2),
                 viewport(width=.95, height=.95))
    group(src, "atop", dst)
    popViewport(2)
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=3),
                 viewport(width=.95, height=.95))
    group(src, "dest", dst)
    popViewport(2)
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=4),
                 viewport(width=.95, height=.95))
    group(src, "dest.over", dst)
    popViewport(2)
    pushViewport(viewport(layout.pos.row=3, layout.pos.col=1),
                 viewport(width=.95, height=.95))
    group(src, "dest.in", dst)
    popViewport(2)
    pushViewport(viewport(layout.pos.row=3, layout.pos.col=2),
                 viewport(width=.95, height=.95))
    group(src, "dest.out", dst)
    popViewport(2)
    pushViewport(viewport(layout.pos.row=3, layout.pos.col=3),
                 viewport(width=.95, height=.95))
    group(src, "dest.atop", dst)
    popViewport(2)
    pushViewport(viewport(layout.pos.row=3, layout.pos.col=4),
                 viewport(width=.95, height=.95))
    group(src, "xor", dst)
    popViewport(2)
    popViewport()
    grid.text(names(dev.cur()), x=unit(1, "mm"), y=unit(1, "mm"),
              just=c("left", "bottom"), 
              gp=gpar(fontfamily="HersheySans", col="grey"))
    grid.text(R.version.string, x=unit(1, "npc") - unit(1, "mm"), 
              y=unit(1, "mm"),
              just=c("right", "bottom"), 
              gp=gpar(fontfamily="HersheySans", col="grey"))
}

pdSimpleSolid <- function() {
    grid.newpage()
    r1 <- rectGrob(x=1/3, y=2/3, width=.5, height=.5,
                   gp=gpar(col=NA, fill=rgb(.7, 0, 0)))
    r2 <- rectGrob(x=2/3, y=1/3, width=.5, height=.5,
                   gp=gpar(col=NA, fill=rgb(0, 0, .9)))
    porterDuff(r2, r1)
}

pdSimpleTrans <- function() {
    grid.newpage()
    r1 <- rectGrob(x=1/3, y=2/3, width=.5, height=.5,
                   gp=gpar(col=NA, fill=rgb(.7, 0, 0, .8)))
    r2 <- rectGrob(x=2/3, y=1/3, width=.5, height=.5,
                   gp=gpar(col=NA, fill=rgb(0, 0, .9, .4)))
    porterDuff(r2, r1)
}

pdStrokeSolid <- function(op=NULL, stroke1=TRUE, stroke2=TRUE) {
    grid.newpage()
    r1 <- rectGrob(x=1/3, y=2/3, width=.5, height=.5,
                   gp=gpar(col=if (stroke1) "black" else NA,
                           lwd=if (stroke1) 20 else NA, fill=rgb(.7, 0, 0)))
    r2 <- rectGrob(x=2/3, y=1/3, width=.5, height=.5,
                   gp=gpar(col=if (stroke2) "black" else NA,
                           lwd=if (stroke2) 20 else NA, fill=rgb(0, 0, .9)))
    if (is.null(op))
        porterDuff(r2, r1)
    else
        group(r2, op, r1)
}

pdStrokeTrans <- function(op=NULL) {
    grid.newpage()
    r1 <- rectGrob(x=1/3, y=2/3, width=.5, height=.5,
                   gp=gpar(col=rgb(0,0,0,.5), lwd=20, fill=rgb(.7, 0, 0)))
    r2 <- rectGrob(x=2/3, y=1/3, width=.5, height=.5,
                   gp=gpar(col=rgb(0,0,0,.5), lwd=20, fill=rgb(0, 0, .9)))
    if (is.null(op))
        porterDuff(r2, r1)
    else
        group(r2, op, r1)
}

pdGroupSolid <- function() {
    grid.newpage()
    r1 <- rectGrob(x=1/3, y=2/3, width=.5, height=.5,
                   gp=gpar(col="black", lwd=20, fill=rgb(.7, 0, 0)))
    r2 <- rectGrob(x=2/3, y=1/3, width=.5, height=.5,
                   gp=gpar(col="black", lwd=20, fill=rgb(0, 0, .9)))
    porterDuff(groupGrob(r2), r1)
}

pdGroupFill <- function() {
    grid.newpage()
    r1 <- rectGrob(x=1/3, y=2/3, width=.5, height=.5,
                   gp=gpar(col=NA, fill=rgb(.7, 0, 0)))
    r2 <- rectGrob(x=2/3, y=1/3, width=.5, height=.5,
                   gp=gpar(col=NA, fill=rgb(0, 0, .9)))
    porterDuff(groupGrob(r2), r1)
}

