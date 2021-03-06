# Encoding: utf8
# Palette with alpha

library(tcltk)
library(tkrplot)

plot.color <- function() {
	#cat('color =',color,'\n')
	p <- par(mar=c(0,0,0,0),oma=c(0,0,0,0))
	plot(0,0,xlim=c(-1,1),ylim=c(-1,1),type='n',xaxt='n',yaxt='n',xlab=NA,ylab=NA)
	axis(1,labels=FALSE,tick=FALSE)
	axis(2,labels=FALSE,tick=FALSE)
	axis(3,labels=FALSE,tick=FALSE)
	axis(4,labels=FALSE,tick=FALSE)
	polygon(c(-1,1,1,-1),c(-1,-1,1,1),col=color,border=NA)
	par(p)
}

color <- 'white'

palette.ui <- tktoplevel()
tkwm.title(palette.ui, 'Color Selection')

pos.frame <- tkframe(palette.ui, borderwidth=2)
color.block <- tkframe(pos.frame, borderwidth=2)
#canvas <- tkcanvas(pos.frame, width='80', height='25', bg='white')
color.plot <- tkrplot(color.block, fun=plot.color, vscale=0.618)
red.block <- tkframe(pos.frame, borderwidth=2)
green.block <- tkframe(pos.frame, borderwidth=2)
blue.block <- tkframe(pos.frame, borderwidth=2)
alpha.block <- tkframe(pos.frame, borderwidth=2)

red.label <- tklabel(red.block, text='Red:')
red.var <- tclVar('255')
red.entry <- tkentry(red.block, width=3, textvariable=red.var, foreground='black', font='bold', borderwidth=0.5)
green.label <- tklabel(green.block, text='Green:')
green.var <- tclVar('255')
green.entry <- tkentry(green.block, width=3, textvariable=green.var, foreground='black', font='bold', borderwidth=0.5)
blue.label <- tklabel(blue.block, text='Blue:')
blue.var <- tclVar('255')
blue.entry <- tkentry(blue.block, width=3, textvariable=blue.var, foreground='black', font='bold', borderwidth=0.5)
alpha.label <- tklabel(alpha.block, text='Alpha:')
alpha.var <- tclVar('255')
alpha.entry <- tkentry(alpha.block, width=3, textvariable=alpha.var, foreground='black', font='bold', borderwidth=0.5)

ok.fun <- function() {
	red <- as.integer(tclvalue(red.var))
	green <- as.integer(tclvalue(green.var))
	blue <- as.integer(tclvalue(blue.var))
	alpha <- as.integer(tclvalue(alpha.var))
	#tkmessage(title='Info', message=sprintf('rgb(%d, %d, %d, %d)',red,green,blue,alpha),icon='info',type='ok')
	cat(sprintf('rgb(%d, %d, %d, %d) = ',red,green,blue,alpha))
	color <<- rgb(red,green,blue,alpha,maxColorValue=255)
	cat(color,'\n')
	#color <<- rgb(red,green,blue,maxColorValue=255)
	#tkconfigure(canvas, bg=color)
	#color.plot <<- tkrplot(color.block, fun=plot.color, vscale=0.618)
	tkrreplot(color.plot)
}
button.frame <- tkframe(palette.ui)
ok.button <- tkbutton(button.frame, text='OK', command=ok.fun)

#tkgrid(canvas)
#tkgrid(color.block, row=1, column=1, rowspan=4, columnspan=3)
#tkgrid(red.label, red.entry)
#tkgrid(red.block, row=1, column=4)
#tkgrid(green.label, green.entry)
#tkgrid(green.block, row=2, column=4)
#tkgrid(blue.label, blue.entry)
#tkgrid(blue.block, row=3, column=4)
#tkgrid(alpha.label, alpha.entry)
#tkgrid(alpha.block, row=4, column=4)
##tkgrid(canvas)
tkgrid(color.plot)
tkgrid(color.block)
tkgrid(red.label, red.entry)
tkgrid(red.block)
tkgrid(green.label, green.entry)
tkgrid(green.block)
tkgrid(blue.label, blue.entry)
tkgrid(blue.block)
tkgrid(alpha.label, alpha.entry)
tkgrid(alpha.block)
tkgrid(pos.frame)
tkgrid(ok.button)
tkgrid(button.frame)
tkfocus(palette.ui)
tkwait.window(palette.ui)

