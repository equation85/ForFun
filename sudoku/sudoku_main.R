#!/usr/bin/env Rscript

library(tcltk)
source('sudoku_worker.R')

tclRequire('Tktable')

# Functions
set.number.at.tk <- function(id,num,...) {
	tclvalue(number.tk[[id]]) <- num
	if(length(list(...))>0) tkconfigure(entry.tk[[id]],...)
}
set.number.all.tk <- function(num,sq.size=3,sq.num=3,color='black') {
	if(is.matrix(num)) num <- as.vector(num)
	if(length(num)!=length(number.tk))
		stop('`num` should have the same length as `number.tk`. Check codes!')
	if(length(color)==1) color <- rep('black',length(num))
	for(i in 1:length(num)) set.number.at.tk(i,num[i],foreground=color[i],font='bold')
}
ok.fun <- function() {
	number <- as.integer(sapply(number.tk,tclvalue))
	mat <- matrix(number,nrow=edge.len)
	res <- solve.sudoku(mat,sq.size,sq.num,FALSE)
	if(is.matrix(res)) {
		#print.sudoku(res,sq.size,sq.num)
		color <- ifelse(as.vector(is.na(mat)),'blue','black')
		set.number.all.tk(res,sq.size,sq.num,color)
	} else {
		tkmessageBox(title='Sudoku',message='Sorry! I can\'t find a solution.',icon='info',type='ok')
	}
}
clear.fun <- function() {
	num <- rep('',length(number.tk))
	set.number.all.tk(num,sq.size,sq.num)
}

### Main
# Set parameters
sq.size <- 3
sq.num <- 3
number <- c(4,1,8, NA,NA,5, NA,NA,6, NA,NA,NA, NA,NA,NA, 4,NA,NA, 5,7,NA, NA,NA,6, NA,NA,2, 6,3,9, 4,5,8, 1,NA,7, 1,8,NA, NA,NA,2, 3,NA,5, 7,2,5, NA,1,NA, 6,NA,4, NA,4,6, 3,NA,9, NA,NA,NA, 2,NA,NA, NA,NA,NA, NA,NA,NA, NA,NA,NA, 7,NA,NA, NA,NA,NA)
number[is.na(number)] <- ''

# Intialize
sudoku.ui <- tktoplevel()
tkwm.title(sudoku.ui,'Sudoku')
edge.len <- sq.size*sq.num
number.tk <- entry.tk <- vector('list',(edge.len)^2)
input.frame <- tkframe(sudoku.ui)
square.frame <- vector('list',sq.num^2)
for(i in 1:length(square.frame)) square.frame[[i]] <- tkframe(input.frame,borderwidth=2)
# Arrange entry widgets
for(i in 1:length(number.tk)) {
	sq.id <- sq2order.id(mat2sq.id(i))
	pos.row <- (i-1) %% sq.size
	pos.col <- (ceiling(i/edge.len)-1) %% sq.size
	number.tk[[i]] <- tclVar(number[i])
	entry.tk[[i]] <- tkentry(square.frame[[sq.id]],width=2,textvariable=number.tk[[i]],foreground='black',font='bold')
	tkgrid(entry.tk[[i]],row=pos.row,column=pos.col)
}
# Arrage frame widgets
for(i in 1:length(square.frame)) {
	pos <- arrayInd(i,.dim=c(sq.num,sq.num))[1,]
	tkgrid(square.frame[[i]],row=pos[1],column=pos[2])
}

button.frame <- tkframe(sudoku.ui)
ok.button <- tkbutton(button.frame,text=' Solve it! ',command=ok.fun)
clear.button <- tkbutton(button.frame,text=' Clear ',command=clear.fun)
quit.button <- tkbutton(button.frame,text=' Quit ',command=function() tkdestroy(sudoku.ui))

tkgrid(input.frame,columnspan=2)
tkgrid(ok.button,clear.button,quit.button)
tkgrid(button.frame)
tkfocus(sudoku.ui)
tkwait.window(sudoku.ui)

