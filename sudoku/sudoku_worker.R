### Print sudoku matrix
print.sudoku <- function(mat, sq.size=3, sq.num=3, na.char='-') {
	mat.string <- matrix(as.character(mat),nrow=nrow(mat),ncol=ncol(mat))
	mat.string[is.na(mat.string)] <- na.char
	print.string <- '\n'
	hline.string <- paste(rep(paste(rep('--',sq.size),collapse=''),sq.num),collapse='-+')
	for(i in 1:(sq.size*sq.num)) {
		for(j in 1:(sq.size*sq.num)) {
			if(j%%sq.size==0 && ceiling(j/sq.size)!=sq.num) {
			# Draw a number
				print.string <- sprintf(' %s %1s',print.string,mat.string[i,j])
			# Draw a vertical line
				print.string <- sprintf(' %s |',print.string)
			} else {
			# Draw a number
				print.string <- sprintf(' %s %1s',print.string,mat.string[i,j])
			}
		}
		print.string <- sprintf('%s\n',print.string)
		if(i%%sq.size==0 && ceiling(i/sq.size)!=sq.num) {
		# Draw a horizontal line
			print.string <- sprintf('%s%s\n',print.string,hline.string)
		}
	}
	cat(print.string)
}

#uncertainty <- list(row=apply(eg.mat,1,function(x) sum(is.na(x))), col=apply(eg.mat,2,function(x) sum(is.na(x))))

### Get square index of a specific postion within the sudoku matrix
mat2sq.id <- get.square.ind <- function(pos, sq.size=3, sq.num=3) {
	if(length(pos)==1) pos <- arrayInd(pos,.dim=c(sq.size*sq.num,sq.size*sq.num))
	return( c(ceiling(pos[1]/sq.size),ceiling(pos[2]/sq.size)) )
}

### Get square's order
sq2order.id <- get.square.order <- function(sq.pos, sq.num=3) {
	return( sq.num*(sq.pos[2]-1)+sq.pos[1] )
}

order2sq.id <- function(order.id, sq.num=3) {
	return( c(ceiling(order.id/sq.num), ifelse(order.id%%sq.num==0,sq.num,order.id%%sq.num)) )
}

### Get values of a specific square as a vector
get.square.vec <- function(mat, sq.pos, sq.size=3) {
	t.id <- 1+sq.size*(sq.pos[1]-1)
	b.id <- sq.size*sq.pos[1]
	l.id <- 1+sq.size*(sq.pos[2]-1)
	r.id <- sq.size*sq.pos[2]
	return( as.vector(mat[t.id:b.id,l.id:r.id]) )
}

### Check if all numbers were different in a vector, NAs omited
check.vec <- function(vec) {
	vec <- vec[!is.na(vec)]
	return( anyDuplicated(vec)==0 )
}

### Check if the sudoku matrix was reasonable
check.sudoku <- function(mat, sq.size=3, sq.num=3) {
	row.check <- apply(mat,1,check.vec)
	col.check <- apply(mat,2,check.vec)
	sq.id <- expand.grid(1:sq.num,1:sq.num)
	sq.check <- apply(sq.id,1,function(x) check.vec(get.square.vec(mat,x)) )
	return( all(row.check,col.check,sq.check) )
}

###
#get.candidates.at <- function(mat, pos, sq.size=3, sq.num=3) {
#	if(length(pos)==1) pos <- arrayInd(pos,.dim=c(sq.size*sq.num,sq.size*sq.num))
#	if(!is.na(mat[pos[1],pos[2]])) {
#		#warning(sprintf('mat[%d,%d] is NOT empty!',pos[1],pos[2]))
#		return(NA)
#	}
#	row.numbers <- mat[pos[1],]
#	col.numbers <- mat[,pos[2]]
#	sq.pos <- get.square.ind(pos, sq.size)
#	sq.numbers <- get.square.vec(mat, sq.pos, sq.size)
#	numbers <- unique(c(row.numbers,col.numbers,sq.numbers))
#	numbers <- numbers[!is.na(numbers)]
#	return( setdiff(1:(sq.size^2),numbers) )
#}
#
#get.candidates.all <- function(mat, sq.size=3, sq.num=3) {
#	candidates <- list()
#	candidates$order <- which(is.na(mat))
#	tmp <- arrayInd(candidates$order,.dim=c(sq.size*sq.num,sq.size*sq.num))
#	candidates$row <- tmp[,1]
#	candidates$col <- tmp[,2]
#	candidates$number <- lapply(candidates$order, function(x,mat,sz,nm) get.candidates.at(mat,x,sz,nm), mat=mat,sz=sq.size,nm=sq.num)
#	count.candi <- function(number) {
#		if(is.na(number[1]) && length(number)==1) return(NA)
#		return(length(number))
#	}
#	candidates$len <- sapply(candidates$number,count.candi)
#	return(candidates)
#}

### 
get.candidates.at <- function(mat, pos, sq.size=3, sq.num=3) {
	if(length(pos)==1) pos <- arrayInd(pos,.dim=c(sq.size*sq.num,sq.size*sq.num))
	if(!is.na(mat[pos[1],pos[2]])) {
		warning(sprintf('mat[%d,%d] is NOT empty!',pos[1],pos[2]))
		return(integer(0))
	}
	row.numbers <- mat[pos[1],]
	col.numbers <- mat[,pos[2]]
	sq.pos <- get.square.ind(pos, sq.size)
	sq.numbers <- get.square.vec(mat, sq.pos, sq.size)
	numbers <- unique(c(row.numbers,col.numbers,sq.numbers))
	numbers <- numbers[!is.na(numbers)]
	return( setdiff(1:(sq.size^2),numbers) )
}

get.candidates.all <- function(mat, sq.size=3, sq.num=3) {
	candidates <- list()
	candidates$order <- which(is.na(mat))
	tmp <- arrayInd(candidates$order,.dim=c(sq.size*sq.num,sq.size*sq.num))
	candidates$row <- tmp[,1]
	candidates$col <- tmp[,2]
	candidates$number <- lapply(candidates$order, function(x,mat,sz,nm) get.candidates.at(mat,x,sz,nm), mat=mat,sz=sq.size,nm=sq.num)
	candidates$len <- sapply(candidates$number,length)
	order2sqid <- function(x,sz,nm) {
		sq2order.id(mat2sq.id(x,sz,nm),nm)
	}
	candidates$square <- sapply(candidates$order,order2sqid,sz=sq.size,nm=sq.num)
	return(candidates)
}

###
check.rule2 <- function(number, by) {
	tapply(number,by,function(x) any(table(unlist(x))==1) )
}

## Initialize
#sudoku <- list()
#sudoku$mat <- eg.mat
#sudoku$square.size <- SQUARE.SIZE
#sudoku$square.num <- SQUARE.NUM
#sudoku$solution <- which(is.na(sudoku$mat),arr.ind=TRUE)
#sudoku$solution <- cbind(sudoku$solution,matrix(NA,nrow=nrow(sudoku$solution),ncol=2,dimnames=list(NULL,c('order','number'))))

#solved.empty.cell <- sudoku$solution[,c('row','col')]   # Keep track with unsolved empty cells
#solved.empty.cell
#solved.mat <- sudoku$mat   # Track solved matrix
#solved.candidates <- apply(solved.empty.cell,1,function(x) get.candidates(solved.mat,x,sq.size=sudoku$square.size) )   # Track possible candidates
#
#try.mat <- solved.mat   # Update during each try
#try.empty.cell <- solved.empty.cell
#try.candidates <- solved.candidates

### Rule 1: Only one candidate
### Rule 2: Only appear once 
### Rule 3: try
### candidates $order $number $row $col $len $square
solve.sudoku.recursive <- function(mat, sq.size=3, sq.num=3) {
	require(reshape2)
#-----------------
#mat <- eg.mat
#sq.size <- sq.num <- 3
#-----------------
	repeat {
		candidates <- get.candidates.all(mat,sq.size,sq.num)
		if(length(candidates$len)==0) return(mat)
		if(sum(candidates$len==0)>0) return(FALSE)
		# Rule 1
		if(sum(candidates$len==1)>0) {
			idx <- which(candidates$len==1)
			mat[candidates$order[idx]] <- unlist(candidates$number[idx])
			if(!check.sudoku(mat,sq.size,sq.num)) return(FALSE)
		} else {
			# Rule 2
			# Check rows
			tmp <- check.rule2(candidates$number,candidates$row)
			if(any(tmp)) {
				tmp.no <- as.integer(names(tmp)[tmp])  # row number
				for(no in tmp.no) {
					tmp.id <- which(candidates$row==no)
					tmp.melt <- melt(candidates$number[tmp.id])
					tmp.val <- tapply(tmp.melt[,'L1'],tmp.melt[,'value'],function(x) ifelse(length(x)==1,x,NA))
					tmp.val <- tmp.val[!is.na(tmp.val)]
					tmp.id <- tmp.id[tmp.val]
					tmp.val <- as.integer(names(tmp.val))
					mat[candidates$order[tmp.id]] <- tmp.val
				}
				if(!check.sudoku(mat,sq.size,sq.num)) return(FALSE)
				next
			}
			# Check columns
			tmp <- check.rule2(candidates$number,candidates$col)
			if(any(tmp)) {
				tmp.no <- as.integer(names(tmp)[tmp])  # col number
				for(no in tmp.no) {
					tmp.id <- which(candidates$col==no)
					tmp.melt <- melt(candidates$number[tmp.id])
					tmp.val <- tapply(tmp.melt[,'L1'],tmp.melt[,'value'],function(x) ifelse(length(x)==1,x,NA))
					tmp.val <- tmp.val[!is.na(tmp.val)]
					tmp.id <- tmp.id[tmp.val]
					tmp.val <- as.integer(names(tmp.val))
					mat[candidates$order[tmp.id]] <- tmp.val
				}
				if(!check.sudoku(mat,sq.size,sq.num)) return(FALSE)
				next
			}
			# Check square
			tmp <- check.rule2(candidates$number,candidates$square)
			if(any(tmp)) {
				tmp.no <- as.integer(names(tmp)[tmp])  # col number
				for(no in tmp.no) {
					tmp.id <- which(candidates$square==no)
					tmp.melt <- melt(candidates$number[tmp.id])
					tmp.val <- tapply(tmp.melt[,'L1'],tmp.melt[,'value'],function(x) ifelse(length(x)==1,x,NA))
					tmp.val <- tmp.val[!is.na(tmp.val)]
					tmp.id <- tmp.id[tmp.val]
					tmp.val <- as.integer(names(tmp.val))
					mat[candidates$order[tmp.id]] <- tmp.val
				}
				if(!check.sudoku(mat,sq.size,sq.num)) return(FALSE)
				next
			}
			break
		} # end if
	} # end repeat
	# Rule 3
	# candidates
	try.id <- which.min(candidates$len)
	repeat {
		if(length(candidates$number[[try.id]])>0) {
			# Try the first candidate number
			mat[candidates$order[try.id]] <- candidates$number[[try.id]][1]
			# Delete it
			candidates$number[[try.id]] <- candidates$number[[try.id]][-1]
			res <- solve.sudoku.recursive(mat,sq.size,sq.num)
			if(is.matrix(res)) {
			# Solution founded
				return(res)
			}
		} else {
			return(FALSE)
		}
	}
}
			
solve.sudoku <- function(mat, sq.size=3, sq.num=3, print=TRUE) {
	res <- solve.sudoku.recursive(mat, sq.size, sq.num)
	if(is.matrix(res)) {
		if(print) cat('Solution found.\n')
		if(print) print.sudoku(res,sq.size,sq.num)
		return(invisible(res))
	} else {
		if(print) cat('Sorry! I can\'t find a solution :(\n')
		return(mat)
	}
}


### Test examples
if(interactive()) {
	SQUARE.SIZE <- 3
	SQUARE.NUM <- 3
	eg.data <- c(4,1,8, NA,NA,5, NA,NA,6, NA,NA,NA, NA,NA,NA, 4,NA,NA, 5,7,NA, NA,NA,6, NA,NA,2, 6,3,9, 4,5,8, 1,NA,7, 1,8,NA, NA,NA,2, 3,NA,5, 7,2,5, NA,1,NA, 6,NA,4, NA,4,6, 3,NA,9, NA,NA,NA, 2,NA,NA, NA,NA,NA, NA,NA,NA, NA,NA,NA, 7,NA,NA, NA,NA,NA)
	eg.mat <- matrix(eg.data,nrow=SQUARE.SIZE*SQUARE.NUM, ncol=SQUARE.SIZE*SQUARE.NUM)
	solve.sudoku(eg.mat)
}

#eg.data2 <- c(2,NA,8, 5,9,NA, NA,NA,NA, NA,NA,NA, NA,NA,NA, NA,NA,NA, 6,7,1, NA,NA,2, NA,NA,NA, 8,9,2, NA,3,4, NA,NA,7, 7,1,3, NA,NA,NA, NA,2,NA, 5,NA,4, NA,NA,7, NA,1,8, 4,3,5, NA,NA,NA, NA,NA,2, 9,8,6, NA,NA,1, NA,NA,NA, NA,2,7, NA,8,NA, NA,5,NA)
#eg.mat2 <- matrix(eg.data2,nrow=9)
#print.sudoku(eg.mat2)
#solve.sudoku(eg.mat2)
#
#eg.data3 <- c(9,8,5, 6,7,NA, NA,3,NA, 1,3,6, NA,4,NA, NA,8,NA, NA,4,7, NA,5,NA, NA,1,NA, 3,1,8, NA,6,NA, NA,NA,NA, 4,NA,NA, 3,NA,7, NA,NA,NA, NA,NA,NA, NA,8,NA, NA,NA,NA, 8,2,3, 9,NA,NA, NA,NA,7, 6,5,4, NA,NA,NA, NA,NA,NA, 7,NA,1, NA,3,NA, 4,NA,NA)
#eg.mat3 <- matrix(eg.data3,nrow=9)
#solve.sudoku(eg.mat3)

