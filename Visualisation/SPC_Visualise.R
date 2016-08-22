#setwd("C:/Users/tw299/git/spc-algorithm/Visualisation")
#source("SPC_Visualise.R")

batch_visualise_spc <- function(path = "C:/Users/tw299/git/spc-algorithm/SPCalgorithm1/data") {
	setwd("C:/Users/tw299/git/spc-algorithm/Visualisation")
	spc_outputs <- load_spc_analyses(path=path)
	spc_outputs <- add_control_limits(spc_outputs)
	pdf_charts(spc_outputs)
}

#DONE: Add timestamp to output filename
#DONE: Consolidate the plot_chart and pdf_chart functions into one
#DONE: Explore options for aspect ratio of plots
#DONE: Sort out the scale on the vertical axis
#DONE: Better solution when error in algorithm output - ignore infinite values
#DONE: Label the axes
#DONE: Label the charts


load_spc_analyses <- function(path=getwd()) {
	load_analysis_files(path=path , mask = "^.*_OUT.csv$")
}

load_analysis_files <- function(path = getwd(), mask) {
	#Load all files in 'path' with filename matching regex 'mask'
	current.path <- getwd()
	setwd(path)
	fl <- list.files(path = path, pattern = mask)
	nonempty <- !(file.info(fl)$size == 0)
	fl <- fl[nonempty]
	
	files <- lapply(fl, function(x) read.csv(x, header = FALSE))
	setwd(current.path)

	files <- lapply(files, function(x) {
							colnames(x) <- c("X","Mean","AMR")
							x
							})
	for (i in 1:length(fl)) {
		attr(files[[i]],"title")=fl[[i]]
	}

	files <- lapply(files, remove_nulls_df)
	
	files
}

add_control_limits <- function(list.data) {
	lapply(list.data, function(x) {
						x[,"LCL"] <- x[,"Mean"] - 2.66*x[,"AMR"]
						x[,"UCL"] <- x[,"Mean"] + 2.66*x[,"AMR"]
						x
						})
}

plot_chart <- function(x) {

	# Define a vector for the horizontal axis values
	t <- c(1:nrow(x))

	# Get the desired vertical axis range
	vr <- get_v_axis_range(x)

	# Plot the chart, ensuring an infinite axis range is not passed.
	par(pch=19, col="black")
	if (all(is.finite(vr))) {
		plot(t, x[,"X"], type="n", ylim = vr, xlab='', ylab='')
		}
	else plot(t, x[,"X"], type="n", ylab='', xlab='')
	title(xlab="i", ylab="X_i", main=attributes(x)$title)
	lines(t, x[,"X"], type="o")
	lines(t, x[,"Mean"], type="l", lty=1)
	lines(t, x[,"LCL"], type="l", lty=2)
	lines(t, x[,"UCL"], type="l", lty=2)
	
}

plot_charts <- function(list.data) {
	lapply(list.data, function(x) {
						dev.new()
						plot_chart(x)
						})
}

pdf_charts <- function(list.data) {
	timestamp <- gsub(":","-",paste(strsplit(x = toString(Sys.time()),split = " ")[[1]], collapse="-"))
	filename <- paste(c("spc_alg_out_",timestamp,".pdf"),collapse="")
	pdf(filename, width = 11.69, height = 8.27)
	lapply(list.data, plot_chart)
	dev.off()
}

get_v_axis_range <- function(x) {

	x <- x[,colnames(x) %in% c("X","Mean","LCL","UCL")]
	chart_min <- min(x[x>-Inf & x<Inf])
	chart_max <- max(x[x>-Inf & x<Inf])
	chart_range <- chart_max - chart_min

	vertical_min <- chart_min - 0.15*chart_range
	vertical_max <- chart_max + 0.15*chart_range

	c(vertical_min,vertical_max)

}

remove_nulls_df <- function(x) {
	title <- attr(x, "title")
	x <- as.data.frame(sapply(x, remove_nulls_col))
	attr(x, "title") = title
	x
}

remove_nulls_col <- function(v) {
	if (is.factor(v)) {
	v <- as.numeric(levels(v))[v]
	v <- v[!is.na(v)] }
	v
}

write_cols_csv <- function(df) {
	sapply(colnames(df), function(x) write.table(df[!is.na(df[,x]),x], file=paste(c(gsub("\\.","_",x),".csv"),collapse=""), sep=",",row.names=FALSE, col.names=FALSE))
}