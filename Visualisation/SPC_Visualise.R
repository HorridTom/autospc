setwd("C:/Users/tw299/git/spc-algorithm/Visualisation")
#source("SPC_Visualise.R")
#spc_outputs <- load_spc_analyses()
#spc_outputs <- add_control_limits(spc_outputs)
#plot_charts(spc_outputs)

#DONE: Add timestamp to output filename
#DONE: Consolidate the plot_chart and pdf_chart functions into one
#DONE: Explore options for aspect ratio of plots
#TODO: Sort out the scale on the vertical axis
#TODO: Label the axes
#TODO: Label the charts


load_spc_analyses <- function() {
	load_analysis_files(path="C:/Users/tw299/git/spc-algorithm/SPCalgorithm1/data" , mask = "^.*_OUT.csv$")
}

load_analysis_files <- function(path = getwd(), mask) {
	#Load all files in 'path' with filename matching regex 'mask'
	
	fl <- list.files(path = path, pattern = mask)
	current.path <- getwd()
	setwd(path)
	files <- lapply(fl, function(x) read.csv(x, header = FALSE))
	setwd(current.path)

	files <- lapply(files, function(x) {
							colnames(x) <- c("X","Mean","AMR")
							x
							})

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

	t <- c(1:nrow(x))
	vr <- get_v_axis_range(x)
	par(pch=19, col="black")
	if (all(is.finite(vr))) {
		plot(t, x[,"X"], type="n", ylim = vr)
		}
	else plot(t, x[,"X"], type="n")
	lines(t, x[,"X"], type="o")
	lines(t, x[,"Mean"], type="l", lty=1)
	lines(t, x[,"LCL"], type="l", lty=2)
	lines(t, x[,"UCL"], type="l", lty=2)
	
}

plot_charts <- function(list.data) {
	lapply(list.data, function(x) {
						dev.new()
						plot_chart
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

	chart_min <- min(x[,colnames(x) %in% c("X","Mean","LCL","UCL")])
	chart_max <- max(x[,colnames(x) %in% c("X","Mean","LCL","UCL")])
	chart_range <- chart_max - chart_min

	vertical_min <- chart_min - 0.15*chart_range
	vertical_max <- chart_max + 0.15*chart_range

	c(vertical_min,vertical_max)

}