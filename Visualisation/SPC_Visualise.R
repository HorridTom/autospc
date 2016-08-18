setwd("C:/Users/tw299/git/spc-algorithm/Visualisation")
#source("SPC_Visualise.R")
#spc_outputs <- load_spc_analyses()
#spc_outputs <- add_control_limits(spc_outputs)
#plot_charts(spc_outputs)


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

	dev.new()
	par(pch=19, col="black")
	plot(t, x[,"X"], type="n")
	lines(t, x[,"X"], type="o")
	lines(t, x[,"Mean"], type="l", lty=1)
	lines(t, x[,"LCL"], type="l", lty=2)
	lines(t, x[,"UCL"], type="l", lty=2)
	
}

plot_charts <- function(list.data) {
	lapply(list.data, plot_chart)
}