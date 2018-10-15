#setwd("C:/Users/tw299/git/spc-algorithm/Visualisation")
#source("SPC_Visualise.R")

run_recalculation_alg <- function(data_path = file.path("~","code","eclipse-workspace","spc-algorithm","SPCalgorithm1","data"),
                                  alg_path = file.path("~","code","eclipse-workspace","spc-algorithm"),
                                  deleteFilesAfterUse = TRUE, log_file_subdir = "Visualisation",
                                  periodMin = "20", runRuleLength = "8", forceRecalc = "false",
                                  mask = "^.*_OUT.csv$") {
  alg_params <- paste0(periodMin, "-", runRuleLength, "-", forceRecalc)
  starting_wd <- getwd()
  setwd(alg_path)
  log_file_name <- paste0("SPCIOR",gsub(" ", "-", gsub(":", "", Sys.time())), alg_params, ".txt")
  system(paste("java", "-jar","spcalg.jar",data_path, periodMin,runRuleLength,forceRecalc,">",file.path(log_file_subdir, log_file_name)))
  
  batch_visualise_spc(path = data_path, deleteFilesAfterUse = deleteFilesAfterUse,
                      alg_params = alg_params, mask=mask)
  setwd(starting_wd)
}


spc_analyse_vector <- function(data_vector, alg_path = file.path("~","code","eclipse-workspace","spc-algorithm"),
                               deleteFilesAfterUse = TRUE,
                               periodMin = "20", runRuleLength = "8", forceRecalc = "false") {
  
  temp_dir <- tempdir()
  data_dir <- file.path(temp_dir, "spcio_data")
  dir.create(data_dir, recursive = TRUE)
  data_file <- tempfile("spcio", tmpdir = data_dir, fileext = ".csv")
  write_csv(as.data.frame(data_vector), data_file, col_names = FALSE)

  starting_wd <- getwd()
  setwd(alg_path)
  system(paste("java", "-jar","spcalg.jar", data_dir, periodMin, runRuleLength, forceRecalc))
  
  out_file <- str_replace(data_file, ".csv", "_endresult_OUT.csv")
  data_out <- read_csv(out_file, col_names = FALSE)
  
  if(deleteFilesAfterUse) {
    unlink(data_dir, recursive = TRUE)
  }
  setwd(starting_wd)
  
  colnames(data_out) <- c("X","Mean","AMR")
  data_out <- add_control_limits(list(data_out))[[1]]
  run_rule_length <- as.integer(runRuleLength)
  
  # This not working. Something must be different about way data is passed in?
  data_out <- add_rule_breaks(list(data_out), run_rule_length = run_rule_length)[[1]]
  
  data_out
}

batch_visualise_spc <- function(path = "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data",
                                deleteFilesAfterUse = TRUE, alg_params = "unknown", mask = "^.*_OUT.csv$") {
	# setwd("/Users/Thomas/code/eclipse-workspace/spc-algorithm/Visualisation")
	spc_outputs <- load_spc_analyses(path=path, mask=mask)
	spc_outputs <- spc_outputs[!(sapply(spc_outputs, nrow) == 0)]
	spc_outputs <- add_control_limits(spc_outputs)
	spc_outputs <- add_rule_breaks(spc_outputs)
	pdf_charts(spc_outputs, alg_params = alg_params)
	if(deleteFilesAfterUse) {delete_analysis_files(path = path)}
}

#DONE: Add timestamp to output filename
#DONE: Consolidate the plot_chart and pdf_chart functions into one
#DONE: Explore options for aspect ratio of plots
#DONE: Sort out the scale on the vertical axis
#DONE: Better solution when error in algorithm output - ignore infinite values
#DONE: Label the axes
#DONE: Label the charts


load_spc_analyses <- function(path=getwd(), mask = "^.*_OUT.csv$") {
	load_analysis_files(path=path , mask)
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

add_rule_breaks <- function(list.data, run_rule_length = 8) {
	lapply(list.data, function(x) {
						x[,"rule1"] <- (x[,"X"] > x[,"UCL"]) | (x[,"X"] < x[,"LCL"])
						x <- rule_two(x, run_rule_length = run_rule_length)
						as.data.frame(x)
						})
}

rule_two <- function(df, run_rule_length = 8) {
	
	runs <- rle(ifelse(df$X > df$Mean,1,-1))
	rulebreakingruns <- runs$lengths >= run_rule_length
	runs$values <- rulebreakingruns
	partofrun <- inverse.rle(runs)
	df$rule2 <- partofrun
	df

}

plot_chart <- function(x) {

	# Define a vector for the horizontal axis values
	t <- c(1:nrow(x))

	# Get the desired vertical axis range
	vr <- get_v_axis_range(x)
	
	# Make vector of marker types
	mark <- ifelse(x$rule1, 1, ifelse(x$rule2, 0, 19))
	
	# Plot the chart, ensuring an infinite axis range is not passed.
	par(pch=19, col="black")
	if (all(is.finite(vr))) {
		plot(t, x[,"X"], type="n", ylim = vr, xlab='', ylab='')
		}
	else plot(t, x[,"X"], type="n", ylab='', xlab='')
	title(xlab="i", ylab="X_i", main=attributes(x)$title)
	lines(t, x[,"X"], type="o", pch=mark)
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

pdf_charts <- function(list.data, out_subdir = "Visualisation", alg_params = "unknown") {
	timestamp <- gsub(":","-",paste(strsplit(x = toString(Sys.time()),split = " ")[[1]], collapse="-"))
	filename <- file.path(out_subdir,paste(c("spc_alg_out_",timestamp,"_",alg_params,".pdf"),collapse=""))
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

delete_analysis_files <- function(path = "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data",
                                  mask = "^.*_OUT.csv$") {
  fl <- list.files(path = path, pattern = mask)
  result <- file.remove(paste0(path,'//',fl))
  all(result)
}