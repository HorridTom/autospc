write_test_csvs <- function(test_data_list,
                            path = "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data") {
  sapply(seq_along(test_data_list), function(i) write.table(test_data_list[[i]], file=paste(path,"/test_data_",i ,".csv",collapse="",sep = ""), sep=",",row.names=FALSE, col.names=FALSE))
}


test_data <- list()
# First some sequences from a standard normal, of lengths
# below, at, and above the cutoffs for
# i) enough data to form limits
# ii) enough data for potentially more than one period
test_data[[1]] <- rnorm(19)
test_data[[2]] <- rnorm(20)
test_data[[3]] <- rnorm(21)
test_data[[4]] <- rnorm(39)
test_data[[5]] <- rnorm(40)
test_data[[6]] <- rnorm(41)

# Now some data to check recalculation in a couple of "obvious" cases.
test_data[[7]] <- c(rnorm(20), rnorm(20, mean = 1, sd = 1))
test_data[[8]] <- c(rnorm(30), rnorm(20, mean = -2, sd = 1))


# Next data constructed to trigger a particular branch of the algorithm,
# or to expose a particular feature/issue.
test_data[[9]] <- test_data[[7]]
org_val <- test_data[[9]][34]
test_data[[9]][34] <- -0.1
test_data[[9]][40] <- test_data[[9]][40] - (test_data[[9]][34] - org_val)
write.table(test_data[[9]], file=paste(
  "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data","/test_data_",9 ,".csv",collapse="",sep = ""),
  sep=",",row.names=FALSE, col.names=FALSE)

test_data[[10]] <- test_data[[7]]
org_val <- test_data[[10]][34]
test_data[[10]][34] <- 0.1
test_data[[10]][40] <- test_data[[10]][40] - (test_data[[10]][34] - org_val)
write.table(test_data[[10]], file=paste(
  "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data","/test_data_",10 ,".csv",collapse="",sep = ""),
  sep=",",row.names=FALSE, col.names=FALSE)

test_data[[11]] <- test_data[[7]]
org_val_1 <- test_data[[11]][34]
org_val_2 <- test_data[[11]][35]
test_data[[11]][34] <- 0.1
test_data[[11]][35] <- 0.2
test_data[[11]][40] <- test_data[[11]][40] - (test_data[[11]][34] - org_val_1) - (test_data[[11]][35] - org_val_2)
write.table(test_data[[11]], file=paste(
  "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data","/test_data_",11 ,".csv",collapse="",sep = ""),
  sep=",",row.names=FALSE, col.names=FALSE)

test_data[[12]] <- test_data[[10]]
test_data[[12]][41:70] <- rnorm(30, mean = 1, sd = 1)
write.table(test_data[[12]], file=paste(
  "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data","/test_data_",12 ,".csv",collapse="",sep = ""),
  sep=",",row.names=FALSE, col.names=FALSE)

test_data[[13]] <- test_data[[10]]
test_data[[13]][41:70] <- rnorm(30, mean = 0, sd = 1)
write.table(test_data[[13]], file=paste(
  "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data","/test_data_",13 ,".csv",collapse="",sep = ""),
  sep=",",row.names=FALSE, col.names=FALSE)

test_data[[14]] <- c(rnorm(25), rnorm(14,1,1), rnorm(13,2,1), rnorm(10,3,1), rnorm(5,4,1))
write.table(test_data[[14]], file=paste(
  "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data","/test_data_",14 ,".csv",collapse="",sep = ""),
  sep=",",row.names=FALSE, col.names=FALSE)

test_data[[15]] <- c(rnorm(25), rnorm(14,0.5,0.125), rnorm(25,2,1))
write.table(test_data[[15]], file=paste(
  "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data","/test_data_",15 ,".csv",collapse="",sep = ""),
  sep=",",row.names=FALSE, col.names=FALSE)

test_data[[16]] <- c(test_data[[15]], rnorm(5,2,1))
write.table(test_data[[16]], file=paste(
  "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data","/test_data_",16 ,".csv",collapse="",sep = ""),
  sep=",",row.names=FALSE, col.names=FALSE)

test_data[[17]] <- test_data[[15]][1:45]
write.table(test_data[[17]], file=paste(
  "/Users/Thomas/code/eclipse-workspace/spc-algorithm/SPCalgorithm1/data","/test_data_",17 ,".csv",collapse="",sep = ""),
  sep=",",row.names=FALSE, col.names=FALSE)


