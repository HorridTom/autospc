context("Identify whether there is a rule 2 break within the calculation period in the opposite direction")

#dataset with second rule break pulling the mean down from triggering rule break
test_op_break1_data <- structure(list(x = 1:46, y = c(11.7048146433946, 13.7015899219746, 
                                                      13.8600550563176, 13.0276563697981, 12.0177913157517, 6.90695189817015, 
                                                      8.28496520534488, 10.0404643132317, 11.4010486184309, 15.5204196581934, 
                                                      14.4942652029531, 10.6745140375668, 9.7172568390774, 8.63317314361218, 
                                                      8.09853445344889, 12.3948127369106, 6.83941238248619, 9.96470704209574, 
                                                      14.5021227929878, 9.82146594699818, 14.2433576943448, 15.4715620863261, 
                                                      15.5775218441984, 14.4959088305509, 14.9254559410412, 14.2173461852723, 
                                                      16.4400658945974, 15.4001393893564, 15.2589443658517, 14.8722323270969, 
                                                      14.101913602725, 1.63862215033372, 1.77411983893357, 2.04268143540359, 
                                                      1.34480496327397, 1.55595619377118, 1.99067103854102, 1.21691643839118, 
                                                      2.32327285310003, 1.09943585201501, 1.82506845830302, 1.71372249250121, 
                                                      1.13664630645258, 1.68410744170508, 0.639672719749582, 0.956982224320062
                                                      )), class = "data.frame", row.names = c(NA, -46L))

#dataset with second rule break pulling the mean up from triggering rule break
test_op_break2_data <- structure(list(x = 1:46, y = c(9.78382521179051, 8.67674416991474, 
                                                 14.8400309427738, 13.1975701396338, 11.104704470363, 13.8533404399691, 
                                                 7.80236464908979, 12.0272276532995, 3.81745752687195, 14.8911976898947, 
                                                 11.2102103090995, 6.9808321095897, 11.3026444470039, 5.41389327143386, 
                                                 14.0585118364591, 7.8927763824055, 12.1820003577632, 8.33148682961424, 
                                                 11.2247412918497, 9.38882350085816, 8.30929875278844, 7.36215714926989, 
                                                 7.18955206526752, 7.46610433877209, 7.29128420217884, 6.81871751193003, 
                                                 6.28857042603552, 6.46176437720142, 6.83014739914007, 8.04258656909944, 
                                                 7.42666614805576, 15.451361864058, 15.2197125148514, 14.036886279779, 
                                                 15.3531136945763, 16.0006585464339, 13.9048955914219, 14.070159491359, 
                                                 16.365866259142, 15.8429421322532, 15.8603266570866, 15.2476541475554, 
                                                 14.4092157738341, 14.8036302699465, 15.1526217871599, 14.8246357129192
                                                 )), class = "data.frame", row.names = c(NA, -46L))

#dataset with triggering and second rule break equidistant from mean
test_op_break3_data <- structure(list(x = 1:46, y = c(10.8127747819916, 14.7245185256203, 
                                                 5.15798470463547, 13.8999802606029, 11.6170638278731, 6.91476833505911, 
                                                 9.35818668810697, 11.3872415934781, 10.0345573372698, 12.2390666867135, 
                                                 7.33568253303961, 14.0085320140821, 12.0972830942552, 14.5600555744061, 
                                                 7.59130386576389, 11.8780813597077, 5.21005752986235, 9.26586024689898, 
                                                 8.35540558702968, 10.8487854324203, 11.3083822000099, 4.40679692976458, 
                                                 5.30549145115121, 6.04405758511215, 4.34376045299631, 5.25533540139407, 
                                                 3.7376622926376, 4.2662364577421, 4.78374287604072, 4.40155824350123, 
                                                 5.31305757897473, 15.3671723172176, 15.6912142378425, 16.1329408375569, 
                                                 14.7947435205572, 15.1534852660436, 15.6342738257245, 14.4551734823257, 
                                                 14.7752432648955, 14.263859883923, 16.8554691332253, 12.4625857270553, 
                                                 16.2920559521444, 15.2529352962512, 13.9043365502238, 15.0933943126417
                                                 )), class = "data.frame", row.names = c(NA, -46L))

#normal rule break with expected recalculation
test_op_break4_data <- structure(list(x = 1:46, y = c(8.42781424302843, 8.98509774532262, 
                                                 13.7968793518498, 5.32633283426313, 12.9871029968435, 13.3705316946239, 
                                                 10.5002448464477, 8.68620916701239, 6.32281734071067, 12.1231011564672, 
                                                 11.1924325449979, 4.64046367175306, 8.72755968947332, 9.91561540194537, 
                                                 7.94014410136788, 10.1835878508561, 12.5872437042144, 5.81118646003203, 
                                                 11.4078004155455, 9.70299409329838, 11.177427009069, 16.0684518554693, 
                                                 14.3539424999831, 14.3399342797403, 15.3023075110731, 14.7666154939391, 
                                                 15.6003898848859, 14.1433675164774, 15.2542672549545, 15.1772523476485, 
                                                 13.9912454558923, 13.1589824053703, 16.0804858263011, 15.0142546144972, 
                                                 15.7442431049004, 12.8072867499788, 15.403262340811, 14.0752408714145, 
                                                 15.4277562368153, 14.3667285604615, 16.1074145352097, 17.25155048873, 
                                                 13.747006211145, 15.6566388744033, 14.8639806483946, 14.6536121204943
                                                 )), class = "data.frame", row.names = c(NA, -46L))

#rule break with further rule break in direction of triggering rule break - expected recalc
test_op_break5_data <- structure(list(x = 1:46, y = c(10.5893889940781, 6.96464109871119, 
                                                 8.11443505843394, 16.0148985308133, 12.3171757000796, 9.37386983822629, 
                                                 6.36724465741905, 9.22768036677708, 6.83849066571604, 12.1665053632313, 
                                                 14.0625939252399, 10.1459576772377, 7.46719505552844, 11.8500506514577, 
                                                 13.8334193285756, 13.7251720243227, 9.37235907316399, 13.2674819218584, 
                                                 13.6834009908637, 3.48767042670435, 4.22165846062775, 14.5320354773048, 
                                                 15.6211515942404, 13.6010643828956, 13.9093162881802, 15.8028435935165, 
                                                 12.7009849054812, 15.289220343267, 13.0475068862217, 14.418122593191, 
                                                 15.5209975468921, 18.5001703298963, 18.3981173774474, 17.1546064077935, 
                                                 18.8227248412977, 19.6832809760349, 18.5729541633448, 17.5726156505484, 
                                                 18.6407979103318, 17.8401509412689, 17.1170446477259, 17.4614536057614, 
                                                 18.2723273997918, 20.1257230847694, 17.1981870784102, 18.7753797373298
                                                 )), class = "data.frame", row.names = c(NA, -46L))

#opposite rule break commencing after the end of the candidate calculation period
test_op_break6_data <- readRDS("testdata/test_oppositeRuleBreak_later.rds")

testthat::test_that("Rule 2 break within candidate period in opposite direction identified correctly",{
  
  #should not recalc due to break in op direction
  test_op_break1 <- plot_auto_SPC(test_op_break1_data, plotChart = F)
  test_op_break1_break_pos <- which(test_op_break1$breakPoint == TRUE)
  
  testthat::expect_equal(test_op_break1_break_pos, integer(0))
  
  #should not recalc due to break in op direction
  test_op_break2 <- plot_auto_SPC(test_op_break2_data, plotChart = F)
  test_op_break2_break_pos <- which(test_op_break2$breakPoint == TRUE)
  
  testthat::expect_equal(test_op_break2_break_pos, integer(0))
  
  #should not recalc due to break in op direction
  test_op_break3 <- plot_auto_SPC(test_op_break3_data, plotChart = F)
  test_op_break3_break_pos <- which(test_op_break3$breakPoint == TRUE)
  
  testthat::expect_equal(test_op_break3_break_pos, integer(0))
  
  #should recalc
  test_op_break4 <- plot_auto_SPC(test_op_break4_data, plotChart = F)
  test_op_break4_break_pos <- which(test_op_break4$breakPoint == TRUE)
  
  testthat::expect_equal(test_op_break4_break_pos, 22)
  
  #should recalc
  test_op_break5 <- plot_auto_SPC(test_op_break5_data, plotChart = F)
  test_op_break5_break_pos <- which(test_op_break5$breakPoint == TRUE)
  
  testthat::expect_equal(test_op_break5_break_pos, 22)
  
})


testthat::test_that("Opposite rule break after candidate calc period doesn't prevent recalculation",{
  #should recalc
  test_op_break6 <- plot_auto_SPC(test_op_break6_data,
                                  noRegrets = TRUE,
                                  plotChart = FALSE)
  
  test_op_break6_break_pos <- which(test_op_break6$breakPoint == TRUE)
  
  testthat::expect_equal(test_op_break6_break_pos, 31L)
})