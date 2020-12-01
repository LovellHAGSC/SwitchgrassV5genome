library(data.table)
library(caret)
library(e1071)
load("cscData4github.rda")
uplandTrainSet <- c(
  "J045.C","J665.A","J665.B","J665.C","J040.A","J257.A","J258.A","J267.B",
  "J267.C","J368.A","J368.B","J368.C","J371.A","J374.B","J376.A","J376.C",
  "J378.A","J378.B","J379.A","J379.B","J380.B","J382.A","J403.A","J403.B",
  "J409.B","J420.B","J582.C")
coastalTrainSet <- c(
  "J418.A","J418.B","J020.B","J020.C","J456.A","J456.C","J458.A","J458.B",
  "J458.C","J576.A","J576.B","J576.C","J584.A","J584.B","J584.C","J585.A",
  "J585.B","J585.C","J586.A","J586.B","J586.C","J602.B","J602.C","J603.A",
  "J603.B","J603.C","J609.A","J609.B","J609.C")
lowlandTrainSet <- c(
  "J441.A","J441.B","J243.A","J022.A","J022.B","J022.C","J022.D","J210.A",
  "J212.A","J216.A","J218.A","J222.A","J226.A","J229.A","J230.A","J235.A",
  "J237.A","J238.A","J245.A","J246.A","J247.A","J251.A","J251.B","J251.C",
  "J280.A","J318.A","J660.A","J660.B","J660.C")
load("cscData4github.rda")

cscUpland <- csc[uplandTrainSet,]
cscLowland <- csc[lowlandTrainSet,]
cscCoastal <- csc[coastalTrainSet,]
cscTrain <- rbind(cscUpland, cscLowland, cscCoastal)
cscID <- factor(c(rep("Upland",length(uplandTrainSet)),
                   rep("Lowland", length(lowlandTrainSet)),
                   rep("Coastal", length(coastalTrainSet))),
                 levels = c("Upland","Lowland", "Coastal"))

trained <- train(
  x = cscTrain,
  y = cscID,
  method = "nnet")

summary(pred <- predict.train(trained, newdata = csc, type = "prob"))
table(call <- predict(trained, newdata = csc))

tmp <- data.table(call = as.character(call), csc)
ldout <- MASS::lda(x = csc, grouping = call)
ldpred <- predict(ldout)
ldpred <- data.table(eco.grp = call,
                     PLANT_ID = rownames(ldpred$x),
                     ld.axis1 = ldpred$x)
ggord::ggord(ldout, call,vec_ext = 5, repel = T,
             coord_fix = T, size = 2,
             cols = c("blue3","red","orange"),
             ellipse_pro =.9, alpha_el = .25)

