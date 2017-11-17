data <- read.csv("~/Code/Project/dataForClassifierSN.csv")
data <- data[!colnames(data) %in% "X"]

dataCR <- data[data$category == "crisis", c("NN", "fpsp", "NNP", "spp", "CC", "NNS") ]
dataNot <- data[(data$category %in% c("red", "amber", "green")), c("NN", "fpsp", "NNP", "spp", "CC", "NNS")]
dataSN <- data[data$category == "suicide note", c("NN", "fpsp", "NNP", "spp", "CC", "NNS")]

#Values in question : fpsp + NN + CC + NNP + NNS
pValuesCR <- apply(dataCR, 2,
                   function(x) {shapiro.test(x)$p.value})
                                        #The above values are not normally distributed
pValuesNot <- apply(dataNot, 2,
                   function(x) {shapiro.test(x)$p.value})
                                        #The above values are not normally distributed
pValuesSN <- apply(dataSN, 2,
                   function(x) {shapiro.test(x)$p.value})
                                        #the above values are not normally distributed
numCol <- ncol(dataCR)
pValuesWilcoxCR = c(1:numCol)
pValuesWilcoxSN = c(1:numCol)
pValuesWilcoxBoth = c(1:numCol);
for(i in c(1:numCol)) {
    pValuesWilcoxCR[i] <- wilcox.test(dataCR[,i], dataNot[,i])$p.value
    pValuesWilcoxSN[i] <- wilcox.test(dataSN[,i], dataNot[,i])$p.value
    pValuesWilcoxBoth[i] <- wilcox.test(dataSN[,i], dataCR[,i])$p.value
}
names <- colnames(dataSN)
names(pValuesWilcoxCR) <- names
names(pValuesWilcoxSN) <- names
names(pValuesWilcoxBoth) <- names

                                        # The distributions are very far from equal and the NN count of SN vs Not has an insane pValue

