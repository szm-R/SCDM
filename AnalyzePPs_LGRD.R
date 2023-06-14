#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code summarizes the results of the code RunPPs_LGRD.R


# The data is the result of running the code RunPPs_LGRD.R

# Clear the environment
rm(list=ls())

PR = 0.125
IterationNum = "H"
SheetName = paste("Rule", PR, sep = '=')


ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData/PP", sep = "/")

library(readxl)
library(xlsx)

FilePath = paste(OutputPath, paste("All_PBCs", "xlsx", sep = "."), sep = "/")
PBCs = read_excel(FilePath, sheet = SheetName)

DesignMatrix_Value = PBCs[,1:5]

New_Combination = c(10.0, "PC_P_NC_A", 1920)
DesignMatrix_Value[,3:5] = do.call("rbind", replicate(length(PBCs$PP),
                                                New_Combination,
                                                simplify = FALSE))

FilePath = paste(OutputPath, paste("RM_LGRD", "xlsx", sep = "."), sep = "/")
ResponseMatrix = read_excel(FilePath, col_names = FALSE, 
                            sheet = paste(SheetName, IterationNum, sep = '--'))


#############################################################################
#############                                                   #############   
#############    Calculating and Saving the Response Summery    #############
#############                                                   #############   
#############################################################################

Response_Summery = as.data.frame(matrix(0, nrow = nrow(DesignMatrix_Value),
                                        ncol = 10))
colnames(Response_Summery) = c("PR" ,"PP", "LGPP", "SGLW", "CM", "SD", 
                               "LGRD", "StD", "SE", "ME")

alpha = 0.05
degrees_of_freedom = ncol(ResponseMatrix)
t_score = qt(p = alpha/2, df = degrees_of_freedom,lower.tail = F)

for (i in 1:nrow(DesignMatrix_Value))
{
    Mean = mean(as.numeric(ResponseMatrix[i,]))
    SD = sd(as.numeric(ResponseMatrix[i,]))
    SE = SD/sqrt(ncol(ResponseMatrix))
    
    Margin_of_Error = t_score*SE
    
    Response_Summery[i,1] = PR
    Response_Summery[i,2:6] = DesignMatrix_Value[i,]
    Response_Summery[i,7] = Mean
    Response_Summery[i,8] = SD
    Response_Summery[i,9] = SE
    Response_Summery[i,10] = Margin_of_Error
}


FilePath = paste(OutputPath, paste("RS_LGRD", "xlsx", sep = "."), sep = "/")
write.xlsx(Response_Summery, file = FilePath,  
           sheetName = paste(SheetName, IterationNum, sep = '--'), 
           row.names = FALSE, append = TRUE)











