#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code summarizes the results of the code RunRules_LGRD.R


# The data is the result of running the code RunRules_LGRD.R

# Clear the environment
rm(list=ls())

# Iteration code
IterationNum = "LPA"


library(readxl)
library(xlsx)

ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData/LGPP-CM", sep = "/")


SheetName = "LGPP=0.1-CM=FC"
FilePath = paste(OutputPath, paste("All_PBCs", "xlsx", sep = "."), sep = "/")
PBCs = read_excel(FilePath, sheet = SheetName)

Parental_Rules = PBCs$Rule

# Use each rule's specific PBC
DesignMatrix_Value = PBCs[,2:6]

# Use a single combination for all rules
# DesignMatrix_Value = c(0.05, 0.4, 10.0, "FC_P_PC_A", 1920)
# DesignMatrix_Value = do.call("rbind", replicate(length(Parental_Rules),
#                                                 DesignMatrix_Value,
#                                                 simplify = FALSE))

FilePath = paste(OutputPath, paste("RM_LGRD", "xlsx", sep = "."), sep = "/")
ResponseMatrix = read_excel(FilePath, col_names = FALSE, 
                            sheet = paste(SheetName, IterationNum, sep = '--'))


#######################################################################################
#############                                                             #############   
#############           Calculating and Saving Response Summery           #############
#############                                                             #############   
#######################################################################################

Response_Summery = as.data.frame(matrix(0, nrow = length(Parental_Rules),
                                        ncol = 10))
colnames(Response_Summery) = c("PR" ,"PP", "LGPP", "SGLW", "CM", "SD", 
                               "LGRD", "StD", "SE", "ME")

alpha = 0.05
degrees_of_freedom = ncol(ResponseMatrix)
t_score = qt(p = alpha/2, df = degrees_of_freedom,lower.tail = F)

for (i in 1:length(Parental_Rules))
{
    Mean = mean(as.numeric(ResponseMatrix[i,]))
    SD = sd(as.numeric(ResponseMatrix[i,]))
    SE = SD/sqrt(ncol(ResponseMatrix))
    
    Margin_of_Error = t_score*SE
    
    Response_Summery[i,1] = Parental_Rules[i]
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











