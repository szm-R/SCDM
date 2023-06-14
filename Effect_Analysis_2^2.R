#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Created on: January 7, 2023
# Description: This code computes the effect matrix of the systematic experiment


# The data is the result of running the code Systematic_Experiments_2^2.R 


# Clear the environment
rm(list=ls())

# The number of current experiment iteration
IterationNum = 1

# Parental Rule
PR = 0.875

# Presence probability
PPv = 0.05

# Factor level values
f1 = c(PPv)          # PP
f2 = c(0.1)          # LGPP
f3 = c(1.0, 4.0)     # SGLW
f4 = c("FC_P_PC_A")  # CM
f5 = c(1008, 1296)   # SD


# Build the design matrix
DesignMatrix_Value <<- expand.grid(f1, f2, f3, f4, f5)


library(readxl)
library(xlsx)


ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData", sep = "/")

SheetName = paste("Iteration", IterationNum)
FileName = paste("RM_PP", PPv, sep = "=")
FilePath = paste(OutputPath, paste(FileName, "xlsx", sep = "."), sep = "/")
ResponseMatrix = read_excel(FilePath, sheet = SheetName, col_names = FALSE)


#############################################################################
#############                                                   #############   
#############    Calculating and Saving the Response Summery    #############
#############                                                   #############   
#############################################################################

Response_Summery = as.data.frame(matrix(0, nrow = nrow(DesignMatrix_Value),
                                        ncol = 9))
colnames(Response_Summery) = c("PP", "LGPP", "SGLW", "CM", "SD", 
                               "Mean", "SD", "95-LB", "95-UB")

alpha = 0.05
degrees_of_freedom = ncol(ResponseMatrix)
t_score = qt(p = alpha/2, df = degrees_of_freedom,lower.tail = F)

for (i in 1:nrow(DesignMatrix_Value))
{
    Mean = mean(as.numeric(ResponseMatrix[i,]))
    SD = sd(as.numeric(ResponseMatrix[i,]))
    SE = SD/sqrt(ncol(ResponseMatrix))
    
    Margin_of_Error = t_score*SE
    Lower_Bound = Mean - Margin_of_Error
    Upper_Bound = Mean + Margin_of_Error
    
    Response_Summery[i,1:5] = DesignMatrix_Value[i,]
    Response_Summery[i,6] = Mean
    Response_Summery[i,7] = SD
    Response_Summery[i,8] = Lower_Bound
    Response_Summery[i,9] = Upper_Bound
}


SheetName = paste("Iteration", IterationNum)
FileName = paste("RS_PP", PPv, sep = "=")
FilePath = paste(OutputPath, paste(FileName, "xlsx", sep = "."), sep = "/")
write.xlsx(Response_Summery, file = FilePath, sheetName = SheetName,
           append = TRUE, row.names = FALSE)


#############################################################################
#############                                                   #############   
#############      Calculating and Saving the Effect Matrix     #############
#############                                                   #############   
#############################################################################

# Factor level representation
f1 = c(-1,1)
f2 = c(-1,1)
DesignMatrix_Rep = expand.grid(f1, f2)

Effect_Names = c("SGLW", "SD", "SGLWxSD")

Run_EffectMatrix = matrix(0, nrow = length(Effect_Names), 
                          ncol = ncol(ResponseMatrix))

# Calculate the main effect of each factor for all runs, for the details
# of these equations refer to the Systematic_Experiment.html file
for (Run_Idx in 1:ncol(ResponseMatrix))
{
    for (F_Idx in 1:ncol(DesignMatrix_Rep))
    {
        Effect = 0
        for (DP in 1:nrow(DesignMatrix_Rep))
        {
            ResponseSign = DesignMatrix_Rep[DP, F_Idx]
            ResponseValue = as.numeric(ResponseMatrix[DP, Run_Idx]) 
            Effect = Effect + (ResponseSign*ResponseValue)
        }
        Run_EffectMatrix[F_Idx, Run_Idx] = Effect/2
    }
}


# Calculate the interaction effect of factors for all runs, for the details
# of these equations refer to the Systematic_Experiment.html file
for (Run_Idx in 1:ncol(ResponseMatrix))
{
    Counter = ncol(DesignMatrix_Rep)
    for (i in 1:ncol(DesignMatrix_Rep))
    {
        for (j in 2:ncol(DesignMatrix_Rep))
        {
            if (j <= i)
                next
            
            Effect = 0
            for (DP in 1:nrow(DesignMatrix_Rep))
            {
                ResponseSign = DesignMatrix_Rep[DP, i]*DesignMatrix_Rep[DP, j]
                ResponseValue = as.numeric(ResponseMatrix[DP, Run_Idx]) 
                Effect = Effect + (ResponseSign*ResponseValue)
            }
            Counter = Counter + 1
            Run_EffectMatrix[Counter, Run_Idx] = 0.5*(Effect)
        }
    }
}

# We have 3 effect in total (2 main effects and 1 interactions), therefore, to have
# at least an overall 90% confidence level, we need to consider a confidence level of 
# 96.67% for each of our 3 effects. In other words, we would be using alpha = 0.033
alpha = 0.033
degrees_of_freedom = ncol(ResponseMatrix)
t_score = qt(p = alpha/2, df = degrees_of_freedom,lower.tail = F)

Overall_EffectMatrix = as.data.frame(matrix(0, nrow = length(Effect_Names), ncol = 4))
colnames(Overall_EffectMatrix) = c("Factor", "Mean", "Lower bound", "Upper bound")

for (i in 1:length(Effect_Names))
{
    Mean = mean(Run_EffectMatrix[i,])
    SD = sd(Run_EffectMatrix[i,])
    SE = SD/sqrt(ncol(ResponseMatrix))
    
    Margin_of_Error = t_score*SE
    Lower_Bound = Mean - Margin_of_Error
    Upper_Bound = Mean + Margin_of_Error
    
    Overall_EffectMatrix[i,1] = Effect_Names[i]
    Overall_EffectMatrix[i,2] = Mean
    Overall_EffectMatrix[i,3] = Lower_Bound
    Overall_EffectMatrix[i,4] = Upper_Bound
}

SheetName = paste("Iteration", IterationNum)
FileName = paste("EM_PP", PPv, sep = "=")
FilePath = paste(OutputPath, paste(FileName, "xlsx", sep = "."), sep = "/")
write.xlsx(Overall_EffectMatrix, file = FilePath, sheetName = SheetName,
           append = TRUE, row.names = FALSE)













