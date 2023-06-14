#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Created on: January 10, 2023
# Description: This code estimates the error variance for the systematic experiments

# Clear the environment
rm(list=ls())

debugSource("Run_Experiment.R")

## Parental Rule
PR = 0.875


## Factor codes
PP = 1
LGPP = 2
SGLW = 3
CM = 4
SD = 5

DesignMatrix_Value = matrix(0, nrow = 4, ncol = 5)
DesignMatrix_Value[1,] = c(0.2, 0.1, 1.0, "PC_P_NC_A", 504)
DesignMatrix_Value[2,] = c(0.5, 0.25, 4.5, "PC_P_NC_A", 768)
DesignMatrix_Value[3,] = c(0.5, 0.25, 4.5, "FC_P_PC_A", 768)
DesignMatrix_Value[4,] = c(0.8, 0.4, 8.0, "FC_P_PC_A", 1008)


Num_Runs = c(100, 200, 300, 400, 600, 800)

ResponseMatrix = as.data.frame(matrix(0, nrow = nrow(DesignMatrix_Value)*3, 
                               ncol = length(Num_Runs)+1))
colnames(ResponseMatrix) = c("Statistics", Num_Runs)



# Loop over the design points (DPs)
for (DP in 1:nrow(DesignMatrix_Value))
{
    print(paste("Running for Design point", DP))
    
    # Loop over run numbers
    for (NR_Idx in 1:length(Num_Runs))
    {
        Run_Response = replicate(Num_Runs[NR_Idx], 0)
        print(paste("   Number of runs:", Num_Runs[NR_Idx]))
        
        for (Run_Idx in 1:Num_Runs[NR_Idx])
        {
            Run_Response[Run_Idx] = Run_Experiment()
        }
        
        ResponseMatrix[(DP-1)*3 + 1, 1] = paste(DP, "Mean", sep = "-")
        ResponseMatrix[(DP-1)*3 + 1, NR_Idx + 1] = mean(Run_Response)
        
        ResponseMatrix[(DP-1)*3 + 2, 1] = paste(DP, "SD", sep = "-")
        ResponseMatrix[(DP-1)*3 + 2, NR_Idx + 1] = sd(Run_Response)
        
        # The estimation of the error variance is based on the 
        # Coefficient of Variance or CV
        ResponseMatrix[(DP-1)*3 + 3, 1] = paste(DP, "CV", sep = "-")
        ResponseMatrix[(DP-1)*3 + 3, NR_Idx + 1] = sd(Run_Response)/mean(Run_Response)
    }
    
}


library(xlsx)

SheetName = paste("Rule", PR, sep = "-")
OutputPath = "Output/SimulationData"
FilePath = paste(OutputPath, "EV_Matrix.xlsx", sep = "/")
write.xlsx(ResponseMatrix, file = FilePath, sheetName = SheetName,
           append = TRUE, row.names = FALSE)












