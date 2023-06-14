#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code implements the process of systematic experiments 

# Clear the environment
rm(list=ls())

debugSource("Run_Experiment.R")


Num_Runs = 100
IterationNum = 3

## Parental Rule
PR = 0.125
PPv = 0.85

# Factor level values
f1 = c(PPv)
f2 = c(0.1)
f3 = c(3.0, 5.0)
f4 = c("PC_P_NC_A")
f5 = c(1296, 1488)

# Building the design matrix
DesignMatrix_Value = expand.grid(f1, f2, f3, f4, f5)


## Factor codes
PP = 1
LGPP = 2
SGLW = 3
CM = 4
SD = 5


ResponseMatrix = matrix(0, nrow = nrow(DesignMatrix_Value), 
                        ncol = Num_Runs)

# Loop over the design points (DPs)
for (DP in 1:nrow(DesignMatrix_Value))
{
    print(paste("Running for Design point", DP))
    Ref_LGR = as.numeric(DesignMatrix_Value[DP,LGPP])
    for (Run_Idx in 1:Num_Runs)
    {
        LGR = Run_Experiment()
        ResponseMatrix[DP, Run_Idx] = LGR - Ref_LGR
    }
}

library(xlsx)

SheetName = paste("Iteration", IterationNum)
ParentDir = "C:/Users/szmoo/Dropbox/PhD/My Manuscripts/12-AB_SCD_M/Code"
OutputPath = paste(ParentDir, "Output/Systematic Experiments", sep = "/")
FileName = paste("RM_PP", PPv, sep = "=")
FilePath = paste(OutputPath, paste(FileName, "xlsx", sep = "."), sep = "/")
write.xlsx(ResponseMatrix, file = FilePath, sheetName = SheetName,
           append = TRUE, row.names = FALSE, col.names = FALSE)



















