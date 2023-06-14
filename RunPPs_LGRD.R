#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code runs design points with different combinations for
#              the PP experiments.

# Clear the environment
rm(list=ls())

debugSource("Run_Experiment.R")

# Parental rule
PR = 0.125

# Iteration code
IterationNum = "H"

# Number of times each design point is repeated
Num_Runs = 100

## Factor codes
PP = 1
LGPP = 2
SGLW = 3
CM = 4
SD = 5

library(readxl)

ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData/PP", sep = "/")

SheetName = paste("Rule", PR, sep = '=')
FilePath = paste(OutputPath, paste("All_PBCs", "xlsx", sep = "."), sep = "/")
PBCs = read_excel(FilePath, sheet = SheetName)

DesignMatrix_Value = PBCs[,1:5]

New_Combination = c(10.0, "PC_P_NC_A", 1920)
DesignMatrix_Value[,3:5] = do.call("rbind", replicate(length(PBCs$PP),
                                                      New_Combination,
                                                      simplify = FALSE))

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

FilePath = paste(OutputPath, paste("RM_LGRD", "xlsx", sep = "."), sep = "/")
write.xlsx(ResponseMatrix, file = FilePath, 
           sheetName = paste(SheetName, IterationNum, sep = '--'),
           row.names = FALSE, col.names = FALSE, append = TRUE)










