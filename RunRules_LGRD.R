#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code runs design points with different combinations for
#              the LGPP-CM experiments.

# Clear the environment
rm(list=ls())

debugSource("Run_Experiment.R")

# Iteration code
IterationNum = "LPA"

SheetName = "LGPP=0.1-CM=FC"

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
OutputPath = paste(ParentDir, "SimulationData/LGPP-CM", sep = "/")

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

ResponseMatrix = matrix(0, nrow = nrow(DesignMatrix_Value), 
                        ncol = Num_Runs)

# Loop over the design points (DPs)
for (DP in 1:nrow(DesignMatrix_Value))
{
    PR = Parental_Rules[DP]
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










