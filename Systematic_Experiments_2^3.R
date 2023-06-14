#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code runs the 2^3 factorial design, where we try to  
#              find a combination of factors PP, SGLW, and SD that results  
#              in a higher LGRD for different LGPP-CM pairs of each rule. 

# Clear the environment
rm(list=ls())

debugSource("Run_Experiment.R")


# Number of times each design point is repeated
Num_Runs = 100

# The number of current experiment iteration
IterationNum = 1


# Parental Rule
PR = 0.875

# Factor level values
f1 = c(0.2, 0.6)     # PP
f2 = c(0.1)          # LGPP
f3 = c(1.0, 4.0)     # SGLW
f4 = c("FC_P_PC_A")  # CM
f5 = c(1008, 1296)   # SD

# Build the design matrix
DesignMatrix_Value = expand.grid(f1, f2, f3, f4, f5)

# Create the response matrix that would contain the LGRD of 
# each run of the design points
ResponseMatrix = matrix(0, nrow = nrow(DesignMatrix_Value), 
                        ncol = Num_Runs)

## Factor codes
PP = 1
LGPP = 2
SGLW = 3
CM = 4
SD = 5

# Loop over the design points (DPs)
for (DP in 1:nrow(DesignMatrix_Value))
{
    print(paste("Running for Design point", DP))
    
    # Set the reference LG ratio to the LG preference probability (LGPP)
    Ref_LGR = as.numeric(DesignMatrix_Value[DP,LGPP])
    
    # Run each design point for Num_Runs times
    for (Run_Idx in 1:Num_Runs)
    {
        # Run the experiment and get the LG ratio
        LGR = Run_Experiment()
        
        # Save the difference between the experiment's LGR and  
        # the LGPP as the main model response (the LGRD)
        ResponseMatrix[DP, Run_Idx] = LGR - Ref_LGR
    }
}


library(xlsx)
SheetName = paste("Iteration", IterationNum)
OutputPath = "Output/SimulationData/"
FileName = paste("RM_Rule", PR, sep = "=")
FilePath = paste(OutputPath, paste(FileName, "xlsx", sep = "."), sep = "/")
write.xlsx(ResponseMatrix, file = FilePath, sheetName = SheetName,
           append = TRUE, row.names = FALSE, col.names = FALSE)



















