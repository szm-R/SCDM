#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Created on: March 10, 2023
# Description: This code compares the results of two iterations of an experiment

# Clear the environment
rm(list=ls())

# Specify the rule ratio for LGPP-CM experiments
PR = 0.75

ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData/LGPP-CM", sep = "/")

# Specify the presence probability for the PP experiments
# PPv = 0.65
# 
# ParentDir = "Output"
# OutputPath = paste(ParentDir, "SimulationData/PP", sep = "/")



# The target design points of the initial iteration (LGPP-CM experiments)
Ref_DP_Idx = c(1,2,3,4,5,6,7,8)
# Ref_DP_Idx = c(2,4,6,8)

# The target design points of the final iteration (LGPP-CM experiments)
New_DP_Idx = c(1,2,3,4,5,6,7,8)
# New_DP_Idx = c(3,4,5,6,7,8)

# The target design points of the initial iteration (PP experiments)
# Ref_DP_Idx = c(1,2,3,4)

# The target design points of the final iteration (PP experiments)
# New_DP_Idx = c(1,2,3,4)
# New_DP_Idx = c(2,4)

# Iteration indices to be compared
Ref_Iteration = 2
New_Iteration = 3





library(readxl)
library(xlsx)


# FileName = paste("RM_PP", PPv, sep = "=")
FileName = paste("RM_Rule", PR, sep = "=")
FilePath = paste(OutputPath, paste(FileName, "xlsx", sep = "."), sep = "/")

SheetName = paste("Iteration", Ref_Iteration)
Ref_ResponseMatrix = read_excel(FilePath, sheet = SheetName, col_names = FALSE)

SheetName = paste("Iteration", New_Iteration)
New_ResponseMatrix = read_excel(FilePath, sheet = SheetName, col_names = FALSE)

# Aggregate the results of target design points into a single sample
Ref_Sample = as.numeric(Ref_ResponseMatrix[Ref_DP_Idx[1],])
for (i in 2:length(Ref_DP_Idx))
{
    Ref_Sample = append(Ref_Sample, as.numeric(Ref_ResponseMatrix[Ref_DP_Idx[i],]))
}

# Aggregate the results of target design points into a single sample
New_Sample = as.numeric(New_ResponseMatrix[New_DP_Idx[1],])
for (i in 2:length(New_DP_Idx))
{
    New_Sample = append(New_Sample, as.numeric(New_ResponseMatrix[New_DP_Idx[i],]))
}

# Compare the new iteration with the reference one using Welch's t-test
welch.test <- t.test(Ref_Sample, New_Sample)

print(welch.test)
















