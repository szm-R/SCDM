#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Created on: January 20, 2023
# Description: This code analyzes the interaction effect of two factors


# The data is the result of running the code Systematic_Experiments_2^3.R

# Clear the environment
rm(list=ls())

# The number of current experiment iteration
IterationNum = 1

# Parental rule
PR = 0.75

## Factor codes
PP = 1
SGLW = 2
SD = 3

# We aim to analyze the i x j interaction effect
i = SD
j = PP

# Read the response matrix of the target Rule
library("readxl")

ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData", sep = "/")

SheetName = paste("Iteration", IterationNum)
FileName = paste("RM_Rule", PR, sep = "=")
FilePath = paste(OutputPath, paste(FileName, "xlsx", sep = "."), sep = "/")
ResponseMatrix = read_excel(FilePath, sheet = SheetName, col_names = FALSE)

# Store the average response of each design point
AverageResponse = rowMeans(ResponseMatrix)


# Initializing the responses

# For j at negative level:
# i at negative level 
R_in_jn = 0
# i at positive level
R_ip_jn = 0

# For j at positive level:
# i at negative level
R_in_jp = 0
# i at positive level
R_ip_jp = 0


# Factor level representation
f1 = c(-1,1)
f2 = c(-1,1)
f3 = c(-1,1)
DesignMatrix_Rep = expand.grid(f1, f2, f3)

# Loop over design points
for (DP in 1:nrow(DesignMatrix_Rep))
{
    if (DesignMatrix_Rep[DP,j] == -1)
    {
        # Compute average responses when factor j is at its 
        # negative level
        
        if (DesignMatrix_Rep[DP,i] == -1)
            R_in_jn = R_in_jn + AverageResponse[DP]
        else
            R_ip_jn = R_ip_jn + AverageResponse[DP]
    }
    else
    {
        # Compute average responses when factor j is at its 
        # positive level
        
        if (DesignMatrix_Rep[DP,i] == -1) 
            R_in_jp = R_in_jp + AverageResponse[DP]
        else
            R_ip_jp = R_ip_jp + AverageResponse[DP]
    }
}

R_in_jn = R_in_jn/2
R_ip_jn = R_ip_jn/2

R_in_jp = R_in_jp/2
R_ip_jp = R_ip_jp/2


Interaction_DF = as.data.frame(matrix(0, nrow = 4, ncol = 3))
colnames(Interaction_DF) = c("Factor1", "AverageResponse", "Factor2")


# Factor level values
f1 = c("low", "high")
f2 = c("low", "high")
f3 = c("low", "high")
FactorLevels = rbind(f1, f2, f3)

Interaction_DF$Factor1[1] = FactorLevels[i,1]
Interaction_DF$Factor1[2] = FactorLevels[i,2]
Interaction_DF$Factor1[3] = FactorLevels[i,1]
Interaction_DF$Factor1[4] = FactorLevels[i,2]

Interaction_DF$AverageResponse[1] = R_in_jn
Interaction_DF$AverageResponse[2] = R_ip_jn
Interaction_DF$AverageResponse[3] = R_in_jp
Interaction_DF$AverageResponse[4] = R_ip_jp

Interaction_DF$Factor2[1] = FactorLevels[j,1]
Interaction_DF$Factor2[2] = FactorLevels[j,1]
Interaction_DF$Factor2[3] = FactorLevels[j,2]
Interaction_DF$Factor2[4] = FactorLevels[j,2]


library(ggplot2)

Interaction_Plot = ggplot(Interaction_DF,
                          aes(x = Factor1, 
                          y = AverageResponse, 
                          group = Factor2)) +
                   geom_line(aes(color = Factor2)) +
                   geom_point(aes(color = Factor2)) +
                   scale_x_discrete(limits = c("low","high")) +
                   scale_color_manual(values=c("#D81B60", "#FFC107")) + 
                   theme_light() 

print(Interaction_Plot)
    
















