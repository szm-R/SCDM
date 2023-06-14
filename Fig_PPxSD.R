#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots the PP x SD interaction figure. 

# Clear the environment
rm(list=ls())

# The input to this code is the RM_Rule=# file, where the # would be 
# replaced by the rule ratio (PR).


# Parental rule ratio
PR = 0.125

# The index of experiment iteration
IterationNum = 2

## Factor codes
PP = 1
SGLW = 2
SD = 3

# We aim to analyze the i x j interaction effect
i = PP
j = SD


# Read the response matrix of the target Rule
library("readxl")

ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData/LGPP-CM/LGPP=0.1 - CM=PC_P_NC_A", 
                   sep = "/")

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

windowsFonts(Times = windowsFont("Times New Roman"))

Interaction_Plot = ggplot(Interaction_DF,
                          aes(x = Factor1, 
                              y = AverageResponse, 
                              group = Factor2)) +
                   geom_line(aes(color = Factor2), linewidth = 1.5) +
                   geom_point(aes(color = Factor2)) +
                   scale_x_discrete(limits = c("low","high")) +
                   labs(y = "LGR difference",
                        x = "Presence probability",
                        color="Simulation duration") +
                   scale_color_manual(values=c("#D81B60", "#FFC107")) + 
                   theme_light() +
                   theme(text=element_text(size=8,family="Times"))

FigurePath = paste(ParentDir, "Figures/fig-125In2-PPSD", sep = "/")
ggsave(file=paste(FigurePath, "jpeg", sep = "."), width=5, height=5, dpi=1000)

print(Interaction_Plot)

















