#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots the LGRD vs. parental rules for two versions of  
#              the FC_P_PC_A compliance mode, one with a normal Perceived_Autonomy
#              value and one with a PA equal to 10% of this normal PA.

# Clear the environment
rm(list=ls())


# The data is the result of running the codes RunRules_LGRD.R and 
# AnalyzeRules_LGRD.R

ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData/LGPP-CM", sep = "/")

FilePath = paste(OutputPath, paste("RS_LGRD", "xlsx", 
                                   sep = "."), sep = "/")

library(readxl)
library(xlsx)

SheetName = "LGPP=0.1-CM=FC"


Aggregated_Output = data.frame()

# The iteration executed with the normal PA
RS1 = read_excel(FilePath, sheet = paste(SheetName, "P", sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = RS1

# The iteration executed with PA = 0.1*PA
RS2 = read_excel(FilePath, sheet = paste(SheetName, "LPA", sep = '--'), 
                 col_names = TRUE)

Aggregated_Output = rbind(Aggregated_Output, RS2)

Iter = rep("High", 18)
Aggregated_Output = cbind(Aggregated_Output, Iter)
Aggregated_Output$Iter[10:18] = "Low"


library(ggplot2)

windowsFonts(Times = windowsFont("Times New Roman"))

FCFC = ggplot(Aggregated_Output, aes(x=PR, y=LGRD, color=Iter)) +
       geom_errorbar(aes(ymin=LGRD-ME, ymax=LGRD+ME), width=.02, linewidth = 1) +
       geom_line(linewidth = 1) + 
       geom_point() +
       labs(y = "LGR difference", 
            x = "Parental rule",
            color="Perceived autonomy") +
       scale_color_manual(values=c("#1E88E5", "#DC267F")) +
       theme_light() +
       theme(text=element_text(size=8,family="Times"))

FigurePath = paste(ParentDir, "Figures/fig-FCFC", sep = "/")
ggsave(file=paste(FigurePath, "jpeg", sep = "."), width=5, height=5, dpi=1000)

print(FCFC)




