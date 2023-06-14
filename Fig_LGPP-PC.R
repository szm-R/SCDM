#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots the LGRD vs. parental rules for different levels
#              of LGPP in the PC_P_NC_A compliance mode.

# Clear the environment
rm(list=ls())

# The data is the result of running the codes RunRules_LGRD.R and 
# AnalyzeRules_LGRD.R

# To create the figure with all rules executed with their specific PBCs, set
# the Iteration_Code to "P".
# Iteration_Code = "P"

# To create the figure with all rules executed with the combination PP = 0.8, 
# SGLW = 10.0, and SD = 1920, set the Iteration_Code to "H".
Iteration_Code = "H"

ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData/LGPP-CM", sep = "/")

FilePath = paste(OutputPath, paste("RS_LGRD", "xlsx", sep = "."), sep = "/")

library(readxl)
library(xlsx)

Aggregated_Output = data.frame()

SheetName = "LGPP=0.1-CM=PC"
RS1 = read_excel(FilePath, sheet = paste(SheetName, Iteration_Code, sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = RS1
Aggregated_Output$LGPP[1:9] = "0.1"


SheetName = "LGPP=0.2-CM=PC"
RS2 = read_excel(FilePath, sheet = paste(SheetName, Iteration_Code, sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = rbind(Aggregated_Output, RS2)
Aggregated_Output$LGPP[10:18] = "0.2"


SheetName = "LGPP=0.3-CM=PC"
RS2 = read_excel(FilePath, sheet = paste(SheetName, Iteration_Code, sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = rbind(Aggregated_Output, RS2)
Aggregated_Output$LGPP[19:27] = "0.3"


SheetName = "LGPP=0.4-CM=PC"
RS2 = read_excel(FilePath, sheet = paste(SheetName, Iteration_Code, sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = rbind(Aggregated_Output, RS2)
Aggregated_Output$LGPP[28:36] = "0.4"

library(ggplot2)

windowsFonts(Times = windowsFont("Times New Roman"))

LGPP = ggplot(Aggregated_Output, aes(x=PR, y=LGRD, color=LGPP)) +
       geom_errorbar(aes(ymin=LGRD-ME, ymax=LGRD+ME), width=.03, linewidth = 1) +
       geom_line(linewidth = 1) + 
       geom_point() +
       labs(y = "LGR difference", 
            x = "Parental rule",
            color="LGPP") +
       scale_color_manual(values=c("#1E88E5", "#DC267F", "#FE6100", "#FFB000")) +
       theme_light() +
       theme(text=element_text(size=8,family="Times"))

FileName = paste("Figures/fig-LGPP-PC-All", Iteration_Code, sep = '')
FigurePath = paste(ParentDir, FileName, sep = "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width=5, height=5, dpi=1000)

ggsave(file=paste(FigurePath, "jpeg", sep = "."), width=5, height=5, dpi=1000)

print(LGPP)



