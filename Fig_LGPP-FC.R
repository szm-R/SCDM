#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots the LGRD vs. parental rules for different levels
#              of LGPP in the FC_P_PC_A compliance mode. 

# Clear the environment
rm(list=ls())

# The data is the result of running the codes RunRules_LGRD.R and 
# AnalyzeRules_LGRD.R

ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData/LGPP-CM", sep = "/")

FilePath = paste(OutputPath, paste("RS_LGRD", "xlsx", sep = "."), sep = "/")

library(readxl)
library(xlsx)

Aggregated_Output = data.frame()

SheetName = "LGPP=0.1-CM=FC"
RS1 = read_excel(FilePath, sheet = paste(SheetName, "P", sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = RS1
Aggregated_Output$LGPP[1:9] = "0.1"


SheetName = "LGPP=0.2-CM=FC"
RS2 = read_excel(FilePath, sheet = paste(SheetName, "P", sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = rbind(Aggregated_Output, RS2)
Aggregated_Output$LGPP[10:18] = "0.2"


SheetName = "LGPP=0.3-CM=FC"
RS2 = read_excel(FilePath, sheet = paste(SheetName, "P", sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = rbind(Aggregated_Output, RS2)
Aggregated_Output$LGPP[19:27] = "0.3"


SheetName = "LGPP=0.4-CM=FC"
RS2 = read_excel(FilePath, sheet = paste(SheetName, "P", sep = '--'), 
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

FigurePath = paste(ParentDir, "Figures/fig-LGPP-FC-AllP", sep = "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width=5, height=5, dpi=1000)

ggsave(file=paste(FigurePath, "jpeg", sep = "."), width=5, height=5, dpi=1000)

print(LGPP)



