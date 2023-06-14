#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code is for creating the SCDC figure. In this figure, we draw the results
#              of running the LGPP=0.1-CM=PC with similar or different combinations. When
#              all use their specific PBCs, when all use the PBC of the best achieving rule,
#              the 0.67, and when all use the PBC of the worst achieving rule, the 0.001.

# Clear the environment
rm(list=ls())

library(readxl)
library(xlsx)

ParentDir = "C:/Users/szmoo/Dropbox/PhD/My Manuscripts/12-AB_SCD_M/Code"
OutputPath = paste(ParentDir, "Output/Systematic Experiments/LGPP-CM", sep = "/")
FilePath = paste(OutputPath, paste("RS_LGRD", "xlsx", 
                                   sep = "."), sep = "/")

SheetName = "LGPP=0.1-CM=PC"
Aggregated_Output = data.frame()

# This iteration contains the results of running with specific PBCs
RS1 = read_excel(FilePath, sheet = paste(SheetName, "P", sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = RS1

# This iteration contains the results of running with the low level combination
RS2 = read_excel(FilePath, sheet = paste(SheetName, "L", sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = rbind(Aggregated_Output, RS2)

# This iteration contains the results of running with the medium level combination
RS3 = read_excel(FilePath, sheet = paste(SheetName, "M", sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = rbind(Aggregated_Output, RS3)

# This iteration contains the results of running with the high level combination
RS4 = read_excel(FilePath, sheet = paste(SheetName, "H3", sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = rbind(Aggregated_Output, RS4)

Iter = rep("PBC", 36)
Aggregated_Output = cbind(Aggregated_Output, Iter)
Aggregated_Output$Iter[10:18] = "LLC"
Aggregated_Output$Iter[19:27] = "MLC"
Aggregated_Output$Iter[28:36] = "HLC"

library(ggplot2)

SCDC = ggplot(Aggregated_Output, aes(x=PR, y=LGRD, color=Iter)) +
              geom_errorbar(aes(ymin=LGRD-ME, ymax=LGRD+ME), width=.02, linewidth = 1) +
              geom_line(linewidth = 1) + 
              geom_point() +
              labs(y = "LGR difference", 
                   x = "Parental rule",
                   color="Combination") +
              scale_color_manual(values=c("#1E88E5", "#DC267F", "#FE6100", "#FFB000")) + 
              theme_light()

print(SCDC)


