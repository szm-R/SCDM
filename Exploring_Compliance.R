#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots the final compliance rate for the two compliance 
#              modes of FC_P_PC_A and PC_P_NC_A.

# Clear the environment
rm(list=ls())

Rules = c("0.875", "0.75", "0.67", "0.5", "0.33", 
          "0.25", "0.125", "0.045", "0.001")
SD = 1000
Num_Runs = 5

Compliance_Rule = data.frame(matrix(ncol = 4, nrow = 0))
Column_Names = c("Rule", "PP", "CLP", "CLA")
colnames(Compliance_Rule) = Column_Names

Compliance_Rate = data.frame(matrix(ncol = 3, nrow = 0))
Column_Names = c("Rule", "PP", "CR")
colnames(Compliance_Rate) = Column_Names


for (Rule in Rules)
{
    print(paste("Running for Rule", Rule))
    for (PP in seq(0.1, 1, 0.05))
    {
        # The PC_P_NC_A compliance mode
        CLP = 1 - PP*(1 - as.numeric(Rule))
        CLA = 0
        
        # The FC_P_PC_A compliance mode
        # CLP = 1
        # CLA = as.numeric(Rule)
        
        NewRow = data.frame(Rule, PP, CLP, CLA)
        names(NewRow) = c("Rule", "PP", "CLP", "CLA")
        Compliance_Rule = rbind(Compliance_Rule, NewRow)
        
        Compliance_Counter = 0
        
        for (Run_Idx in 1:Num_Runs)
        {
            for (iter in 1:SD)
            {
                Presence = sample(c(FALSE, TRUE), size = 1, 
                                  prob = c((1-PP), PP))
                
                if (Presence)
                {
                    CS = sample(c(FALSE, TRUE), size = 1, 
                                prob = c((1-CLP), CLP))
                }  
                else
                {
                    CS = sample(c(FALSE, TRUE), size = 1,
                                prob = c((1-CLA), CLA))
                }
                
                if (CS)
                    Compliance_Counter = Compliance_Counter + 1
            }
        }
        
        
        NewRow = data.frame(Rule, PP, Compliance_Counter/(SD*Num_Runs))
        names(NewRow) = c("Rule", "PP", "CR")
        Compliance_Rate = rbind(Compliance_Rate, NewRow)
    }
}


library(ggplot2)
library(viridis)

windowsFonts(Times = windowsFont("Times New Roman"))

CR_Plot = ggplot(Compliance_Rate, aes(x = PP, y = CR, group = Rule)) +
          geom_line(aes(color = Rule), linewidth = 0.5) +
          geom_point(aes(color = Rule)) +
          scale_color_viridis(option = "plasma", discrete = TRUE) +
          labs(y = "Compliance rate", 
               x = "Parental presence probability",
               color = "Parental rule") +
          theme_light() +
          theme(text=element_text(size=8,family="Times"))

FigurePath = paste("Output/Figures/fig-CR-PC", sep = "/")
ggsave(file=paste(FigurePath, "jpeg", sep = "."), width=5, height=5, dpi=1000)















