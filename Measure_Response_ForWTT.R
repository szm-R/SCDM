#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This function measures and records the results for the 
#              WTT single experiment case

Measure_Response_ForWTT <- function()
{
    # Execute the response steps
    SimD = SD
    RS = Response_Steps
    Update_StateSpace <<- FALSE
    
    for (step in SimD+1:RS) 
    {
        Compliance_State <<- FALSE
        
        EA_Output <<- Execute_Action()
        
        Choose_Next_SSP()
        
        Update_ActionsHistory(step)
    }
    
    if (SeparateDays)
    {
        StepsPerDay = Reset_Steps
        NumDaysInRS = Response_Steps/StepsPerDay
        
        Temp_RM1 = matrix(0, nrow = NumDaysInRS, ncol = 2)
        Column_Names = c("LGRD", "TimeTag")
        colnames(Temp_RM1) = Column_Names
        
        for (i in 1:NumDaysInRS)
        {
            SI = (i-1)*StepsPerDay
            EI = i*StepsPerDay
            NumLG = length(which(Actions_History$Selected_Action[SI:EI] == LG_Idx))
            LGR = NumLG/StepsPerDay
            Temp_RM1[i, 1] = LGR - LGP_Probability
            Temp_RM1[i, 2] = "Initial days"
        }
        
        Temp_RM2 = matrix(0, nrow = NumDaysInRS, ncol = 2)
        Column_Names = c("LGRD", "TimeTag")
        colnames(Temp_RM2) = Column_Names
        
        for (i in 1:NumDaysInRS)
        {
            SI = (i-1)*StepsPerDay
            EI = i*StepsPerDay
            
            NumLG = length(which(Actions_History$Selected_Action[SimD+SI:EI] == LG_Idx))
            LGR = NumLG/StepsPerDay
            Temp_RM2[i, 1] = LGR - LGP_Probability
            Temp_RM2[i, 2] = "Last days"
        }
        
        ResponseMatrix <<- rbind(ResponseMatrix, Temp_RM1)
        ResponseMatrix <<- rbind(ResponseMatrix, Temp_RM2)
    } else
    {
        NumLG = length(which(Actions_History$Selected_Action[1:RS] == LG_Idx))
        Init_LGR = NumLG/RS

        NumLG = length(which(Actions_History$Selected_Action[SimD+1:RS] == LG_Idx))
        Final_LGR = NumLG/RS

        ResponseMatrix[Run_Idx, 1] <<- Init_LGR - LGP_Probability
        ResponseMatrix[Run_Idx, 2] <<- "Initial days"

        ResponseMatrix[Num_Runs+Run_Idx, 1] <<- Final_LGR - LGP_Probability
        ResponseMatrix[Num_Runs+Run_Idx, 2] <<- "Last days"
    }
}














