#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This function measures and records the results

Measure_Response <- function()
{
    
    #############################################################################
    #############                                                   #############   
    #############            Running the Response Steps             #############
    #############                                                   #############   
    #############################################################################
    
    SimD = as.numeric(DesignMatrix_Value[DP,SD])
    RS = Response_Steps
    
    # We freeze the state space in the response steps
    Update_StateSpace <<- FALSE
    
    # Loop over response steps
    for (step in SimD+1:RS) 
    {
        # We lift the parental rule in the response steps by
        # assigning a false value to the Compliance_State
        Compliance_State <<- FALSE
        
        EA_Output <<- Execute_Action()
        
        Choose_Next_SSP()
        
        Update_ActionsHistory(step)
    }
    
    # Number of executed LG actions after lifting the parental rule
    NumLG_AML = length(which(Actions_History$Executed_Action[SimD+1:RS] == LG_Idx))
    
    LGR_AML = NumLG_AML/RS
    
    return(LGR_AML)
}














