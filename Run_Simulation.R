#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This function runs the main body of the simulation

debugSource("Determine_Presence.R")
debugSource("Determine_Compliance.R")
debugSource("Execute_Action.R")
debugSource("Update_StateVariables.R")
debugSource("Choose_Next_SSP.R")
debugSource("Update_ActionsHistory.R")

Run_Simulation <- function()
{
    # In the main body of the simulation, the model updates 
    # the values of the state spaces
    Update_StateSpace <<- TRUE
    
    for (step in 1:SimDuration) 
    {
        Presence <<- Determine_Presence()
        
        Compliance_State <<- Determine_Compliance()
        
        EA_Output <<- Execute_Action()
        
        Update_StateVariables()
        
        Choose_Next_SSP()
        
        Update_ActionsHistory(step)
    }
}