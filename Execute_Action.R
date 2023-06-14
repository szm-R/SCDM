#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel selects and executes an action.

debugSource("Select_Action.R")
debugSource("Enforce_Parental_Rule.R")
debugSource("Generate_Output.R")

Execute_Action <- function()
{
    ###########################################################################
    # This submodel consists of three functions:                              #
    #                                                                         #
    #    1. Select_Action: This function selects an action with a weighted    #
    #       random process based on two criteria.                             #
    #    2. Enforce_Parental_Rule: In this function, the Adolescent agent     #
    #       checks its selected action against the parental rule.             #
    #    3. Generate_Output: The last function outputs the index of the       #
    #       action to be executed and the state of the Forced_Choice flag     #
    ###########################################################################
    
    Selected_Action = Select_Action()
    
    Approval_Status = Enforce_Parental_Rule(Selected_Action)
    
    EA_Output = Generate_Output(Selected_Action, Approval_Status)
        
    return(EA_Output)
}