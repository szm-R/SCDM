#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel updates the action history of the adolescent agent


Update_ActionsHistory <- function(step)
{
    ###########################################################################
    # We store the history of the executed actions in a dataframe with        #
    # 6 columns:                                                              #
    #                                                                         #
    #    - Step: The current step number in the simulation process.           #
    #    - Selected_Action: The index of the selected goal, 1 for LG and      #
    #      2 for SG.                                                          #
    #    - Executed_Action: The index of the executed goal, 1 for LG and      #
    #      2 for SG                                                           #
    #    - Forced_Choice: The flag indicating whether the executed action     #
    #      differs from the selected one, filled with TRUE if it differs      #
    #      and FALSE if it doesn't.                                           #
    #    - P_Presence: The flag indicating parental presence in the current   #
    #      step, filled with TRUE if they are present and FALSE if they       #
    #      are not.                                                           #
    #    - Compliance_State: The flag indicating the adolescent agent's       #
    #      compliance state, filled with TRUE if the agent is complying       #
    #      with the parental rule in the current simulation step and FALSE    #
    #      if it is not.                                                      #
    ###########################################################################
    
    Selected_Action = EA_Output[[1]] 
    Executed_Action = EA_Output[[2]]
    Forced_Choice = EA_Output[[3]]
    
    NewRow = data.frame(step, Selected_Action, Executed_Action, 
                        Forced_Choice, Presence, Compliance_State)
    names(NewRow) = c("Step", "Selected_Action", "Executed_Action", 
                       "Forced_Choice", "P_Presence", "Compliance_State")
    Actions_History <<- rbind(Actions_History, NewRow)
}





