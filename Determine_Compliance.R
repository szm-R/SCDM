#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel assigns a TRUE/FAlSE value to the  
#              Compliance_State variable.

Determine_Compliance <- function()
{
    ###########################################################################
    # We use different compliance likelihoods for the Presence and Absence    #
    # steps (steps where the parent agent is present or absent).              #
    #                                                                         #
    # The CLP denotes the adolescent's Compliance Likelihood in the Presence  #
    # steps and the CLA specifies its Compliance Likelihood in the Absence    #
    # steps.                                                                  #
    #                                                                         #
    # We initialize the compliance likelihoods at the beginning of the        #
    # simulation loop and keep them constant throughout the simulation. We    #
    # use these two constants as the probabilities of setting the             #
    # Compliance_State to TRUE in the corresponding simulation steps.         #
    ###########################################################################
    
    if (Presence)
    {
        Compliance_State <<- sample(c(FALSE, TRUE), size = 1, 
                                    prob = c((1-CLP), CLP))
    }
    else
    {
        Compliance_State <<- sample(c(FALSE, TRUE), size = 1,
                                    prob = c((1-CLA), CLA))
    }
    
    return(Compliance_State)
}




