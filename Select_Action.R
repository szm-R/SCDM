#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This function selects an action under the Execute_Action submodel.
            

Select_Action <- function()
{
    ###########################################################################
    # The agent selects actions (goals) using two criteria:                   #
    #                                                                         #
    #   - The Goal Preference Probability (GP_Probability)                    #
    #   - The State-Goal Link (SLG_Links and SSG_links)                       #
    #                                                                         #
    # The parameter SGL_Weight captures the relative weight of the two        #
    # criteria.                                                               #
    #                                                                         #
    # This function selects a goal based on these criteria and outputs        #
    # the index of the selected goal                                          #
    ###########################################################################
    
    
    # First, we read the State-Goal link of each goal from the
    # corresponding dataframes
    
    # The current position in each state space
    Current_LGSSP = LG_SS_Position[1:2]
    Current_SGSSP = SG_SS_Position[1:2]
    
    # The value of the State-Goal links at the current states
    SLG_Link = SLG_Links[Current_LGSSP[1], Current_LGSSP[2]]
    SSG_Link = SSG_Links[Current_SGSSP[1], Current_SGSSP[2]]
    
    if (Use_Normalized_SGL)
    {
        # In this case, we first normalize the values of the State-Goal   
        # links to the range [0,1]
        
        # First, the values are set in the range [-1,0]
        
        Min_SLGL = min(SLG_Links)
        Max_SLGL = max(SLG_Links)
        
        if ((Max_SLGL - Min_SLGL) != 0)
            Norm_SLGL = -1 + (SLG_Link - Min_SLGL)/(Max_SLGL - Min_SLGL)
        else
            Norm_SLGL = SLG_Link
        
        
        Min_SSGL = min(SSG_Links)
        Max_SSGL = max(SSG_Links)
        
        if ((Max_SSGL - Min_SSGL) != 0)
            Norm_SSGL = -1 + (SSG_Link - Min_SSGL)/(Max_SSGL - Min_SSGL)
        else
            Norm_SSGL = SSG_Link
        
        # The final combination is set to fall into the range [0,1]
        StateGoal_Link = c(-1*Norm_SLGL, -1*Norm_SSGL)
    } 
    else
    {
        # Otherwise, we use the original State-Goal links 
        StateGoal_Link = c(-1*SLG_Link, -1*SSG_Link)
    }
        

   
    # Now, we combine the two criteria to make a final score representing 
    # each goal
    Final_LG_Score = LGP_Probability + SGL_Weight*StateGoal_Link[1]
    Final_SG_Score = (1-LGP_Probability) + SGL_Weight*StateGoal_Link[2]
    Combined_Scores = c(Final_LG_Score, Final_SG_Score)
    
    
    # Next, using the combined scores we create proportional scores 
    # to represent each goal
    Proportional_Scores = Combined_Scores/sum(Combined_Scores)
    
    
    # Finally, we use the proportional scores as the probabilities in  
    # a weighted random selection to choose between the goals
    Selected_Action = sample(1:2, size = 1, prob = Proportional_Scores)
    
    return(Selected_Action)
}