import pandas as pd

def generate_waiting_list(capacity_df, referrals_df, stock_df, stage, num_weeks, df_TM_routine, df_TM_urgent, df_TM_twoWeek):
    """
    Generates the waiting list for a given stage.
    
    inputs: capacity_df: Capacity dataframe containing capacity for each stage
            referrals_df: Referrals dataframe containing the number of GP referrals for each priority
            stock_df: The stock (starting) waiting lists for each stage and each priority
            stage: The stage (location) that you want to generate the waiting list for
            num_weeks: Number of weeks you want to simulate the waiting list for
            
    outputs: df_WL: Contains the waiting list dataframe for the given stage. Also contains number
                    of patients seen for each priority and each simulated week
    """
    # initialise waiting list df
    d = {"week":list(range(0,num_weeks+1)),
         "twoWeek_WL":0,"urgent_WL":0,"routine_WL":0,
         "twoWeek_seen":0,"urgent_seen":0,"routine_seen":0}
    df_WL = pd.DataFrame(d)
    df_WL = df_WL.set_index("week")
    
    # set stock values as the waiting list value for week 0
    df_WL.loc[0,["twoWeek_WL", "urgent_WL", "routine_WL"]] = stock_df.loc[stage].values
    
    # get recurrent transition probability
    recurrent_prob_routine = df_TM_routine.loc[stage][stage]
    recurrent_prob_urgent = df_TM_urgent.loc[stage][stage]
    recurrent_prob_twoWeek = df_TM_twoWeek.loc[stage][stage]
    
    # for each week that we want to generate the waiting list
    for i in range(1, num_weeks+1):
        # get previous weeks waiting list value
        twoWeek_old_WL, urgent_old_WL, routine_old_WL = df_WL.loc[i-1,["twoWeek_WL", "urgent_WL", "routine_WL"]].values
        # get referral values
        twoWeek_referral, urgent_referral, routine_referral = referrals_df.loc[i].values
        # get capacity values
        capacity = capacity_df.loc[i,stage]
        
        # new twoWeek WL = (old twoWeek WL) + (twoWeek referrals) - (capacity used by twoWeek patients)
        # Capacity used is going to be the smaller of the avaiable capacity and old WL + referrals
        twoWeek_capacity_used = min(twoWeek_old_WL + twoWeek_referral, capacity)
        twoWeek_new_WL = twoWeek_old_WL + twoWeek_referral - twoWeek_capacity_used + twoWeek_capacity_used*recurrent_prob_twoWeek
        
        # new urgent WL = (old urgent WL) + (urgent referrals) - (capacity used by urgent patients)
        # capacity avaiable to urgent patients = total capacity - capacity used by 2week patients
        # therefore, capacity used by urgent patients = capacity available
        urgent_capacity_available = capacity - twoWeek_capacity_used
        urgent_capacity_used = min(urgent_old_WL + urgent_referral, urgent_capacity_available)
        urgent_new_WL = urgent_old_WL + urgent_referral - urgent_capacity_used + urgent_capacity_used*recurrent_prob_urgent
        
        # Same logic for routine
        routine_capacity_available = urgent_capacity_available - urgent_capacity_used
        routine_capacity_used = min(routine_old_WL + routine_referral, routine_capacity_available)
        routine_new_WL = routine_old_WL + routine_referral - routine_capacity_used + routine_capacity_used*recurrent_prob_routine
        
        df_WL.loc[i] = [twoWeek_new_WL, urgent_new_WL, routine_new_WL,
                        twoWeek_capacity_used, urgent_capacity_used, routine_capacity_used]
          
    return df_WL

def inflow(from_stage, to_stage, from_df, df_TM_twoWeek, df_TM_urgent, df_TM_routine):
	import pandas as pd
	# Get transition probabilities for each priority  
	twoWeek_probability = df_TM_twoWeek.loc[from_stage,to_stage]
	urgent_probability = df_TM_urgent.loc[from_stage,to_stage]
	routine_probability = df_TM_routine.loc[from_stage,to_stage]
    
	# Get the number of patients seen at each priority
	if from_stage.startswith("referral"):
		twoWeek_seen = from_df.twoWeek_referrals.values
		urgent_seen = from_df.urgent_referrals.values
		routine_seen = from_df.routine_referrals.values
	else:
		twoWeek_seen = from_df.twoWeek_seen.values
		urgent_seen = from_df.urgent_seen.values
		routine_seen = from_df.routine_seen.values
    
	# Estimate inflow
	inflow = {"twoWeek_inflow":twoWeek_seen*twoWeek_probability,
              "urgent_inflow":urgent_seen*urgent_probability,
              "routine_inflow":routine_seen*routine_probability}
    
	if from_stage.startswith("referral"):
		inflow_df = pd.DataFrame(inflow)
		inflow_df.index += 1
	else:
		inflow_df = pd.DataFrame(inflow).loc[1:]
    
	return inflow_df