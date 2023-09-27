import pandas as pd
import numpy as np

def generate_waiting_list(capacity_df, referrals_df, stock_df, stage, num_weeks, df_TM_routine, df_TM_urgent, df_TM_twoWeek):
    import pandas as pd
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
    
####### Store data sent from Qlik to Python in df

df = q.copy()

####### Retrieve capacity parameters

cap_dir = "//wwlqlikview1/_Everyone/SharedDataFiles/WL_modelling/Waiting_List_Capacity.xlsx"
df_capacity = pd.read_excel(
    cap_dir,
    index_col="week")
weeks = len(df_capacity)

####### Make dataframe of outpatient and inpatient referrals

d_dict = {
    "week": list(range(1, weeks + 1)),
    "twoWeek_referrals": np.repeat(df[df.prio == "2 Week"].avg_refs.values[0],
                                   weeks),
    "urgent_referrals": np.repeat(df[df.prio == "Urgent"].avg_refs.values[0], weeks),
    "routine_referrals": np.repeat(df[df.prio == "Routine"].avg_refs.values[0],
                                   weeks)
}
df_referrals_op = pd.DataFrame(d_dict).set_index("week")

d_dict = {
    "week": list(range(1, weeks + 1)),
    "twoWeek_referrals": np.repeat(0,
                                   weeks),
    "urgent_referrals": np.repeat(df[df.prio_ip == "Urgent"].avg_ip_refs.values[0], weeks),
    "routine_referrals": np.repeat(df[df.prio_ip == "Routine"].avg_ip_refs.values[0],
                                   weeks)
}
df_referrals_ip = pd.DataFrame(d_dict).set_index("week")

####### Make dataframe of inpatient and outpatient current waiting list (stock)

d_dict = {
    "stage": df_capacity.columns.values,
    "twoWeek":np.insert(df[df.priority_description == "2 Week"][["wl_category","stock"]].set_index("wl_category").loc[["First","Follow Up"]].stock.values,0,0),
    "urgent":df[df.priority_description == "Urgent"][["wl_category","stock"]].set_index("wl_category").loc[["inpatient","First","Follow Up"]].stock.values,
    "routine":df[df.priority_description == "Routine"][["wl_category","stock"]].set_index("wl_category").loc[["inpatient","First","Follow Up"]].stock.values
}
df_stock = pd.DataFrame(d_dict).set_index("stage")

####### Make transition matrices (one for each priority) for probability of walking between stages

d_dict = {
    "idx": ["referral",  "Follow Up", "Discharge", "First", "referral_ip", "inpatient"],
    "referral": [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
    "Follow Up": [0.0, 0.3, 0.0, 0.29, 0.0, 0.0],
    "Discharge": [0.89, 0.70, 0.0, 0.63, 0.0, 0.0],
    "First": [0.11, 0.0, 0, 0.08, 0.0, 0.00],
    "referral_ip": [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
    "inpatient": [0, 0, 0, 0.0, 1.0, 0]
}
df_TM_twoWeek = pd.DataFrame(d_dict).set_index("idx")

d_dict = {
    "idx": ["referral",  "Follow Up", "Discharge", "First", "referral_ip", "inpatient"],
    "referral": [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
    "Follow Up": [0.01, 0.28, 0.0, 0.18, 0.0, 0.0],
    "Discharge": [0.01, 0.72, 0.0, 0.70, 0.0, 0.0],
    "First": [0.98, 0.00, 0.00, 0.12, 0.0, 0.00],
    "referral_ip": [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
    "inpatient": [0, 0, 0, 0.0, 1.0, 0]
}
df_TM_urgent = pd.DataFrame(d_dict).set_index("idx")


df_TM_routine = df_TM_twoWeek.copy()
d_dict = {
    "idx": ["referral",  "Follow Up", "Discharge", "First", "referral_ip", "inpatient"],
    "referral": [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
    "Follow Up": [0.12, 0.36, 0.0, 0.10, 0.0, 0.0],
    "Discharge": [0.16, 0.64, 0.0, 0.72, 0.0, 0.0],
    "First": [0.72, 0.00, 0, 0.18, 0.0, 0.00],
    "referral_ip": [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
    "inpatient": [0, 0, 0, 0.0, 1.0, 0]
}
df_TM_routine = pd.DataFrame(d_dict).set_index("idx")


####### Generate waiting lists

# Referrals to First
inflow_referrals_First = inflow("referral", "First", df_referrals_op, df_TM_twoWeek, df_TM_urgent, df_TM_routine)
df_WL_First = generate_waiting_list(df_capacity, inflow_referrals_First, df_stock, "First", weeks, df_TM_routine, df_TM_urgent, df_TM_twoWeek)

# First to Follow Up
inflow_First_FU = inflow("First", "Follow Up", df_WL_First, df_TM_twoWeek, df_TM_urgent, df_TM_routine)
inflow_referrals_FU = inflow("referral","Follow Up", df_referrals_op, df_TM_twoWeek, df_TM_urgent, df_TM_routine)
df_WL_FU = generate_waiting_list(df_capacity, inflow_First_FU+inflow_referrals_FU, df_stock, "Follow Up", weeks, df_TM_routine, df_TM_urgent, df_TM_twoWeek)

# Inpatient Referrals to Inpatient Spell
inflow_ip_referrals_inp = inflow("referral_ip", "inpatient", df_referrals_ip, df_TM_twoWeek, df_TM_urgent, df_TM_routine)
df_WL_inp = generate_waiting_list(df_capacity, inflow_ip_referrals_inp, df_stock, "inpatient", weeks, df_TM_routine, df_TM_urgent, df_TM_twoWeek)

# Put data into single dataframe to be returned to qlik

df_WL_First = df_WL_First.add_suffix("_First")
df_WL_inp = df_WL_inp.add_suffix("_inp")
df_WL_FU = df_WL_FU.add_suffix("_FU")
df_capacity = df_capacity.add_suffix("_capacity")
df_referrals_ip = df_referrals_ip.add_suffix("_ip")
df_referrals_op = df_referrals_op.add_suffix("_op")

dfs = [df_WL_First, df_WL_inp, df_WL_FU, df_referrals_ip, df_referrals_op, df_capacity]
return_df = pd.concat(dfs, join="inner", axis=1)
return_df.reset_index(level=0, inplace=True)

start_week = pd.Timestamp.today().date() - pd.DateOffset(days=pd.Timestamp.today().dayofweek)
date_refs = []
for i in range(weeks):
	date_refs.append((start_week + pd.DateOffset(days=7*i)).strftime("%d/%m/%Y"))
return_df["snapshot_date_dt"] = date_refs

######################################################################

# Setup table to be returned to qlik

tableDescription = True	          # tableDescription must be set to True if it should be sent to Qlik
table.name = "Waiting Lists"

for col_name in return_df.columns:
    field = table.fields.add()
    field.name = col_name
    if col_name == "snapshot_date_dt":
    	field.dataType = 0
    else:
    	field.dataType = 1

qResult = return_df.values