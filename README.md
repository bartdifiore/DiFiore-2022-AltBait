# Alternative bait for the American lobster fishery

### PI's: Adrian Jordaan, Brian Cheng, Amanda Kinchla, and Jynessa Dutka-Gianelli

### Repository and report prepared by Bart DiFiore

This repository serves to house data, metadata, preliminary analyses, and methods associated with the summer 2022 field sampling season for the alternative bait project. <br>

For questions regarding the report or data contact [Bart DiFiore](mailto:bdifiore@ucsb.edu).<br>

Raw data and documents are also archived in a google drive folder [here](https://drive.google.com/open?id=1FddRBsIQjnhlC8Ou6a2bvTekOQfMXLVr&authuser=bdifiore%40ucsb.edu&usp=drive_fs).<br>

## Field sampling methods

The overall goal of the sampling in summer 2022 was to trial different prototypes of an alternative lobster bait. Specifically, we sought to identify prototypes that caught lobsters at similar rates to traditional baits (herring, *Clupea harengus*) by refining the bait formulation.<br>

We conducted the trials using commercial-style lobster traps (3', single parlor constuction) deployed in a paired design, with a control trap (herring bait) set within ~20 m of each alternative bait trap. Traps were hauled approximately every three nights when deployed with alternative. All traps were set within 2 km of the Gloucester Marine station in the areas locally known as Hodgkins Point and "the ledges" off of Plum Cove beach. Depth ranged from 15-55'. We took care to set trap pairs at similar depths and proximities to structure. For example if setting on a small rock outcropping, we would set one trap on either side of the boulder at similar depths.<br>

When hauling traps, we recorded the number and size class of each lobster. Size classes followed commercial fishing regulations of carapace length and reproductive status (short (< 3.25"), keeper (3.25-5"), egged female, v-notched female, and jumbo (>5")). We also recorded the number of crabs and fish caught in each trap. If a jumbo lobster had eggs or was v-notched it was recorded as an egged or v-notched female.<br>

We purchased previously frozen herring bait landed in the Faroe Islands from CapeAnn Lobstermen. Herring were large, and unless otherwise noted in the dataset, we used two defrosted herring in each bait bag (~0.3-0.6 kg). All bait (herring and alternative) was fished in large mesh traditional-style bait bags hung in the trap's kitchen from a bait cleat. Only bait that had been defrosted once was used in the trials.<br>


## Metadata

### Rawdata_20221017.csv

This file contains all observations from the core sampling conducted in summer 2022. 

Location - The general geographic area where a trap was set.<br>
Trap_pair - Unique identifier of each trap pair. Two taps were lost over the course of the season, and the remaining traps were paired. However, we retain the original trap pair numbering for consistency in the dataset.<br>
Buoy_number - Unique identifier of each trap. Numbers were carved into the buoys. 
Bait_type - categorical predictor (either "Herring" or "Alt"), where "Alt" stands for alternative bait.<br>
Alternative_label - Identifier for each alternative bait prototype. Metadata associated with each prototype can be found [here](https://docs.google.com/spreadsheets/d/1QMPQGISEwp9p9eJ8eS26mkG6gupRmHZ8yigrYQaDgqQ?authuser=bdifiore%40ucsb.edu&usp=drive_fs).<br>
Amount - Amount of traditional bait used. In units of ind. herring.<br> 
Date_set - Date trap was set<br>
Time_set - Approximate time traps were set. For example if we set gear from noon - 2 pm, the "Time_set" would be 1 pm, or 1300.<br> 
Date_retrieved - Date trap was hauled<br>
Time_revieved - see Time_set<br>
Deployment_nights - Number of nights a trap was soaked between the set and retrieval dates.<br> 
Lobster - number of lob individuals in a trap of all size/sex classes<br>
Num_short - number of short (<3.25") lobsters<br>
Num_legals - number of keeper lobsters (3.25-5"), no v-notch, and no eggs.<br> 
Num_Vnotch - number of female lobsters with a v-notched tail (v-notched lobsters cannot be retained)<br>
Num_egged - number of egged females of any size (short, legal-sized, jumbo)<br>
Total_crab - number of crabs captured in a trap. It was not always possible to count crabs. In the event that crabs were not counted the data was left blank (e.g. NA). Zeros are evidence that crabs were counted and not present.<br>
Rock_crab - number of rock crabs (either *Cancer borealis* or *Cancer irroratus*)<br>
Green_crab - number of green crabs (*Carcinus maenas*)<br>
Bait_remaining - % of bait remaining estimated visually by BD<br>
Notes - any notes associated with the haul. Fish species and number were recorded under notes.<br>

### InitialSet.csv

This is an ancilary dataset collected based on an initial deployment in late May 2022. It is provided only for future reference and should not be analyzed with the broader dataset. Traps were deployed at the mouth of the  
