# Repository Purpose
Compile dataset to be used calculated the historical abundance of mosquitoes from trap data which is defined by the average number of mosquitoes per trap per night. For each zone each week should add up to expected trap number unless that isn't trapping that week 
Light traps (method = L) are used for abundance. We still keep gravid trap (method = G) to be used for pooled infection rate calculations.

## Year 2015-2019: CMC (NW, NE, SW, SE, LV, BE)
### Background
Prior to 2019 the company was called CMC and formatted there data differently to present day.
The traps should be present from week 23 to 37

### Formatting
 The files are in `1_input/culex_sheet`
The pattern of these files is:
`LC*Week<WW>_*<YYYY>_*###*.xls` note that the naming pattern is highly inconsistent

Week	Trap Date	Trap Number	Zone	Trap Description	Light / Gravid	Malfunction	Cx tarsalis	Cx pipiens	Total CX	Total Females	Floater Location	Comments
24	06/16/2016	FC-029gr	SE	Bens Park	GRAVID	NO	0	25	25	25		
24	06/13/2016	FC-040gr	NE	Redwood	GRAVID	NO	1	17	18	19		
24	06/15/2016	LV-020	26/27	Cattail Pond	LIGHT	NO	17	0	17	53		
24	06/14/2016	LV-104	36	County Road 20C and County Road 9	LIGHT	NO	14	0	14	53		
24	06/15/2016	LV-042	29W	2001 South Douglas	LIGHT	NO	10	3	13	16		
24	06/13/2016	FC-092gr	NE	Udall Natural Area	GRAVID	NO	0	9	9	9		
24	06/16/2016	FC-089gr	SW	Kunz Ct and Brook Dr	GRAVID	NO	0	9	9	10		

## Year 2019-present: VDCI
### Background
The vector control company VDCI sets out traps every week in the summer to be tested by our lab
The information about the traps are shared in a sheet called `LC Week<WW>_<YYYY>_Culex.csv`
They sort the mosquitoes and pool the culex species in pools of up to 50
This data is capture in a file called `Week <WW> WNV <YYYY> CSU Datasheet.xls` this naming is not perfectly consistent
The traps should be present from week 23 to 37

### Formatting
It is formatted as follows

date_trap_set	contract	location	zone	mosquito_species	mosquito_count	trap_type	trap_name
9/11/23	Fort Collins	N. Linden	NE	Culex pipiens	3	CDC Light Trap	FC-006
9/11/23	Fort Collins	FC Visitor Center	NE	Culex pipiens	224	CDC Light Trap	FC-014
9/11/23	Fort Collins	FC Visitor Center	NE	Culex tarsalis	5	CDC Light Trap	FC-014
9/11/23	Fort Collins	Stuart and Dorset	NW	Culex pipiens	1	CDC Light Trap	FC-015
9/11/23	Fort Collins	Country Club	NE	Culex pipiens	1	CDC Light Trap	FC-034
9/11/23	Fort Collins	Country Club	NE	Culex tarsalis	5	CDC Light Trap	FC-034

## 2021-present: Boulder County (BC)
### Background
The start and end up trapping is somewhat sporadic. Typically around week 25 to week 35 or so. They typically only ever send 6 pools from various traps so there pools are never indicative of there trap abundance

### Formatting
File pattern: Abundance_Culex_DD_<MonthFullMame>YYYY.csv OR Boulder D<MonthFullMame>_YYYY_Culex.xlsx


## Special note on zones
zones are groupes of traps used to calculate final statistics for a report

## Berthoud (BE)
### Background
In 2024 CDC helped set 5 additional traps for Berthoud to help test expanding there trap surveillance. They didn't submit trap level data but they gave us all the culex mosquitoes so we can supplement the trap level data
### Expected Active Trap Number: pre 2024 5, post 10-

## Fort Collins
Fort Collins FC is broken up into four zones (NW, NE, SW, SE)
### Expected active Trap Number: 53
NE	14
NW	1
SE	18
SW	10

## Loveland
Submits only 6 traps for testing but sets out ~40 or so. Usually stops testing at week 36
### Expected active Trap Number: 6

# Steps

## 1. combine trap level data
### Desc:
Combine all trap level datasets from the West Nile virus surveillance program from desperate files across years into a single dataframe structured as follows:

structure(list(X = 1:6, trap_id = c("LC-001", "LC-001", "LC-049", 
"LC-054", "FC-006", "FC-014"), trap_date = c("2024-06-19", "2024-06-19", 
"2024-06-19", "2024-06-19", "2024-06-17", "2024-06-17"), spp = c("Tarsalis", 
"Pipiens", "Tarsalis", "Tarsalis", "Pipiens", "Tarsalis"), method = c("L", 
"L", "L", "L", "L", "L"), total = c(1L, 1L, 1L, 4L, 2L, 8L), 
    zone = c("BE", "BE", "BE", "BE", "NE", "NE"), year = c(2024L, 
    2024L, 2024L, 2024L, 2024L, 2024L), week = c(25L, 25L, 25L, 
    25L, 25L, 25L)), row.names = c(NA, 6L), class = "data.frame")

## 2. Clean Data

## 3. Supplement Trap level data with pool data
### desc:
The dataset first utilizes the culex sheet (trap level data) then is supplemented by pooled data (wnv-s_database) from times when the trap level data wasn't available. Example CDC started trapping Berthoud in 2024 before VDCI took over the contract. VDCI is the company that sends us the culex sheet. In addition, in high abundance years like 2023 VDCI only sent us a portion of the culex caught because we were at capacity. Which is why we can't just use the pool level data.




## 4. Filter Data
### desc:
Currently this step is in after clean but I want to save all traps and all spp before creating a filtered dataset.

#### Steps:
1. Deduplicate: Unique Key
traps observations should be uniquely identified by trap_id, week, and spp
2. keep active traps
3. keep only culex Tarsalis and Pipiens

## Final Product
Full dataset with all traps and filtered dataset with active traps and only culex that is 
formatted as follows:

trap_id	zone	zone2	trap_date	year	week	spp	method	trap_status	total
LV-089	LV	LV	2023-07-03	2023	27	Tarsalis	L	culex	2219
LV-089	LV	LV	2023-07-02	2023	27	Tarsalis	L	culex	2219
LV-069	LV	LV	2023-07-02	2023	27	Tarsalis	L	culex	2128
WC-055	BE	BE	2023-07-11	2023	28	Tarsalis	L	culex	1968
LV-089	LV	LV	2023-07-17	2023	29	Tarsalis	L	culex	1936
LC-054	BE	BE	2023-06-27	2023	26	Tarsalis	L	culex	1815
FC-053	SE	FC	2017-07-11	2017	28	Tarsalis	L	culex	1772
LV-095	LV	LV	2023-07-02	2023	27	Tarsalis	L	culex	1712
LV-104	LV	LV	2023-07-02	2023	27	Tarsalis	L	culex	1654

### Data Dictionary
variable	desc	key
trap_id	unique trap id  	FC/LV/BE-#### (GR = Gravid Trap
zone	zone trap located in	NW, NE, SW, SE, LV, BE, BC
zone2	higher level zones collapsing Fort Collins	FC, LV, BE, BC
trap_date	date trap collected	YYYY-MM-DD
year	year of trap collection	YYYY
week	week of trap collection	WW
spp	species of mosquito trapped. pipiens and tarsalis only	Tarsalis, Pipiens
method	type of trap used	L, G
trap_status	species the status of the trap culex = tarsalis/pipiens > 1, malfunction true null not counted in trap #, no mosquitoes = true 0, other spp = true 0	culex, malfunction, no mosquitoes, other species
total	total number of mosquitoes in a trap	


### Total Expected Number 
zone	G	L	Grand Total
BC		5	5
BE		10	10
LV		6	6
NE	4	10	14
NW	2	9	11
SE	3	15	18
SW	1	9	10