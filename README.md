# Physician Fee Schedule 2010 Update

This repo provides the necessary code and data to estimate price changes for physician fees generated by the 2010 physician fee schedule (PFS) update. [Dranove and Ody (2019)](https://www.aeaweb.org/articles?id=10.1257/pol.20170020) discuss the nature of the price change in detail, along with a set of Stata do files, data, and some documentation. This repo is largely a replication of their efforts but limited only to estimated price changes for a given procedure. To form a physician-level measure of the magnitude of the update, you will need some measure of quantity per physician. See Dranove and Ody (2019) for some ideas on how to do this without claims data. Below, I include a brief section on the [Discussion of price changes](https://github.com/imccart/PFS_Update_2010#discussion-of-price-changes)


## Master Code File
Each of the relevant data sources are available individually and discussed in more detail below. Just scroll down to the [Raw Data Sources](https://github.com/imccart/PFS_Update_2010#raw-data-sources-and-code). All of these files can also be called as part of a master file,  [_FinalData.R](https://github.com/imccart/PFS_Update_2010/blob/master/code/_FinalData.R). I name these files so that they appear (by default) in Windows folders in a way that makes sense to me. So the master file begins with an underscore, and the individual files are numbered. This is just personal preference but helps me to keep things in order.

## Raw Data Sources and Code
All of the raw data are publicly available; however, I have been unable to find the identical datasets from CMS that match to Dranove and Ody (2019). In the case of BETOS crosswalks, CMS no longer provides these data online at all. Nonetheless, links (where available) to all datasets are provided below. Otherwise the data are available from [Dranove and Ody (2019) Supplemental Materials](https://www.aeaweb.org/articles?id=10.1257/pol.20170020).

1. Ambulatory Payment Classifications (APC): These are the fees paid by CMS to outpatient facilities as part of the Outpatient Prospective Payment System. Each APC consists of a group of similar services, which are themselves identified by groups of HCPCS codes. I have only found data for the top 30 APCs available online, [CMS APC data](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Outpatient); however, Dranove and Ody (2019) provide these data from 2011 to 2014 as part of their supplemental appendix. They provide the data in a different format than what is currently available from CMS.

R code to read and clean the APC data from Dranove and Ody (2019) is avalable [here](code/1_APC_Data.R).

2. Physician Fee Schedule (PFS): These are the fees paid by CMS to physicians. These data are available from CMS [here](https://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/PhysicianFeeSched/PFS-Relative-Value-Files). Dranove and Ody (2019) also provide these data in a similar format as part of their supplemental materials. Note that the CMS files are not really organized in an easy-to-use way. When downloading from CMS, I've used the "D" versions of each file in the prior year. For example, the 2012D file reflects updates as of October 2012. I take this as
the relevant data for 2013. I do this because the federal fiscal year for 2013 begins in October of 2012. This also appears to match the files from Dranove and Ody (2019).

R code to read and clean the PFS data from Dranove and Ody (2019) is available [here](code/2_Physician_Prices.R).

3. BETOS Data. BETOS stands for the Berenson-Eggers Type of Service and is a higher-level categorization of services than HCPCS codes. Dranove and Ody (2019) use these to crosswalk with HCPCS codes, ultimately to identify services that are substitutable between office and facility settings. CMS does not appear to host these crosswalk files online anymore; however, they are available as part of a separate GitHub Repository [here]((https://github.com/chse-ohsu/PublicUseData/tree/master/BETOS). The data are also provided as part of the supplemental material for Dranove and Ody (2019). 

R code to read and clean the BETOS data from Dranove and Ody (2019) is available [here](code/3_Betos.R).


## Discussion of price changes




