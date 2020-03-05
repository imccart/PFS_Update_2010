

# Read APC Data -----------------------------------------------------------

for (y in 2007:2013) {
  apc.path <- paste0(path.data.apc,"Y",y,"/January ",y,"B.xlsx")

  if (y==2007 | y==2012) {
    apc.dat=read_xlsx(apc.path,
                     skip=1,
                     col_names=c("hcpcs","description","ci","si","apc","relative_weight","payment_rate",
                                 "natl_unadj_copay","min_unadj_copay","change"))
  } else if (y==2008) {
    apc.dat=read_xlsx(apc.path,
                      skip=1,
                      col_names=c("hcpcs","description","ci","si","apc","relative_weight","payment_rate",
                                  "natl_unadj_copay","min_unadj_copay"))
    apc.dat <- apc.dat %>%
      mutate(change = NA)
    
  } else {
    apc.dat=read_xlsx(apc.path,
                      skip=1,
                      col_names=c("hcpcs","description","si","apc","relative_weight","payment_rate",
                                  "natl_unadj_copay","min_unadj_copay","change"))
  }
  
  apc.dat <- apc.dat %>%
    mutate(apc = str_sub(str_trim(apc, side = "both"),-4,4)) %>%
    mutate(si = str_trim(si, side = "both")) %>%
    mutate(change = case_when(
      change == "*" ~ 1,
      is.na(change) ~ 0
    )) %>%
    mutate(si = case_when(
      si == "A" ~ "A - Services Paid under Fee Schedule or Payment System other than OPPS",
      si == "B" ~ "B - Codes Not Recognized by OPPS when submitted on Outpatient Hospital Part B Bill Type (12x/13x)",
      si == "C" ~ "C - Inpatient Procedures, not paid under OPPS",
      si == "D" ~ "D - Discontinued Codes",
      si == "E" ~ "E - Non-Covered Service, not paid under OPPS",
      si == "F" ~ "F - Corneal, CRNA and Hepatitis B",
      si == "G" ~ "G - Pass-Through Drugs and Biologicals",
      si == "H" ~ "H - Pass-Through Device Categories",
      si == "K" ~ "K - Nonpass-Through Drugs and Nonimplantable Biologicals, Including Therapeutic Radiopharmaceuticals",
      si == "L" ~ "L - Influenza Vaccine; Pneumococcal Pneumonia Vaccine",
      si == "M" ~ "M - Items and Services Not Billable to the Fiscal Intermediary/MAC",
      si == "N" ~ "N - Items and Services Packaged into APC Rates",
      si == "P" ~ "P - Partial Hospitalization",
      si == "Q1" ~ "Q1 - STVX-Packaged Codes",
      si == "Q2" ~ "Q2 - T-Packaged Codes",
      si == "Q3" ~ "Q3 - Codes That May Be Paid Through a Composite APC",
      si == "R" ~ "R - Blood and Blood Products",
      si == "S" ~ "S - Significant Procedure, Not Discounted When Multiple",
      si == "T" ~ "T - Significant Procedure, Multiple Reduction Applies",
      si == "U" ~ "U - Brachytherapy Sources",
      si == "V" ~ "V - Clinic or Emergency Department Visit",
      si == "X" ~ "X - Ancillary Services",
      si == "Y" ~ "Y - Non-Implantable Durable Medical Equipment"
    )) %>%
    select(hcpcs, description, si, apc, relative_weight, payment_rate,
           natl_unadj_copay, min_unadj_copay, change) %>%
    mutate(year = y)
  
  assign(paste0("apc.",y),apc.dat)
}
full.apc.data=rbind(apc.2007, apc.2008, apc.2009, apc.2010, apc.2011, apc.2012, apc.2013)

## check length of apc string
table(full.apc.data %>% mutate(apc_length = str_count(apc)) %>% select(apc_length))


# Read MedPac APC Data ----------------------------------------------------
## These data provide a crosswalk from APCs to MedPac Types

medpac.path <- paste0(path.data.apc,"MedpacAPCs.xlsx")
medpac.dat=read_xlsx(medpac.path,
                     skip=1,
                     col_names=c("apc","apc_description","medpac_type"))

medpac.dat <- medpac.dat %>%
  mutate(apc = str_pad(apc,width=4,side="left",pad="0")) %>%
  select(apc, medpac_type)

## check length of apc string
table(medpac.dat %>% mutate(apc_length = str_count(apc)) %>% select(apc_length))



# Join Full APC and MedPac APC Data ---------------------------------------
full.apc.data <- full.apc.data %>%
  left_join(medpac.dat, by="apc")


write_tsv(full.apc.data,path='data/full_apc_data.txt',append=FALSE,col_names=TRUE)
write_rds(full.apc.data,'data/full_apc_data.rds')
