

# Read Physician Fee Schedule Data ----------------------------------------

for (y in 2007:2013) {
  pfs.path <- paste0(path.data.pfs,"PFSExport",y,".csv")
  
  pfs.dat=read_csv(pfs.path,
                   skip=1,
                   col_names = c("hcpcs","modifier","description","proc_stat_1",
                                 "mac_locality","nonfac_price","fac_price",
                                 "nonfac_lim_charge","fac_lim_charge",
                                 "conv_factor_1","gpci_work","gpci_pe","gpci_mp",
                                 "proc_stat_2","pctc_1","not_medicare_1","work_rvu",
                                 "nonfac_pe_rvu_flag","nonfac_pe_rvu",
                                 "nonfac_trans_pe_rvu_flag","nonfac_trans_pe_rvu",
                                 "nonfac_pe_rvu_full_flag","nonfac_pe_rvu_full",
                                 "fac_trans_pe_rvu_flag","fac_pe_rvu_flag","fac_pe_rvu",
                                 "fac_trans_pe_rvu",
                                 "fac_pe_rvu_full_flag","fac_pe_rvu_full",
                                 "mp_rvu","nonfac_total","nonfac_trans_total",
                                 "fac_total","fac_trans_total","nonfac_total_full",
                                 "fac_total_full","pctc_2","global","pre_op",
                                 "intra_op","post_op","mult_surg","bilt_surg",
                                 "asst_surg","co_surg","team_surg","phys_supv",
                                 "endobase","conv_factor_2","not_medicare_2",
                                 "diag_imaging","opps_nonfac_pay","opps_fac_pay",
                                 "opps_nonfac_pe","opps_fac_pe","malpractice"),
                   col_types = cols(
                     hcpcs = col_character(),
                     modifier = col_character(),
                     description = col_character(),
                     proc_stat_1 = col_character(),
                     mac_locality = col_logical(),
                     nonfac_price = col_number(),
                     fac_price = col_number(),
                     nonfac_lim_charge = col_number(),
                     fac_lim_charge = col_number(),
                     conv_factor_1 = col_number(),
                     gpci_work = col_logical(),
                     gpci_pe = col_logical(),
                     gpci_mp = col_logical(),
                     proc_stat_2 = col_character(),
                     pctc_1 = col_number(),
                     not_medicare_1 = col_logical(),
                     work_rvu = col_number(),
                     nonfac_pe_rvu_flag = col_logical(),
                     nonfac_pe_rvu = col_number(),
                     nonfac_trans_pe_rvu_flag = col_logical(),
                     nonfac_trans_pe_rvu = col_number(),
                     nonfac_pe_rvu_full_flag = col_logical(),
                     nonfac_pe_rvu_full = col_number(),                     
                     fac_trans_pe_rvu_flag = col_logical(),
                     fac_pe_rvu_flag = col_logical(),
                     fac_pe_rvu = col_number(),
                     fac_trans_pe_rvu = col_number(),
                     fac_pe_rvu_full_flag = col_logical(),
                     fac_pe_rvu_full = col_number(),
                     mp_rvu = col_number(),
                     nonfac_total = col_number(),
                     nonfac_trans_total = col_number(),
                     fac_total = col_number(),
                     fac_trans_total = col_number(),
                     nonfac_total_full = col_number(),
                     fac_total_full = col_number(),
                     pctc_2 = col_number(),
                     global = col_character(),
                     pre_op = col_number(),
                     intra_op = col_number(),
                     post_op = col_number(),
                     mult_surg = col_number(),
                     bilt_surg = col_number(),
                     asst_surg = col_number(),
                     co_surg = col_number(),
                     team_surg = col_number(),
                     phys_supv = col_number(),
                     endobase = col_number(),
                     conv_factor_2 = col_number(),
                     not_medicare_2 = col_logical(),
                     diag_imaging = col_number(),
                     opps_nonfac_pay = col_number(),
                     opps_fac_pay = col_number(),
                     opps_nonfac_pe = col_number(),
                     opps_fac_pe = col_number(),
                     malpractice = col_number()
                   ))
  
  pfs.dat <- pfs.dat %>%
    select(hcpcs, modifier, proc_stat_1, proc_stat_2, work_rvu, nonfac_trans_pe_rvu, 
           nonfac_pe_rvu_full, fac_trans_pe_rvu, fac_pe_rvu_full, mp_rvu,
           conv_factor_1, conv_factor_2, fac_pe_rvu, nonfac_pe_rvu, 
           nonfac_price, opps_nonfac_pay, opps_fac_pay) %>%
    mutate(year = y)
  
  assign(paste0("pfs.",y),pfs.dat)  
}

full.pfs.data=rbind(pfs.2007, pfs.2008, pfs.2009, pfs.2010, pfs.2011, pfs.2012, pfs.2013)


# Clean and remove modifiers -------------------------------------------

full.pfs.data <- full.pfs.data %>%
  mutate(fac_trans_pe_rvu = ifelse(year==2013,fac_pe_rvu_full,fac_trans_pe_rvu)) %>%
  mutate(nonfac_trans_pe_rvu = ifelse(year==2013,nonfac_pe_rvu_full,nonfac_trans_pe_rvu)) %>%
  select(-c(fac_pe_rvu, nonfac_pe_rvu, proc_stat_2, conv_factor_2, opps_fac_pay)) %>%
  rename(proc_stat=proc_stat_1, 
         conv_factor=conv_factor_1,
         Tprice_opps=opps_nonfac_pay) %>%
  mutate(price_fac_trans = (fac_trans_pe_rvu + work_rvu + mp_rvu)*conv_factor,
         price_nonfac_trans = (nonfac_trans_pe_rvu + work_rvu + mp_rvu)*conv_factor,
         price_fac_full = (fac_pe_rvu_full + work_rvu + mp_rvu)*conv_factor,
         price_nonfac_full = (nonfac_pe_rvu_full + work_rvu + mp_rvu)*conv_factor)

opps.data <- full.pfs.data %>%
  filter(is.na(modifier)) %>%
  select(hcpcs, year, Tprice_opps)

clean.pfs.data <- full.pfs.data %>%
  filter(proc_stat != "C" | is.na(proc_stat)) %>%
  select(-Tprice_opps)

clean.pfs.data <- pivot_longer(clean.pfs.data, 
                               cols = starts_with("price"),
                               names_to = "type",
                               names_prefix = "price",
                               values_to = "price") %>%
  filter(modifier!="53" | is.na(modifier))

table(clean.pfs.data$modifier, exclude = NULL)

clean.pfs.data <- clean.pfs.data %>%
  group_by(hcpcs, year) %>%
  mutate(any_mod = sum(!is.na(modifier))) %>%
  ungroup() %>%
  mutate(drop1 = (any_mod>0 & modifier!="26" & type %in% c("_fac_trans","_fac_full")),
         drop2 = (any_mod>0 & !is.na(modifier) & type %in% c("_nonfac_trans","_nonfac_full"))) %>%
  filter(drop1==FALSE & drop2==FALSE) %>%
  select(-modifier) %>%
  distinct()

clean.pfs.data <- clean.pfs.data %>%
  group_by(hcpcs, year, type) %>% mutate(hcpcs_count = seq(n())) %>%
  ungroup() %>%
  filter(hcpcs_count==1) %>%
  select(hcpcs, year, price, proc_stat, any_mod, type)

final.pfs <- pivot_wider(clean.pfs.data, 
                         names_from = "type",
                         values_from = "price",
                         names_prefix = "price") %>%
  left_join(opps.data, by=c("hcpcs", "year")) %>%
  select(-any_mod)


# Incorporate Price Changes -------------------------------------------------
pfs.change <- final.pfs %>%
  filter(year %in% c(2007, 2010)) %>%
  mutate(dprice_fac = price_fac_full - price_fac_trans,
         dprice_nonfac = price_nonfac_full - price_nonfac_trans,
         price_fac_orig = price_fac_full - dprice_fac,
         price_nonfac_orig = price_nonfac_full - dprice_nonfac,
         dpct_price_fac = dprice_fac/price_fac_orig,
         dpct_price_nonfac = dprice_nonfac/price_nonfac_orig,
         dprice_rel = dprice_fac - dprice_nonfac,
         dpct_price_rel = (dprice_fac - dprice_nonfac)/price_nonfac_orig ) %>%
  select(hcpcs, year, dprice_fac, dprice_nonfac, price_fac_orig, price_nonfac_orig,
         dpct_price_fac, dpct_price_nonfac, dprice_rel, dpct_price_rel)

pfs.change.2007 <- pfs.change %>%
  filter(year==2007) %>%
  rename_at(vars(contains("_")), list(~ paste0(.,"_2007"))) %>%
  select(-year)

pfs.change.2010 <- pfs.change %>%
  filter(year==2010) %>%
  rename_at(vars(contains("_")), list(~ paste0(.,"_2010"))) %>%
  select(-year)


final.pfs.data <- final.pfs %>%
  left_join(pfs.change.2007, by=c("hcpcs")) %>%
  left_join(pfs.change.2010, by=c("hcpcs"))


write_tsv(final.pfs.data,path=paste('data/final_pfs_data.txt'),append=FALSE,col_names=TRUE)
write_rds(final.pfs.data,'data/final_pfs_data.rds')

