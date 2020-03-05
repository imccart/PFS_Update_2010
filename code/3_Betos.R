

# Read BETOS Data ---------------------------------------------------------

for (y in 2010:2014) {
  y.abb <- y-2000
  betos.path <- paste0(path.data.betos,"betpuf",y.abb,"/betpuf",y.abb,".txt")
  
  betos.dat <- read_csv(betos.path, col_names = FALSE)
  
  betos.dat <- betos.dat %>%
    mutate(hcpcs = str_trim(str_sub(X1,1,5), side="both"),
           betos = str_trim(str_sub(X1,7,9), side="both"),
           year = y) %>%
    select(-X1)
  
  assign(paste0("betos.",y),betos.dat)  
}

full.betos.data <- rbind(betos.2010, betos.2011, betos.2012, betos.2013, betos.2014)


# Clean BETOS data --------------------------------------------------------

unique.betos <- full.betos.data %>%
  group_by(hcpcs, betos) %>% 
  mutate(hcpcs_count = seq(n())) %>%
  filter(hcpcs_count==1) %>%
  select(hcpcs, betos) %>% ungroup() %>%
  filter(betos!="" & !is.na(betos)) %>%
  group_by(hcpcs) %>% mutate(betos_count=n()) %>%
  filter(betos_count==1) %>% ungroup() %>%
  select(hcpcs, betos)



# Merge BETOS Description  -------------------------------------------------

betos.desc <- read_xlsx(paste0(path.data.betos,"betos descs.xlsx"))
final.betos.data <- unique.betos %>%
  left_join(betos.desc, by="betos")


write_tsv(final.betos.data,path='data/final_betos_data.txt',append=FALSE,col_names=TRUE)
write_rds(final.betos.data,'data/final_betos_data.rds')
