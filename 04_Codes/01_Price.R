# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ MAX Price
# Purpose:      AZ Price
# programmer:   Zhe Liu
# Date:         2020-10-14
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin raw data ----
az.max.raw <- read.csv('02_Inputs/AZ_MAX_201701-202006.csv', encoding = 'UTF-8')
mapping.raw <- read.xlsx('02_Inputs/az+sanofi_待清洗测试版20200211.xlsx', sheet = 5)
ims.chpa <- read.xlsx('02_Inputs/ims_chpa_to20Q2.xlsx', startRow = 4, cols = 1:21)

##---- Cleaning ----
# packid.mapping <- ims.chpa %>% 
#   distinct(Pack_ID, Pck_Desc) %>% 
#   mutate(locate = stri_locate_last(Pck_Desc, regex = "\\s")[,1], 
#          Pack = as.integer(str_squish(stri_sub(Pck_Desc, locate, nchar(Pck_Desc)))), 
#          prodid = stri_sub(Pack_ID, 1, 5)) %>% 
#   distinct(Pack_ID, prodid, Pack)

mapping <- mapping.raw[1:25] %>% 
  distinct(Pack_ID = `PFC（来自于文博的外部版本，文博版本的变动需要加到这里）`, min2) %>% 
  mutate(Pack_ID = as.numeric(Pack_ID), 
         Pack_ID = stri_pad_left(Pack_ID, 7, 0)) %>% 
  filter(!is.na(Pack_ID), Pack_ID != '0000000') %>% 
  filter(Pack_ID != '6590104')

chk <- mapping %>% add_count(min2, Molecule) %>% filter(n > 1)
chk1 <- mapping.raw[1:25] %>% 
  filter(min2 == '二甲双胍|TAB|250MG|48|贵州天安药业股份有限公司')

az.max.price <- az.max.raw %>% 
  left_join(mapping, by = c('Prod_Name' = 'min2')) %>% 
  filter(!is.na(Pack_ID)) %>% 
  mutate(Quarter = stri_sub(Date, 5, 6), 
         Quarter = if_else(Quarter %in% c('01', '02', '03'), 'Q1', 
                           if_else(Quarter %in% c('04', '05', '06'), 'Q2', 
                                   if_else(Quarter %in% c('07', '08', '09'), 'Q3', 
                                           if_else(Quarter %in% c('10', '11', '12'), 'Q4', 
                                                   NA_character_)))), 
         Quarter = stri_paste(stri_sub(Date, 1, 4), Quarter)) %>% 
  separate(Prod_Name, c('Prod_Name', 'Dosage', 'Spec', 'Pack', 'Corp'), sep = '[|]') %>% 
  mutate(Pack = as.integer(Pack)) %>% 
  group_by(Province, City, Quarter, Pack, Pack_ID) %>% 
  summarise(Predict_Sales = sum(Predict_Sales, na.rm = TRUE), 
            Predict_Unit = sum(Predict_Unit, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Price = Predict_Sales / Predict_Unit * Pack)

write.csv(az.max.price, '03_Outputs/AZ_MAX_Price.csv')







