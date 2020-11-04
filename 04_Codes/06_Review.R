# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  WYETH Ca
# Purpose:      Review
# programmer:   Zhe Liu
# Date:         2020-11-03
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Projection rate ----
raw.sum <- raw.total %>% 
  group_by(province, city, quarter, packid) %>% 
  summarise(units_raw = sum(units, na.rm = TRUE), 
            sales_raw = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

imp.sum <- imp.total %>% 
  group_by(province, city, quarter, packid) %>% 
  summarise(units_imp = sum(units, na.rm = TRUE), 
            sales_imp = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

proj.sum <- proj.price %>% 
  group_by(province, city, quarter, packid) %>% 
  summarise(units_proj = sum(units, na.rm = TRUE), 
            sales_proj = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

proj.rate <- raw.sum %>% 
  full_join(imp.sum, by = c('province', 'city', 'quarter', 'packid')) %>% 
  full_join(proj.sum, by = c('province', 'city', 'quarter', 'packid')) %>% 
  mutate(sales_imp_rate = sales_imp / sales_raw, 
         sales_proj_rate = sales_proj / sales_imp) %>% 
  left_join(pack.info, by = "packid")

write.xlsx(proj.rate, '05_Internal_Review/Projection_Rate.xlsx')


##---- Price check ----
price.check <- wyeth.delivery %>% 
  mutate(price = round(Sales / Units)) %>% 
  distinct(City, Pack_ID, Date, price)
  
write.xlsx(price.check, '05_Internal_Review/Price_Check.xlsx')


