# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  WYETH Ca
# Purpose:      Readin and format raw data
# programmer:   Zhe Liu
# Date:         2020-10-23
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
# PCHC code
pchc.universe.raw <- read.xlsx("02_Inputs/Universe_PCHCCode_20201019.xlsx", sheet = "PCHC")

pchc.universe1 <- pchc.universe.raw %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc = first(PCHC_Code), 
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup()

pchc.universe2 <- pchc.universe.raw %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc = first(PCHC_Code), 
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup()

pchc.mapping <- bind_rows(pchc.universe1, pchc.universe2) %>% 
  distinct(province, city, district, hospital, pchc)

# pack info
ims.prod <- fread("02_Inputs/cn_prod_ref_201903_1.txt") %>% 
  setDF() %>% 
  mutate(Pack_Id = str_pad(Pack_Id, 7, "left", pad = "0")) %>% 
  select(Pack_Id, NFC123_Code)

ims.raw <- read.xlsx("02_Inputs/ims_chpa_to19Q4.xlsx")

pack.info <- ims.raw[, 1:21] %>% 
  distinct() %>% 
  filter(!is.na(Pack_Id)) %>% 
  left_join(ims.prod, by = "Pack_Id") %>% 
  select(packid = Pack_ID, Corp_ID, Corp_Desc, MNF_TYPE, MnfType_Desc, 
         Mnf_Desc, ATC4_Code, NFC123_Code, Prd_desc, Pck_Desc, 
         Molecule_Desc)

# SKU
sku.info <- read.xlsx('02_Inputs/CA_ahbjjssdzj181920Q1Q2_sku_packid_price_ims_chk_new.xlsx') %>% 
  mutate(Prd_desc_ZB = if_else(is.na(Prd_desc_ZB), '无', Prd_desc_ZB))

# market definition
market.def <- read_xlsx("02_Inputs/钙尔奇 招标数据缺失产品汇总.xlsx", sheet = "市场定义同分子下的packid")
market.def <- sort(c(unique(market.def$Pack_ID), "0242704"))

# target city
target.city <- c("北京", "广州", "杭州", "南京", "宁波", "上海", "苏州")


##---- Raw data ----
# 广东
raw.gd1 <- read.csv('02_Inputs/data/gzs 20q1.csv') %>% 
  mutate(date = gsub('[-]', '0', period), 
         quarter_m = stri_sub(date, 5, 6)) %>% 
  distinct(year = stri_sub(date, 1, 4), 
           quarter = ifelse(quarter_m %in% c("01", "02", "03"), 
                            stri_paste(year, "Q1"), 
                            ifelse(quarter_m %in% c("04", "05", "06"), 
                                   stri_paste(year, "Q2"), 
                                   ifelse(quarter_m %in% c("07", "08", "09"), 
                                          stri_paste(year, "Q3"), 
                                          ifelse(quarter_m %in% c("10", "11", "12"), 
                                                 stri_paste(year, "Q4"), 
                                                 year)))), 
           date, 
           province = '广东', 
           city = '广州', 
           hospital = name, 
           packid = stri_pad_left(pfc, 7, 0), 
           units = unit, 
           sales = value) %>% 
  left_join(pchc.mapping, by = c('province', 'city', 'hospital')) %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

raw.gd2 <- read.xlsx('02_Inputs/data/2jd_date.xlsx') %>% 
  mutate(quarter_m = stri_sub(date, 5, 6)) %>% 
  distinct(year = stri_sub(date, 1, 4), 
           quarter = ifelse(quarter_m %in% c("01", "02", "03"), 
                            stri_paste(year, "Q1"), 
                            ifelse(quarter_m %in% c("04", "05", "06"), 
                                   stri_paste(year, "Q2"), 
                                   ifelse(quarter_m %in% c("07", "08", "09"), 
                                          stri_paste(year, "Q3"), 
                                          ifelse(quarter_m %in% c("10", "11", "12"), 
                                                 stri_paste(year, "Q4"), 
                                                 year)))), 
           date = stri_sub(date, 1, 6), 
           province = '广东', 
           city = '广州', 
           hospital = name, 
           packid = stri_pad_left(pfc, 7, 0), 
           units = unit, 
           sales = value) %>% 
  left_join(pchc.mapping, by = c('province', 'city', 'hospital')) %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

# 安徽北京江苏山东浙江
raw.ahbjjssdzj.pack <- read.xlsx('02_Inputs/data/CA_ahbjjssdzj_CHC_2020Q1Q2.xlsx')
raw.ahbjjssdzj.non <- read.xlsx('02_Inputs/data/CA_ahbjjssdzj_2020Q1Q2.xlsx')

raw.ahbjjssdzj <- raw.ahbjjssdzj.pack %>% 
  left_join(sku.info[, 1:7], 
            by = c('Molecule_Desc_ZB', 'Prd_desc_ZB', 'SPEC' = 'SPEC_ZB', 
                   'Dosage' = 'Dosage_ZB', 'PACK' = 'PACK_ZB', 'Mnf_Desc_ZB')) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           packid = stri_pad_left(Pack_ID, 7, 0), 
           units = Volume, 
           sales = Value) %>% 
  left_join(pchc.mapping, by = c('province', 'city', 'district', 'hospital'))

# bind
raw.total <- bind_rows(raw.ahbjjssdzj, raw.gd1, raw.gd2) %>% 
  filter(!is.na(pchc), !is.na(packid), 
         packid %in% market.def, city %in% target.city) %>% 
  arrange(date, province, city, district) %>% 
  group_by(pchc) %>% 
  mutate(province = first(na.omit(province)),
         city = first(na.omit(city)),
         district = first(na.omit(district))) %>% 
  ungroup() %>% 
  group_by(year, date, quarter, province, city, district, pchc, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(raw.total, '03_Outputs/01_Ca_Raw_2020Q1Q2.xlsx')


