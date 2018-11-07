###################################################################################
##### Estimating mortality rates of various chronic liver disease aetiologies #####
###################################################################################

rm(list=ls())

library(feather)
library(caret)

setwd("/Users/matthewkeys/Desktop/CRES/Liver-Health-Project-2/")

# Load Data 
data = read.csv('/Users/matthewkeys/Desktop/CRES/Liver-Health-Project-2/final_data.csv')

View(data)

## Final Preprocessing 

# Fix column names 
names(data) <- c("Index", "Gender", "dAll.Ages", "dLess.Than.1.yo", "d1", "d2", 
                 "d3", "d4", "d5.to.9", "d10.to.14",
                 "d15.to.19", "d20.to.24", "d25.to.29", "d30.to.34",
                 "d35.to.39", "d40.to.44", "d45.to.49",
                 "d50.to.54", "d55.to.59", "d60.to.64", "d65.to.69", 
                 "d70.to.74", "d75.to.79", "d80.to.84", 
                 "d85.to.89", "d90.to.94", "dGreater.Than.95", "Death.Code", "Year",
                 "All.Ages", "0.to.4", "5.to.9", "10.to.14", "15.to.19", "20.to.24", "25.to.29",         
                 "30.to.34", "35.to.39", "40.to.44", "45.to.49", "50.to.54", "55.to.59", 
                 "60.to.64", "65.to.69", "70.to.74", "75.to.79", "80.to.84", "85.to.89", 
                 "90.to.94", "95.to.99", "100.or.More")

# Drop NAs
data = data[complete.cases(data), ]
  
# Fix age bands, make both datasets compatible 
data['Greater.Than.95'] <- data['95.to.99'] + data['100.or.More']
data['d0.to.4'] <- data['dLess.Than.1.yo'] + data['d1'] + data['d2'] + data['d3'] + data['d4'] 

# Drop unnecessary columns 
data = subset(data, select=-c(dLess.Than.1.yo, d1, d2, d3, d4, `95.to.99`,  `100.or.More`))

## Make aetiology groups 
HBV <- c('B16',    # Acute Hepatitis B 
         'B17.0',  # Acute infection (superinfection) by delta agent in the hepatitis B carrier
         'B18.0',  # Chronic Viral Hepaitis B with delta agent
         'B18.1'   # Chronic Viral Hepaitis B without delta agent
         ) 

#^^^ Note, there is no code for unspecified viral hepatitis B, which is code B19.1 in ICD-10 documentation. 

HCV <- c('B17.1',  # Acute Hepatitis C 
         'B18.2'   # Chronic Viral Hepatitis C 
         ) 

#^^^ Similarly, there is no code for unspecified viral hepatitis C, which is B19.2 in the ICD-10 documentation. 

ALD <- c('K70.0',   # Alcoholic Adipose Liver (Fatty liver) 
         'K70.1',   # Alcholic Hepatitis 
         'K70.2',   # Alcholic Fibrosis and Sclerosis of the liver 
         'K70.3',   # Alcholic liver cirrhosis 
         'K70.4',   # Alchoholic hepatic insufficiency 
         'K70.9'    # Alcholic liver disease, unspecified
         ) 

NAFLD <- c('K76.0',  # Fatty degeneration of the liver, not elsewhere classified
           'K75.8'   # Other inflammatory diseases of the liver, specified. 
           )

#^^^ There is noexact code for Nonalcoholic steatohepatitis. I would guess 
# 'K75.8 Other inflammatory diseases of the liver, specified' is our equivalent. 

## Subset & Group Data 

# Remove all white space from the dataframe 
# data <- data.frame(lapply(data, trimws), stringsAsFactors = FALSE)

# Remove Population Data so we can collapse the code data 
population_vars1 = c("Year", "Gender", "All.Ages", "0.to.4", "5.to.9", "10.to.14", 
                     "15.to.19", "20.to.24", "25.to.29", "30.to.34", "35.to.39",
                     "40.to.44", "45.to.49", "50.to.54", "55.to.59", "60.to.64",
                     "65.to.69", "70.to.74", "75.to.79", "80.to.84", "85.to.89",      
                     "90.to.94", "Greater.Than.95")

population_vars2 = c("All.Ages", "0.to.4", "5.to.9", "10.to.14", 
                     "15.to.19", "20.to.24", "25.to.29", "30.to.34", "35.to.39",
                     "40.to.44", "45.to.49", "50.to.54", "55.to.59", "60.to.64",
                     "65.to.69", "70.to.74", "75.to.79", "80.to.84", "85.to.89",      
                     "90.to.94", "Greater.Than.95")


pop <- data[, population_vars1]
data <- data[, !(colnames(data) %in% population_vars2)]

# Subset 
NAFLD_data <- data[data$Death.Code %in% NAFLD, ]
ALD_data <- data[data$Death.Code %in% ALD, ]
HBV_data <- data[data$Death.Code %in% HBV, ]
HCV_data <- data[data$Death.Code %in% HCV, ]

# Collapse data on death codes in each disease category (sum the rows) 
cols_to_aggregate = c("dAll.Ages", "d0.to.4", "d5.to.9", "d10.to.14", "d15.to.19", "d20.to.24", "d25.to.29",     
                      "d30.to.34", "d35.to.39", "d40.to.44", "d45.to.49", "d50.to.54", "d55.to.59", "d60.to.64",      
                      "d65.to.69", "d70.to.74", "d75.to.79", "d80.to.84", "d85.to.89", "d90.to.94", "dGreater.Than.95")
  
agg_NAFLD_data <- aggregate(NAFLD_data[,cols_to_aggregate], 
                            by=list(Gender = NAFLD_data$Gender,
                                    Year = NAFLD_data$Year), FUN=sum)

agg_ALD_data <- aggregate(ALD_data[,cols_to_aggregate], 
                            by=list(Gender = ALD_data$Gender,
                                    Year = ALD_data$Year), FUN=sum)

agg_HBV_data <- aggregate(HBV_data[,cols_to_aggregate], 
                            by=list(Gender = HBV_data$Gender,
                                    Year = HBV_data$Year), FUN=sum)

agg_HCV_data <- aggregate(HCV_data[,cols_to_aggregate], 
                          by=list(Gender = HCV_data$Gender,
                                  Year = HCV_data$Year), FUN=sum)

## Compute Age-Population Proportions 
pop$`0.to.4%` <- pop$`0.to.4` / pop$`All.Ages`
pop$`5.to.9%` <- pop$`5.to.9` / pop$`All.Ages`
pop$`10.to.14%` <- pop$`10.to.14` / pop$`All.Ages`
pop$`15.to.19%` <- pop$`15.to.19` / pop$`All.Ages`
pop$`20.to.24%` <- pop$`20.to.24` / pop$`All.Ages`
pop$`25.to.29%` <- pop$`25.to.29` / pop$`All.Ages`
pop$`30.to.34%` <- pop$`30.to.34` / pop$`All.Ages`
pop$`35.to.39%` <- pop$`35.to.39` / pop$`All.Ages`
pop$`40.to.44%` <- pop$`40.to.44` / pop$`All.Ages`
pop$`45.to.49%` <- pop$`45.to.49` / pop$`All.Ages`
pop$`50.to.54%` <- pop$`50.to.54` / pop$`All.Ages`
pop$`55.to.59%` <- pop$`55.to.59` / pop$`All.Ages`
pop$`60.to.64%` <- pop$`60.to.64` / pop$`All.Ages`
pop$`65.to.69%` <- pop$`65.to.69` / pop$`All.Ages`
pop$`70.to.74%` <- pop$`70.to.74` / pop$`All.Ages`
pop$`75.to.79%` <- pop$`75.to.79` / pop$`All.Ages`
pop$`80.to.84%` <- pop$`80.to.84` / pop$`All.Ages`
pop$`85.to.89%` <- pop$`85.to.89` / pop$`All.Ages`
pop$`90.to.94%` <- pop$`90.to.94` / pop$`All.Ages`
pop$`Greater.Than.95%` <- pop$`Greater.Than.95` / pop$`All.Ages`

# Drop duplicates 
popp = pop[!duplicated(pop), ]

# Merge with mortality datasetes
agg_NAFLD_data = merge(agg_NAFLD_data, popp, by=c('Gender', 'Year'))
agg_ALD_data = merge(agg_ALD_data, popp, by=c('Gender', 'Year'))
agg_HBV_data = merge(agg_HBV_data, popp, by=c('Gender', 'Year'))
agg_HCV_data = merge(agg_HCV_data, popp, by=c('Gender', 'Year'))

# Compute Age Specific Mortalities Per 100,000 
agg_NAFLD_data$`0.to.4.mortality%` <- agg_NAFLD_data$`d0.to.4`/agg_NAFLD_data$`0.to.4`*100000
agg_NAFLD_data$`5.to.9.mortality%` <- agg_NAFLD_data$`d5.to.9`/agg_NAFLD_data$`5.to.9`*100000
agg_NAFLD_data$`10.to.14.mortality%` <- agg_NAFLD_data$`d10.to.14`/agg_NAFLD_data$`10.to.14` *100000
agg_NAFLD_data$`15.to.19.mortality%` <- agg_NAFLD_data$`d15.to.19`/agg_NAFLD_data$`15.to.19`*100000
agg_NAFLD_data$`20.to.24.mortality%` <- agg_NAFLD_data$`d20.to.24`/agg_NAFLD_data$`20.to.24`*100000
agg_NAFLD_data$`25.to.29.mortality%` <- agg_NAFLD_data$`d25.to.29`/agg_NAFLD_data$`25.to.29`*100000
agg_NAFLD_data$`30.to.34.mortality%` <- agg_NAFLD_data$`d30.to.34`/agg_NAFLD_data$`30.to.34`*100000
agg_NAFLD_data$`35.to.39.mortality%` <- agg_NAFLD_data$`d35.to.39`/agg_NAFLD_data$`35.to.39`*100000
agg_NAFLD_data$`40.to.44.mortality%` <- agg_NAFLD_data$`d40.to.44`/agg_NAFLD_data$`40.to.44`*100000
agg_NAFLD_data$`45.to.49.mortality%` <- agg_NAFLD_data$`d45.to.49`/agg_NAFLD_data$`45.to.49`*100000
agg_NAFLD_data$`50.to.54.mortality%` <- agg_NAFLD_data$`d50.to.54`/agg_NAFLD_data$`50.to.54`*100000
agg_NAFLD_data$`55.to.59.mortality%` <- agg_NAFLD_data$`d55.to.59`/agg_NAFLD_data$`55.to.59`*100000
agg_NAFLD_data$`60.to.64.mortality%` <- agg_NAFLD_data$`d60.to.64`/agg_NAFLD_data$`60.to.64`*100000
agg_NAFLD_data$`65.to.69.mortality%` <- agg_NAFLD_data$`d65.to.69`/agg_NAFLD_data$`65.to.69`*100000
agg_NAFLD_data$`70.to.74.mortality%` <- agg_NAFLD_data$`d70.to.74`/agg_NAFLD_data$`70.to.74`*100000
agg_NAFLD_data$`75.to.79.mortality%` <- agg_NAFLD_data$`d75.to.79`/agg_NAFLD_data$`75.to.79`*100000
agg_NAFLD_data$`80.to.84.mortality%` <- agg_NAFLD_data$`d80.to.84`/agg_NAFLD_data$`80.to.84`*100000
agg_NAFLD_data$`85.to.89.mortality%` <- agg_NAFLD_data$`d85.to.89`/agg_NAFLD_data$`85.to.89`*100000
agg_NAFLD_data$`90.to.94.mortality%` <- agg_NAFLD_data$`d90.to.94`/agg_NAFLD_data$`90.to.94`*100000
agg_NAFLD_data$`Greater.Than.95.mortality%` <- agg_NAFLD_data$`dGreater.Than.95`/agg_NAFLD_data$`Greater.Than.95`*100000

agg_ALD_data$`0.to.4.mortality%` <- agg_ALD_data$`d0.to.4`/agg_ALD_data$`0.to.4`*100000
agg_ALD_data$`5.to.9.mortality%` <- agg_ALD_data$`d5.to.9`/agg_ALD_data$`5.to.9`*100000
agg_ALD_data$`10.to.14.mortality%` <- agg_ALD_data$`d10.to.14`/agg_ALD_data$`10.to.14`*100000
agg_ALD_data$`15.to.19.mortality%` <- agg_ALD_data$`d15.to.19`/agg_ALD_data$`15.to.19`*100000
agg_ALD_data$`20.to.24.mortality%` <- agg_ALD_data$`d20.to.24`/agg_ALD_data$`20.to.24`*100000
agg_ALD_data$`25.to.29.mortality%` <- agg_ALD_data$`d25.to.29`/agg_ALD_data$`25.to.29`*100000
agg_ALD_data$`30.to.34.mortality%` <- agg_ALD_data$`d30.to.34`/agg_ALD_data$`30.to.34`*100000
agg_ALD_data$`35.to.39.mortality%` <- agg_ALD_data$`d35.to.39`/agg_ALD_data$`35.to.39`*100000
agg_ALD_data$`40.to.44.mortality%` <- agg_ALD_data$`d40.to.44`/agg_ALD_data$`40.to.44`*100000
agg_ALD_data$`45.to.49.mortality%` <- agg_ALD_data$`d45.to.49`/agg_ALD_data$`45.to.49`*100000
agg_ALD_data$`50.to.54.mortality%` <- agg_ALD_data$`d50.to.54`/agg_ALD_data$`50.to.54`*100000
agg_ALD_data$`55.to.59.mortality%` <- agg_ALD_data$`d55.to.59`/agg_ALD_data$`55.to.59`*100000
agg_ALD_data$`60.to.64.mortality%` <- agg_ALD_data$`d60.to.64`/agg_ALD_data$`60.to.64`*100000
agg_ALD_data$`65.to.69.mortality%` <- agg_ALD_data$`d65.to.69`/agg_ALD_data$`65.to.69`*100000
agg_ALD_data$`70.to.74.mortality%` <- agg_ALD_data$`d70.to.74`/agg_ALD_data$`70.to.74`*100000
agg_ALD_data$`75.to.79.mortality%` <- agg_ALD_data$`d75.to.79`/agg_ALD_data$`75.to.79`*100000
agg_ALD_data$`80.to.84.mortality%` <- agg_ALD_data$`d80.to.84`/agg_ALD_data$`80.to.84`*100000
agg_ALD_data$`85.to.89.mortality%` <- agg_ALD_data$`d85.to.89`/agg_ALD_data$`85.to.89`*100000
agg_ALD_data$`90.to.94.mortality%` <- agg_ALD_data$`d90.to.94`/agg_ALD_data$`90.to.94`*100000
agg_ALD_data$`Greater.Than.95.mortality%` <- agg_ALD_data$`dGreater.Than.95`/agg_ALD_data$`Greater.Than.95`*100000

agg_HBV_data$`0.to.4.mortality%` <- agg_HBV_data$`d0.to.4`/agg_HBV_data$`0.to.4`*100000
agg_HBV_data$`5.to.9.mortality%` <- agg_HBV_data$`d5.to.9`/agg_HBV_data$`5.to.9`*100000
agg_HBV_data$`10.to.14.mortality%` <- agg_HBV_data$`d10.to.14`/agg_HBV_data$`10.to.14`*100000
agg_HBV_data$`15.to.19.mortality%` <- agg_HBV_data$`d15.to.19`/agg_HBV_data$`15.to.19`*100000
agg_HBV_data$`20.to.24.mortality%` <- agg_HBV_data$`d20.to.24`/agg_HBV_data$`20.to.24`*100000
agg_HBV_data$`25.to.29.mortality%` <- agg_HBV_data$`d25.to.29`/agg_HBV_data$`25.to.29`*100000
agg_HBV_data$`30.to.34.mortality%` <- agg_HBV_data$`d30.to.34`/agg_HBV_data$`30.to.34`*100000
agg_HBV_data$`35.to.39.mortality%` <- agg_HBV_data$`d35.to.39`/agg_HBV_data$`35.to.39`*100000
agg_HBV_data$`40.to.44.mortality%` <- agg_HBV_data$`d40.to.44`/agg_HBV_data$`40.to.44`*100000
agg_HBV_data$`45.to.49.mortality%` <- agg_HBV_data$`d45.to.49`/agg_HBV_data$`45.to.49`*100000
agg_HBV_data$`50.to.54.mortality%` <- agg_HBV_data$`d50.to.54`/agg_HBV_data$`50.to.54`*100000
agg_HBV_data$`55.to.59.mortality%` <- agg_HBV_data$`d55.to.59`/agg_HBV_data$`55.to.59`*100000
agg_HBV_data$`60.to.64.mortality%` <- agg_HBV_data$`d60.to.64`/agg_HBV_data$`60.to.64`*100000
agg_HBV_data$`65.to.69.mortality%` <- agg_HBV_data$`d65.to.69`/agg_HBV_data$`65.to.69`*100000
agg_HBV_data$`70.to.74.mortality%` <- agg_HBV_data$`d70.to.74`/agg_HBV_data$`70.to.74`*100000
agg_HBV_data$`75.to.79.mortality%` <- agg_HBV_data$`d75.to.79`/agg_HBV_data$`75.to.79`*100000
agg_HBV_data$`80.to.84.mortality%` <- agg_HBV_data$`d80.to.84`/agg_HBV_data$`80.to.84`*100000
agg_HBV_data$`85.to.89.mortality%` <- agg_HBV_data$`d85.to.89`/agg_HBV_data$`85.to.89`*100000
agg_HBV_data$`90.to.94.mortality%` <- agg_HBV_data$`d90.to.94`/agg_HBV_data$`90.to.94`*100000
agg_HBV_data$`Greater.Than.95.mortality%` <- agg_HBV_data$`dGreater.Than.95`/agg_HBV_data$`Greater.Than.95`*100000

agg_HCV_data$`0.to.4.mortality%` <- agg_HCV_data$`d0.to.4`/agg_HCV_data$`0.to.4`*100000
agg_HCV_data$`5.to.9.mortality%` <- agg_HCV_data$`d5.to.9`/agg_HCV_data$`5.to.9`*100000
agg_HCV_data$`10.to.14.mortality%` <- agg_HCV_data$`d10.to.14`/agg_HCV_data$`10.to.14`*100000
agg_HCV_data$`15.to.19.mortality%` <- agg_HCV_data$`d15.to.19`/agg_HCV_data$`15.to.19`*100000
agg_HCV_data$`20.to.24.mortality%` <- agg_HCV_data$`d20.to.24`/agg_HCV_data$`20.to.24`*100000
agg_HCV_data$`25.to.29.mortality%` <- agg_HCV_data$`d25.to.29`/agg_HCV_data$`25.to.29`*100000
agg_HCV_data$`30.to.34.mortality%` <- agg_HCV_data$`d30.to.34`/agg_HCV_data$`30.to.34`*100000
agg_HCV_data$`35.to.39.mortality%` <- agg_HCV_data$`d35.to.39`/agg_HCV_data$`35.to.39`*100000
agg_HCV_data$`40.to.44.mortality%` <- agg_HCV_data$`d40.to.44`/agg_HCV_data$`40.to.44`*100000
agg_HCV_data$`45.to.49.mortality%` <- agg_HCV_data$`d45.to.49`/agg_HCV_data$`45.to.49`*100000
agg_HCV_data$`50.to.54.mortality%` <- agg_HCV_data$`d50.to.54`/agg_HCV_data$`50.to.54`*100000
agg_HCV_data$`55.to.59.mortality%` <- agg_HCV_data$`d55.to.59`/agg_HCV_data$`55.to.59`*100000
agg_HCV_data$`60.to.64.mortality%` <- agg_HCV_data$`d60.to.64`/agg_HCV_data$`60.to.64`*100000
agg_HCV_data$`65.to.69.mortality%` <- agg_HCV_data$`d65.to.69`/agg_HCV_data$`65.to.69`*100000
agg_HCV_data$`70.to.74.mortality%` <- agg_HCV_data$`d70.to.74`/agg_HCV_data$`70.to.74`*100000
agg_HCV_data$`75.to.79.mortality%` <- agg_HCV_data$`d75.to.79`/agg_HCV_data$`75.to.79`*100000
agg_HCV_data$`80.to.84.mortality%` <- agg_HCV_data$`d80.to.84`/agg_HCV_data$`80.to.84`*100000
agg_HCV_data$`85.to.89.mortality%` <- agg_HCV_data$`d85.to.89`/agg_HCV_data$`85.to.89`*100000
agg_HCV_data$`90.to.94.mortality%` <- agg_HCV_data$`d90.to.94`/agg_HCV_data$`90.to.94`*100000
agg_HCV_data$`Greater.Than.95.mortality%` <- agg_HCV_data$`dGreater.Than.95`/agg_HCV_data$`Greater.Than.95`*100000

# Function For Computing Age-Adjusted Mortality Rates Given a dataframe
aam <- function(df) { # Computes a convex combination  
  
  aam <- df$`0.to.4.mortality%` * df$`0.to.4%` + df$`5.to.9.mortality%` * df$`5.to.9%` +
    df$`10.to.14.mortality%` * df$`10.to.14%` + df$`15.to.19.mortality%` * df$`15.to.19%` +
    df$`20.to.24.mortality%` * df$`20.to.24%` + df$`25.to.29.mortality%` * df$`25.to.29%` +
    df$`30.to.34.mortality%` * df$`30.to.34%` + df$`35.to.39.mortality%` * df$`35.to.39%` +
    df$`40.to.44.mortality%` * df$`40.to.44%` + df$`45.to.49.mortality%` * df$`45.to.49%` + 
    df$`50.to.54.mortality%` * df$`50.to.54%` + df$`55.to.59.mortality%` * df$`55.to.59%` + 
    df$`60.to.64.mortality%` * df$`60.to.64%` + df$`65.to.69.mortality%` * df$`65.to.69%` + 
    df$`70.to.74.mortality%` * df$`70.to.74%` + df$`75.to.79.mortality%` * df$`75.to.79%` + 
    df$`80.to.84.mortality%` * df$`80.to.84%` + df$`85.to.89.mortality%` * df$`85.to.89%` + 
    df$`90.to.94.mortality%` * df$`90.to.94%` + df$`Greater.Than.95.mortality%` * df$`Greater.Than.95%`
   
  return(aam) 
  
  }

# Compute Age-Adjusted Mortality Rates 
agg_NAFLD_data$`Age.Adjusted.Mortality.Rate` = aam(agg_NAFLD_data)
agg_ALD_data$`Age.Adjusted.Mortality.Rate` = aam(agg_ALD_data)
agg_HBV_data$`Age.Adjusted.Mortality.Rate` = aam(agg_HBV_data)
agg_HCV_data$`Age.Adjusted.Mortality.Rate` = aam(agg_HCV_data)

# Compute Confidence Intervals 
  


# Plots 
library(ggplot2)

# Both Genders 
ggplot(data=agg_NAFLD_data[agg_NAFLD_data['Gender'] == 'Both Genders', ] , aes(x=Year, y=Age.Adjusted.Mortality.Rate)) +
  geom_line(colour="blue") +
  geom_line(colour="red", data=agg_ALD_data[agg_ALD_data['Gender'] == 'Both Genders', ]) +
  geom_line(colour="green", data=agg_HBV_data[agg_HBV_data['Gender'] == 'Both Genders', ]) +
  geom_line(colour="purple", data=agg_HCV_data[agg_HCV_data['Gender'] == 'Both Genders', ]) + 
  geom_point(colour="blue", shape=15, size=3) + 
  geom_point(colour="red", shape=17, size=3, data=agg_ALD_data[agg_ALD_data['Gender'] == 'Both Genders', ]) + 
  geom_point(colour="green", shape=18, size=3, data=agg_HBV_data[agg_HBV_data['Gender'] == 'Both Genders', ]) +
  geom_point(colour="purple", shape=19, size=3, data=agg_HCV_data[agg_HCV_data['Gender'] == 'Both Genders', ]) +
  xlab("Year") + ylab("Age-Adjusted Mortality Rate per 100,000") + 
  ggtitle("Age Adjusted Mortality Rates over time for Various CLD Aetiologies") + 
  theme_bw() + 
  scale_colour_manual("Parameters",values=c("blue","red"))

# Male 




# Female 








