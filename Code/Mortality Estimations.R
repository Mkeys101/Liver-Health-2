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

# Make aetiology groups 
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


# Subset data 





# Estimation 



# Plots 








