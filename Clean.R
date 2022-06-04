setwd('C:/Users/Lee/iCloudDrive/Document/Boston University/CS544 Foundation with R/Final Project')
getwd()

##### read the file
df = read.csv('Unicorn_Companies.csv')

##### clean the data
# check none for each columns
# we need to find NA. 
# in this dataset, it uses 'None' as empty data
numberOfNa = function(df){
  flag = 'None' # set 'None' as a checking flag
  for(i in 1:ncol(df)){
    temp = df[, i] # extract column one by one
    n = length(temp[temp == flag]) # count how many 'None'
    print(paste(colnames(df)[i], n)) # print column name and 'None' quantity
  }
}
dropNone = function(df, columnName){
  drop = which(df[, columnName] == 'None')
  df = df[-drop, ]
  return(df)
}
checkType = function(df){
  for(i in 1:ncol(df)){
    temp = df[, i]
    print(paste(colnames(df)[i], '--->', typeof(temp)))
  }
}

numberOfNa(df) # check na 
### Drop useless columns
# we have only 1037 data rows
# Financial.Stage and Portfolio.Exits have 988 NA 
df = df[, -which(colnames(df) %in% c('Financial.Stage', 'Portfolio.Exits'))] # Drop these two columns

# after check column Select.Inverstors
# it's not suitable to analysis
# and I think it's not important
df = df[, -which(colnames(df) %in% c('Select.Inverstors'))] # Drop 



### Drop NA for each columns
df = dropNone(df, 'Founded.Year') # drop NA in Founded.Year
nrow(df)

df = dropNone(df, 'Deal.Terms') # drop NA in Deal.Terms
nrow(df)

df = dropNone(df, 'Total.Raised') # drop NA in Total.Raised
nrow(df)





### change data type for Valuation...B. and Total.Raised
# change string '$140' into numerical data 140
# change string '$7.44B' into numerical data 7.44
# Valuation...B.
temp = c() # set an empty list
for(i in 1:nrow(df)){
  d = df[i, 'Valuation...B.'] # take string out, e.g. '$114
  d = substring(d, 2) # extract into '114'
  d = as.numeric(d) # change datatype into numeric, e.g. 114
  temp = append(temp, d) # save into a temporary list
}
df$Valuation...B. = temp # replace the old data with new data

# Total.Raised
# set 'million' as the column unit
# e.g. '$12B' into 12000
temp = c() # set an empty list
for(i in 1:nrow(df)){
  d = df[i, 'Total.Raised']
  n = nchar(d) # length of the character
  unit = substring(d,n) # extract 'B' or 'M'
  number = substring(d, 2, n-1) # extract number part, e.g. '114'
  number = as.numeric(number) # turn string into numeric, e.g. 114
  
  if(unit == "B"){
    number = number * 1000 
    # if unit is 'Billion', times 1000 to make the unit into million
    # e.g. 3B is 3000M
  }           
  
  temp = append(temp, number) # append the number into the temp list
}
df$Total.Raised = temp # replace the old data with new data, which is the temp list

### clean the time data column
# Date.Joined
# split date data into year, month, and day
# split into three columns
dayJoin = c() # create temp list
monthJoin = c()
yearJoin = c()

for(i in 1:nrow(df)){
  d = df[i, 'Date.Joined']
  d = strsplit(d, split = '/') # split timestamp
  names(d) = 'timestamp' # redundant but important thing, or we can't access the list
  
  # access the splited time stamp
  day = d$timestamp[2]
  month = d$timestamp[1]
  year = d$timestamp[3]
  
  # append them into the temp lists
  dayJoin = append(dayJoin, as.numeric(day)) 
  monthJoin = append(monthJoin, as.numeric(month))
  yearJoin = append(yearJoin, as.numeric(year))
}

df$dayJoin = dayJoin # create new columns 
df$monthJoin = monthJoin
df$yearJoin = yearJoin

# Founded.Year, turn string into numeric
# e.g. '2022' into 2022
df$Founded.Year = as.numeric(df$Founded.Year)

### turn the rest string data type into numeric data type
# Deal.Terms
df$Deal.Terms = as.numeric(df$Deal.Terms)

# Investors.Count
df$Investors.Count = as.numeric(df$Investors.Count)

##### final check
checkType(df) # check type
numberOfNa(df) # check na

##### save as csv
write.csv(df, file = 'AfterClean.csv',  row.names = FALSE)
