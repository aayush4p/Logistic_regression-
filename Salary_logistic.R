adult <- read.csv('adult.csv')
head(adult)

print(summary(adult))
str(adult)
##data cleaning 

print(table(adult$workclass))

unemp <- function(job)
{
  job <- as.character(job)
  if(job == 'Never-worked' | job == 'Without-pay') {
    return ('unemployed')
  }
  else{
      return(job)
    }
}

##implementing merging unemp function

adult$workclass <- sapply(adult$workclass,unemp)

print(table(adult$workclass))

##merging self-emp-inc and self-emp-not-inc into self-imp

selfemp <- function(job)
{
  job <- as.character(job)
  if(job == 'Self-emp-inc' | job == 'Self-emp-not-inc') {
    return ('Self-emp')
  }
  else if(job == 'State-gov' | job == 'Local-gov')
        return('SL-gov')
      else{
    return(job)
  }
}


adult$workclass <- sapply(adult$workclass,selfemp)

print(table(adult$workclass))
####
###marital status cleaning
marriage <- function(marry)
{
  marry <- as.character(marry)
  if(marry == 'Divorced' | marry == 'Separated' | marry=='Widowed') {
    return ('Not-married')
  }
  else if(marry == 'Never-married'){
    return(marry)
  }
  else{
    return('married')
  }
  
}


adult$marital.status <- sapply(adult$marital.status,marriage)

print(table(adult$marital.status))

##country grouped into continents

asia <- c( 'China','Hong','India ','Iran','Cambodia', 'Japan', 'Laos', 'Philippines',  'Vietnam', 'Taiwan','Thialand')
North.america <- c('Canada','United-States','Puerto-Rico')
Europe <- c('England','France','Germany','Greece','Holand-Netherlands','Hungary','Ireland','Portugal','Italy','Poland',
            'Scotland','Yogoslavia')

latin.and.southAmerica <- c('Columbia','Cuba',  'Dominican-Republic','Ecuador','El-Salvador','Guatemala','Haiti',
                            'Hondurus','Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru','Jamaica','Trinadad&Tobago')

others <- c('South')

grp_country <- function(country)
  
{
  if(country %in% asia)
    return('asia')
  else if(country %in% North.america)
    return('North.america')
  else if(country %in% latin.and.southAmerica)
    return('latin.and.southAmerica')
  else if(country %in% Europe)
    return('Europe')
  else
    return('others')
  
}

adult$native.country <- sapply(adult$native.country,grp_country)
print(table(adult$native.country))


str(adult)


##applying our factors to new modifications

adult$workclass <- sapply(adult$workclass,factor)
adult$marital.status <- sapply(adult$marital.status,factor)
adult$native.country <- sapply(adult$native.country,factor)

library(Amelia)


###changing ? values to NA

adult[adult== '?'] <- NA

adult$workclass <- sapply(adult$workclass,factor)
adult$marital.status <- sapply(adult$marital.status,factor)
adult$native.country <- sapply(adult$native.country,factor)

print(table(adult$workclass))
