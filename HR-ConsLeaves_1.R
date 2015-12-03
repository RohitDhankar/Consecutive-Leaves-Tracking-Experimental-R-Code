# HR Department -Consecutive Leaves Problem 
# An obs - "1" is a Present day and "0" is a Leave day 
# 500 Employees 1 Qtr Leave Data 
d <- read.csv("C:/Users/Rohit/Desktop/HR-Cons.Leaves/d.csv")
# Extracting Emp ID's 
eID <- as.data.frame(d[,1]) ;colnames(eID)[1] <- "Emp-ID"
# Deleting 1st column / variable - Emp ID
d <- as.data.frame(d[2:91])
# Summing all rows using "apply" # create an "integer column vector" 
# cbind with DF "d" # Rename new variable / Column as - "Sum-P"
d<-cbind(d,as.data.frame(apply(d, 1, sum)))
colnames(d)[91] <- "Sum-P"
# Counting all "0" in a Row # create an "integer column vector" 
# cbind with DF "d" # Rename new variable / Column as - "Sum-L"
d<-cbind(d,as.data.frame(apply(d==0, 1, sum)))
colnames(d)[92] <- "Sum-L"
# calculate the startZeros  
startZs<-apply(d,1,function(x)which.min(x==0)-1) 
# gives out only "0" values as None of our ROWS starts with a ZERO value 
stopZs<-apply(d,1,function(x)which.min(rev(x==0))-1)
# gives out only "0" values as None of our ROWS ends with a ZERO value 
# calculate -longest run of zeros
longestRun<-apply(d,1,function(x)
  {
  y = rle(x);
  max(y$lengths[y$values==0],0)}) 
# extracting MAX value for Longest Run of Value = "0" 
# take the max of the two values
Max_Zeros<-as.data.frame(pmax(longestRun,startZs+stopZs))
ConsWks<-ifelse(Max_Zeros>14,"Yes","No") # Converting to Yes / No factor variable for "Two Consecutive Weeks of Leave"
d<-cbind(d,as.data.frame(Max_Zeros))
d<-cbind(d,as.data.frame(ConsWks))
colnames(d)[93] <-"Max.Zeros" ; colnames(d)[94] <-"Cons.Wks"
d<-cbind(eID,d)
# ?rle # Help for rle 
# m <- rev(rep(6:10, 1:5))
# rle(m)
# "Run Length Encoding" - lengths are the - Consecutive number or repetioion fo the value 
# the Values are - the numeric "values"  which are gettting repeated - are the - Consecutive numbers . 
# lengths: int [1:5] 5 4 3 2 1
# values : int [1:5] 10 9 8 7 6
# ?pmax

