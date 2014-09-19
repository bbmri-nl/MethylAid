##demo version of the MethylAid App

##global accessible by all users
library(MethylAid)
data(exampleData, package="MethylAid")

MethylAid:::server450k(exampleData)
