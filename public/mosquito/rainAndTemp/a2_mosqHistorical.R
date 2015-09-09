############
# SET WD TO WHERE I HAVE ALL YEARS OF MOSQUITO DATA
############
setwd("E:/workingdirectory/mosquito/historical")

############
# READ IN ALL CSVS INTO INDIVIDUAL DFs
############
for (i in dir()){
  assign(gsub(".csv", "",i),read.csv(i, skip=3))
}

############
# LIST METHOD
############

temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)


files = list.files(pattern="*.csv")
myfiles = do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Won't work due to various formats

############
#
############






############
#
############






############
#
############






############
#
############






############
#
############






############
#
############






############
#
############






############
#
############






############
#
############






############
#
############






############
#
############






############
#
############






############
#
############






############
#
############






############
#
############





