
##### wnv 1 - WNV
#SUBSET BY DATE and wnv
jun04wnv <- subset(mergedwnv, date2  == as.Date("2013-06-04"))
jun12wnv <- subset(mergedwnv, date2  == as.Date("2013-06-12"))
jun18wnv <- subset(mergedwnv, date2  == as.Date("2013-06-18"))
jun25wnv <- subset(mergedwnv, date2  == as.Date("2013-06-25"))
jul04wnv <- subset(mergedwnv, date2  == as.Date("2013-07-04"))
jul10wnv <- subset(mergedwnv, date2  == as.Date("2013-07-10"))
jul17wnv <- subset(mergedwnv, date2  == as.Date("2013-07-17"))
jul24wnv <- subset(mergedwnv, date2  == as.Date("2013-07-24"))
jul30wnv <- subset(mergedwnv, date2  == as.Date("2013-07-30"))
aug06wnv <- subset(mergedwnv, date2  == as.Date("2013-08-06"))
aug13wnv <- subset(mergedwnv, date2  == as.Date("2013-08-13"))
aug20wnv <- subset(mergedwnv, date2  == as.Date("2013-08-20"))
sep03wnv <- subset(mergedwnv, date2  == as.Date("2013-09-03"))


####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE wnv SITE

#JUN 04 wnv
jun04wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-06-04"))
jun04wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-06-04"))


#jun12 wnv
jun12wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-06-12"))
jun12wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-06-12"))

#jun18 wnv
jun18wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-06-18"))
jun18wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-06-18"))

#jun25 wnv
jun25wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-06-25"))
jun25wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-06-25"))

#jul04 wnv
jul04wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-07-04"))
jul04wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-07-04"))

#jul10 wnv
jul10wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-07-10"))
jul10wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-07-10"))

#jul17 wnv
jul17wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-07-17"))
jul17wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-07-17"))

#jul24 wnv
jul24wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-07-24"))
jul24wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-07-24"))

#jul30 wnv
jul30wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-07-30"))
jul30wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-07-30"))

#aug06 wnv
aug06wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-08-06"))
aug06wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-08-06"))

#aug13 wnv
aug13wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-08-13"))
aug13wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-08-13"))

#aug20 wnv
aug20wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-08-20"))
aug20wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-08-20"))

#sep03 wnv
sep03wnvsite1 <-subset(merged1wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite2 <-subset(merged2wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite3 <-subset(merged3wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite4 <-subset(merged4wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite5 <-subset(merged5wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite6 <-subset(merged6wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite7 <-subset(merged7wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite8 <-subset(merged8wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite9 <-subset(merged9wnv, date2  == as.Date("2013-09-03"))
sep03wnvsite10 <-subset(merged10wnv, date2  == as.Date("2013-09-03"))

##### ALL SITES COMBINED
#wnv
ajun04wnvtotal <- sum(jun04wnv$total, na.rm=T)
ajun12wnvtotal <- sum(jun12wnv$total, na.rm=T)
ajun18wnvtotal <- sum(jun18wnv$total, na.rm=T)
ajun25wnvtotal <- sum(jun25wnv$total, na.rm=T)
ajul04wnvtotal <- sum(jul04wnv$total, na.rm=T)
ajul10wnvtotal <- sum(jul10wnv$total, na.rm=T)
ajul17wnvtotal <- sum(jul17wnv$total, na.rm=T)
ajul24wnvtotal <- sum(jul24wnv$total, na.rm=T)
ajul30wnvtotal <- sum(jul30wnv$total, na.rm=T)
aaug06wnvtotal <- sum(aug06wnv$total, na.rm=T)
aaug13wnvtotal <- sum(aug13wnv$total, na.rm=T)
aaug20wnvtotal <- sum(aug20wnv$total, na.rm=T)
asep03wnvtotal <- sum(sep03wnv$total, na.rm=T)



#####################################
##### site1
#wnv
ajun04wnvsite1 <- sum(jun04wnvsite1$X1, na.rm=T)
ajun12wnvsite1 <- sum(jun12wnvsite1$X1, na.rm=T)
ajun18wnvsite1 <- sum(jun18wnvsite1$X1, na.rm=T)
ajun25wnvsite1 <- sum(jun25wnvsite1$X1, na.rm=T)
ajul04wnvsite1 <- sum(jul04wnvsite1$X1, na.rm=T)
ajul10wnvsite1 <- sum(jul10wnvsite1$X1, na.rm=T)
ajul17wnvsite1 <- sum(jul17wnvsite1$X1, na.rm=T)
ajul24wnvsite1 <- sum(jul24wnvsite1$X1, na.rm=T)
ajul30wnvsite1 <- sum(jul30wnvsite1$X1, na.rm=T)
aaug06wnvsite1 <- sum(aug06wnvsite1$X1, na.rm=T)
aaug13wnvsite1 <- sum(aug13wnvsite1$X1, na.rm=T)
aaug20wnvsite1 <- sum(aug20wnvsite1$X1, na.rm=T)
asep03wnvsite1 <- sum(sep03wnvsite1$X1, na.rm=T)


#NONwnv
ajun04nonwnvsite1 <- sum(jun04nonwnvsite1$X1, na.rm=T)
ajun12nonwnvsite1 <- sum(jun12nonwnvsite1$X1, na.rm=T)
ajun18nonwnvsite1 <- sum(jun18nonwnvsite1$X1, na.rm=T)
ajun25nonwnvsite1 <- sum(jun25nonwnvsite1$X1, na.rm=T)
ajul04nonwnvsite1 <- sum(jul04nonwnvsite1$X1, na.rm=T)
ajul10nonwnvsite1 <- sum(jul10nonwnvsite1$X1, na.rm=T)
ajul17nonwnvsite1 <- sum(jul17nonwnvsite1$X1, na.rm=T)
ajul24nonwnvsite1 <- sum(jul24nonwnvsite1$X1, na.rm=T)
ajul30nonwnvsite1 <- sum(jul30nonwnvsite1$X1, na.rm=T)
aaug06nonwnvsite1 <- sum(aug06nonwnvsite1$X1, na.rm=T)
aaug13nonwnvsite1 <- sum(aug13nonwnvsite1$X1, na.rm=T)
aaug20nonwnvsite1 <- sum(aug20nonwnvsite1$X1, na.rm=T)
asep03nonwnvsite1 <- sum(sep03nonwnvsite1$X1, na.rm=T)

#####################################
##### site2
#wnv
ajun04wnvsite2 <- sum(jun04wnvsite2$X2, na.rm=T)
ajun12wnvsite2 <- sum(jun12wnvsite2$X2, na.rm=T)
ajun18wnvsite2 <- sum(jun18wnvsite2$X2, na.rm=T)
ajun25wnvsite2 <- sum(jun25wnvsite2$X2, na.rm=T)
ajul04wnvsite2 <- sum(jul04wnvsite2$X2, na.rm=T)
ajul10wnvsite2 <- sum(jul10wnvsite2$X2, na.rm=T)
ajul17wnvsite2 <- sum(jul17wnvsite2$X2, na.rm=T)
ajul24wnvsite2 <- sum(jul24wnvsite2$X2, na.rm=T)
ajul30wnvsite2 <- sum(jul30wnvsite2$X2, na.rm=T)
aaug06wnvsite2 <- sum(aug06wnvsite2$X2, na.rm=T)
aaug13wnvsite2 <- sum(aug13wnvsite2$X2, na.rm=T)
aaug20wnvsite2 <- sum(aug20wnvsite2$X2, na.rm=T)
asep03wnvsite2 <- sum(sep03wnvsite2$X2, na.rm=T)


#NONwnv
ajun04nonwnvsite2 <- sum(jun04nonwnvsite2$X2, na.rm=T)
ajun12nonwnvsite2 <- sum(jun12nonwnvsite2$X2, na.rm=T)
ajun18nonwnvsite2 <- sum(jun18nonwnvsite2$X2, na.rm=T)
ajun25nonwnvsite2 <- sum(jun25nonwnvsite2$X2, na.rm=T)
ajul04nonwnvsite2 <- sum(jul04nonwnvsite2$X2, na.rm=T)
ajul10nonwnvsite2 <- sum(jul10nonwnvsite2$X2, na.rm=T)
ajul17nonwnvsite2 <- sum(jul17nonwnvsite2$X2, na.rm=T)
ajul24nonwnvsite2 <- sum(jul24nonwnvsite2$X2, na.rm=T)
ajul30nonwnvsite2 <- sum(jul30nonwnvsite2$X2, na.rm=T)
aaug06nonwnvsite2 <- sum(aug06nonwnvsite2$X2, na.rm=T)
aaug13nonwnvsite2 <- sum(aug13nonwnvsite2$X2, na.rm=T)
aaug20nonwnvsite2 <- sum(aug20nonwnvsite2$X2, na.rm=T)
asep03nonwnvsite2 <- sum(sep03nonwnvsite2$X2, na.rm=T)

#####################################
##### site3
#wnv
ajun04wnvsite3 <- sum(jun04wnvsite3$X3, na.rm=T)
ajun12wnvsite3 <- sum(jun12wnvsite3$X3, na.rm=T)
ajun18wnvsite3 <- sum(jun18wnvsite3$X3, na.rm=T)
ajun25wnvsite3 <- sum(jun25wnvsite3$X3, na.rm=T)
ajul04wnvsite3 <- sum(jul04wnvsite3$X3, na.rm=T)
ajul10wnvsite3 <- sum(jul10wnvsite3$X3, na.rm=T)
ajul17wnvsite3 <- sum(jul17wnvsite3$X3, na.rm=T)
ajul24wnvsite3 <- sum(jul24wnvsite3$X3, na.rm=T)
ajul30wnvsite3 <- sum(jul30wnvsite3$X3, na.rm=T)
aaug06wnvsite3 <- sum(aug06wnvsite3$X3, na.rm=T)
aaug13wnvsite3 <- sum(aug13wnvsite3$X3, na.rm=T)
aaug20wnvsite3 <- sum(aug20wnvsite3$X3, na.rm=T)
asep03wnvsite3 <- sum(sep03wnvsite3$X3, na.rm=T)


#NONwnv
ajun04nonwnvsite3 <- sum(jun04nonwnvsite3$X3, na.rm=T)
ajun12nonwnvsite3 <- sum(jun12nonwnvsite3$X3, na.rm=T)
ajun18nonwnvsite3 <- sum(jun18nonwnvsite3$X3, na.rm=T)
ajun25nonwnvsite3 <- sum(jun25nonwnvsite3$X3, na.rm=T)
ajul04nonwnvsite3 <- sum(jul04nonwnvsite3$X3, na.rm=T)
ajul10nonwnvsite3 <- sum(jul10nonwnvsite3$X3, na.rm=T)
ajul17nonwnvsite3 <- sum(jul17nonwnvsite3$X3, na.rm=T)
ajul24nonwnvsite3 <- sum(jul24nonwnvsite3$X3, na.rm=T)
ajul30nonwnvsite3 <- sum(jul30nonwnvsite3$X3, na.rm=T)
aaug06nonwnvsite3 <- sum(aug06nonwnvsite3$X3, na.rm=T)
aaug13nonwnvsite3 <- sum(aug13nonwnvsite3$X3, na.rm=T)
aaug20nonwnvsite3 <- sum(aug20nonwnvsite3$X3, na.rm=T)
asep03nonwnvsite3 <- sum(sep03nonwnvsite3$X3, na.rm=T)


#####################################
##### site4
#wnv
ajun04wnvsite4 <- sum(jun04wnvsite4$X4, na.rm=T)
ajun12wnvsite4 <- sum(jun12wnvsite4$X4, na.rm=T)
ajun18wnvsite4 <- sum(jun18wnvsite4$X4, na.rm=T)
ajun25wnvsite4 <- sum(jun25wnvsite4$X4, na.rm=T)
ajul04wnvsite4 <- sum(jul04wnvsite4$X4, na.rm=T)
ajul10wnvsite4 <- sum(jul10wnvsite4$X4, na.rm=T)
ajul17wnvsite4 <- sum(jul17wnvsite4$X4, na.rm=T)
ajul24wnvsite4 <- sum(jul24wnvsite4$X4, na.rm=T)
ajul30wnvsite4 <- sum(jul30wnvsite4$X4, na.rm=T)
aaug06wnvsite4 <- sum(aug06wnvsite4$X4, na.rm=T)
aaug13wnvsite4 <- sum(aug13wnvsite4$X4, na.rm=T)
aaug20wnvsite4 <- sum(aug20wnvsite4$X4, na.rm=T)
asep03wnvsite4 <- sum(sep03wnvsite4$X4, na.rm=T)


#NONwnv
ajun04nonwnvsite4 <- sum(jun04nonwnvsite4$X4, na.rm=T)
ajun12nonwnvsite4 <- sum(jun12nonwnvsite4$X4, na.rm=T)
ajun18nonwnvsite4 <- sum(jun18nonwnvsite4$X4, na.rm=T)
ajun25nonwnvsite4 <- sum(jun25nonwnvsite4$X4, na.rm=T)
ajul04nonwnvsite4 <- sum(jul04nonwnvsite4$X4, na.rm=T)
ajul10nonwnvsite4 <- sum(jul10nonwnvsite4$X4, na.rm=T)
ajul17nonwnvsite4 <- sum(jul17nonwnvsite4$X4, na.rm=T)
ajul24nonwnvsite4 <- sum(jul24nonwnvsite4$X4, na.rm=T)
ajul30nonwnvsite4 <- sum(jul30nonwnvsite4$X4, na.rm=T)
aaug06nonwnvsite4 <- sum(aug06nonwnvsite4$X4, na.rm=T)
aaug13nonwnvsite4 <- sum(aug13nonwnvsite4$X4, na.rm=T)
aaug20nonwnvsite4 <- sum(aug20nonwnvsite4$X4, na.rm=T)
asep03nonwnvsite4 <- sum(sep03nonwnvsite4$X4, na.rm=T)

#####################################
##### site5
#wnv
ajun04wnvsite5 <- sum(jun04wnvsite5$X5, na.rm=T)
ajun12wnvsite5 <- sum(jun12wnvsite5$X5, na.rm=T)
ajun18wnvsite5 <- sum(jun18wnvsite5$X5, na.rm=T)
ajun25wnvsite5 <- sum(jun25wnvsite5$X5, na.rm=T)
ajul04wnvsite5 <- sum(jul04wnvsite5$X5, na.rm=T)
ajul10wnvsite5 <- sum(jul10wnvsite5$X5, na.rm=T)
ajul17wnvsite5 <- sum(jul17wnvsite5$X5, na.rm=T)
ajul24wnvsite5 <- sum(jul24wnvsite5$X5, na.rm=T)
ajul30wnvsite5 <- sum(jul30wnvsite5$X5, na.rm=T)
aaug06wnvsite5 <- sum(aug06wnvsite5$X5, na.rm=T)
aaug13wnvsite5 <- sum(aug13wnvsite5$X5, na.rm=T)
aaug20wnvsite5 <- sum(aug20wnvsite5$X5, na.rm=T)
asep03wnvsite5 <- sum(sep03wnvsite5$X5, na.rm=T)


#NONwnv
ajun04nonwnvsite5 <- sum(jun04nonwnvsite5$X5, na.rm=T)
ajun12nonwnvsite5 <- sum(jun12nonwnvsite5$X5, na.rm=T)
ajun18nonwnvsite5 <- sum(jun18nonwnvsite5$X5, na.rm=T)
ajun25nonwnvsite5 <- sum(jun25nonwnvsite5$X5, na.rm=T)
ajul04nonwnvsite5 <- sum(jul04nonwnvsite5$X5, na.rm=T)
ajul10nonwnvsite5 <- sum(jul10nonwnvsite5$X5, na.rm=T)
ajul17nonwnvsite5 <- sum(jul17nonwnvsite5$X5, na.rm=T)
ajul24nonwnvsite5 <- sum(jul24nonwnvsite5$X5, na.rm=T)
ajul30nonwnvsite5 <- sum(jul30nonwnvsite5$X5, na.rm=T)
aaug06nonwnvsite5 <- sum(aug06nonwnvsite5$X5, na.rm=T)
aaug13nonwnvsite5 <- sum(aug13nonwnvsite5$X5, na.rm=T)
aaug20nonwnvsite5 <- sum(aug20nonwnvsite5$X5, na.rm=T)
asep03nonwnvsite5 <- sum(sep03nonwnvsite5$X5, na.rm=T)


#####################################
##### site6
#wnv
jun04wnvsite6$X6 <- as.numeric(jun04wnvsite6$X6)
jun12wnvsite6$X6 <- as.numeric(jun12wnvsite6$X6)
jun18wnvsite6$X6 <- as.numeric(jun18wnvsite6$X6)
jun25wnvsite6$X6 <- as.numeric(jun25wnvsite6$X6)
jul04wnvsite6$X6 <- as.numeric(jul04wnvsite6$X6)
jul10wnvsite6$X6 <- as.numeric(jul10wnvsite6$X6)
jul17wnvsite6$X6 <- as.numeric(jul17wnvsite6$X6)
jul24wnvsite6$X6 <- as.numeric(jul24wnvsite6$X6)
jul30wnvsite6$X6 <- as.numeric(jul30wnvsite6$X6)
aug06wnvsite6$X6 <- as.numeric(aug06wnvsite6$X6)
aug13wnvsite6$X6 <- as.numeric(aug13wnvsite6$X6)
aug20wnvsite6$X6 <- as.numeric(aug20wnvsite6$X6)
sep03wnvsite6$X6 <- as.numeric(sep03wnvsite6$X6)


ajun04wnvsite6 <- sum(jun04wnvsite6$X6, na.rm=T)
ajun12wnvsite6 <- sum(jun12wnvsite6$X6, na.rm=T)
ajun18wnvsite6 <- sum(jun18wnvsite6$X6, na.rm=T)
ajun25wnvsite6 <- sum(jun25wnvsite6$X6, na.rm=T)
ajul04wnvsite6 <- sum(jul04wnvsite6$X6, na.rm=T)
ajul10wnvsite6 <- sum(jul10wnvsite6$X6, na.rm=T)
ajul17wnvsite6 <- sum(jul17wnvsite6$X6, na.rm=T)
ajul24wnvsite6 <- sum(jul24wnvsite6$X6, na.rm=T)
ajul30wnvsite6 <- sum(jul30wnvsite6$X6, na.rm=T)
aaug06wnvsite6 <- sum(aug06wnvsite6$X6, na.rm=T)
aaug13wnvsite6 <- sum(aug13wnvsite6$X6, na.rm=T)
aaug20wnvsite6 <- sum(aug20wnvsite6$X6, na.rm=T)
asep03wnvsite6 <- sum(sep03wnvsite6$X6, na.rm=T)


#NONwnv
jun04nonwnvsite6$X6 <- as.numeric(jun04nonwnvsite6$X6)
jun12nonwnvsite6$X6 <- as.numeric(jun12nonwnvsite6$X6)
jun18nonwnvsite6$X6 <- as.numeric(jun18nonwnvsite6$X6)
jun25nonwnvsite6$X6 <- as.numeric(jun25nonwnvsite6$X6)
jul04nonwnvsite6$X6 <- as.numeric(jul04nonwnvsite6$X6)
jul10nonwnvsite6$X6 <- as.numeric(jul10nonwnvsite6$X6)
jul17nonwnvsite6$X6 <- as.numeric(jul17nonwnvsite6$X6)
jul24nonwnvsite6$X6 <- as.numeric(jul24nonwnvsite6$X6)
jul30nonwnvsite6$X6 <- as.numeric(jul30nonwnvsite6$X6)
aug06nonwnvsite6$X6 <- as.numeric(aug06nonwnvsite6$X6)
aug13nonwnvsite6$X6 <- as.numeric(aug13nonwnvsite6$X6)
aug20nonwnvsite6$X6 <- as.numeric(aug20nonwnvsite6$X6)
sep03nonwnvsite6$X6 <- as.numeric(sep03nonwnvsite6$X6)


ajun04nonwnvsite6 <- sum(jun04nonwnvsite6$X6, na.rm=T)
ajun12nonwnvsite6 <- sum(jun12nonwnvsite6$X6, na.rm=T)
ajun18nonwnvsite6 <- sum(jun18nonwnvsite6$X6, na.rm=T)
ajun25nonwnvsite6 <- sum(jun25nonwnvsite6$X6, na.rm=T)
ajul04nonwnvsite6 <- sum(jul04nonwnvsite6$X6, na.rm=T)
ajul10nonwnvsite6 <- sum(jul10nonwnvsite6$X6, na.rm=T)
ajul17nonwnvsite6 <- sum(jul17nonwnvsite6$X6, na.rm=T)
ajul24nonwnvsite6 <- sum(jul24nonwnvsite6$X6, na.rm=T)
ajul30nonwnvsite6 <- sum(jul30nonwnvsite6$X6, na.rm=T)
aaug06nonwnvsite6 <- sum(aug06nonwnvsite6$X6, na.rm=T)
aaug13nonwnvsite6 <- sum(aug13nonwnvsite6$X6, na.rm=T)
aaug20nonwnvsite6 <- sum(aug20nonwnvsite6$X6, na.rm=T)
asep03nonwnvsite6 <- sum(sep03nonwnvsite6$X6, na.rm=T)


#####################################
##### site7
#wnv
ajun04wnvsite7 <- sum(jun04wnvsite7$X7, na.rm=T)
ajun12wnvsite7 <- sum(jun12wnvsite7$X7, na.rm=T)
ajun18wnvsite7 <- sum(jun18wnvsite7$X7, na.rm=T)
ajun25wnvsite7 <- sum(jun25wnvsite7$X7, na.rm=T)
ajul04wnvsite7 <- sum(jul04wnvsite7$X7, na.rm=T)
ajul10wnvsite7 <- sum(jul10wnvsite7$X7, na.rm=T)
ajul17wnvsite7 <- sum(jul17wnvsite7$X7, na.rm=T)
ajul24wnvsite7 <- sum(jul24wnvsite7$X7, na.rm=T)
ajul30wnvsite7 <- sum(jul30wnvsite7$X7, na.rm=T)
aaug06wnvsite7 <- sum(aug06wnvsite7$X7, na.rm=T)
aaug13wnvsite7 <- sum(aug13wnvsite7$X7, na.rm=T)
aaug20wnvsite7 <- sum(aug20wnvsite7$X7, na.rm=T)
asep03wnvsite7 <- sum(sep03wnvsite7$X7, na.rm=T)


#NONwnv
ajun04nonwnvsite7 <- sum(jun04nonwnvsite7$X7, na.rm=T)
ajun12nonwnvsite7 <- sum(jun12nonwnvsite7$X7, na.rm=T)
ajun18nonwnvsite7 <- sum(jun18nonwnvsite7$X7, na.rm=T)
ajun25nonwnvsite7 <- sum(jun25nonwnvsite7$X7, na.rm=T)
ajul04nonwnvsite7 <- sum(jul04nonwnvsite7$X7, na.rm=T)
ajul10nonwnvsite7 <- sum(jul10nonwnvsite7$X7, na.rm=T)
ajul17nonwnvsite7 <- sum(jul17nonwnvsite7$X7, na.rm=T)
ajul24nonwnvsite7 <- sum(jul24nonwnvsite7$X7, na.rm=T)
ajul30nonwnvsite7 <- sum(jul30nonwnvsite7$X7, na.rm=T)
aaug06nonwnvsite7 <- sum(aug06nonwnvsite7$X7, na.rm=T)
aaug13nonwnvsite7 <- sum(aug13nonwnvsite7$X7, na.rm=T)
aaug20nonwnvsite7 <- sum(aug20nonwnvsite7$X7, na.rm=T)
asep03nonwnvsite7 <- sum(sep03nonwnvsite7$X7, na.rm=T)


#####################################
##### site8
#wnv
ajun04wnvsite8 <- sum(jun04wnvsite8$X8, na.rm=T)
ajun12wnvsite8 <- sum(jun12wnvsite8$X8, na.rm=T)
ajun18wnvsite8 <- sum(jun18wnvsite8$X8, na.rm=T)
ajun25wnvsite8 <- sum(jun25wnvsite8$X8, na.rm=T)
ajul04wnvsite8 <- sum(jul04wnvsite8$X8, na.rm=T)
ajul10wnvsite8 <- sum(jul10wnvsite8$X8, na.rm=T)
ajul17wnvsite8 <- sum(jul17wnvsite8$X8, na.rm=T)
ajul24wnvsite8 <- sum(jul24wnvsite8$X8, na.rm=T)
ajul30wnvsite8 <- sum(jul30wnvsite8$X8, na.rm=T)
aaug06wnvsite8 <- sum(aug06wnvsite8$X8, na.rm=T)
aaug13wnvsite8 <- sum(aug13wnvsite8$X8, na.rm=T)
aaug20wnvsite8 <- sum(aug20wnvsite8$X8, na.rm=T)
asep03wnvsite8 <- sum(sep03wnvsite8$X8, na.rm=T)


#NONwnv
ajun04nonwnvsite8 <- sum(jun04nonwnvsite8$X8, na.rm=T)
ajun12nonwnvsite8 <- sum(jun12nonwnvsite8$X8, na.rm=T)
ajun18nonwnvsite8 <- sum(jun18nonwnvsite8$X8, na.rm=T)
ajun25nonwnvsite8 <- sum(jun25nonwnvsite8$X8, na.rm=T)
ajul04nonwnvsite8 <- sum(jul04nonwnvsite8$X8, na.rm=T)
ajul10nonwnvsite8 <- sum(jul10nonwnvsite8$X8, na.rm=T)
ajul17nonwnvsite8 <- sum(jul17nonwnvsite8$X8, na.rm=T)
ajul24nonwnvsite8 <- sum(jul24nonwnvsite8$X8, na.rm=T)
ajul30nonwnvsite8 <- sum(jul30nonwnvsite8$X8, na.rm=T)
aaug06nonwnvsite8 <- sum(aug06nonwnvsite8$X8, na.rm=T)
aaug13nonwnvsite8 <- sum(aug13nonwnvsite8$X8, na.rm=T)
aaug20nonwnvsite8 <- sum(aug20nonwnvsite8$X8, na.rm=T)
asep03nonwnvsite8 <- sum(sep03nonwnvsite8$X8, na.rm=T)


#####################################
##### site9
#wnv

jun04wnvsite9$X9 <- as.numeric(jun04wnvsite9$X9)
jun12wnvsite9$X9 <- as.numeric(jun12wnvsite9$X9)
jun18wnvsite9$X9 <- as.numeric(jun18wnvsite9$X9)
jun25wnvsite9$X9 <- as.numeric(jun25wnvsite9$X9)
jul04wnvsite9$X9 <- as.numeric(jul04wnvsite9$X9)
jul10wnvsite9$X9 <- as.numeric(jul10wnvsite9$X9)
jul17wnvsite9$X9 <- as.numeric(jul17wnvsite9$X9)
jul24wnvsite9$X9 <- as.numeric(jul24wnvsite9$X9)
jul30wnvsite9$X9 <- as.numeric(jul30wnvsite9$X9)
aug06wnvsite9$X9 <- as.numeric(aug06wnvsite9$X9)
aug13wnvsite9$X9 <- as.numeric(aug13wnvsite9$X9)
aug20wnvsite9$X9 <- as.numeric(aug20wnvsite9$X9)
sep03wnvsite9$X9 <- as.numeric(sep03wnvsite9$X9)


ajun04wnvsite9 <- sum(jun04wnvsite9$X9, na.rm=T)
ajun12wnvsite9 <- sum(jun12wnvsite9$X9, na.rm=T)
ajun18wnvsite9 <- sum(jun18wnvsite9$X9, na.rm=T)
ajun25wnvsite9 <- sum(jun25wnvsite9$X9, na.rm=T)
ajul04wnvsite9 <- sum(jul04wnvsite9$X9, na.rm=T)
ajul10wnvsite9 <- sum(jul10wnvsite9$X9, na.rm=T)
ajul17wnvsite9 <- sum(jul17wnvsite9$X9, na.rm=T)
ajul24wnvsite9 <- sum(jul24wnvsite9$X9, na.rm=T)
ajul30wnvsite9 <- sum(jul30wnvsite9$X9, na.rm=T)
aaug06wnvsite9 <- sum(aug06wnvsite9$X9, na.rm=T)
aaug13wnvsite9 <- sum(aug13wnvsite9$X9, na.rm=T)
aaug20wnvsite9 <- sum(aug20wnvsite9$X9, na.rm=T)
asep03wnvsite9 <- sum(sep03wnvsite9$X9, na.rm=T)


#NONwnv
jun04nonwnvsite9$X9 <- as.numeric(jun04nonwnvsite9$X9)
jun12nonwnvsite9$X9 <- as.numeric(jun12nonwnvsite9$X9)
jun18nonwnvsite9$X9 <- as.numeric(jun18nonwnvsite9$X9)
jun25nonwnvsite9$X9 <- as.numeric(jun25nonwnvsite9$X9)
jul04nonwnvsite9$X9 <- as.numeric(jul04nonwnvsite9$X9)
jul10nonwnvsite9$X9 <- as.numeric(jul10nonwnvsite9$X9)
jul17nonwnvsite9$X9 <- as.numeric(jul17nonwnvsite9$X9)
jul24nonwnvsite9$X9 <- as.numeric(jul24nonwnvsite9$X9)
jul30nonwnvsite9$X9 <- as.numeric(jul30nonwnvsite9$X9)
aug06nonwnvsite9$X9 <- as.numeric(aug06nonwnvsite9$X9)
aug13nonwnvsite9$X9 <- as.numeric(aug13nonwnvsite9$X9)
aug20nonwnvsite9$X9 <- as.numeric(aug20nonwnvsite9$X9)
sep03nonwnvsite9$X9 <- as.numeric(sep03nonwnvsite9$X9)


ajun04nonwnvsite9 <- sum(jun04nonwnvsite9$X9, na.rm=T)
ajun12nonwnvsite9 <- sum(jun12nonwnvsite9$X9, na.rm=T)
ajun18nonwnvsite9 <- sum(jun18nonwnvsite9$X9, na.rm=T)
ajun25nonwnvsite9 <- sum(jun25nonwnvsite9$X9, na.rm=T)
ajul04nonwnvsite9 <- sum(jul04nonwnvsite9$X9, na.rm=T)
ajul10nonwnvsite9 <- sum(jul10nonwnvsite9$X9, na.rm=T)
ajul17nonwnvsite9 <- sum(jul17nonwnvsite9$X9, na.rm=T)
ajul24nonwnvsite9 <- sum(jul24nonwnvsite9$X9, na.rm=T)
ajul30nonwnvsite9 <- sum(jul30nonwnvsite9$X9, na.rm=T)
aaug06nonwnvsite9 <- sum(aug06nonwnvsite9$X9, na.rm=T)
aaug13nonwnvsite9 <- sum(aug13nonwnvsite9$X9, na.rm=T)
aaug20nonwnvsite9 <- sum(aug20nonwnvsite9$X9, na.rm=T)
asep03nonwnvsite9 <- sum(sep03nonwnvsite9$X9, na.rm=T)

#####################################
##### site10
#wnv
jun04wnvsite10$X10 <- as.numeric(jun04wnvsite10$X10)
jun12wnvsite10$X10 <- as.numeric(jun12wnvsite10$X10)
jun18wnvsite10$X10 <- as.numeric(jun18wnvsite10$X10)
jun25wnvsite10$X10 <- as.numeric(jun25wnvsite10$X10)
jul04wnvsite10$X10 <- as.numeric(jul04wnvsite10$X10)
jul10wnvsite10$X10 <- as.numeric(jul10wnvsite10$X10)
jul17wnvsite10$X10 <- as.numeric(jul17wnvsite10$X10)
jul24wnvsite10$X10 <- as.numeric(jul24wnvsite10$X10)
jul30wnvsite10$X10 <- as.numeric(jul30wnvsite10$X10)
aug06wnvsite10$X10 <- as.numeric(aug06wnvsite10$X10)
aug13wnvsite10$X10 <- as.numeric(aug13wnvsite10$X10)
aug20wnvsite10$X10 <- as.numeric(aug20wnvsite10$X10)
sep03wnvsite10$X10 <- as.numeric(sep03wnvsite10$X10)


ajun04wnvsite10 <- sum(jun04wnvsite10$X10, na.rm=T)
ajun12wnvsite10 <- sum(jun12wnvsite10$X10, na.rm=T)
ajun18wnvsite10 <- sum(jun18wnvsite10$X10, na.rm=T)
ajun25wnvsite10 <- sum(jun25wnvsite10$X10, na.rm=T)
ajul04wnvsite10 <- sum(jul04wnvsite10$X10, na.rm=T)
ajul10wnvsite10 <- sum(jul10wnvsite10$X10, na.rm=T)
ajul17wnvsite10 <- sum(jul17wnvsite10$X10, na.rm=T)
ajul24wnvsite10 <- sum(jul24wnvsite10$X10, na.rm=T)
ajul30wnvsite10 <- sum(jul30wnvsite10$X10, na.rm=T)
aaug06wnvsite10 <- sum(aug06wnvsite10$X10, na.rm=T)
aaug13wnvsite10 <- sum(aug13wnvsite10$X10, na.rm=T)
aaug20wnvsite10 <- sum(aug20wnvsite10$X10, na.rm=T)
asep03wnvsite10 <- sum(sep03wnvsite10$X10, na.rm=T)


#NONwnv
jun04nonwnvsite10$X10 <- as.numeric(jun04nonwnvsite10$X10)
jun12nonwnvsite10$X10 <- as.numeric(jun12nonwnvsite10$X10)
jun18nonwnvsite10$X10 <- as.numeric(jun18nonwnvsite10$X10)
jun25nonwnvsite10$X10 <- as.numeric(jun25nonwnvsite10$X10)
jul04nonwnvsite10$X10 <- as.numeric(jul04nonwnvsite10$X10)
jul10nonwnvsite10$X10 <- as.numeric(jul10nonwnvsite10$X10)
jul17nonwnvsite10$X10 <- as.numeric(jul17nonwnvsite10$X10)
jul24nonwnvsite10$X10 <- as.numeric(jul24nonwnvsite10$X10)
jul30nonwnvsite10$X10 <- as.numeric(jul30nonwnvsite10$X10)
aug06nonwnvsite10$X10 <- as.numeric(aug06nonwnvsite10$X10)
aug13nonwnvsite10$X10 <- as.numeric(aug13nonwnvsite10$X10)
aug20nonwnvsite10$X10 <- as.numeric(aug20nonwnvsite10$X10)
sep03nonwnvsite10$X10 <- as.numeric(sep03nonwnvsite10$X10)


ajun04nonwnvsite10 <- sum(jun04nonwnvsite10$X10, na.rm=T)
ajun12nonwnvsite10 <- sum(jun12nonwnvsite10$X10, na.rm=T)
ajun18nonwnvsite10 <- sum(jun18nonwnvsite10$X10, na.rm=T)
ajun25nonwnvsite10 <- sum(jun25nonwnvsite10$X10, na.rm=T)
ajul04nonwnvsite10 <- sum(jul04nonwnvsite10$X10, na.rm=T)
ajul10nonwnvsite10 <- sum(jul10nonwnvsite10$X10, na.rm=T)
ajul17nonwnvsite10 <- sum(jul17nonwnvsite10$X10, na.rm=T)
ajul24nonwnvsite10 <- sum(jul24nonwnvsite10$X10, na.rm=T)
ajul30nonwnvsite10 <- sum(jul30nonwnvsite10$X10, na.rm=T)
aaug06nonwnvsite10 <- sum(aug06nonwnvsite10$X10, na.rm=T)
aaug13nonwnvsite10 <- sum(aug13nonwnvsite10$X10, na.rm=T)
aaug20nonwnvsite10 <- sum(aug20nonwnvsite10$X10, na.rm=T)
asep03nonwnvsite10 <- sum(sep03nonwnvsite10$X10, na.rm=T)



#####################
#
#
#
#
#
################################


#CREATE wnvS FOR PLOTTING
#Site 1
wnv1 <- c(ajun04wnvsite1,
          ajun12wnvsite1,
          ajun18wnvsite1,
          ajun25wnvsite1,
          ajul04wnvsite1,
          ajul10wnvsite1,
          ajul17wnvsite1,
          ajul24wnvsite1,
          ajul30wnvsite1,
          aaug06wnvsite1,
          aaug13wnvsite1,
          aaug20wnvsite1,
          asep03wnvsite1)

#Site 2
wnv2 <- c(ajun04wnvsite2,
          ajun12wnvsite2,
          ajun18wnvsite2,
          ajun25wnvsite2,
          ajul04wnvsite2,
          ajul10wnvsite2,
          ajul17wnvsite2,
          ajul24wnvsite2,
          ajul30wnvsite2,
          aaug06wnvsite2,
          aaug13wnvsite2,
          aaug20wnvsite2,
          asep03wnvsite2)


#Site 3
wnv3 <- c(ajun04wnvsite3,
          ajun12wnvsite3,
          ajun18wnvsite3,
          ajun25wnvsite3,
          ajul04wnvsite3,
          ajul10wnvsite3,
          ajul17wnvsite3,
          ajul24wnvsite3,
          ajul30wnvsite3,
          aaug06wnvsite3,
          aaug13wnvsite3,
          aaug20wnvsite3,
          asep03wnvsite3)


#Site 4
wnv4 <- c(ajun04wnvsite4,
          ajun12wnvsite4,
          ajun18wnvsite4,
          ajun25wnvsite4,
          ajul04wnvsite4,
          ajul10wnvsite4,
          ajul17wnvsite4,
          ajul24wnvsite4,
          ajul30wnvsite4,
          aaug06wnvsite4,
          aaug13wnvsite4,
          aaug20wnvsite4,
          asep03wnvsite4)


#Site 5
wnv5 <- c(ajun04wnvsite5,
          ajun12wnvsite5,
          ajun18wnvsite5,
          ajun25wnvsite5,
          ajul04wnvsite5,
          ajul10wnvsite5,
          ajul17wnvsite5,
          ajul24wnvsite5,
          ajul30wnvsite5,
          aaug06wnvsite5,
          aaug13wnvsite5,
          aaug20wnvsite5,
          asep03wnvsite5)


#Site 6
wnv6 <- c(ajun04wnvsite6,
          ajun12wnvsite6,
          ajun18wnvsite6,
          ajun25wnvsite6,
          ajul04wnvsite6,
          ajul10wnvsite6,
          ajul17wnvsite6,
          ajul24wnvsite6,
          ajul30wnvsite6,
          aaug06wnvsite6,
          aaug13wnvsite6,
          aaug20wnvsite6,
          asep03wnvsite6)


#Site 7
wnv7 <- c(ajun04wnvsite7,
          ajun12wnvsite7,
          ajun18wnvsite7,
          ajun25wnvsite7,
          ajul04wnvsite7,
          ajul10wnvsite7,
          ajul17wnvsite7,
          ajul24wnvsite7,
          ajul30wnvsite7,
          aaug06wnvsite7,
          aaug13wnvsite7,
          aaug20wnvsite7,
          asep03wnvsite7)


#Site 8
wnv8 <- c(ajun04wnvsite8,
          ajun12wnvsite8,
          ajun18wnvsite8,
          ajun25wnvsite8,
          ajul04wnvsite8,
          ajul10wnvsite8,
          ajul17wnvsite8,
          ajul24wnvsite8,
          ajul30wnvsite8,
          aaug06wnvsite8,
          aaug13wnvsite8,
          aaug20wnvsite8,
          asep03wnvsite8)


#Site 9
wnv9 <- c(ajun04wnvsite9,
          ajun12wnvsite9,
          ajun18wnvsite9,
          ajun25wnvsite9,
          ajul04wnvsite9,
          ajul10wnvsite9,
          ajul17wnvsite9,
          ajul24wnvsite9,
          ajul30wnvsite9,
          aaug06wnvsite9,
          aaug13wnvsite9,
          aaug20wnvsite9,
          asep03wnvsite9)


#Site 10
wnv10 <- c(ajun04wnvsite10,
           ajun12wnvsite10,
           ajun18wnvsite10,
           ajun25wnvsite10,
           ajul04wnvsite10,
           ajul10wnvsite10,
           ajul17wnvsite10,
           ajul24wnvsite10,
           ajul30wnvsite10,
           aaug06wnvsite10,
           aaug13wnvsite10,
           aaug20wnvsite10,
           asep03wnvsite10)



wnvtotal <- wnv1+wnv2+wnv3+wnv4+wnv5+wnv6+wnv7+wnv8+wnv9+wnv10


##### slev 2 - SLEV
#SUBSET BY DATE and slev
jun04slev <- subset(mergedslev, date2  == as.Date("2013-06-04"))
jun12slev <- subset(mergedslev, date2  == as.Date("2013-06-12"))
jun18slev <- subset(mergedslev, date2  == as.Date("2013-06-18"))
jun25slev <- subset(mergedslev, date2  == as.Date("2013-06-25"))
jul04slev <- subset(mergedslev, date2  == as.Date("2013-07-04"))
jul10slev <- subset(mergedslev, date2  == as.Date("2013-07-10"))
jul17slev <- subset(mergedslev, date2  == as.Date("2013-07-17"))
jul24slev <- subset(mergedslev, date2  == as.Date("2013-07-24"))
jul30slev <- subset(mergedslev, date2  == as.Date("2013-07-30"))
aug06slev <- subset(mergedslev, date2  == as.Date("2013-08-06"))
aug13slev <- subset(mergedslev, date2  == as.Date("2013-08-13"))
aug20slev <- subset(mergedslev, date2  == as.Date("2013-08-20"))
sep03slev <- subset(mergedslev, date2  == as.Date("2013-09-03"))


####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE slev SITE

#JUN 04 slev
jun04slevsite1 <-subset(merged1slev, date2  == as.Date("2013-06-04"))
jun04slevsite2 <-subset(merged2slev, date2  == as.Date("2013-06-04"))
jun04slevsite3 <-subset(merged3slev, date2  == as.Date("2013-06-04"))
jun04slevsite4 <-subset(merged4slev, date2  == as.Date("2013-06-04"))
jun04slevsite5 <-subset(merged5slev, date2  == as.Date("2013-06-04"))
jun04slevsite6 <-subset(merged6slev, date2  == as.Date("2013-06-04"))
jun04slevsite7 <-subset(merged7slev, date2  == as.Date("2013-06-04"))
jun04slevsite8 <-subset(merged8slev, date2  == as.Date("2013-06-04"))
jun04slevsite9 <-subset(merged9slev, date2  == as.Date("2013-06-04"))
jun04slevsite10 <-subset(merged10slev, date2  == as.Date("2013-06-04"))


#jun12 slev
jun12slevsite1 <-subset(merged1slev, date2  == as.Date("2013-06-12"))
jun12slevsite2 <-subset(merged2slev, date2  == as.Date("2013-06-12"))
jun12slevsite3 <-subset(merged3slev, date2  == as.Date("2013-06-12"))
jun12slevsite4 <-subset(merged4slev, date2  == as.Date("2013-06-12"))
jun12slevsite5 <-subset(merged5slev, date2  == as.Date("2013-06-12"))
jun12slevsite6 <-subset(merged6slev, date2  == as.Date("2013-06-12"))
jun12slevsite7 <-subset(merged7slev, date2  == as.Date("2013-06-12"))
jun12slevsite8 <-subset(merged8slev, date2  == as.Date("2013-06-12"))
jun12slevsite9 <-subset(merged9slev, date2  == as.Date("2013-06-12"))
jun12slevsite10 <-subset(merged10slev, date2  == as.Date("2013-06-12"))

#jun18 slev
jun18slevsite1 <-subset(merged1slev, date2  == as.Date("2013-06-18"))
jun18slevsite2 <-subset(merged2slev, date2  == as.Date("2013-06-18"))
jun18slevsite3 <-subset(merged3slev, date2  == as.Date("2013-06-18"))
jun18slevsite4 <-subset(merged4slev, date2  == as.Date("2013-06-18"))
jun18slevsite5 <-subset(merged5slev, date2  == as.Date("2013-06-18"))
jun18slevsite6 <-subset(merged6slev, date2  == as.Date("2013-06-18"))
jun18slevsite7 <-subset(merged7slev, date2  == as.Date("2013-06-18"))
jun18slevsite8 <-subset(merged8slev, date2  == as.Date("2013-06-18"))
jun18slevsite9 <-subset(merged9slev, date2  == as.Date("2013-06-18"))
jun18slevsite10 <-subset(merged10slev, date2  == as.Date("2013-06-18"))

#jun25 slev
jun25slevsite1 <-subset(merged1slev, date2  == as.Date("2013-06-25"))
jun25slevsite2 <-subset(merged2slev, date2  == as.Date("2013-06-25"))
jun25slevsite3 <-subset(merged3slev, date2  == as.Date("2013-06-25"))
jun25slevsite4 <-subset(merged4slev, date2  == as.Date("2013-06-25"))
jun25slevsite5 <-subset(merged5slev, date2  == as.Date("2013-06-25"))
jun25slevsite6 <-subset(merged6slev, date2  == as.Date("2013-06-25"))
jun25slevsite7 <-subset(merged7slev, date2  == as.Date("2013-06-25"))
jun25slevsite8 <-subset(merged8slev, date2  == as.Date("2013-06-25"))
jun25slevsite9 <-subset(merged9slev, date2  == as.Date("2013-06-25"))
jun25slevsite10 <-subset(merged10slev, date2  == as.Date("2013-06-25"))

#jul04 slev
jul04slevsite1 <-subset(merged1slev, date2  == as.Date("2013-07-04"))
jul04slevsite2 <-subset(merged2slev, date2  == as.Date("2013-07-04"))
jul04slevsite3 <-subset(merged3slev, date2  == as.Date("2013-07-04"))
jul04slevsite4 <-subset(merged4slev, date2  == as.Date("2013-07-04"))
jul04slevsite5 <-subset(merged5slev, date2  == as.Date("2013-07-04"))
jul04slevsite6 <-subset(merged6slev, date2  == as.Date("2013-07-04"))
jul04slevsite7 <-subset(merged7slev, date2  == as.Date("2013-07-04"))
jul04slevsite8 <-subset(merged8slev, date2  == as.Date("2013-07-04"))
jul04slevsite9 <-subset(merged9slev, date2  == as.Date("2013-07-04"))
jul04slevsite10 <-subset(merged10slev, date2  == as.Date("2013-07-04"))

#jul10 slev
jul10slevsite1 <-subset(merged1slev, date2  == as.Date("2013-07-10"))
jul10slevsite2 <-subset(merged2slev, date2  == as.Date("2013-07-10"))
jul10slevsite3 <-subset(merged3slev, date2  == as.Date("2013-07-10"))
jul10slevsite4 <-subset(merged4slev, date2  == as.Date("2013-07-10"))
jul10slevsite5 <-subset(merged5slev, date2  == as.Date("2013-07-10"))
jul10slevsite6 <-subset(merged6slev, date2  == as.Date("2013-07-10"))
jul10slevsite7 <-subset(merged7slev, date2  == as.Date("2013-07-10"))
jul10slevsite8 <-subset(merged8slev, date2  == as.Date("2013-07-10"))
jul10slevsite9 <-subset(merged9slev, date2  == as.Date("2013-07-10"))
jul10slevsite10 <-subset(merged10slev, date2  == as.Date("2013-07-10"))

#jul17 slev
jul17slevsite1 <-subset(merged1slev, date2  == as.Date("2013-07-17"))
jul17slevsite2 <-subset(merged2slev, date2  == as.Date("2013-07-17"))
jul17slevsite3 <-subset(merged3slev, date2  == as.Date("2013-07-17"))
jul17slevsite4 <-subset(merged4slev, date2  == as.Date("2013-07-17"))
jul17slevsite5 <-subset(merged5slev, date2  == as.Date("2013-07-17"))
jul17slevsite6 <-subset(merged6slev, date2  == as.Date("2013-07-17"))
jul17slevsite7 <-subset(merged7slev, date2  == as.Date("2013-07-17"))
jul17slevsite8 <-subset(merged8slev, date2  == as.Date("2013-07-17"))
jul17slevsite9 <-subset(merged9slev, date2  == as.Date("2013-07-17"))
jul17slevsite10 <-subset(merged10slev, date2  == as.Date("2013-07-17"))

#jul24 slev
jul24slevsite1 <-subset(merged1slev, date2  == as.Date("2013-07-24"))
jul24slevsite2 <-subset(merged2slev, date2  == as.Date("2013-07-24"))
jul24slevsite3 <-subset(merged3slev, date2  == as.Date("2013-07-24"))
jul24slevsite4 <-subset(merged4slev, date2  == as.Date("2013-07-24"))
jul24slevsite5 <-subset(merged5slev, date2  == as.Date("2013-07-24"))
jul24slevsite6 <-subset(merged6slev, date2  == as.Date("2013-07-24"))
jul24slevsite7 <-subset(merged7slev, date2  == as.Date("2013-07-24"))
jul24slevsite8 <-subset(merged8slev, date2  == as.Date("2013-07-24"))
jul24slevsite9 <-subset(merged9slev, date2  == as.Date("2013-07-24"))
jul24slevsite10 <-subset(merged10slev, date2  == as.Date("2013-07-24"))

#jul30 slev
jul30slevsite1 <-subset(merged1slev, date2  == as.Date("2013-07-30"))
jul30slevsite2 <-subset(merged2slev, date2  == as.Date("2013-07-30"))
jul30slevsite3 <-subset(merged3slev, date2  == as.Date("2013-07-30"))
jul30slevsite4 <-subset(merged4slev, date2  == as.Date("2013-07-30"))
jul30slevsite5 <-subset(merged5slev, date2  == as.Date("2013-07-30"))
jul30slevsite6 <-subset(merged6slev, date2  == as.Date("2013-07-30"))
jul30slevsite7 <-subset(merged7slev, date2  == as.Date("2013-07-30"))
jul30slevsite8 <-subset(merged8slev, date2  == as.Date("2013-07-30"))
jul30slevsite9 <-subset(merged9slev, date2  == as.Date("2013-07-30"))
jul30slevsite10 <-subset(merged10slev, date2  == as.Date("2013-07-30"))

#aug06 slev
aug06slevsite1 <-subset(merged1slev, date2  == as.Date("2013-08-06"))
aug06slevsite2 <-subset(merged2slev, date2  == as.Date("2013-08-06"))
aug06slevsite3 <-subset(merged3slev, date2  == as.Date("2013-08-06"))
aug06slevsite4 <-subset(merged4slev, date2  == as.Date("2013-08-06"))
aug06slevsite5 <-subset(merged5slev, date2  == as.Date("2013-08-06"))
aug06slevsite6 <-subset(merged6slev, date2  == as.Date("2013-08-06"))
aug06slevsite7 <-subset(merged7slev, date2  == as.Date("2013-08-06"))
aug06slevsite8 <-subset(merged8slev, date2  == as.Date("2013-08-06"))
aug06slevsite9 <-subset(merged9slev, date2  == as.Date("2013-08-06"))
aug06slevsite10 <-subset(merged10slev, date2  == as.Date("2013-08-06"))

#aug13 slev
aug13slevsite1 <-subset(merged1slev, date2  == as.Date("2013-08-13"))
aug13slevsite2 <-subset(merged2slev, date2  == as.Date("2013-08-13"))
aug13slevsite3 <-subset(merged3slev, date2  == as.Date("2013-08-13"))
aug13slevsite4 <-subset(merged4slev, date2  == as.Date("2013-08-13"))
aug13slevsite5 <-subset(merged5slev, date2  == as.Date("2013-08-13"))
aug13slevsite6 <-subset(merged6slev, date2  == as.Date("2013-08-13"))
aug13slevsite7 <-subset(merged7slev, date2  == as.Date("2013-08-13"))
aug13slevsite8 <-subset(merged8slev, date2  == as.Date("2013-08-13"))
aug13slevsite9 <-subset(merged9slev, date2  == as.Date("2013-08-13"))
aug13slevsite10 <-subset(merged10slev, date2  == as.Date("2013-08-13"))

#aug20 slev
aug20slevsite1 <-subset(merged1slev, date2  == as.Date("2013-08-20"))
aug20slevsite2 <-subset(merged2slev, date2  == as.Date("2013-08-20"))
aug20slevsite3 <-subset(merged3slev, date2  == as.Date("2013-08-20"))
aug20slevsite4 <-subset(merged4slev, date2  == as.Date("2013-08-20"))
aug20slevsite5 <-subset(merged5slev, date2  == as.Date("2013-08-20"))
aug20slevsite6 <-subset(merged6slev, date2  == as.Date("2013-08-20"))
aug20slevsite7 <-subset(merged7slev, date2  == as.Date("2013-08-20"))
aug20slevsite8 <-subset(merged8slev, date2  == as.Date("2013-08-20"))
aug20slevsite9 <-subset(merged9slev, date2  == as.Date("2013-08-20"))
aug20slevsite10 <-subset(merged10slev, date2  == as.Date("2013-08-20"))

#sep03 slev
sep03slevsite1 <-subset(merged1slev, date2  == as.Date("2013-09-03"))
sep03slevsite2 <-subset(merged2slev, date2  == as.Date("2013-09-03"))
sep03slevsite3 <-subset(merged3slev, date2  == as.Date("2013-09-03"))
sep03slevsite4 <-subset(merged4slev, date2  == as.Date("2013-09-03"))
sep03slevsite5 <-subset(merged5slev, date2  == as.Date("2013-09-03"))
sep03slevsite6 <-subset(merged6slev, date2  == as.Date("2013-09-03"))
sep03slevsite7 <-subset(merged7slev, date2  == as.Date("2013-09-03"))
sep03slevsite8 <-subset(merged8slev, date2  == as.Date("2013-09-03"))
sep03slevsite9 <-subset(merged9slev, date2  == as.Date("2013-09-03"))
sep03slevsite10 <-subset(merged10slev, date2  == as.Date("2013-09-03"))

##### ALL SITES COMBINED
#slev
ajun04slevtotal <- sum(jun04slev$total, na.rm=T)
ajun12slevtotal <- sum(jun12slev$total, na.rm=T)
ajun18slevtotal <- sum(jun18slev$total, na.rm=T)
ajun25slevtotal <- sum(jun25slev$total, na.rm=T)
ajul04slevtotal <- sum(jul04slev$total, na.rm=T)
ajul10slevtotal <- sum(jul10slev$total, na.rm=T)
ajul17slevtotal <- sum(jul17slev$total, na.rm=T)
ajul24slevtotal <- sum(jul24slev$total, na.rm=T)
ajul30slevtotal <- sum(jul30slev$total, na.rm=T)
aaug06slevtotal <- sum(aug06slev$total, na.rm=T)
aaug13slevtotal <- sum(aug13slev$total, na.rm=T)
aaug20slevtotal <- sum(aug20slev$total, na.rm=T)
asep03slevtotal <- sum(sep03slev$total, na.rm=T)



#####################################
##### site1
#slev
ajun04slevsite1 <- sum(jun04slevsite1$X1, na.rm=T)
ajun12slevsite1 <- sum(jun12slevsite1$X1, na.rm=T)
ajun18slevsite1 <- sum(jun18slevsite1$X1, na.rm=T)
ajun25slevsite1 <- sum(jun25slevsite1$X1, na.rm=T)
ajul04slevsite1 <- sum(jul04slevsite1$X1, na.rm=T)
ajul10slevsite1 <- sum(jul10slevsite1$X1, na.rm=T)
ajul17slevsite1 <- sum(jul17slevsite1$X1, na.rm=T)
ajul24slevsite1 <- sum(jul24slevsite1$X1, na.rm=T)
ajul30slevsite1 <- sum(jul30slevsite1$X1, na.rm=T)
aaug06slevsite1 <- sum(aug06slevsite1$X1, na.rm=T)
aaug13slevsite1 <- sum(aug13slevsite1$X1, na.rm=T)
aaug20slevsite1 <- sum(aug20slevsite1$X1, na.rm=T)
asep03slevsite1 <- sum(sep03slevsite1$X1, na.rm=T)


#NONslev
ajun04nonslevsite1 <- sum(jun04nonslevsite1$X1, na.rm=T)
ajun12nonslevsite1 <- sum(jun12nonslevsite1$X1, na.rm=T)
ajun18nonslevsite1 <- sum(jun18nonslevsite1$X1, na.rm=T)
ajun25nonslevsite1 <- sum(jun25nonslevsite1$X1, na.rm=T)
ajul04nonslevsite1 <- sum(jul04nonslevsite1$X1, na.rm=T)
ajul10nonslevsite1 <- sum(jul10nonslevsite1$X1, na.rm=T)
ajul17nonslevsite1 <- sum(jul17nonslevsite1$X1, na.rm=T)
ajul24nonslevsite1 <- sum(jul24nonslevsite1$X1, na.rm=T)
ajul30nonslevsite1 <- sum(jul30nonslevsite1$X1, na.rm=T)
aaug06nonslevsite1 <- sum(aug06nonslevsite1$X1, na.rm=T)
aaug13nonslevsite1 <- sum(aug13nonslevsite1$X1, na.rm=T)
aaug20nonslevsite1 <- sum(aug20nonslevsite1$X1, na.rm=T)
asep03nonslevsite1 <- sum(sep03nonslevsite1$X1, na.rm=T)

#####################################
##### site2
#slev
ajun04slevsite2 <- sum(jun04slevsite2$X2, na.rm=T)
ajun12slevsite2 <- sum(jun12slevsite2$X2, na.rm=T)
ajun18slevsite2 <- sum(jun18slevsite2$X2, na.rm=T)
ajun25slevsite2 <- sum(jun25slevsite2$X2, na.rm=T)
ajul04slevsite2 <- sum(jul04slevsite2$X2, na.rm=T)
ajul10slevsite2 <- sum(jul10slevsite2$X2, na.rm=T)
ajul17slevsite2 <- sum(jul17slevsite2$X2, na.rm=T)
ajul24slevsite2 <- sum(jul24slevsite2$X2, na.rm=T)
ajul30slevsite2 <- sum(jul30slevsite2$X2, na.rm=T)
aaug06slevsite2 <- sum(aug06slevsite2$X2, na.rm=T)
aaug13slevsite2 <- sum(aug13slevsite2$X2, na.rm=T)
aaug20slevsite2 <- sum(aug20slevsite2$X2, na.rm=T)
asep03slevsite2 <- sum(sep03slevsite2$X2, na.rm=T)


#NONslev
ajun04nonslevsite2 <- sum(jun04nonslevsite2$X2, na.rm=T)
ajun12nonslevsite2 <- sum(jun12nonslevsite2$X2, na.rm=T)
ajun18nonslevsite2 <- sum(jun18nonslevsite2$X2, na.rm=T)
ajun25nonslevsite2 <- sum(jun25nonslevsite2$X2, na.rm=T)
ajul04nonslevsite2 <- sum(jul04nonslevsite2$X2, na.rm=T)
ajul10nonslevsite2 <- sum(jul10nonslevsite2$X2, na.rm=T)
ajul17nonslevsite2 <- sum(jul17nonslevsite2$X2, na.rm=T)
ajul24nonslevsite2 <- sum(jul24nonslevsite2$X2, na.rm=T)
ajul30nonslevsite2 <- sum(jul30nonslevsite2$X2, na.rm=T)
aaug06nonslevsite2 <- sum(aug06nonslevsite2$X2, na.rm=T)
aaug13nonslevsite2 <- sum(aug13nonslevsite2$X2, na.rm=T)
aaug20nonslevsite2 <- sum(aug20nonslevsite2$X2, na.rm=T)
asep03nonslevsite2 <- sum(sep03nonslevsite2$X2, na.rm=T)

#####################################
##### site3
#slev
ajun04slevsite3 <- sum(jun04slevsite3$X3, na.rm=T)
ajun12slevsite3 <- sum(jun12slevsite3$X3, na.rm=T)
ajun18slevsite3 <- sum(jun18slevsite3$X3, na.rm=T)
ajun25slevsite3 <- sum(jun25slevsite3$X3, na.rm=T)
ajul04slevsite3 <- sum(jul04slevsite3$X3, na.rm=T)
ajul10slevsite3 <- sum(jul10slevsite3$X3, na.rm=T)
ajul17slevsite3 <- sum(jul17slevsite3$X3, na.rm=T)
ajul24slevsite3 <- sum(jul24slevsite3$X3, na.rm=T)
ajul30slevsite3 <- sum(jul30slevsite3$X3, na.rm=T)
aaug06slevsite3 <- sum(aug06slevsite3$X3, na.rm=T)
aaug13slevsite3 <- sum(aug13slevsite3$X3, na.rm=T)
aaug20slevsite3 <- sum(aug20slevsite3$X3, na.rm=T)
asep03slevsite3 <- sum(sep03slevsite3$X3, na.rm=T)


#NONslev
ajun04nonslevsite3 <- sum(jun04nonslevsite3$X3, na.rm=T)
ajun12nonslevsite3 <- sum(jun12nonslevsite3$X3, na.rm=T)
ajun18nonslevsite3 <- sum(jun18nonslevsite3$X3, na.rm=T)
ajun25nonslevsite3 <- sum(jun25nonslevsite3$X3, na.rm=T)
ajul04nonslevsite3 <- sum(jul04nonslevsite3$X3, na.rm=T)
ajul10nonslevsite3 <- sum(jul10nonslevsite3$X3, na.rm=T)
ajul17nonslevsite3 <- sum(jul17nonslevsite3$X3, na.rm=T)
ajul24nonslevsite3 <- sum(jul24nonslevsite3$X3, na.rm=T)
ajul30nonslevsite3 <- sum(jul30nonslevsite3$X3, na.rm=T)
aaug06nonslevsite3 <- sum(aug06nonslevsite3$X3, na.rm=T)
aaug13nonslevsite3 <- sum(aug13nonslevsite3$X3, na.rm=T)
aaug20nonslevsite3 <- sum(aug20nonslevsite3$X3, na.rm=T)
asep03nonslevsite3 <- sum(sep03nonslevsite3$X3, na.rm=T)


#####################################
##### site4
#slev
ajun04slevsite4 <- sum(jun04slevsite4$X4, na.rm=T)
ajun12slevsite4 <- sum(jun12slevsite4$X4, na.rm=T)
ajun18slevsite4 <- sum(jun18slevsite4$X4, na.rm=T)
ajun25slevsite4 <- sum(jun25slevsite4$X4, na.rm=T)
ajul04slevsite4 <- sum(jul04slevsite4$X4, na.rm=T)
ajul10slevsite4 <- sum(jul10slevsite4$X4, na.rm=T)
ajul17slevsite4 <- sum(jul17slevsite4$X4, na.rm=T)
ajul24slevsite4 <- sum(jul24slevsite4$X4, na.rm=T)
ajul30slevsite4 <- sum(jul30slevsite4$X4, na.rm=T)
aaug06slevsite4 <- sum(aug06slevsite4$X4, na.rm=T)
aaug13slevsite4 <- sum(aug13slevsite4$X4, na.rm=T)
aaug20slevsite4 <- sum(aug20slevsite4$X4, na.rm=T)
asep03slevsite4 <- sum(sep03slevsite4$X4, na.rm=T)


#NONslev
ajun04nonslevsite4 <- sum(jun04nonslevsite4$X4, na.rm=T)
ajun12nonslevsite4 <- sum(jun12nonslevsite4$X4, na.rm=T)
ajun18nonslevsite4 <- sum(jun18nonslevsite4$X4, na.rm=T)
ajun25nonslevsite4 <- sum(jun25nonslevsite4$X4, na.rm=T)
ajul04nonslevsite4 <- sum(jul04nonslevsite4$X4, na.rm=T)
ajul10nonslevsite4 <- sum(jul10nonslevsite4$X4, na.rm=T)
ajul17nonslevsite4 <- sum(jul17nonslevsite4$X4, na.rm=T)
ajul24nonslevsite4 <- sum(jul24nonslevsite4$X4, na.rm=T)
ajul30nonslevsite4 <- sum(jul30nonslevsite4$X4, na.rm=T)
aaug06nonslevsite4 <- sum(aug06nonslevsite4$X4, na.rm=T)
aaug13nonslevsite4 <- sum(aug13nonslevsite4$X4, na.rm=T)
aaug20nonslevsite4 <- sum(aug20nonslevsite4$X4, na.rm=T)
asep03nonslevsite4 <- sum(sep03nonslevsite4$X4, na.rm=T)

#####################################
##### site5
#slev
ajun04slevsite5 <- sum(jun04slevsite5$X5, na.rm=T)
ajun12slevsite5 <- sum(jun12slevsite5$X5, na.rm=T)
ajun18slevsite5 <- sum(jun18slevsite5$X5, na.rm=T)
ajun25slevsite5 <- sum(jun25slevsite5$X5, na.rm=T)
ajul04slevsite5 <- sum(jul04slevsite5$X5, na.rm=T)
ajul10slevsite5 <- sum(jul10slevsite5$X5, na.rm=T)
ajul17slevsite5 <- sum(jul17slevsite5$X5, na.rm=T)
ajul24slevsite5 <- sum(jul24slevsite5$X5, na.rm=T)
ajul30slevsite5 <- sum(jul30slevsite5$X5, na.rm=T)
aaug06slevsite5 <- sum(aug06slevsite5$X5, na.rm=T)
aaug13slevsite5 <- sum(aug13slevsite5$X5, na.rm=T)
aaug20slevsite5 <- sum(aug20slevsite5$X5, na.rm=T)
asep03slevsite5 <- sum(sep03slevsite5$X5, na.rm=T)


#NONslev
ajun04nonslevsite5 <- sum(jun04nonslevsite5$X5, na.rm=T)
ajun12nonslevsite5 <- sum(jun12nonslevsite5$X5, na.rm=T)
ajun18nonslevsite5 <- sum(jun18nonslevsite5$X5, na.rm=T)
ajun25nonslevsite5 <- sum(jun25nonslevsite5$X5, na.rm=T)
ajul04nonslevsite5 <- sum(jul04nonslevsite5$X5, na.rm=T)
ajul10nonslevsite5 <- sum(jul10nonslevsite5$X5, na.rm=T)
ajul17nonslevsite5 <- sum(jul17nonslevsite5$X5, na.rm=T)
ajul24nonslevsite5 <- sum(jul24nonslevsite5$X5, na.rm=T)
ajul30nonslevsite5 <- sum(jul30nonslevsite5$X5, na.rm=T)
aaug06nonslevsite5 <- sum(aug06nonslevsite5$X5, na.rm=T)
aaug13nonslevsite5 <- sum(aug13nonslevsite5$X5, na.rm=T)
aaug20nonslevsite5 <- sum(aug20nonslevsite5$X5, na.rm=T)
asep03nonslevsite5 <- sum(sep03nonslevsite5$X5, na.rm=T)


#####################################
##### site6
#slev
jun04slevsite6$X6 <- as.numeric(jun04slevsite6$X6)
jun12slevsite6$X6 <- as.numeric(jun12slevsite6$X6)
jun18slevsite6$X6 <- as.numeric(jun18slevsite6$X6)
jun25slevsite6$X6 <- as.numeric(jun25slevsite6$X6)
jul04slevsite6$X6 <- as.numeric(jul04slevsite6$X6)
jul10slevsite6$X6 <- as.numeric(jul10slevsite6$X6)
jul17slevsite6$X6 <- as.numeric(jul17slevsite6$X6)
jul24slevsite6$X6 <- as.numeric(jul24slevsite6$X6)
jul30slevsite6$X6 <- as.numeric(jul30slevsite6$X6)
aug06slevsite6$X6 <- as.numeric(aug06slevsite6$X6)
aug13slevsite6$X6 <- as.numeric(aug13slevsite6$X6)
aug20slevsite6$X6 <- as.numeric(aug20slevsite6$X6)
sep03slevsite6$X6 <- as.numeric(sep03slevsite6$X6)


ajun04slevsite6 <- sum(jun04slevsite6$X6, na.rm=T)
ajun12slevsite6 <- sum(jun12slevsite6$X6, na.rm=T)
ajun18slevsite6 <- sum(jun18slevsite6$X6, na.rm=T)
ajun25slevsite6 <- sum(jun25slevsite6$X6, na.rm=T)
ajul04slevsite6 <- sum(jul04slevsite6$X6, na.rm=T)
ajul10slevsite6 <- sum(jul10slevsite6$X6, na.rm=T)
ajul17slevsite6 <- sum(jul17slevsite6$X6, na.rm=T)
ajul24slevsite6 <- sum(jul24slevsite6$X6, na.rm=T)
ajul30slevsite6 <- sum(jul30slevsite6$X6, na.rm=T)
aaug06slevsite6 <- sum(aug06slevsite6$X6, na.rm=T)
aaug13slevsite6 <- sum(aug13slevsite6$X6, na.rm=T)
aaug20slevsite6 <- sum(aug20slevsite6$X6, na.rm=T)
asep03slevsite6 <- sum(sep03slevsite6$X6, na.rm=T)


#NONslev
jun04nonslevsite6$X6 <- as.numeric(jun04nonslevsite6$X6)
jun12nonslevsite6$X6 <- as.numeric(jun12nonslevsite6$X6)
jun18nonslevsite6$X6 <- as.numeric(jun18nonslevsite6$X6)
jun25nonslevsite6$X6 <- as.numeric(jun25nonslevsite6$X6)
jul04nonslevsite6$X6 <- as.numeric(jul04nonslevsite6$X6)
jul10nonslevsite6$X6 <- as.numeric(jul10nonslevsite6$X6)
jul17nonslevsite6$X6 <- as.numeric(jul17nonslevsite6$X6)
jul24nonslevsite6$X6 <- as.numeric(jul24nonslevsite6$X6)
jul30nonslevsite6$X6 <- as.numeric(jul30nonslevsite6$X6)
aug06nonslevsite6$X6 <- as.numeric(aug06nonslevsite6$X6)
aug13nonslevsite6$X6 <- as.numeric(aug13nonslevsite6$X6)
aug20nonslevsite6$X6 <- as.numeric(aug20nonslevsite6$X6)
sep03nonslevsite6$X6 <- as.numeric(sep03nonslevsite6$X6)


ajun04nonslevsite6 <- sum(jun04nonslevsite6$X6, na.rm=T)
ajun12nonslevsite6 <- sum(jun12nonslevsite6$X6, na.rm=T)
ajun18nonslevsite6 <- sum(jun18nonslevsite6$X6, na.rm=T)
ajun25nonslevsite6 <- sum(jun25nonslevsite6$X6, na.rm=T)
ajul04nonslevsite6 <- sum(jul04nonslevsite6$X6, na.rm=T)
ajul10nonslevsite6 <- sum(jul10nonslevsite6$X6, na.rm=T)
ajul17nonslevsite6 <- sum(jul17nonslevsite6$X6, na.rm=T)
ajul24nonslevsite6 <- sum(jul24nonslevsite6$X6, na.rm=T)
ajul30nonslevsite6 <- sum(jul30nonslevsite6$X6, na.rm=T)
aaug06nonslevsite6 <- sum(aug06nonslevsite6$X6, na.rm=T)
aaug13nonslevsite6 <- sum(aug13nonslevsite6$X6, na.rm=T)
aaug20nonslevsite6 <- sum(aug20nonslevsite6$X6, na.rm=T)
asep03nonslevsite6 <- sum(sep03nonslevsite6$X6, na.rm=T)


#####################################
##### site7
#slev
ajun04slevsite7 <- sum(jun04slevsite7$X7, na.rm=T)
ajun12slevsite7 <- sum(jun12slevsite7$X7, na.rm=T)
ajun18slevsite7 <- sum(jun18slevsite7$X7, na.rm=T)
ajun25slevsite7 <- sum(jun25slevsite7$X7, na.rm=T)
ajul04slevsite7 <- sum(jul04slevsite7$X7, na.rm=T)
ajul10slevsite7 <- sum(jul10slevsite7$X7, na.rm=T)
ajul17slevsite7 <- sum(jul17slevsite7$X7, na.rm=T)
ajul24slevsite7 <- sum(jul24slevsite7$X7, na.rm=T)
ajul30slevsite7 <- sum(jul30slevsite7$X7, na.rm=T)
aaug06slevsite7 <- sum(aug06slevsite7$X7, na.rm=T)
aaug13slevsite7 <- sum(aug13slevsite7$X7, na.rm=T)
aaug20slevsite7 <- sum(aug20slevsite7$X7, na.rm=T)
asep03slevsite7 <- sum(sep03slevsite7$X7, na.rm=T)


#NONslev
ajun04nonslevsite7 <- sum(jun04nonslevsite7$X7, na.rm=T)
ajun12nonslevsite7 <- sum(jun12nonslevsite7$X7, na.rm=T)
ajun18nonslevsite7 <- sum(jun18nonslevsite7$X7, na.rm=T)
ajun25nonslevsite7 <- sum(jun25nonslevsite7$X7, na.rm=T)
ajul04nonslevsite7 <- sum(jul04nonslevsite7$X7, na.rm=T)
ajul10nonslevsite7 <- sum(jul10nonslevsite7$X7, na.rm=T)
ajul17nonslevsite7 <- sum(jul17nonslevsite7$X7, na.rm=T)
ajul24nonslevsite7 <- sum(jul24nonslevsite7$X7, na.rm=T)
ajul30nonslevsite7 <- sum(jul30nonslevsite7$X7, na.rm=T)
aaug06nonslevsite7 <- sum(aug06nonslevsite7$X7, na.rm=T)
aaug13nonslevsite7 <- sum(aug13nonslevsite7$X7, na.rm=T)
aaug20nonslevsite7 <- sum(aug20nonslevsite7$X7, na.rm=T)
asep03nonslevsite7 <- sum(sep03nonslevsite7$X7, na.rm=T)


#####################################
##### site8
#slev
ajun04slevsite8 <- sum(jun04slevsite8$X8, na.rm=T)
ajun12slevsite8 <- sum(jun12slevsite8$X8, na.rm=T)
ajun18slevsite8 <- sum(jun18slevsite8$X8, na.rm=T)
ajun25slevsite8 <- sum(jun25slevsite8$X8, na.rm=T)
ajul04slevsite8 <- sum(jul04slevsite8$X8, na.rm=T)
ajul10slevsite8 <- sum(jul10slevsite8$X8, na.rm=T)
ajul17slevsite8 <- sum(jul17slevsite8$X8, na.rm=T)
ajul24slevsite8 <- sum(jul24slevsite8$X8, na.rm=T)
ajul30slevsite8 <- sum(jul30slevsite8$X8, na.rm=T)
aaug06slevsite8 <- sum(aug06slevsite8$X8, na.rm=T)
aaug13slevsite8 <- sum(aug13slevsite8$X8, na.rm=T)
aaug20slevsite8 <- sum(aug20slevsite8$X8, na.rm=T)
asep03slevsite8 <- sum(sep03slevsite8$X8, na.rm=T)


#NONslev
ajun04nonslevsite8 <- sum(jun04nonslevsite8$X8, na.rm=T)
ajun12nonslevsite8 <- sum(jun12nonslevsite8$X8, na.rm=T)
ajun18nonslevsite8 <- sum(jun18nonslevsite8$X8, na.rm=T)
ajun25nonslevsite8 <- sum(jun25nonslevsite8$X8, na.rm=T)
ajul04nonslevsite8 <- sum(jul04nonslevsite8$X8, na.rm=T)
ajul10nonslevsite8 <- sum(jul10nonslevsite8$X8, na.rm=T)
ajul17nonslevsite8 <- sum(jul17nonslevsite8$X8, na.rm=T)
ajul24nonslevsite8 <- sum(jul24nonslevsite8$X8, na.rm=T)
ajul30nonslevsite8 <- sum(jul30nonslevsite8$X8, na.rm=T)
aaug06nonslevsite8 <- sum(aug06nonslevsite8$X8, na.rm=T)
aaug13nonslevsite8 <- sum(aug13nonslevsite8$X8, na.rm=T)
aaug20nonslevsite8 <- sum(aug20nonslevsite8$X8, na.rm=T)
asep03nonslevsite8 <- sum(sep03nonslevsite8$X8, na.rm=T)


#####################################
##### site9
#slev

jun04slevsite9$X9 <- as.numeric(jun04slevsite9$X9)
jun12slevsite9$X9 <- as.numeric(jun12slevsite9$X9)
jun18slevsite9$X9 <- as.numeric(jun18slevsite9$X9)
jun25slevsite9$X9 <- as.numeric(jun25slevsite9$X9)
jul04slevsite9$X9 <- as.numeric(jul04slevsite9$X9)
jul10slevsite9$X9 <- as.numeric(jul10slevsite9$X9)
jul17slevsite9$X9 <- as.numeric(jul17slevsite9$X9)
jul24slevsite9$X9 <- as.numeric(jul24slevsite9$X9)
jul30slevsite9$X9 <- as.numeric(jul30slevsite9$X9)
aug06slevsite9$X9 <- as.numeric(aug06slevsite9$X9)
aug13slevsite9$X9 <- as.numeric(aug13slevsite9$X9)
aug20slevsite9$X9 <- as.numeric(aug20slevsite9$X9)
sep03slevsite9$X9 <- as.numeric(sep03slevsite9$X9)


ajun04slevsite9 <- sum(jun04slevsite9$X9, na.rm=T)
ajun12slevsite9 <- sum(jun12slevsite9$X9, na.rm=T)
ajun18slevsite9 <- sum(jun18slevsite9$X9, na.rm=T)
ajun25slevsite9 <- sum(jun25slevsite9$X9, na.rm=T)
ajul04slevsite9 <- sum(jul04slevsite9$X9, na.rm=T)
ajul10slevsite9 <- sum(jul10slevsite9$X9, na.rm=T)
ajul17slevsite9 <- sum(jul17slevsite9$X9, na.rm=T)
ajul24slevsite9 <- sum(jul24slevsite9$X9, na.rm=T)
ajul30slevsite9 <- sum(jul30slevsite9$X9, na.rm=T)
aaug06slevsite9 <- sum(aug06slevsite9$X9, na.rm=T)
aaug13slevsite9 <- sum(aug13slevsite9$X9, na.rm=T)
aaug20slevsite9 <- sum(aug20slevsite9$X9, na.rm=T)
asep03slevsite9 <- sum(sep03slevsite9$X9, na.rm=T)


#NONslev
jun04nonslevsite9$X9 <- as.numeric(jun04nonslevsite9$X9)
jun12nonslevsite9$X9 <- as.numeric(jun12nonslevsite9$X9)
jun18nonslevsite9$X9 <- as.numeric(jun18nonslevsite9$X9)
jun25nonslevsite9$X9 <- as.numeric(jun25nonslevsite9$X9)
jul04nonslevsite9$X9 <- as.numeric(jul04nonslevsite9$X9)
jul10nonslevsite9$X9 <- as.numeric(jul10nonslevsite9$X9)
jul17nonslevsite9$X9 <- as.numeric(jul17nonslevsite9$X9)
jul24nonslevsite9$X9 <- as.numeric(jul24nonslevsite9$X9)
jul30nonslevsite9$X9 <- as.numeric(jul30nonslevsite9$X9)
aug06nonslevsite9$X9 <- as.numeric(aug06nonslevsite9$X9)
aug13nonslevsite9$X9 <- as.numeric(aug13nonslevsite9$X9)
aug20nonslevsite9$X9 <- as.numeric(aug20nonslevsite9$X9)
sep03nonslevsite9$X9 <- as.numeric(sep03nonslevsite9$X9)


ajun04nonslevsite9 <- sum(jun04nonslevsite9$X9, na.rm=T)
ajun12nonslevsite9 <- sum(jun12nonslevsite9$X9, na.rm=T)
ajun18nonslevsite9 <- sum(jun18nonslevsite9$X9, na.rm=T)
ajun25nonslevsite9 <- sum(jun25nonslevsite9$X9, na.rm=T)
ajul04nonslevsite9 <- sum(jul04nonslevsite9$X9, na.rm=T)
ajul10nonslevsite9 <- sum(jul10nonslevsite9$X9, na.rm=T)
ajul17nonslevsite9 <- sum(jul17nonslevsite9$X9, na.rm=T)
ajul24nonslevsite9 <- sum(jul24nonslevsite9$X9, na.rm=T)
ajul30nonslevsite9 <- sum(jul30nonslevsite9$X9, na.rm=T)
aaug06nonslevsite9 <- sum(aug06nonslevsite9$X9, na.rm=T)
aaug13nonslevsite9 <- sum(aug13nonslevsite9$X9, na.rm=T)
aaug20nonslevsite9 <- sum(aug20nonslevsite9$X9, na.rm=T)
asep03nonslevsite9 <- sum(sep03nonslevsite9$X9, na.rm=T)

#####################################
##### site10
#slev
jun04slevsite10$X10 <- as.numeric(jun04slevsite10$X10)
jun12slevsite10$X10 <- as.numeric(jun12slevsite10$X10)
jun18slevsite10$X10 <- as.numeric(jun18slevsite10$X10)
jun25slevsite10$X10 <- as.numeric(jun25slevsite10$X10)
jul04slevsite10$X10 <- as.numeric(jul04slevsite10$X10)
jul10slevsite10$X10 <- as.numeric(jul10slevsite10$X10)
jul17slevsite10$X10 <- as.numeric(jul17slevsite10$X10)
jul24slevsite10$X10 <- as.numeric(jul24slevsite10$X10)
jul30slevsite10$X10 <- as.numeric(jul30slevsite10$X10)
aug06slevsite10$X10 <- as.numeric(aug06slevsite10$X10)
aug13slevsite10$X10 <- as.numeric(aug13slevsite10$X10)
aug20slevsite10$X10 <- as.numeric(aug20slevsite10$X10)
sep03slevsite10$X10 <- as.numeric(sep03slevsite10$X10)


ajun04slevsite10 <- sum(jun04slevsite10$X10, na.rm=T)
ajun12slevsite10 <- sum(jun12slevsite10$X10, na.rm=T)
ajun18slevsite10 <- sum(jun18slevsite10$X10, na.rm=T)
ajun25slevsite10 <- sum(jun25slevsite10$X10, na.rm=T)
ajul04slevsite10 <- sum(jul04slevsite10$X10, na.rm=T)
ajul10slevsite10 <- sum(jul10slevsite10$X10, na.rm=T)
ajul17slevsite10 <- sum(jul17slevsite10$X10, na.rm=T)
ajul24slevsite10 <- sum(jul24slevsite10$X10, na.rm=T)
ajul30slevsite10 <- sum(jul30slevsite10$X10, na.rm=T)
aaug06slevsite10 <- sum(aug06slevsite10$X10, na.rm=T)
aaug13slevsite10 <- sum(aug13slevsite10$X10, na.rm=T)
aaug20slevsite10 <- sum(aug20slevsite10$X10, na.rm=T)
asep03slevsite10 <- sum(sep03slevsite10$X10, na.rm=T)


#NONslev
jun04nonslevsite10$X10 <- as.numeric(jun04nonslevsite10$X10)
jun12nonslevsite10$X10 <- as.numeric(jun12nonslevsite10$X10)
jun18nonslevsite10$X10 <- as.numeric(jun18nonslevsite10$X10)
jun25nonslevsite10$X10 <- as.numeric(jun25nonslevsite10$X10)
jul04nonslevsite10$X10 <- as.numeric(jul04nonslevsite10$X10)
jul10nonslevsite10$X10 <- as.numeric(jul10nonslevsite10$X10)
jul17nonslevsite10$X10 <- as.numeric(jul17nonslevsite10$X10)
jul24nonslevsite10$X10 <- as.numeric(jul24nonslevsite10$X10)
jul30nonslevsite10$X10 <- as.numeric(jul30nonslevsite10$X10)
aug06nonslevsite10$X10 <- as.numeric(aug06nonslevsite10$X10)
aug13nonslevsite10$X10 <- as.numeric(aug13nonslevsite10$X10)
aug20nonslevsite10$X10 <- as.numeric(aug20nonslevsite10$X10)
sep03nonslevsite10$X10 <- as.numeric(sep03nonslevsite10$X10)


ajun04nonslevsite10 <- sum(jun04nonslevsite10$X10, na.rm=T)
ajun12nonslevsite10 <- sum(jun12nonslevsite10$X10, na.rm=T)
ajun18nonslevsite10 <- sum(jun18nonslevsite10$X10, na.rm=T)
ajun25nonslevsite10 <- sum(jun25nonslevsite10$X10, na.rm=T)
ajul04nonslevsite10 <- sum(jul04nonslevsite10$X10, na.rm=T)
ajul10nonslevsite10 <- sum(jul10nonslevsite10$X10, na.rm=T)
ajul17nonslevsite10 <- sum(jul17nonslevsite10$X10, na.rm=T)
ajul24nonslevsite10 <- sum(jul24nonslevsite10$X10, na.rm=T)
ajul30nonslevsite10 <- sum(jul30nonslevsite10$X10, na.rm=T)
aaug06nonslevsite10 <- sum(aug06nonslevsite10$X10, na.rm=T)
aaug13nonslevsite10 <- sum(aug13nonslevsite10$X10, na.rm=T)
aaug20nonslevsite10 <- sum(aug20nonslevsite10$X10, na.rm=T)
asep03nonslevsite10 <- sum(sep03nonslevsite10$X10, na.rm=T)



#####################
#
#
#
#
#
################################


#CREATE slevS FOR PLOTTING
#Site 1
slev1 <- c(ajun04slevsite1,
          ajun12slevsite1,
          ajun18slevsite1,
          ajun25slevsite1,
          ajul04slevsite1,
          ajul10slevsite1,
          ajul17slevsite1,
          ajul24slevsite1,
          ajul30slevsite1,
          aaug06slevsite1,
          aaug13slevsite1,
          aaug20slevsite1,
          asep03slevsite1)

#Site 2
slev2 <- c(ajun04slevsite2,
          ajun12slevsite2,
          ajun18slevsite2,
          ajun25slevsite2,
          ajul04slevsite2,
          ajul10slevsite2,
          ajul17slevsite2,
          ajul24slevsite2,
          ajul30slevsite2,
          aaug06slevsite2,
          aaug13slevsite2,
          aaug20slevsite2,
          asep03slevsite2)


#Site 3
slev3 <- c(ajun04slevsite3,
          ajun12slevsite3,
          ajun18slevsite3,
          ajun25slevsite3,
          ajul04slevsite3,
          ajul10slevsite3,
          ajul17slevsite3,
          ajul24slevsite3,
          ajul30slevsite3,
          aaug06slevsite3,
          aaug13slevsite3,
          aaug20slevsite3,
          asep03slevsite3)


#Site 4
slev4 <- c(ajun04slevsite4,
          ajun12slevsite4,
          ajun18slevsite4,
          ajun25slevsite4,
          ajul04slevsite4,
          ajul10slevsite4,
          ajul17slevsite4,
          ajul24slevsite4,
          ajul30slevsite4,
          aaug06slevsite4,
          aaug13slevsite4,
          aaug20slevsite4,
          asep03slevsite4)


#Site 5
slev5 <- c(ajun04slevsite5,
          ajun12slevsite5,
          ajun18slevsite5,
          ajun25slevsite5,
          ajul04slevsite5,
          ajul10slevsite5,
          ajul17slevsite5,
          ajul24slevsite5,
          ajul30slevsite5,
          aaug06slevsite5,
          aaug13slevsite5,
          aaug20slevsite5,
          asep03slevsite5)


#Site 6
slev6 <- c(ajun04slevsite6,
          ajun12slevsite6,
          ajun18slevsite6,
          ajun25slevsite6,
          ajul04slevsite6,
          ajul10slevsite6,
          ajul17slevsite6,
          ajul24slevsite6,
          ajul30slevsite6,
          aaug06slevsite6,
          aaug13slevsite6,
          aaug20slevsite6,
          asep03slevsite6)


#Site 7
slev7 <- c(ajun04slevsite7,
          ajun12slevsite7,
          ajun18slevsite7,
          ajun25slevsite7,
          ajul04slevsite7,
          ajul10slevsite7,
          ajul17slevsite7,
          ajul24slevsite7,
          ajul30slevsite7,
          aaug06slevsite7,
          aaug13slevsite7,
          aaug20slevsite7,
          asep03slevsite7)


#Site 8
slev8 <- c(ajun04slevsite8,
          ajun12slevsite8,
          ajun18slevsite8,
          ajun25slevsite8,
          ajul04slevsite8,
          ajul10slevsite8,
          ajul17slevsite8,
          ajul24slevsite8,
          ajul30slevsite8,
          aaug06slevsite8,
          aaug13slevsite8,
          aaug20slevsite8,
          asep03slevsite8)


#Site 9
slev9 <- c(ajun04slevsite9,
          ajun12slevsite9,
          ajun18slevsite9,
          ajun25slevsite9,
          ajul04slevsite9,
          ajul10slevsite9,
          ajul17slevsite9,
          ajul24slevsite9,
          ajul30slevsite9,
          aaug06slevsite9,
          aaug13slevsite9,
          aaug20slevsite9,
          asep03slevsite9)


#Site 10
slev10 <- c(ajun04slevsite10,
           ajun12slevsite10,
           ajun18slevsite10,
           ajun25slevsite10,
           ajul04slevsite10,
           ajul10slevsite10,
           ajul17slevsite10,
           ajul24slevsite10,
           ajul30slevsite10,
           aaug06slevsite10,
           aaug13slevsite10,
           aaug20slevsite10,
           asep03slevsite10)



slevtotal <- slev1+slev2+slev3+slev4+slev5+slev6+slev7+slev8+slev9+slev10


##### eeev 3 - EEEV
#SUBSET BY DATE and eeev
jun04eeev <- subset(mergedeeev, date2  == as.Date("2013-06-04"))
jun12eeev <- subset(mergedeeev, date2  == as.Date("2013-06-12"))
jun18eeev <- subset(mergedeeev, date2  == as.Date("2013-06-18"))
jun25eeev <- subset(mergedeeev, date2  == as.Date("2013-06-25"))
jul04eeev <- subset(mergedeeev, date2  == as.Date("2013-07-04"))
jul10eeev <- subset(mergedeeev, date2  == as.Date("2013-07-10"))
jul17eeev <- subset(mergedeeev, date2  == as.Date("2013-07-17"))
jul24eeev <- subset(mergedeeev, date2  == as.Date("2013-07-24"))
jul30eeev <- subset(mergedeeev, date2  == as.Date("2013-07-30"))
aug06eeev <- subset(mergedeeev, date2  == as.Date("2013-08-06"))
aug13eeev <- subset(mergedeeev, date2  == as.Date("2013-08-13"))
aug20eeev <- subset(mergedeeev, date2  == as.Date("2013-08-20"))
sep03eeev <- subset(mergedeeev, date2  == as.Date("2013-09-03"))


####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE eeev SITE

#JUN 04 eeev
jun04eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-06-04"))
jun04eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-06-04"))


#jun12 eeev
jun12eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-06-12"))
jun12eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-06-12"))

#jun18 eeev
jun18eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-06-18"))
jun18eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-06-18"))

#jun25 eeev
jun25eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-06-25"))
jun25eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-06-25"))

#jul04 eeev
jul04eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-07-04"))
jul04eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-07-04"))

#jul10 eeev
jul10eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-07-10"))
jul10eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-07-10"))

#jul17 eeev
jul17eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-07-17"))
jul17eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-07-17"))

#jul24 eeev
jul24eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-07-24"))
jul24eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-07-24"))

#jul30 eeev
jul30eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-07-30"))
jul30eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-07-30"))

#aug06 eeev
aug06eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-08-06"))
aug06eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-08-06"))

#aug13 eeev
aug13eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-08-13"))
aug13eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-08-13"))

#aug20 eeev
aug20eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-08-20"))
aug20eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-08-20"))

#sep03 eeev
sep03eeevsite1 <-subset(merged1eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite2 <-subset(merged2eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite3 <-subset(merged3eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite4 <-subset(merged4eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite5 <-subset(merged5eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite6 <-subset(merged6eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite7 <-subset(merged7eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite8 <-subset(merged8eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite9 <-subset(merged9eeev, date2  == as.Date("2013-09-03"))
sep03eeevsite10 <-subset(merged10eeev, date2  == as.Date("2013-09-03"))

##### ALL SITES COMBINED
#eeev
ajun04eeevtotal <- sum(jun04eeev$total, na.rm=T)
ajun12eeevtotal <- sum(jun12eeev$total, na.rm=T)
ajun18eeevtotal <- sum(jun18eeev$total, na.rm=T)
ajun25eeevtotal <- sum(jun25eeev$total, na.rm=T)
ajul04eeevtotal <- sum(jul04eeev$total, na.rm=T)
ajul10eeevtotal <- sum(jul10eeev$total, na.rm=T)
ajul17eeevtotal <- sum(jul17eeev$total, na.rm=T)
ajul24eeevtotal <- sum(jul24eeev$total, na.rm=T)
ajul30eeevtotal <- sum(jul30eeev$total, na.rm=T)
aaug06eeevtotal <- sum(aug06eeev$total, na.rm=T)
aaug13eeevtotal <- sum(aug13eeev$total, na.rm=T)
aaug20eeevtotal <- sum(aug20eeev$total, na.rm=T)
asep03eeevtotal <- sum(sep03eeev$total, na.rm=T)



#####################################
##### site1
#eeev
ajun04eeevsite1 <- sum(jun04eeevsite1$X1, na.rm=T)
ajun12eeevsite1 <- sum(jun12eeevsite1$X1, na.rm=T)
ajun18eeevsite1 <- sum(jun18eeevsite1$X1, na.rm=T)
ajun25eeevsite1 <- sum(jun25eeevsite1$X1, na.rm=T)
ajul04eeevsite1 <- sum(jul04eeevsite1$X1, na.rm=T)
ajul10eeevsite1 <- sum(jul10eeevsite1$X1, na.rm=T)
ajul17eeevsite1 <- sum(jul17eeevsite1$X1, na.rm=T)
ajul24eeevsite1 <- sum(jul24eeevsite1$X1, na.rm=T)
ajul30eeevsite1 <- sum(jul30eeevsite1$X1, na.rm=T)
aaug06eeevsite1 <- sum(aug06eeevsite1$X1, na.rm=T)
aaug13eeevsite1 <- sum(aug13eeevsite1$X1, na.rm=T)
aaug20eeevsite1 <- sum(aug20eeevsite1$X1, na.rm=T)
asep03eeevsite1 <- sum(sep03eeevsite1$X1, na.rm=T)


#NONeeev
ajun04noneeevsite1 <- sum(jun04noneeevsite1$X1, na.rm=T)
ajun12noneeevsite1 <- sum(jun12noneeevsite1$X1, na.rm=T)
ajun18noneeevsite1 <- sum(jun18noneeevsite1$X1, na.rm=T)
ajun25noneeevsite1 <- sum(jun25noneeevsite1$X1, na.rm=T)
ajul04noneeevsite1 <- sum(jul04noneeevsite1$X1, na.rm=T)
ajul10noneeevsite1 <- sum(jul10noneeevsite1$X1, na.rm=T)
ajul17noneeevsite1 <- sum(jul17noneeevsite1$X1, na.rm=T)
ajul24noneeevsite1 <- sum(jul24noneeevsite1$X1, na.rm=T)
ajul30noneeevsite1 <- sum(jul30noneeevsite1$X1, na.rm=T)
aaug06noneeevsite1 <- sum(aug06noneeevsite1$X1, na.rm=T)
aaug13noneeevsite1 <- sum(aug13noneeevsite1$X1, na.rm=T)
aaug20noneeevsite1 <- sum(aug20noneeevsite1$X1, na.rm=T)
asep03noneeevsite1 <- sum(sep03noneeevsite1$X1, na.rm=T)

#####################################
##### site2
#eeev
ajun04eeevsite2 <- sum(jun04eeevsite2$X2, na.rm=T)
ajun12eeevsite2 <- sum(jun12eeevsite2$X2, na.rm=T)
ajun18eeevsite2 <- sum(jun18eeevsite2$X2, na.rm=T)
ajun25eeevsite2 <- sum(jun25eeevsite2$X2, na.rm=T)
ajul04eeevsite2 <- sum(jul04eeevsite2$X2, na.rm=T)
ajul10eeevsite2 <- sum(jul10eeevsite2$X2, na.rm=T)
ajul17eeevsite2 <- sum(jul17eeevsite2$X2, na.rm=T)
ajul24eeevsite2 <- sum(jul24eeevsite2$X2, na.rm=T)
ajul30eeevsite2 <- sum(jul30eeevsite2$X2, na.rm=T)
aaug06eeevsite2 <- sum(aug06eeevsite2$X2, na.rm=T)
aaug13eeevsite2 <- sum(aug13eeevsite2$X2, na.rm=T)
aaug20eeevsite2 <- sum(aug20eeevsite2$X2, na.rm=T)
asep03eeevsite2 <- sum(sep03eeevsite2$X2, na.rm=T)


#NONeeev
ajun04noneeevsite2 <- sum(jun04noneeevsite2$X2, na.rm=T)
ajun12noneeevsite2 <- sum(jun12noneeevsite2$X2, na.rm=T)
ajun18noneeevsite2 <- sum(jun18noneeevsite2$X2, na.rm=T)
ajun25noneeevsite2 <- sum(jun25noneeevsite2$X2, na.rm=T)
ajul04noneeevsite2 <- sum(jul04noneeevsite2$X2, na.rm=T)
ajul10noneeevsite2 <- sum(jul10noneeevsite2$X2, na.rm=T)
ajul17noneeevsite2 <- sum(jul17noneeevsite2$X2, na.rm=T)
ajul24noneeevsite2 <- sum(jul24noneeevsite2$X2, na.rm=T)
ajul30noneeevsite2 <- sum(jul30noneeevsite2$X2, na.rm=T)
aaug06noneeevsite2 <- sum(aug06noneeevsite2$X2, na.rm=T)
aaug13noneeevsite2 <- sum(aug13noneeevsite2$X2, na.rm=T)
aaug20noneeevsite2 <- sum(aug20noneeevsite2$X2, na.rm=T)
asep03noneeevsite2 <- sum(sep03noneeevsite2$X2, na.rm=T)

#####################################
##### site3
#eeev
ajun04eeevsite3 <- sum(jun04eeevsite3$X3, na.rm=T)
ajun12eeevsite3 <- sum(jun12eeevsite3$X3, na.rm=T)
ajun18eeevsite3 <- sum(jun18eeevsite3$X3, na.rm=T)
ajun25eeevsite3 <- sum(jun25eeevsite3$X3, na.rm=T)
ajul04eeevsite3 <- sum(jul04eeevsite3$X3, na.rm=T)
ajul10eeevsite3 <- sum(jul10eeevsite3$X3, na.rm=T)
ajul17eeevsite3 <- sum(jul17eeevsite3$X3, na.rm=T)
ajul24eeevsite3 <- sum(jul24eeevsite3$X3, na.rm=T)
ajul30eeevsite3 <- sum(jul30eeevsite3$X3, na.rm=T)
aaug06eeevsite3 <- sum(aug06eeevsite3$X3, na.rm=T)
aaug13eeevsite3 <- sum(aug13eeevsite3$X3, na.rm=T)
aaug20eeevsite3 <- sum(aug20eeevsite3$X3, na.rm=T)
asep03eeevsite3 <- sum(sep03eeevsite3$X3, na.rm=T)


#NONeeev
ajun04noneeevsite3 <- sum(jun04noneeevsite3$X3, na.rm=T)
ajun12noneeevsite3 <- sum(jun12noneeevsite3$X3, na.rm=T)
ajun18noneeevsite3 <- sum(jun18noneeevsite3$X3, na.rm=T)
ajun25noneeevsite3 <- sum(jun25noneeevsite3$X3, na.rm=T)
ajul04noneeevsite3 <- sum(jul04noneeevsite3$X3, na.rm=T)
ajul10noneeevsite3 <- sum(jul10noneeevsite3$X3, na.rm=T)
ajul17noneeevsite3 <- sum(jul17noneeevsite3$X3, na.rm=T)
ajul24noneeevsite3 <- sum(jul24noneeevsite3$X3, na.rm=T)
ajul30noneeevsite3 <- sum(jul30noneeevsite3$X3, na.rm=T)
aaug06noneeevsite3 <- sum(aug06noneeevsite3$X3, na.rm=T)
aaug13noneeevsite3 <- sum(aug13noneeevsite3$X3, na.rm=T)
aaug20noneeevsite3 <- sum(aug20noneeevsite3$X3, na.rm=T)
asep03noneeevsite3 <- sum(sep03noneeevsite3$X3, na.rm=T)


#####################################
##### site4
#eeev
ajun04eeevsite4 <- sum(jun04eeevsite4$X4, na.rm=T)
ajun12eeevsite4 <- sum(jun12eeevsite4$X4, na.rm=T)
ajun18eeevsite4 <- sum(jun18eeevsite4$X4, na.rm=T)
ajun25eeevsite4 <- sum(jun25eeevsite4$X4, na.rm=T)
ajul04eeevsite4 <- sum(jul04eeevsite4$X4, na.rm=T)
ajul10eeevsite4 <- sum(jul10eeevsite4$X4, na.rm=T)
ajul17eeevsite4 <- sum(jul17eeevsite4$X4, na.rm=T)
ajul24eeevsite4 <- sum(jul24eeevsite4$X4, na.rm=T)
ajul30eeevsite4 <- sum(jul30eeevsite4$X4, na.rm=T)
aaug06eeevsite4 <- sum(aug06eeevsite4$X4, na.rm=T)
aaug13eeevsite4 <- sum(aug13eeevsite4$X4, na.rm=T)
aaug20eeevsite4 <- sum(aug20eeevsite4$X4, na.rm=T)
asep03eeevsite4 <- sum(sep03eeevsite4$X4, na.rm=T)


#NONeeev
ajun04noneeevsite4 <- sum(jun04noneeevsite4$X4, na.rm=T)
ajun12noneeevsite4 <- sum(jun12noneeevsite4$X4, na.rm=T)
ajun18noneeevsite4 <- sum(jun18noneeevsite4$X4, na.rm=T)
ajun25noneeevsite4 <- sum(jun25noneeevsite4$X4, na.rm=T)
ajul04noneeevsite4 <- sum(jul04noneeevsite4$X4, na.rm=T)
ajul10noneeevsite4 <- sum(jul10noneeevsite4$X4, na.rm=T)
ajul17noneeevsite4 <- sum(jul17noneeevsite4$X4, na.rm=T)
ajul24noneeevsite4 <- sum(jul24noneeevsite4$X4, na.rm=T)
ajul30noneeevsite4 <- sum(jul30noneeevsite4$X4, na.rm=T)
aaug06noneeevsite4 <- sum(aug06noneeevsite4$X4, na.rm=T)
aaug13noneeevsite4 <- sum(aug13noneeevsite4$X4, na.rm=T)
aaug20noneeevsite4 <- sum(aug20noneeevsite4$X4, na.rm=T)
asep03noneeevsite4 <- sum(sep03noneeevsite4$X4, na.rm=T)

#####################################
##### site5
#eeev
ajun04eeevsite5 <- sum(jun04eeevsite5$X5, na.rm=T)
ajun12eeevsite5 <- sum(jun12eeevsite5$X5, na.rm=T)
ajun18eeevsite5 <- sum(jun18eeevsite5$X5, na.rm=T)
ajun25eeevsite5 <- sum(jun25eeevsite5$X5, na.rm=T)
ajul04eeevsite5 <- sum(jul04eeevsite5$X5, na.rm=T)
ajul10eeevsite5 <- sum(jul10eeevsite5$X5, na.rm=T)
ajul17eeevsite5 <- sum(jul17eeevsite5$X5, na.rm=T)
ajul24eeevsite5 <- sum(jul24eeevsite5$X5, na.rm=T)
ajul30eeevsite5 <- sum(jul30eeevsite5$X5, na.rm=T)
aaug06eeevsite5 <- sum(aug06eeevsite5$X5, na.rm=T)
aaug13eeevsite5 <- sum(aug13eeevsite5$X5, na.rm=T)
aaug20eeevsite5 <- sum(aug20eeevsite5$X5, na.rm=T)
asep03eeevsite5 <- sum(sep03eeevsite5$X5, na.rm=T)


#NONeeev
ajun04noneeevsite5 <- sum(jun04noneeevsite5$X5, na.rm=T)
ajun12noneeevsite5 <- sum(jun12noneeevsite5$X5, na.rm=T)
ajun18noneeevsite5 <- sum(jun18noneeevsite5$X5, na.rm=T)
ajun25noneeevsite5 <- sum(jun25noneeevsite5$X5, na.rm=T)
ajul04noneeevsite5 <- sum(jul04noneeevsite5$X5, na.rm=T)
ajul10noneeevsite5 <- sum(jul10noneeevsite5$X5, na.rm=T)
ajul17noneeevsite5 <- sum(jul17noneeevsite5$X5, na.rm=T)
ajul24noneeevsite5 <- sum(jul24noneeevsite5$X5, na.rm=T)
ajul30noneeevsite5 <- sum(jul30noneeevsite5$X5, na.rm=T)
aaug06noneeevsite5 <- sum(aug06noneeevsite5$X5, na.rm=T)
aaug13noneeevsite5 <- sum(aug13noneeevsite5$X5, na.rm=T)
aaug20noneeevsite5 <- sum(aug20noneeevsite5$X5, na.rm=T)
asep03noneeevsite5 <- sum(sep03noneeevsite5$X5, na.rm=T)


#####################################
##### site6
#eeev
jun04eeevsite6$X6 <- as.numeric(jun04eeevsite6$X6)
jun12eeevsite6$X6 <- as.numeric(jun12eeevsite6$X6)
jun18eeevsite6$X6 <- as.numeric(jun18eeevsite6$X6)
jun25eeevsite6$X6 <- as.numeric(jun25eeevsite6$X6)
jul04eeevsite6$X6 <- as.numeric(jul04eeevsite6$X6)
jul10eeevsite6$X6 <- as.numeric(jul10eeevsite6$X6)
jul17eeevsite6$X6 <- as.numeric(jul17eeevsite6$X6)
jul24eeevsite6$X6 <- as.numeric(jul24eeevsite6$X6)
jul30eeevsite6$X6 <- as.numeric(jul30eeevsite6$X6)
aug06eeevsite6$X6 <- as.numeric(aug06eeevsite6$X6)
aug13eeevsite6$X6 <- as.numeric(aug13eeevsite6$X6)
aug20eeevsite6$X6 <- as.numeric(aug20eeevsite6$X6)
sep03eeevsite6$X6 <- as.numeric(sep03eeevsite6$X6)


ajun04eeevsite6 <- sum(jun04eeevsite6$X6, na.rm=T)
ajun12eeevsite6 <- sum(jun12eeevsite6$X6, na.rm=T)
ajun18eeevsite6 <- sum(jun18eeevsite6$X6, na.rm=T)
ajun25eeevsite6 <- sum(jun25eeevsite6$X6, na.rm=T)
ajul04eeevsite6 <- sum(jul04eeevsite6$X6, na.rm=T)
ajul10eeevsite6 <- sum(jul10eeevsite6$X6, na.rm=T)
ajul17eeevsite6 <- sum(jul17eeevsite6$X6, na.rm=T)
ajul24eeevsite6 <- sum(jul24eeevsite6$X6, na.rm=T)
ajul30eeevsite6 <- sum(jul30eeevsite6$X6, na.rm=T)
aaug06eeevsite6 <- sum(aug06eeevsite6$X6, na.rm=T)
aaug13eeevsite6 <- sum(aug13eeevsite6$X6, na.rm=T)
aaug20eeevsite6 <- sum(aug20eeevsite6$X6, na.rm=T)
asep03eeevsite6 <- sum(sep03eeevsite6$X6, na.rm=T)


#NONeeev
jun04noneeevsite6$X6 <- as.numeric(jun04noneeevsite6$X6)
jun12noneeevsite6$X6 <- as.numeric(jun12noneeevsite6$X6)
jun18noneeevsite6$X6 <- as.numeric(jun18noneeevsite6$X6)
jun25noneeevsite6$X6 <- as.numeric(jun25noneeevsite6$X6)
jul04noneeevsite6$X6 <- as.numeric(jul04noneeevsite6$X6)
jul10noneeevsite6$X6 <- as.numeric(jul10noneeevsite6$X6)
jul17noneeevsite6$X6 <- as.numeric(jul17noneeevsite6$X6)
jul24noneeevsite6$X6 <- as.numeric(jul24noneeevsite6$X6)
jul30noneeevsite6$X6 <- as.numeric(jul30noneeevsite6$X6)
aug06noneeevsite6$X6 <- as.numeric(aug06noneeevsite6$X6)
aug13noneeevsite6$X6 <- as.numeric(aug13noneeevsite6$X6)
aug20noneeevsite6$X6 <- as.numeric(aug20noneeevsite6$X6)
sep03noneeevsite6$X6 <- as.numeric(sep03noneeevsite6$X6)


ajun04noneeevsite6 <- sum(jun04noneeevsite6$X6, na.rm=T)
ajun12noneeevsite6 <- sum(jun12noneeevsite6$X6, na.rm=T)
ajun18noneeevsite6 <- sum(jun18noneeevsite6$X6, na.rm=T)
ajun25noneeevsite6 <- sum(jun25noneeevsite6$X6, na.rm=T)
ajul04noneeevsite6 <- sum(jul04noneeevsite6$X6, na.rm=T)
ajul10noneeevsite6 <- sum(jul10noneeevsite6$X6, na.rm=T)
ajul17noneeevsite6 <- sum(jul17noneeevsite6$X6, na.rm=T)
ajul24noneeevsite6 <- sum(jul24noneeevsite6$X6, na.rm=T)
ajul30noneeevsite6 <- sum(jul30noneeevsite6$X6, na.rm=T)
aaug06noneeevsite6 <- sum(aug06noneeevsite6$X6, na.rm=T)
aaug13noneeevsite6 <- sum(aug13noneeevsite6$X6, na.rm=T)
aaug20noneeevsite6 <- sum(aug20noneeevsite6$X6, na.rm=T)
asep03noneeevsite6 <- sum(sep03noneeevsite6$X6, na.rm=T)


#####################################
##### site7
#eeev
ajun04eeevsite7 <- sum(jun04eeevsite7$X7, na.rm=T)
ajun12eeevsite7 <- sum(jun12eeevsite7$X7, na.rm=T)
ajun18eeevsite7 <- sum(jun18eeevsite7$X7, na.rm=T)
ajun25eeevsite7 <- sum(jun25eeevsite7$X7, na.rm=T)
ajul04eeevsite7 <- sum(jul04eeevsite7$X7, na.rm=T)
ajul10eeevsite7 <- sum(jul10eeevsite7$X7, na.rm=T)
ajul17eeevsite7 <- sum(jul17eeevsite7$X7, na.rm=T)
ajul24eeevsite7 <- sum(jul24eeevsite7$X7, na.rm=T)
ajul30eeevsite7 <- sum(jul30eeevsite7$X7, na.rm=T)
aaug06eeevsite7 <- sum(aug06eeevsite7$X7, na.rm=T)
aaug13eeevsite7 <- sum(aug13eeevsite7$X7, na.rm=T)
aaug20eeevsite7 <- sum(aug20eeevsite7$X7, na.rm=T)
asep03eeevsite7 <- sum(sep03eeevsite7$X7, na.rm=T)


#NONeeev
ajun04noneeevsite7 <- sum(jun04noneeevsite7$X7, na.rm=T)
ajun12noneeevsite7 <- sum(jun12noneeevsite7$X7, na.rm=T)
ajun18noneeevsite7 <- sum(jun18noneeevsite7$X7, na.rm=T)
ajun25noneeevsite7 <- sum(jun25noneeevsite7$X7, na.rm=T)
ajul04noneeevsite7 <- sum(jul04noneeevsite7$X7, na.rm=T)
ajul10noneeevsite7 <- sum(jul10noneeevsite7$X7, na.rm=T)
ajul17noneeevsite7 <- sum(jul17noneeevsite7$X7, na.rm=T)
ajul24noneeevsite7 <- sum(jul24noneeevsite7$X7, na.rm=T)
ajul30noneeevsite7 <- sum(jul30noneeevsite7$X7, na.rm=T)
aaug06noneeevsite7 <- sum(aug06noneeevsite7$X7, na.rm=T)
aaug13noneeevsite7 <- sum(aug13noneeevsite7$X7, na.rm=T)
aaug20noneeevsite7 <- sum(aug20noneeevsite7$X7, na.rm=T)
asep03noneeevsite7 <- sum(sep03noneeevsite7$X7, na.rm=T)


#####################################
##### site8
#eeev
ajun04eeevsite8 <- sum(jun04eeevsite8$X8, na.rm=T)
ajun12eeevsite8 <- sum(jun12eeevsite8$X8, na.rm=T)
ajun18eeevsite8 <- sum(jun18eeevsite8$X8, na.rm=T)
ajun25eeevsite8 <- sum(jun25eeevsite8$X8, na.rm=T)
ajul04eeevsite8 <- sum(jul04eeevsite8$X8, na.rm=T)
ajul10eeevsite8 <- sum(jul10eeevsite8$X8, na.rm=T)
ajul17eeevsite8 <- sum(jul17eeevsite8$X8, na.rm=T)
ajul24eeevsite8 <- sum(jul24eeevsite8$X8, na.rm=T)
ajul30eeevsite8 <- sum(jul30eeevsite8$X8, na.rm=T)
aaug06eeevsite8 <- sum(aug06eeevsite8$X8, na.rm=T)
aaug13eeevsite8 <- sum(aug13eeevsite8$X8, na.rm=T)
aaug20eeevsite8 <- sum(aug20eeevsite8$X8, na.rm=T)
asep03eeevsite8 <- sum(sep03eeevsite8$X8, na.rm=T)


#NONeeev
ajun04noneeevsite8 <- sum(jun04noneeevsite8$X8, na.rm=T)
ajun12noneeevsite8 <- sum(jun12noneeevsite8$X8, na.rm=T)
ajun18noneeevsite8 <- sum(jun18noneeevsite8$X8, na.rm=T)
ajun25noneeevsite8 <- sum(jun25noneeevsite8$X8, na.rm=T)
ajul04noneeevsite8 <- sum(jul04noneeevsite8$X8, na.rm=T)
ajul10noneeevsite8 <- sum(jul10noneeevsite8$X8, na.rm=T)
ajul17noneeevsite8 <- sum(jul17noneeevsite8$X8, na.rm=T)
ajul24noneeevsite8 <- sum(jul24noneeevsite8$X8, na.rm=T)
ajul30noneeevsite8 <- sum(jul30noneeevsite8$X8, na.rm=T)
aaug06noneeevsite8 <- sum(aug06noneeevsite8$X8, na.rm=T)
aaug13noneeevsite8 <- sum(aug13noneeevsite8$X8, na.rm=T)
aaug20noneeevsite8 <- sum(aug20noneeevsite8$X8, na.rm=T)
asep03noneeevsite8 <- sum(sep03noneeevsite8$X8, na.rm=T)


#####################################
##### site9
#eeev

jun04eeevsite9$X9 <- as.numeric(jun04eeevsite9$X9)
jun12eeevsite9$X9 <- as.numeric(jun12eeevsite9$X9)
jun18eeevsite9$X9 <- as.numeric(jun18eeevsite9$X9)
jun25eeevsite9$X9 <- as.numeric(jun25eeevsite9$X9)
jul04eeevsite9$X9 <- as.numeric(jul04eeevsite9$X9)
jul10eeevsite9$X9 <- as.numeric(jul10eeevsite9$X9)
jul17eeevsite9$X9 <- as.numeric(jul17eeevsite9$X9)
jul24eeevsite9$X9 <- as.numeric(jul24eeevsite9$X9)
jul30eeevsite9$X9 <- as.numeric(jul30eeevsite9$X9)
aug06eeevsite9$X9 <- as.numeric(aug06eeevsite9$X9)
aug13eeevsite9$X9 <- as.numeric(aug13eeevsite9$X9)
aug20eeevsite9$X9 <- as.numeric(aug20eeevsite9$X9)
sep03eeevsite9$X9 <- as.numeric(sep03eeevsite9$X9)


ajun04eeevsite9 <- sum(jun04eeevsite9$X9, na.rm=T)
ajun12eeevsite9 <- sum(jun12eeevsite9$X9, na.rm=T)
ajun18eeevsite9 <- sum(jun18eeevsite9$X9, na.rm=T)
ajun25eeevsite9 <- sum(jun25eeevsite9$X9, na.rm=T)
ajul04eeevsite9 <- sum(jul04eeevsite9$X9, na.rm=T)
ajul10eeevsite9 <- sum(jul10eeevsite9$X9, na.rm=T)
ajul17eeevsite9 <- sum(jul17eeevsite9$X9, na.rm=T)
ajul24eeevsite9 <- sum(jul24eeevsite9$X9, na.rm=T)
ajul30eeevsite9 <- sum(jul30eeevsite9$X9, na.rm=T)
aaug06eeevsite9 <- sum(aug06eeevsite9$X9, na.rm=T)
aaug13eeevsite9 <- sum(aug13eeevsite9$X9, na.rm=T)
aaug20eeevsite9 <- sum(aug20eeevsite9$X9, na.rm=T)
asep03eeevsite9 <- sum(sep03eeevsite9$X9, na.rm=T)


#NONeeev
jun04noneeevsite9$X9 <- as.numeric(jun04noneeevsite9$X9)
jun12noneeevsite9$X9 <- as.numeric(jun12noneeevsite9$X9)
jun18noneeevsite9$X9 <- as.numeric(jun18noneeevsite9$X9)
jun25noneeevsite9$X9 <- as.numeric(jun25noneeevsite9$X9)
jul04noneeevsite9$X9 <- as.numeric(jul04noneeevsite9$X9)
jul10noneeevsite9$X9 <- as.numeric(jul10noneeevsite9$X9)
jul17noneeevsite9$X9 <- as.numeric(jul17noneeevsite9$X9)
jul24noneeevsite9$X9 <- as.numeric(jul24noneeevsite9$X9)
jul30noneeevsite9$X9 <- as.numeric(jul30noneeevsite9$X9)
aug06noneeevsite9$X9 <- as.numeric(aug06noneeevsite9$X9)
aug13noneeevsite9$X9 <- as.numeric(aug13noneeevsite9$X9)
aug20noneeevsite9$X9 <- as.numeric(aug20noneeevsite9$X9)
sep03noneeevsite9$X9 <- as.numeric(sep03noneeevsite9$X9)


ajun04noneeevsite9 <- sum(jun04noneeevsite9$X9, na.rm=T)
ajun12noneeevsite9 <- sum(jun12noneeevsite9$X9, na.rm=T)
ajun18noneeevsite9 <- sum(jun18noneeevsite9$X9, na.rm=T)
ajun25noneeevsite9 <- sum(jun25noneeevsite9$X9, na.rm=T)
ajul04noneeevsite9 <- sum(jul04noneeevsite9$X9, na.rm=T)
ajul10noneeevsite9 <- sum(jul10noneeevsite9$X9, na.rm=T)
ajul17noneeevsite9 <- sum(jul17noneeevsite9$X9, na.rm=T)
ajul24noneeevsite9 <- sum(jul24noneeevsite9$X9, na.rm=T)
ajul30noneeevsite9 <- sum(jul30noneeevsite9$X9, na.rm=T)
aaug06noneeevsite9 <- sum(aug06noneeevsite9$X9, na.rm=T)
aaug13noneeevsite9 <- sum(aug13noneeevsite9$X9, na.rm=T)
aaug20noneeevsite9 <- sum(aug20noneeevsite9$X9, na.rm=T)
asep03noneeevsite9 <- sum(sep03noneeevsite9$X9, na.rm=T)

#####################################
##### site10
#eeev
jun04eeevsite10$X10 <- as.numeric(jun04eeevsite10$X10)
jun12eeevsite10$X10 <- as.numeric(jun12eeevsite10$X10)
jun18eeevsite10$X10 <- as.numeric(jun18eeevsite10$X10)
jun25eeevsite10$X10 <- as.numeric(jun25eeevsite10$X10)
jul04eeevsite10$X10 <- as.numeric(jul04eeevsite10$X10)
jul10eeevsite10$X10 <- as.numeric(jul10eeevsite10$X10)
jul17eeevsite10$X10 <- as.numeric(jul17eeevsite10$X10)
jul24eeevsite10$X10 <- as.numeric(jul24eeevsite10$X10)
jul30eeevsite10$X10 <- as.numeric(jul30eeevsite10$X10)
aug06eeevsite10$X10 <- as.numeric(aug06eeevsite10$X10)
aug13eeevsite10$X10 <- as.numeric(aug13eeevsite10$X10)
aug20eeevsite10$X10 <- as.numeric(aug20eeevsite10$X10)
sep03eeevsite10$X10 <- as.numeric(sep03eeevsite10$X10)


ajun04eeevsite10 <- sum(jun04eeevsite10$X10, na.rm=T)
ajun12eeevsite10 <- sum(jun12eeevsite10$X10, na.rm=T)
ajun18eeevsite10 <- sum(jun18eeevsite10$X10, na.rm=T)
ajun25eeevsite10 <- sum(jun25eeevsite10$X10, na.rm=T)
ajul04eeevsite10 <- sum(jul04eeevsite10$X10, na.rm=T)
ajul10eeevsite10 <- sum(jul10eeevsite10$X10, na.rm=T)
ajul17eeevsite10 <- sum(jul17eeevsite10$X10, na.rm=T)
ajul24eeevsite10 <- sum(jul24eeevsite10$X10, na.rm=T)
ajul30eeevsite10 <- sum(jul30eeevsite10$X10, na.rm=T)
aaug06eeevsite10 <- sum(aug06eeevsite10$X10, na.rm=T)
aaug13eeevsite10 <- sum(aug13eeevsite10$X10, na.rm=T)
aaug20eeevsite10 <- sum(aug20eeevsite10$X10, na.rm=T)
asep03eeevsite10 <- sum(sep03eeevsite10$X10, na.rm=T)


#NONeeev
jun04noneeevsite10$X10 <- as.numeric(jun04noneeevsite10$X10)
jun12noneeevsite10$X10 <- as.numeric(jun12noneeevsite10$X10)
jun18noneeevsite10$X10 <- as.numeric(jun18noneeevsite10$X10)
jun25noneeevsite10$X10 <- as.numeric(jun25noneeevsite10$X10)
jul04noneeevsite10$X10 <- as.numeric(jul04noneeevsite10$X10)
jul10noneeevsite10$X10 <- as.numeric(jul10noneeevsite10$X10)
jul17noneeevsite10$X10 <- as.numeric(jul17noneeevsite10$X10)
jul24noneeevsite10$X10 <- as.numeric(jul24noneeevsite10$X10)
jul30noneeevsite10$X10 <- as.numeric(jul30noneeevsite10$X10)
aug06noneeevsite10$X10 <- as.numeric(aug06noneeevsite10$X10)
aug13noneeevsite10$X10 <- as.numeric(aug13noneeevsite10$X10)
aug20noneeevsite10$X10 <- as.numeric(aug20noneeevsite10$X10)
sep03noneeevsite10$X10 <- as.numeric(sep03noneeevsite10$X10)


ajun04noneeevsite10 <- sum(jun04noneeevsite10$X10, na.rm=T)
ajun12noneeevsite10 <- sum(jun12noneeevsite10$X10, na.rm=T)
ajun18noneeevsite10 <- sum(jun18noneeevsite10$X10, na.rm=T)
ajun25noneeevsite10 <- sum(jun25noneeevsite10$X10, na.rm=T)
ajul04noneeevsite10 <- sum(jul04noneeevsite10$X10, na.rm=T)
ajul10noneeevsite10 <- sum(jul10noneeevsite10$X10, na.rm=T)
ajul17noneeevsite10 <- sum(jul17noneeevsite10$X10, na.rm=T)
ajul24noneeevsite10 <- sum(jul24noneeevsite10$X10, na.rm=T)
ajul30noneeevsite10 <- sum(jul30noneeevsite10$X10, na.rm=T)
aaug06noneeevsite10 <- sum(aug06noneeevsite10$X10, na.rm=T)
aaug13noneeevsite10 <- sum(aug13noneeevsite10$X10, na.rm=T)
aaug20noneeevsite10 <- sum(aug20noneeevsite10$X10, na.rm=T)
asep03noneeevsite10 <- sum(sep03noneeevsite10$X10, na.rm=T)



#####################
#
#
#
#
#
################################


#CREATE eeevS FOR PLOTTING
#Site 1
eeev1 <- c(ajun04eeevsite1,
          ajun12eeevsite1,
          ajun18eeevsite1,
          ajun25eeevsite1,
          ajul04eeevsite1,
          ajul10eeevsite1,
          ajul17eeevsite1,
          ajul24eeevsite1,
          ajul30eeevsite1,
          aaug06eeevsite1,
          aaug13eeevsite1,
          aaug20eeevsite1,
          asep03eeevsite1)

#Site 2
eeev2 <- c(ajun04eeevsite2,
          ajun12eeevsite2,
          ajun18eeevsite2,
          ajun25eeevsite2,
          ajul04eeevsite2,
          ajul10eeevsite2,
          ajul17eeevsite2,
          ajul24eeevsite2,
          ajul30eeevsite2,
          aaug06eeevsite2,
          aaug13eeevsite2,
          aaug20eeevsite2,
          asep03eeevsite2)


#Site 3
eeev3 <- c(ajun04eeevsite3,
          ajun12eeevsite3,
          ajun18eeevsite3,
          ajun25eeevsite3,
          ajul04eeevsite3,
          ajul10eeevsite3,
          ajul17eeevsite3,
          ajul24eeevsite3,
          ajul30eeevsite3,
          aaug06eeevsite3,
          aaug13eeevsite3,
          aaug20eeevsite3,
          asep03eeevsite3)


#Site 4
eeev4 <- c(ajun04eeevsite4,
          ajun12eeevsite4,
          ajun18eeevsite4,
          ajun25eeevsite4,
          ajul04eeevsite4,
          ajul10eeevsite4,
          ajul17eeevsite4,
          ajul24eeevsite4,
          ajul30eeevsite4,
          aaug06eeevsite4,
          aaug13eeevsite4,
          aaug20eeevsite4,
          asep03eeevsite4)


#Site 5
eeev5 <- c(ajun04eeevsite5,
          ajun12eeevsite5,
          ajun18eeevsite5,
          ajun25eeevsite5,
          ajul04eeevsite5,
          ajul10eeevsite5,
          ajul17eeevsite5,
          ajul24eeevsite5,
          ajul30eeevsite5,
          aaug06eeevsite5,
          aaug13eeevsite5,
          aaug20eeevsite5,
          asep03eeevsite5)


#Site 6
eeev6 <- c(ajun04eeevsite6,
          ajun12eeevsite6,
          ajun18eeevsite6,
          ajun25eeevsite6,
          ajul04eeevsite6,
          ajul10eeevsite6,
          ajul17eeevsite6,
          ajul24eeevsite6,
          ajul30eeevsite6,
          aaug06eeevsite6,
          aaug13eeevsite6,
          aaug20eeevsite6,
          asep03eeevsite6)


#Site 7
eeev7 <- c(ajun04eeevsite7,
          ajun12eeevsite7,
          ajun18eeevsite7,
          ajun25eeevsite7,
          ajul04eeevsite7,
          ajul10eeevsite7,
          ajul17eeevsite7,
          ajul24eeevsite7,
          ajul30eeevsite7,
          aaug06eeevsite7,
          aaug13eeevsite7,
          aaug20eeevsite7,
          asep03eeevsite7)


#Site 8
eeev8 <- c(ajun04eeevsite8,
          ajun12eeevsite8,
          ajun18eeevsite8,
          ajun25eeevsite8,
          ajul04eeevsite8,
          ajul10eeevsite8,
          ajul17eeevsite8,
          ajul24eeevsite8,
          ajul30eeevsite8,
          aaug06eeevsite8,
          aaug13eeevsite8,
          aaug20eeevsite8,
          asep03eeevsite8)


#Site 9
eeev9 <- c(ajun04eeevsite9,
          ajun12eeevsite9,
          ajun18eeevsite9,
          ajun25eeevsite9,
          ajul04eeevsite9,
          ajul10eeevsite9,
          ajul17eeevsite9,
          ajul24eeevsite9,
          ajul30eeevsite9,
          aaug06eeevsite9,
          aaug13eeevsite9,
          aaug20eeevsite9,
          asep03eeevsite9)


#Site 10
eeev10 <- c(ajun04eeevsite10,
           ajun12eeevsite10,
           ajun18eeevsite10,
           ajun25eeevsite10,
           ajul04eeevsite10,
           ajul10eeevsite10,
           ajul17eeevsite10,
           ajul24eeevsite10,
           ajul30eeevsite10,
           aaug06eeevsite10,
           aaug13eeevsite10,
           aaug20eeevsite10,
           asep03eeevsite10)



eeevtotal <- eeev1+eeev2+eeev3+eeev4+eeev5+eeev6+eeev7+eeev8+eeev9+eeev10


##### chik 4 - CHIK
#SUBSET BY DATE and chik
jun04chik <- subset(mergedchik, date2  == as.Date("2013-06-04"))
jun12chik <- subset(mergedchik, date2  == as.Date("2013-06-12"))
jun18chik <- subset(mergedchik, date2  == as.Date("2013-06-18"))
jun25chik <- subset(mergedchik, date2  == as.Date("2013-06-25"))
jul04chik <- subset(mergedchik, date2  == as.Date("2013-07-04"))
jul10chik <- subset(mergedchik, date2  == as.Date("2013-07-10"))
jul17chik <- subset(mergedchik, date2  == as.Date("2013-07-17"))
jul24chik <- subset(mergedchik, date2  == as.Date("2013-07-24"))
jul30chik <- subset(mergedchik, date2  == as.Date("2013-07-30"))
aug06chik <- subset(mergedchik, date2  == as.Date("2013-08-06"))
aug13chik <- subset(mergedchik, date2  == as.Date("2013-08-13"))
aug20chik <- subset(mergedchik, date2  == as.Date("2013-08-20"))
sep03chik <- subset(mergedchik, date2  == as.Date("2013-09-03"))


####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE chik SITE

#JUN 04 chik
jun04chiksite1 <-subset(merged1chik, date2  == as.Date("2013-06-04"))
jun04chiksite2 <-subset(merged2chik, date2  == as.Date("2013-06-04"))
jun04chiksite3 <-subset(merged3chik, date2  == as.Date("2013-06-04"))
jun04chiksite4 <-subset(merged4chik, date2  == as.Date("2013-06-04"))
jun04chiksite5 <-subset(merged5chik, date2  == as.Date("2013-06-04"))
jun04chiksite6 <-subset(merged6chik, date2  == as.Date("2013-06-04"))
jun04chiksite7 <-subset(merged7chik, date2  == as.Date("2013-06-04"))
jun04chiksite8 <-subset(merged8chik, date2  == as.Date("2013-06-04"))
jun04chiksite9 <-subset(merged9chik, date2  == as.Date("2013-06-04"))
jun04chiksite10 <-subset(merged10chik, date2  == as.Date("2013-06-04"))


#jun12 chik
jun12chiksite1 <-subset(merged1chik, date2  == as.Date("2013-06-12"))
jun12chiksite2 <-subset(merged2chik, date2  == as.Date("2013-06-12"))
jun12chiksite3 <-subset(merged3chik, date2  == as.Date("2013-06-12"))
jun12chiksite4 <-subset(merged4chik, date2  == as.Date("2013-06-12"))
jun12chiksite5 <-subset(merged5chik, date2  == as.Date("2013-06-12"))
jun12chiksite6 <-subset(merged6chik, date2  == as.Date("2013-06-12"))
jun12chiksite7 <-subset(merged7chik, date2  == as.Date("2013-06-12"))
jun12chiksite8 <-subset(merged8chik, date2  == as.Date("2013-06-12"))
jun12chiksite9 <-subset(merged9chik, date2  == as.Date("2013-06-12"))
jun12chiksite10 <-subset(merged10chik, date2  == as.Date("2013-06-12"))

#jun18 chik
jun18chiksite1 <-subset(merged1chik, date2  == as.Date("2013-06-18"))
jun18chiksite2 <-subset(merged2chik, date2  == as.Date("2013-06-18"))
jun18chiksite3 <-subset(merged3chik, date2  == as.Date("2013-06-18"))
jun18chiksite4 <-subset(merged4chik, date2  == as.Date("2013-06-18"))
jun18chiksite5 <-subset(merged5chik, date2  == as.Date("2013-06-18"))
jun18chiksite6 <-subset(merged6chik, date2  == as.Date("2013-06-18"))
jun18chiksite7 <-subset(merged7chik, date2  == as.Date("2013-06-18"))
jun18chiksite8 <-subset(merged8chik, date2  == as.Date("2013-06-18"))
jun18chiksite9 <-subset(merged9chik, date2  == as.Date("2013-06-18"))
jun18chiksite10 <-subset(merged10chik, date2  == as.Date("2013-06-18"))

#jun25 chik
jun25chiksite1 <-subset(merged1chik, date2  == as.Date("2013-06-25"))
jun25chiksite2 <-subset(merged2chik, date2  == as.Date("2013-06-25"))
jun25chiksite3 <-subset(merged3chik, date2  == as.Date("2013-06-25"))
jun25chiksite4 <-subset(merged4chik, date2  == as.Date("2013-06-25"))
jun25chiksite5 <-subset(merged5chik, date2  == as.Date("2013-06-25"))
jun25chiksite6 <-subset(merged6chik, date2  == as.Date("2013-06-25"))
jun25chiksite7 <-subset(merged7chik, date2  == as.Date("2013-06-25"))
jun25chiksite8 <-subset(merged8chik, date2  == as.Date("2013-06-25"))
jun25chiksite9 <-subset(merged9chik, date2  == as.Date("2013-06-25"))
jun25chiksite10 <-subset(merged10chik, date2  == as.Date("2013-06-25"))

#jul04 chik
jul04chiksite1 <-subset(merged1chik, date2  == as.Date("2013-07-04"))
jul04chiksite2 <-subset(merged2chik, date2  == as.Date("2013-07-04"))
jul04chiksite3 <-subset(merged3chik, date2  == as.Date("2013-07-04"))
jul04chiksite4 <-subset(merged4chik, date2  == as.Date("2013-07-04"))
jul04chiksite5 <-subset(merged5chik, date2  == as.Date("2013-07-04"))
jul04chiksite6 <-subset(merged6chik, date2  == as.Date("2013-07-04"))
jul04chiksite7 <-subset(merged7chik, date2  == as.Date("2013-07-04"))
jul04chiksite8 <-subset(merged8chik, date2  == as.Date("2013-07-04"))
jul04chiksite9 <-subset(merged9chik, date2  == as.Date("2013-07-04"))
jul04chiksite10 <-subset(merged10chik, date2  == as.Date("2013-07-04"))

#jul10 chik
jul10chiksite1 <-subset(merged1chik, date2  == as.Date("2013-07-10"))
jul10chiksite2 <-subset(merged2chik, date2  == as.Date("2013-07-10"))
jul10chiksite3 <-subset(merged3chik, date2  == as.Date("2013-07-10"))
jul10chiksite4 <-subset(merged4chik, date2  == as.Date("2013-07-10"))
jul10chiksite5 <-subset(merged5chik, date2  == as.Date("2013-07-10"))
jul10chiksite6 <-subset(merged6chik, date2  == as.Date("2013-07-10"))
jul10chiksite7 <-subset(merged7chik, date2  == as.Date("2013-07-10"))
jul10chiksite8 <-subset(merged8chik, date2  == as.Date("2013-07-10"))
jul10chiksite9 <-subset(merged9chik, date2  == as.Date("2013-07-10"))
jul10chiksite10 <-subset(merged10chik, date2  == as.Date("2013-07-10"))

#jul17 chik
jul17chiksite1 <-subset(merged1chik, date2  == as.Date("2013-07-17"))
jul17chiksite2 <-subset(merged2chik, date2  == as.Date("2013-07-17"))
jul17chiksite3 <-subset(merged3chik, date2  == as.Date("2013-07-17"))
jul17chiksite4 <-subset(merged4chik, date2  == as.Date("2013-07-17"))
jul17chiksite5 <-subset(merged5chik, date2  == as.Date("2013-07-17"))
jul17chiksite6 <-subset(merged6chik, date2  == as.Date("2013-07-17"))
jul17chiksite7 <-subset(merged7chik, date2  == as.Date("2013-07-17"))
jul17chiksite8 <-subset(merged8chik, date2  == as.Date("2013-07-17"))
jul17chiksite9 <-subset(merged9chik, date2  == as.Date("2013-07-17"))
jul17chiksite10 <-subset(merged10chik, date2  == as.Date("2013-07-17"))

#jul24 chik
jul24chiksite1 <-subset(merged1chik, date2  == as.Date("2013-07-24"))
jul24chiksite2 <-subset(merged2chik, date2  == as.Date("2013-07-24"))
jul24chiksite3 <-subset(merged3chik, date2  == as.Date("2013-07-24"))
jul24chiksite4 <-subset(merged4chik, date2  == as.Date("2013-07-24"))
jul24chiksite5 <-subset(merged5chik, date2  == as.Date("2013-07-24"))
jul24chiksite6 <-subset(merged6chik, date2  == as.Date("2013-07-24"))
jul24chiksite7 <-subset(merged7chik, date2  == as.Date("2013-07-24"))
jul24chiksite8 <-subset(merged8chik, date2  == as.Date("2013-07-24"))
jul24chiksite9 <-subset(merged9chik, date2  == as.Date("2013-07-24"))
jul24chiksite10 <-subset(merged10chik, date2  == as.Date("2013-07-24"))

#jul30 chik
jul30chiksite1 <-subset(merged1chik, date2  == as.Date("2013-07-30"))
jul30chiksite2 <-subset(merged2chik, date2  == as.Date("2013-07-30"))
jul30chiksite3 <-subset(merged3chik, date2  == as.Date("2013-07-30"))
jul30chiksite4 <-subset(merged4chik, date2  == as.Date("2013-07-30"))
jul30chiksite5 <-subset(merged5chik, date2  == as.Date("2013-07-30"))
jul30chiksite6 <-subset(merged6chik, date2  == as.Date("2013-07-30"))
jul30chiksite7 <-subset(merged7chik, date2  == as.Date("2013-07-30"))
jul30chiksite8 <-subset(merged8chik, date2  == as.Date("2013-07-30"))
jul30chiksite9 <-subset(merged9chik, date2  == as.Date("2013-07-30"))
jul30chiksite10 <-subset(merged10chik, date2  == as.Date("2013-07-30"))

#aug06 chik
aug06chiksite1 <-subset(merged1chik, date2  == as.Date("2013-08-06"))
aug06chiksite2 <-subset(merged2chik, date2  == as.Date("2013-08-06"))
aug06chiksite3 <-subset(merged3chik, date2  == as.Date("2013-08-06"))
aug06chiksite4 <-subset(merged4chik, date2  == as.Date("2013-08-06"))
aug06chiksite5 <-subset(merged5chik, date2  == as.Date("2013-08-06"))
aug06chiksite6 <-subset(merged6chik, date2  == as.Date("2013-08-06"))
aug06chiksite7 <-subset(merged7chik, date2  == as.Date("2013-08-06"))
aug06chiksite8 <-subset(merged8chik, date2  == as.Date("2013-08-06"))
aug06chiksite9 <-subset(merged9chik, date2  == as.Date("2013-08-06"))
aug06chiksite10 <-subset(merged10chik, date2  == as.Date("2013-08-06"))

#aug13 chik
aug13chiksite1 <-subset(merged1chik, date2  == as.Date("2013-08-13"))
aug13chiksite2 <-subset(merged2chik, date2  == as.Date("2013-08-13"))
aug13chiksite3 <-subset(merged3chik, date2  == as.Date("2013-08-13"))
aug13chiksite4 <-subset(merged4chik, date2  == as.Date("2013-08-13"))
aug13chiksite5 <-subset(merged5chik, date2  == as.Date("2013-08-13"))
aug13chiksite6 <-subset(merged6chik, date2  == as.Date("2013-08-13"))
aug13chiksite7 <-subset(merged7chik, date2  == as.Date("2013-08-13"))
aug13chiksite8 <-subset(merged8chik, date2  == as.Date("2013-08-13"))
aug13chiksite9 <-subset(merged9chik, date2  == as.Date("2013-08-13"))
aug13chiksite10 <-subset(merged10chik, date2  == as.Date("2013-08-13"))

#aug20 chik
aug20chiksite1 <-subset(merged1chik, date2  == as.Date("2013-08-20"))
aug20chiksite2 <-subset(merged2chik, date2  == as.Date("2013-08-20"))
aug20chiksite3 <-subset(merged3chik, date2  == as.Date("2013-08-20"))
aug20chiksite4 <-subset(merged4chik, date2  == as.Date("2013-08-20"))
aug20chiksite5 <-subset(merged5chik, date2  == as.Date("2013-08-20"))
aug20chiksite6 <-subset(merged6chik, date2  == as.Date("2013-08-20"))
aug20chiksite7 <-subset(merged7chik, date2  == as.Date("2013-08-20"))
aug20chiksite8 <-subset(merged8chik, date2  == as.Date("2013-08-20"))
aug20chiksite9 <-subset(merged9chik, date2  == as.Date("2013-08-20"))
aug20chiksite10 <-subset(merged10chik, date2  == as.Date("2013-08-20"))

#sep03 chik
sep03chiksite1 <-subset(merged1chik, date2  == as.Date("2013-09-03"))
sep03chiksite2 <-subset(merged2chik, date2  == as.Date("2013-09-03"))
sep03chiksite3 <-subset(merged3chik, date2  == as.Date("2013-09-03"))
sep03chiksite4 <-subset(merged4chik, date2  == as.Date("2013-09-03"))
sep03chiksite5 <-subset(merged5chik, date2  == as.Date("2013-09-03"))
sep03chiksite6 <-subset(merged6chik, date2  == as.Date("2013-09-03"))
sep03chiksite7 <-subset(merged7chik, date2  == as.Date("2013-09-03"))
sep03chiksite8 <-subset(merged8chik, date2  == as.Date("2013-09-03"))
sep03chiksite9 <-subset(merged9chik, date2  == as.Date("2013-09-03"))
sep03chiksite10 <-subset(merged10chik, date2  == as.Date("2013-09-03"))

##### ALL SITES COMBINED
#chik
ajun04chiktotal <- sum(jun04chik$total, na.rm=T)
ajun12chiktotal <- sum(jun12chik$total, na.rm=T)
ajun18chiktotal <- sum(jun18chik$total, na.rm=T)
ajun25chiktotal <- sum(jun25chik$total, na.rm=T)
ajul04chiktotal <- sum(jul04chik$total, na.rm=T)
ajul10chiktotal <- sum(jul10chik$total, na.rm=T)
ajul17chiktotal <- sum(jul17chik$total, na.rm=T)
ajul24chiktotal <- sum(jul24chik$total, na.rm=T)
ajul30chiktotal <- sum(jul30chik$total, na.rm=T)
aaug06chiktotal <- sum(aug06chik$total, na.rm=T)
aaug13chiktotal <- sum(aug13chik$total, na.rm=T)
aaug20chiktotal <- sum(aug20chik$total, na.rm=T)
asep03chiktotal <- sum(sep03chik$total, na.rm=T)



#####################################
##### site1
#chik
ajun04chiksite1 <- sum(jun04chiksite1$X1, na.rm=T)
ajun12chiksite1 <- sum(jun12chiksite1$X1, na.rm=T)
ajun18chiksite1 <- sum(jun18chiksite1$X1, na.rm=T)
ajun25chiksite1 <- sum(jun25chiksite1$X1, na.rm=T)
ajul04chiksite1 <- sum(jul04chiksite1$X1, na.rm=T)
ajul10chiksite1 <- sum(jul10chiksite1$X1, na.rm=T)
ajul17chiksite1 <- sum(jul17chiksite1$X1, na.rm=T)
ajul24chiksite1 <- sum(jul24chiksite1$X1, na.rm=T)
ajul30chiksite1 <- sum(jul30chiksite1$X1, na.rm=T)
aaug06chiksite1 <- sum(aug06chiksite1$X1, na.rm=T)
aaug13chiksite1 <- sum(aug13chiksite1$X1, na.rm=T)
aaug20chiksite1 <- sum(aug20chiksite1$X1, na.rm=T)
asep03chiksite1 <- sum(sep03chiksite1$X1, na.rm=T)


#NONchik
ajun04nonchiksite1 <- sum(jun04nonchiksite1$X1, na.rm=T)
ajun12nonchiksite1 <- sum(jun12nonchiksite1$X1, na.rm=T)
ajun18nonchiksite1 <- sum(jun18nonchiksite1$X1, na.rm=T)
ajun25nonchiksite1 <- sum(jun25nonchiksite1$X1, na.rm=T)
ajul04nonchiksite1 <- sum(jul04nonchiksite1$X1, na.rm=T)
ajul10nonchiksite1 <- sum(jul10nonchiksite1$X1, na.rm=T)
ajul17nonchiksite1 <- sum(jul17nonchiksite1$X1, na.rm=T)
ajul24nonchiksite1 <- sum(jul24nonchiksite1$X1, na.rm=T)
ajul30nonchiksite1 <- sum(jul30nonchiksite1$X1, na.rm=T)
aaug06nonchiksite1 <- sum(aug06nonchiksite1$X1, na.rm=T)
aaug13nonchiksite1 <- sum(aug13nonchiksite1$X1, na.rm=T)
aaug20nonchiksite1 <- sum(aug20nonchiksite1$X1, na.rm=T)
asep03nonchiksite1 <- sum(sep03nonchiksite1$X1, na.rm=T)

#####################################
##### site2
#chik
ajun04chiksite2 <- sum(jun04chiksite2$X2, na.rm=T)
ajun12chiksite2 <- sum(jun12chiksite2$X2, na.rm=T)
ajun18chiksite2 <- sum(jun18chiksite2$X2, na.rm=T)
ajun25chiksite2 <- sum(jun25chiksite2$X2, na.rm=T)
ajul04chiksite2 <- sum(jul04chiksite2$X2, na.rm=T)
ajul10chiksite2 <- sum(jul10chiksite2$X2, na.rm=T)
ajul17chiksite2 <- sum(jul17chiksite2$X2, na.rm=T)
ajul24chiksite2 <- sum(jul24chiksite2$X2, na.rm=T)
ajul30chiksite2 <- sum(jul30chiksite2$X2, na.rm=T)
aaug06chiksite2 <- sum(aug06chiksite2$X2, na.rm=T)
aaug13chiksite2 <- sum(aug13chiksite2$X2, na.rm=T)
aaug20chiksite2 <- sum(aug20chiksite2$X2, na.rm=T)
asep03chiksite2 <- sum(sep03chiksite2$X2, na.rm=T)


#NONchik
ajun04nonchiksite2 <- sum(jun04nonchiksite2$X2, na.rm=T)
ajun12nonchiksite2 <- sum(jun12nonchiksite2$X2, na.rm=T)
ajun18nonchiksite2 <- sum(jun18nonchiksite2$X2, na.rm=T)
ajun25nonchiksite2 <- sum(jun25nonchiksite2$X2, na.rm=T)
ajul04nonchiksite2 <- sum(jul04nonchiksite2$X2, na.rm=T)
ajul10nonchiksite2 <- sum(jul10nonchiksite2$X2, na.rm=T)
ajul17nonchiksite2 <- sum(jul17nonchiksite2$X2, na.rm=T)
ajul24nonchiksite2 <- sum(jul24nonchiksite2$X2, na.rm=T)
ajul30nonchiksite2 <- sum(jul30nonchiksite2$X2, na.rm=T)
aaug06nonchiksite2 <- sum(aug06nonchiksite2$X2, na.rm=T)
aaug13nonchiksite2 <- sum(aug13nonchiksite2$X2, na.rm=T)
aaug20nonchiksite2 <- sum(aug20nonchiksite2$X2, na.rm=T)
asep03nonchiksite2 <- sum(sep03nonchiksite2$X2, na.rm=T)

#####################################
##### site3
#chik
ajun04chiksite3 <- sum(jun04chiksite3$X3, na.rm=T)
ajun12chiksite3 <- sum(jun12chiksite3$X3, na.rm=T)
ajun18chiksite3 <- sum(jun18chiksite3$X3, na.rm=T)
ajun25chiksite3 <- sum(jun25chiksite3$X3, na.rm=T)
ajul04chiksite3 <- sum(jul04chiksite3$X3, na.rm=T)
ajul10chiksite3 <- sum(jul10chiksite3$X3, na.rm=T)
ajul17chiksite3 <- sum(jul17chiksite3$X3, na.rm=T)
ajul24chiksite3 <- sum(jul24chiksite3$X3, na.rm=T)
ajul30chiksite3 <- sum(jul30chiksite3$X3, na.rm=T)
aaug06chiksite3 <- sum(aug06chiksite3$X3, na.rm=T)
aaug13chiksite3 <- sum(aug13chiksite3$X3, na.rm=T)
aaug20chiksite3 <- sum(aug20chiksite3$X3, na.rm=T)
asep03chiksite3 <- sum(sep03chiksite3$X3, na.rm=T)


#NONchik
ajun04nonchiksite3 <- sum(jun04nonchiksite3$X3, na.rm=T)
ajun12nonchiksite3 <- sum(jun12nonchiksite3$X3, na.rm=T)
ajun18nonchiksite3 <- sum(jun18nonchiksite3$X3, na.rm=T)
ajun25nonchiksite3 <- sum(jun25nonchiksite3$X3, na.rm=T)
ajul04nonchiksite3 <- sum(jul04nonchiksite3$X3, na.rm=T)
ajul10nonchiksite3 <- sum(jul10nonchiksite3$X3, na.rm=T)
ajul17nonchiksite3 <- sum(jul17nonchiksite3$X3, na.rm=T)
ajul24nonchiksite3 <- sum(jul24nonchiksite3$X3, na.rm=T)
ajul30nonchiksite3 <- sum(jul30nonchiksite3$X3, na.rm=T)
aaug06nonchiksite3 <- sum(aug06nonchiksite3$X3, na.rm=T)
aaug13nonchiksite3 <- sum(aug13nonchiksite3$X3, na.rm=T)
aaug20nonchiksite3 <- sum(aug20nonchiksite3$X3, na.rm=T)
asep03nonchiksite3 <- sum(sep03nonchiksite3$X3, na.rm=T)


#####################################
##### site4
#chik
ajun04chiksite4 <- sum(jun04chiksite4$X4, na.rm=T)
ajun12chiksite4 <- sum(jun12chiksite4$X4, na.rm=T)
ajun18chiksite4 <- sum(jun18chiksite4$X4, na.rm=T)
ajun25chiksite4 <- sum(jun25chiksite4$X4, na.rm=T)
ajul04chiksite4 <- sum(jul04chiksite4$X4, na.rm=T)
ajul10chiksite4 <- sum(jul10chiksite4$X4, na.rm=T)
ajul17chiksite4 <- sum(jul17chiksite4$X4, na.rm=T)
ajul24chiksite4 <- sum(jul24chiksite4$X4, na.rm=T)
ajul30chiksite4 <- sum(jul30chiksite4$X4, na.rm=T)
aaug06chiksite4 <- sum(aug06chiksite4$X4, na.rm=T)
aaug13chiksite4 <- sum(aug13chiksite4$X4, na.rm=T)
aaug20chiksite4 <- sum(aug20chiksite4$X4, na.rm=T)
asep03chiksite4 <- sum(sep03chiksite4$X4, na.rm=T)


#NONchik
ajun04nonchiksite4 <- sum(jun04nonchiksite4$X4, na.rm=T)
ajun12nonchiksite4 <- sum(jun12nonchiksite4$X4, na.rm=T)
ajun18nonchiksite4 <- sum(jun18nonchiksite4$X4, na.rm=T)
ajun25nonchiksite4 <- sum(jun25nonchiksite4$X4, na.rm=T)
ajul04nonchiksite4 <- sum(jul04nonchiksite4$X4, na.rm=T)
ajul10nonchiksite4 <- sum(jul10nonchiksite4$X4, na.rm=T)
ajul17nonchiksite4 <- sum(jul17nonchiksite4$X4, na.rm=T)
ajul24nonchiksite4 <- sum(jul24nonchiksite4$X4, na.rm=T)
ajul30nonchiksite4 <- sum(jul30nonchiksite4$X4, na.rm=T)
aaug06nonchiksite4 <- sum(aug06nonchiksite4$X4, na.rm=T)
aaug13nonchiksite4 <- sum(aug13nonchiksite4$X4, na.rm=T)
aaug20nonchiksite4 <- sum(aug20nonchiksite4$X4, na.rm=T)
asep03nonchiksite4 <- sum(sep03nonchiksite4$X4, na.rm=T)

#####################################
##### site5
#chik
ajun04chiksite5 <- sum(jun04chiksite5$X5, na.rm=T)
ajun12chiksite5 <- sum(jun12chiksite5$X5, na.rm=T)
ajun18chiksite5 <- sum(jun18chiksite5$X5, na.rm=T)
ajun25chiksite5 <- sum(jun25chiksite5$X5, na.rm=T)
ajul04chiksite5 <- sum(jul04chiksite5$X5, na.rm=T)
ajul10chiksite5 <- sum(jul10chiksite5$X5, na.rm=T)
ajul17chiksite5 <- sum(jul17chiksite5$X5, na.rm=T)
ajul24chiksite5 <- sum(jul24chiksite5$X5, na.rm=T)
ajul30chiksite5 <- sum(jul30chiksite5$X5, na.rm=T)
aaug06chiksite5 <- sum(aug06chiksite5$X5, na.rm=T)
aaug13chiksite5 <- sum(aug13chiksite5$X5, na.rm=T)
aaug20chiksite5 <- sum(aug20chiksite5$X5, na.rm=T)
asep03chiksite5 <- sum(sep03chiksite5$X5, na.rm=T)


#NONchik
ajun04nonchiksite5 <- sum(jun04nonchiksite5$X5, na.rm=T)
ajun12nonchiksite5 <- sum(jun12nonchiksite5$X5, na.rm=T)
ajun18nonchiksite5 <- sum(jun18nonchiksite5$X5, na.rm=T)
ajun25nonchiksite5 <- sum(jun25nonchiksite5$X5, na.rm=T)
ajul04nonchiksite5 <- sum(jul04nonchiksite5$X5, na.rm=T)
ajul10nonchiksite5 <- sum(jul10nonchiksite5$X5, na.rm=T)
ajul17nonchiksite5 <- sum(jul17nonchiksite5$X5, na.rm=T)
ajul24nonchiksite5 <- sum(jul24nonchiksite5$X5, na.rm=T)
ajul30nonchiksite5 <- sum(jul30nonchiksite5$X5, na.rm=T)
aaug06nonchiksite5 <- sum(aug06nonchiksite5$X5, na.rm=T)
aaug13nonchiksite5 <- sum(aug13nonchiksite5$X5, na.rm=T)
aaug20nonchiksite5 <- sum(aug20nonchiksite5$X5, na.rm=T)
asep03nonchiksite5 <- sum(sep03nonchiksite5$X5, na.rm=T)


#####################################
##### site6
#chik
jun04chiksite6$X6 <- as.numeric(jun04chiksite6$X6)
jun12chiksite6$X6 <- as.numeric(jun12chiksite6$X6)
jun18chiksite6$X6 <- as.numeric(jun18chiksite6$X6)
jun25chiksite6$X6 <- as.numeric(jun25chiksite6$X6)
jul04chiksite6$X6 <- as.numeric(jul04chiksite6$X6)
jul10chiksite6$X6 <- as.numeric(jul10chiksite6$X6)
jul17chiksite6$X6 <- as.numeric(jul17chiksite6$X6)
jul24chiksite6$X6 <- as.numeric(jul24chiksite6$X6)
jul30chiksite6$X6 <- as.numeric(jul30chiksite6$X6)
aug06chiksite6$X6 <- as.numeric(aug06chiksite6$X6)
aug13chiksite6$X6 <- as.numeric(aug13chiksite6$X6)
aug20chiksite6$X6 <- as.numeric(aug20chiksite6$X6)
sep03chiksite6$X6 <- as.numeric(sep03chiksite6$X6)


ajun04chiksite6 <- sum(jun04chiksite6$X6, na.rm=T)
ajun12chiksite6 <- sum(jun12chiksite6$X6, na.rm=T)
ajun18chiksite6 <- sum(jun18chiksite6$X6, na.rm=T)
ajun25chiksite6 <- sum(jun25chiksite6$X6, na.rm=T)
ajul04chiksite6 <- sum(jul04chiksite6$X6, na.rm=T)
ajul10chiksite6 <- sum(jul10chiksite6$X6, na.rm=T)
ajul17chiksite6 <- sum(jul17chiksite6$X6, na.rm=T)
ajul24chiksite6 <- sum(jul24chiksite6$X6, na.rm=T)
ajul30chiksite6 <- sum(jul30chiksite6$X6, na.rm=T)
aaug06chiksite6 <- sum(aug06chiksite6$X6, na.rm=T)
aaug13chiksite6 <- sum(aug13chiksite6$X6, na.rm=T)
aaug20chiksite6 <- sum(aug20chiksite6$X6, na.rm=T)
asep03chiksite6 <- sum(sep03chiksite6$X6, na.rm=T)


#NONchik
jun04nonchiksite6$X6 <- as.numeric(jun04nonchiksite6$X6)
jun12nonchiksite6$X6 <- as.numeric(jun12nonchiksite6$X6)
jun18nonchiksite6$X6 <- as.numeric(jun18nonchiksite6$X6)
jun25nonchiksite6$X6 <- as.numeric(jun25nonchiksite6$X6)
jul04nonchiksite6$X6 <- as.numeric(jul04nonchiksite6$X6)
jul10nonchiksite6$X6 <- as.numeric(jul10nonchiksite6$X6)
jul17nonchiksite6$X6 <- as.numeric(jul17nonchiksite6$X6)
jul24nonchiksite6$X6 <- as.numeric(jul24nonchiksite6$X6)
jul30nonchiksite6$X6 <- as.numeric(jul30nonchiksite6$X6)
aug06nonchiksite6$X6 <- as.numeric(aug06nonchiksite6$X6)
aug13nonchiksite6$X6 <- as.numeric(aug13nonchiksite6$X6)
aug20nonchiksite6$X6 <- as.numeric(aug20nonchiksite6$X6)
sep03nonchiksite6$X6 <- as.numeric(sep03nonchiksite6$X6)


ajun04nonchiksite6 <- sum(jun04nonchiksite6$X6, na.rm=T)
ajun12nonchiksite6 <- sum(jun12nonchiksite6$X6, na.rm=T)
ajun18nonchiksite6 <- sum(jun18nonchiksite6$X6, na.rm=T)
ajun25nonchiksite6 <- sum(jun25nonchiksite6$X6, na.rm=T)
ajul04nonchiksite6 <- sum(jul04nonchiksite6$X6, na.rm=T)
ajul10nonchiksite6 <- sum(jul10nonchiksite6$X6, na.rm=T)
ajul17nonchiksite6 <- sum(jul17nonchiksite6$X6, na.rm=T)
ajul24nonchiksite6 <- sum(jul24nonchiksite6$X6, na.rm=T)
ajul30nonchiksite6 <- sum(jul30nonchiksite6$X6, na.rm=T)
aaug06nonchiksite6 <- sum(aug06nonchiksite6$X6, na.rm=T)
aaug13nonchiksite6 <- sum(aug13nonchiksite6$X6, na.rm=T)
aaug20nonchiksite6 <- sum(aug20nonchiksite6$X6, na.rm=T)
asep03nonchiksite6 <- sum(sep03nonchiksite6$X6, na.rm=T)


#####################################
##### site7
#chik
ajun04chiksite7 <- sum(jun04chiksite7$X7, na.rm=T)
ajun12chiksite7 <- sum(jun12chiksite7$X7, na.rm=T)
ajun18chiksite7 <- sum(jun18chiksite7$X7, na.rm=T)
ajun25chiksite7 <- sum(jun25chiksite7$X7, na.rm=T)
ajul04chiksite7 <- sum(jul04chiksite7$X7, na.rm=T)
ajul10chiksite7 <- sum(jul10chiksite7$X7, na.rm=T)
ajul17chiksite7 <- sum(jul17chiksite7$X7, na.rm=T)
ajul24chiksite7 <- sum(jul24chiksite7$X7, na.rm=T)
ajul30chiksite7 <- sum(jul30chiksite7$X7, na.rm=T)
aaug06chiksite7 <- sum(aug06chiksite7$X7, na.rm=T)
aaug13chiksite7 <- sum(aug13chiksite7$X7, na.rm=T)
aaug20chiksite7 <- sum(aug20chiksite7$X7, na.rm=T)
asep03chiksite7 <- sum(sep03chiksite7$X7, na.rm=T)


#NONchik
ajun04nonchiksite7 <- sum(jun04nonchiksite7$X7, na.rm=T)
ajun12nonchiksite7 <- sum(jun12nonchiksite7$X7, na.rm=T)
ajun18nonchiksite7 <- sum(jun18nonchiksite7$X7, na.rm=T)
ajun25nonchiksite7 <- sum(jun25nonchiksite7$X7, na.rm=T)
ajul04nonchiksite7 <- sum(jul04nonchiksite7$X7, na.rm=T)
ajul10nonchiksite7 <- sum(jul10nonchiksite7$X7, na.rm=T)
ajul17nonchiksite7 <- sum(jul17nonchiksite7$X7, na.rm=T)
ajul24nonchiksite7 <- sum(jul24nonchiksite7$X7, na.rm=T)
ajul30nonchiksite7 <- sum(jul30nonchiksite7$X7, na.rm=T)
aaug06nonchiksite7 <- sum(aug06nonchiksite7$X7, na.rm=T)
aaug13nonchiksite7 <- sum(aug13nonchiksite7$X7, na.rm=T)
aaug20nonchiksite7 <- sum(aug20nonchiksite7$X7, na.rm=T)
asep03nonchiksite7 <- sum(sep03nonchiksite7$X7, na.rm=T)


#####################################
##### site8
#chik
ajun04chiksite8 <- sum(jun04chiksite8$X8, na.rm=T)
ajun12chiksite8 <- sum(jun12chiksite8$X8, na.rm=T)
ajun18chiksite8 <- sum(jun18chiksite8$X8, na.rm=T)
ajun25chiksite8 <- sum(jun25chiksite8$X8, na.rm=T)
ajul04chiksite8 <- sum(jul04chiksite8$X8, na.rm=T)
ajul10chiksite8 <- sum(jul10chiksite8$X8, na.rm=T)
ajul17chiksite8 <- sum(jul17chiksite8$X8, na.rm=T)
ajul24chiksite8 <- sum(jul24chiksite8$X8, na.rm=T)
ajul30chiksite8 <- sum(jul30chiksite8$X8, na.rm=T)
aaug06chiksite8 <- sum(aug06chiksite8$X8, na.rm=T)
aaug13chiksite8 <- sum(aug13chiksite8$X8, na.rm=T)
aaug20chiksite8 <- sum(aug20chiksite8$X8, na.rm=T)
asep03chiksite8 <- sum(sep03chiksite8$X8, na.rm=T)


#NONchik
ajun04nonchiksite8 <- sum(jun04nonchiksite8$X8, na.rm=T)
ajun12nonchiksite8 <- sum(jun12nonchiksite8$X8, na.rm=T)
ajun18nonchiksite8 <- sum(jun18nonchiksite8$X8, na.rm=T)
ajun25nonchiksite8 <- sum(jun25nonchiksite8$X8, na.rm=T)
ajul04nonchiksite8 <- sum(jul04nonchiksite8$X8, na.rm=T)
ajul10nonchiksite8 <- sum(jul10nonchiksite8$X8, na.rm=T)
ajul17nonchiksite8 <- sum(jul17nonchiksite8$X8, na.rm=T)
ajul24nonchiksite8 <- sum(jul24nonchiksite8$X8, na.rm=T)
ajul30nonchiksite8 <- sum(jul30nonchiksite8$X8, na.rm=T)
aaug06nonchiksite8 <- sum(aug06nonchiksite8$X8, na.rm=T)
aaug13nonchiksite8 <- sum(aug13nonchiksite8$X8, na.rm=T)
aaug20nonchiksite8 <- sum(aug20nonchiksite8$X8, na.rm=T)
asep03nonchiksite8 <- sum(sep03nonchiksite8$X8, na.rm=T)


#####################################
##### site9
#chik

jun04chiksite9$X9 <- as.numeric(jun04chiksite9$X9)
jun12chiksite9$X9 <- as.numeric(jun12chiksite9$X9)
jun18chiksite9$X9 <- as.numeric(jun18chiksite9$X9)
jun25chiksite9$X9 <- as.numeric(jun25chiksite9$X9)
jul04chiksite9$X9 <- as.numeric(jul04chiksite9$X9)
jul10chiksite9$X9 <- as.numeric(jul10chiksite9$X9)
jul17chiksite9$X9 <- as.numeric(jul17chiksite9$X9)
jul24chiksite9$X9 <- as.numeric(jul24chiksite9$X9)
jul30chiksite9$X9 <- as.numeric(jul30chiksite9$X9)
aug06chiksite9$X9 <- as.numeric(aug06chiksite9$X9)
aug13chiksite9$X9 <- as.numeric(aug13chiksite9$X9)
aug20chiksite9$X9 <- as.numeric(aug20chiksite9$X9)
sep03chiksite9$X9 <- as.numeric(sep03chiksite9$X9)


ajun04chiksite9 <- sum(jun04chiksite9$X9, na.rm=T)
ajun12chiksite9 <- sum(jun12chiksite9$X9, na.rm=T)
ajun18chiksite9 <- sum(jun18chiksite9$X9, na.rm=T)
ajun25chiksite9 <- sum(jun25chiksite9$X9, na.rm=T)
ajul04chiksite9 <- sum(jul04chiksite9$X9, na.rm=T)
ajul10chiksite9 <- sum(jul10chiksite9$X9, na.rm=T)
ajul17chiksite9 <- sum(jul17chiksite9$X9, na.rm=T)
ajul24chiksite9 <- sum(jul24chiksite9$X9, na.rm=T)
ajul30chiksite9 <- sum(jul30chiksite9$X9, na.rm=T)
aaug06chiksite9 <- sum(aug06chiksite9$X9, na.rm=T)
aaug13chiksite9 <- sum(aug13chiksite9$X9, na.rm=T)
aaug20chiksite9 <- sum(aug20chiksite9$X9, na.rm=T)
asep03chiksite9 <- sum(sep03chiksite9$X9, na.rm=T)


#NONchik
jun04nonchiksite9$X9 <- as.numeric(jun04nonchiksite9$X9)
jun12nonchiksite9$X9 <- as.numeric(jun12nonchiksite9$X9)
jun18nonchiksite9$X9 <- as.numeric(jun18nonchiksite9$X9)
jun25nonchiksite9$X9 <- as.numeric(jun25nonchiksite9$X9)
jul04nonchiksite9$X9 <- as.numeric(jul04nonchiksite9$X9)
jul10nonchiksite9$X9 <- as.numeric(jul10nonchiksite9$X9)
jul17nonchiksite9$X9 <- as.numeric(jul17nonchiksite9$X9)
jul24nonchiksite9$X9 <- as.numeric(jul24nonchiksite9$X9)
jul30nonchiksite9$X9 <- as.numeric(jul30nonchiksite9$X9)
aug06nonchiksite9$X9 <- as.numeric(aug06nonchiksite9$X9)
aug13nonchiksite9$X9 <- as.numeric(aug13nonchiksite9$X9)
aug20nonchiksite9$X9 <- as.numeric(aug20nonchiksite9$X9)
sep03nonchiksite9$X9 <- as.numeric(sep03nonchiksite9$X9)


ajun04nonchiksite9 <- sum(jun04nonchiksite9$X9, na.rm=T)
ajun12nonchiksite9 <- sum(jun12nonchiksite9$X9, na.rm=T)
ajun18nonchiksite9 <- sum(jun18nonchiksite9$X9, na.rm=T)
ajun25nonchiksite9 <- sum(jun25nonchiksite9$X9, na.rm=T)
ajul04nonchiksite9 <- sum(jul04nonchiksite9$X9, na.rm=T)
ajul10nonchiksite9 <- sum(jul10nonchiksite9$X9, na.rm=T)
ajul17nonchiksite9 <- sum(jul17nonchiksite9$X9, na.rm=T)
ajul24nonchiksite9 <- sum(jul24nonchiksite9$X9, na.rm=T)
ajul30nonchiksite9 <- sum(jul30nonchiksite9$X9, na.rm=T)
aaug06nonchiksite9 <- sum(aug06nonchiksite9$X9, na.rm=T)
aaug13nonchiksite9 <- sum(aug13nonchiksite9$X9, na.rm=T)
aaug20nonchiksite9 <- sum(aug20nonchiksite9$X9, na.rm=T)
asep03nonchiksite9 <- sum(sep03nonchiksite9$X9, na.rm=T)

#####################################
##### site10
#chik
jun04chiksite10$X10 <- as.numeric(jun04chiksite10$X10)
jun12chiksite10$X10 <- as.numeric(jun12chiksite10$X10)
jun18chiksite10$X10 <- as.numeric(jun18chiksite10$X10)
jun25chiksite10$X10 <- as.numeric(jun25chiksite10$X10)
jul04chiksite10$X10 <- as.numeric(jul04chiksite10$X10)
jul10chiksite10$X10 <- as.numeric(jul10chiksite10$X10)
jul17chiksite10$X10 <- as.numeric(jul17chiksite10$X10)
jul24chiksite10$X10 <- as.numeric(jul24chiksite10$X10)
jul30chiksite10$X10 <- as.numeric(jul30chiksite10$X10)
aug06chiksite10$X10 <- as.numeric(aug06chiksite10$X10)
aug13chiksite10$X10 <- as.numeric(aug13chiksite10$X10)
aug20chiksite10$X10 <- as.numeric(aug20chiksite10$X10)
sep03chiksite10$X10 <- as.numeric(sep03chiksite10$X10)


ajun04chiksite10 <- sum(jun04chiksite10$X10, na.rm=T)
ajun12chiksite10 <- sum(jun12chiksite10$X10, na.rm=T)
ajun18chiksite10 <- sum(jun18chiksite10$X10, na.rm=T)
ajun25chiksite10 <- sum(jun25chiksite10$X10, na.rm=T)
ajul04chiksite10 <- sum(jul04chiksite10$X10, na.rm=T)
ajul10chiksite10 <- sum(jul10chiksite10$X10, na.rm=T)
ajul17chiksite10 <- sum(jul17chiksite10$X10, na.rm=T)
ajul24chiksite10 <- sum(jul24chiksite10$X10, na.rm=T)
ajul30chiksite10 <- sum(jul30chiksite10$X10, na.rm=T)
aaug06chiksite10 <- sum(aug06chiksite10$X10, na.rm=T)
aaug13chiksite10 <- sum(aug13chiksite10$X10, na.rm=T)
aaug20chiksite10 <- sum(aug20chiksite10$X10, na.rm=T)
asep03chiksite10 <- sum(sep03chiksite10$X10, na.rm=T)


#NONchik
jun04nonchiksite10$X10 <- as.numeric(jun04nonchiksite10$X10)
jun12nonchiksite10$X10 <- as.numeric(jun12nonchiksite10$X10)
jun18nonchiksite10$X10 <- as.numeric(jun18nonchiksite10$X10)
jun25nonchiksite10$X10 <- as.numeric(jun25nonchiksite10$X10)
jul04nonchiksite10$X10 <- as.numeric(jul04nonchiksite10$X10)
jul10nonchiksite10$X10 <- as.numeric(jul10nonchiksite10$X10)
jul17nonchiksite10$X10 <- as.numeric(jul17nonchiksite10$X10)
jul24nonchiksite10$X10 <- as.numeric(jul24nonchiksite10$X10)
jul30nonchiksite10$X10 <- as.numeric(jul30nonchiksite10$X10)
aug06nonchiksite10$X10 <- as.numeric(aug06nonchiksite10$X10)
aug13nonchiksite10$X10 <- as.numeric(aug13nonchiksite10$X10)
aug20nonchiksite10$X10 <- as.numeric(aug20nonchiksite10$X10)
sep03nonchiksite10$X10 <- as.numeric(sep03nonchiksite10$X10)


ajun04nonchiksite10 <- sum(jun04nonchiksite10$X10, na.rm=T)
ajun12nonchiksite10 <- sum(jun12nonchiksite10$X10, na.rm=T)
ajun18nonchiksite10 <- sum(jun18nonchiksite10$X10, na.rm=T)
ajun25nonchiksite10 <- sum(jun25nonchiksite10$X10, na.rm=T)
ajul04nonchiksite10 <- sum(jul04nonchiksite10$X10, na.rm=T)
ajul10nonchiksite10 <- sum(jul10nonchiksite10$X10, na.rm=T)
ajul17nonchiksite10 <- sum(jul17nonchiksite10$X10, na.rm=T)
ajul24nonchiksite10 <- sum(jul24nonchiksite10$X10, na.rm=T)
ajul30nonchiksite10 <- sum(jul30nonchiksite10$X10, na.rm=T)
aaug06nonchiksite10 <- sum(aug06nonchiksite10$X10, na.rm=T)
aaug13nonchiksite10 <- sum(aug13nonchiksite10$X10, na.rm=T)
aaug20nonchiksite10 <- sum(aug20nonchiksite10$X10, na.rm=T)
asep03nonchiksite10 <- sum(sep03nonchiksite10$X10, na.rm=T)



#####################
#
#
#
#
#
################################


#CREATE chikS FOR PLOTTING
#Site 1
chik1 <- c(ajun04chiksite1,
          ajun12chiksite1,
          ajun18chiksite1,
          ajun25chiksite1,
          ajul04chiksite1,
          ajul10chiksite1,
          ajul17chiksite1,
          ajul24chiksite1,
          ajul30chiksite1,
          aaug06chiksite1,
          aaug13chiksite1,
          aaug20chiksite1,
          asep03chiksite1)

#Site 2
chik2 <- c(ajun04chiksite2,
          ajun12chiksite2,
          ajun18chiksite2,
          ajun25chiksite2,
          ajul04chiksite2,
          ajul10chiksite2,
          ajul17chiksite2,
          ajul24chiksite2,
          ajul30chiksite2,
          aaug06chiksite2,
          aaug13chiksite2,
          aaug20chiksite2,
          asep03chiksite2)


#Site 3
chik3 <- c(ajun04chiksite3,
          ajun12chiksite3,
          ajun18chiksite3,
          ajun25chiksite3,
          ajul04chiksite3,
          ajul10chiksite3,
          ajul17chiksite3,
          ajul24chiksite3,
          ajul30chiksite3,
          aaug06chiksite3,
          aaug13chiksite3,
          aaug20chiksite3,
          asep03chiksite3)


#Site 4
chik4 <- c(ajun04chiksite4,
          ajun12chiksite4,
          ajun18chiksite4,
          ajun25chiksite4,
          ajul04chiksite4,
          ajul10chiksite4,
          ajul17chiksite4,
          ajul24chiksite4,
          ajul30chiksite4,
          aaug06chiksite4,
          aaug13chiksite4,
          aaug20chiksite4,
          asep03chiksite4)


#Site 5
chik5 <- c(ajun04chiksite5,
          ajun12chiksite5,
          ajun18chiksite5,
          ajun25chiksite5,
          ajul04chiksite5,
          ajul10chiksite5,
          ajul17chiksite5,
          ajul24chiksite5,
          ajul30chiksite5,
          aaug06chiksite5,
          aaug13chiksite5,
          aaug20chiksite5,
          asep03chiksite5)


#Site 6
chik6 <- c(ajun04chiksite6,
          ajun12chiksite6,
          ajun18chiksite6,
          ajun25chiksite6,
          ajul04chiksite6,
          ajul10chiksite6,
          ajul17chiksite6,
          ajul24chiksite6,
          ajul30chiksite6,
          aaug06chiksite6,
          aaug13chiksite6,
          aaug20chiksite6,
          asep03chiksite6)


#Site 7
chik7 <- c(ajun04chiksite7,
          ajun12chiksite7,
          ajun18chiksite7,
          ajun25chiksite7,
          ajul04chiksite7,
          ajul10chiksite7,
          ajul17chiksite7,
          ajul24chiksite7,
          ajul30chiksite7,
          aaug06chiksite7,
          aaug13chiksite7,
          aaug20chiksite7,
          asep03chiksite7)


#Site 8
chik8 <- c(ajun04chiksite8,
          ajun12chiksite8,
          ajun18chiksite8,
          ajun25chiksite8,
          ajul04chiksite8,
          ajul10chiksite8,
          ajul17chiksite8,
          ajul24chiksite8,
          ajul30chiksite8,
          aaug06chiksite8,
          aaug13chiksite8,
          aaug20chiksite8,
          asep03chiksite8)


#Site 9
chik9 <- c(ajun04chiksite9,
          ajun12chiksite9,
          ajun18chiksite9,
          ajun25chiksite9,
          ajul04chiksite9,
          ajul10chiksite9,
          ajul17chiksite9,
          ajul24chiksite9,
          ajul30chiksite9,
          aaug06chiksite9,
          aaug13chiksite9,
          aaug20chiksite9,
          asep03chiksite9)


#Site 10
chik10 <- c(ajun04chiksite10,
           ajun12chiksite10,
           ajun18chiksite10,
           ajun25chiksite10,
           ajul04chiksite10,
           ajul10chiksite10,
           ajul17chiksite10,
           ajul24chiksite10,
           ajul30chiksite10,
           aaug06chiksite10,
           aaug13chiksite10,
           aaug20chiksite10,
           asep03chiksite10)



chiktotal <- chik1+chik2+chik3+chik4+chik5+chik6+chik7+chik8+chik9+chik10


##### MALARIA 5 - MALARIA
#SUBSET BY DATE and malaria
jun04malaria <- subset(mergedmalaria, date2  == as.Date("2013-06-04"))
jun12malaria <- subset(mergedmalaria, date2  == as.Date("2013-06-12"))
jun18malaria <- subset(mergedmalaria, date2  == as.Date("2013-06-18"))
jun25malaria <- subset(mergedmalaria, date2  == as.Date("2013-06-25"))
jul04malaria <- subset(mergedmalaria, date2  == as.Date("2013-07-04"))
jul10malaria <- subset(mergedmalaria, date2  == as.Date("2013-07-10"))
jul17malaria <- subset(mergedmalaria, date2  == as.Date("2013-07-17"))
jul24malaria <- subset(mergedmalaria, date2  == as.Date("2013-07-24"))
jul30malaria <- subset(mergedmalaria, date2  == as.Date("2013-07-30"))
aug06malaria <- subset(mergedmalaria, date2  == as.Date("2013-08-06"))
aug13malaria <- subset(mergedmalaria, date2  == as.Date("2013-08-13"))
aug20malaria <- subset(mergedmalaria, date2  == as.Date("2013-08-20"))
sep03malaria <- subset(mergedmalaria, date2  == as.Date("2013-09-03"))


####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE malaria SITE

#JUN 04 malaria
jun04malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-06-04"))
jun04malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-06-04"))
jun04malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-06-04"))
jun04malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-06-04"))
jun04malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-06-04"))
jun04malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-06-04"))
jun04malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-06-04"))
jun04malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-06-04"))
jun04malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-06-04"))
jun04malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-06-04"))


#jun12 malaria
jun12malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-06-12"))
jun12malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-06-12"))
jun12malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-06-12"))
jun12malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-06-12"))
jun12malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-06-12"))
jun12malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-06-12"))
jun12malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-06-12"))
jun12malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-06-12"))
jun12malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-06-12"))
jun12malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-06-12"))

#jun18 malaria
jun18malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-06-18"))
jun18malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-06-18"))
jun18malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-06-18"))
jun18malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-06-18"))
jun18malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-06-18"))
jun18malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-06-18"))
jun18malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-06-18"))
jun18malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-06-18"))
jun18malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-06-18"))
jun18malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-06-18"))

#jun25 malaria
jun25malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-06-25"))
jun25malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-06-25"))
jun25malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-06-25"))
jun25malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-06-25"))
jun25malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-06-25"))
jun25malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-06-25"))
jun25malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-06-25"))
jun25malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-06-25"))
jun25malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-06-25"))
jun25malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-06-25"))

#jul04 malaria
jul04malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-07-04"))
jul04malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-07-04"))
jul04malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-07-04"))
jul04malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-07-04"))
jul04malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-07-04"))
jul04malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-07-04"))
jul04malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-07-04"))
jul04malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-07-04"))
jul04malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-07-04"))
jul04malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-07-04"))

#jul10 malaria
jul10malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-07-10"))
jul10malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-07-10"))
jul10malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-07-10"))
jul10malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-07-10"))
jul10malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-07-10"))
jul10malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-07-10"))
jul10malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-07-10"))
jul10malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-07-10"))
jul10malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-07-10"))
jul10malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-07-10"))

#jul17 malaria
jul17malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-07-17"))
jul17malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-07-17"))
jul17malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-07-17"))
jul17malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-07-17"))
jul17malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-07-17"))
jul17malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-07-17"))
jul17malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-07-17"))
jul17malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-07-17"))
jul17malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-07-17"))
jul17malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-07-17"))

#jul24 malaria
jul24malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-07-24"))
jul24malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-07-24"))
jul24malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-07-24"))
jul24malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-07-24"))
jul24malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-07-24"))
jul24malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-07-24"))
jul24malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-07-24"))
jul24malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-07-24"))
jul24malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-07-24"))
jul24malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-07-24"))

#jul30 malaria
jul30malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-07-30"))
jul30malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-07-30"))
jul30malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-07-30"))
jul30malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-07-30"))
jul30malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-07-30"))
jul30malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-07-30"))
jul30malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-07-30"))
jul30malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-07-30"))
jul30malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-07-30"))
jul30malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-07-30"))

#aug06 malaria
aug06malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-08-06"))
aug06malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-08-06"))
aug06malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-08-06"))
aug06malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-08-06"))
aug06malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-08-06"))
aug06malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-08-06"))
aug06malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-08-06"))
aug06malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-08-06"))
aug06malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-08-06"))
aug06malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-08-06"))

#aug13 malaria
aug13malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-08-13"))
aug13malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-08-13"))
aug13malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-08-13"))
aug13malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-08-13"))
aug13malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-08-13"))
aug13malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-08-13"))
aug13malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-08-13"))
aug13malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-08-13"))
aug13malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-08-13"))
aug13malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-08-13"))

#aug20 malaria
aug20malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-08-20"))
aug20malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-08-20"))
aug20malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-08-20"))
aug20malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-08-20"))
aug20malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-08-20"))
aug20malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-08-20"))
aug20malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-08-20"))
aug20malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-08-20"))
aug20malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-08-20"))
aug20malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-08-20"))

#sep03 malaria
sep03malariasite1 <-subset(merged1malaria, date2  == as.Date("2013-09-03"))
sep03malariasite2 <-subset(merged2malaria, date2  == as.Date("2013-09-03"))
sep03malariasite3 <-subset(merged3malaria, date2  == as.Date("2013-09-03"))
sep03malariasite4 <-subset(merged4malaria, date2  == as.Date("2013-09-03"))
sep03malariasite5 <-subset(merged5malaria, date2  == as.Date("2013-09-03"))
sep03malariasite6 <-subset(merged6malaria, date2  == as.Date("2013-09-03"))
sep03malariasite7 <-subset(merged7malaria, date2  == as.Date("2013-09-03"))
sep03malariasite8 <-subset(merged8malaria, date2  == as.Date("2013-09-03"))
sep03malariasite9 <-subset(merged9malaria, date2  == as.Date("2013-09-03"))
sep03malariasite10 <-subset(merged10malaria, date2  == as.Date("2013-09-03"))

##### ALL SITES COMBINED
#malaria
ajun04malariatotal <- sum(jun04malaria$total, na.rm=T)
ajun12malariatotal <- sum(jun12malaria$total, na.rm=T)
ajun18malariatotal <- sum(jun18malaria$total, na.rm=T)
ajun25malariatotal <- sum(jun25malaria$total, na.rm=T)
ajul04malariatotal <- sum(jul04malaria$total, na.rm=T)
ajul10malariatotal <- sum(jul10malaria$total, na.rm=T)
ajul17malariatotal <- sum(jul17malaria$total, na.rm=T)
ajul24malariatotal <- sum(jul24malaria$total, na.rm=T)
ajul30malariatotal <- sum(jul30malaria$total, na.rm=T)
aaug06malariatotal <- sum(aug06malaria$total, na.rm=T)
aaug13malariatotal <- sum(aug13malaria$total, na.rm=T)
aaug20malariatotal <- sum(aug20malaria$total, na.rm=T)
asep03malariatotal <- sum(sep03malaria$total, na.rm=T)



#####################################
##### site1
#malaria
ajun04malariasite1 <- sum(jun04malariasite1$X1, na.rm=T)
ajun12malariasite1 <- sum(jun12malariasite1$X1, na.rm=T)
ajun18malariasite1 <- sum(jun18malariasite1$X1, na.rm=T)
ajun25malariasite1 <- sum(jun25malariasite1$X1, na.rm=T)
ajul04malariasite1 <- sum(jul04malariasite1$X1, na.rm=T)
ajul10malariasite1 <- sum(jul10malariasite1$X1, na.rm=T)
ajul17malariasite1 <- sum(jul17malariasite1$X1, na.rm=T)
ajul24malariasite1 <- sum(jul24malariasite1$X1, na.rm=T)
ajul30malariasite1 <- sum(jul30malariasite1$X1, na.rm=T)
aaug06malariasite1 <- sum(aug06malariasite1$X1, na.rm=T)
aaug13malariasite1 <- sum(aug13malariasite1$X1, na.rm=T)
aaug20malariasite1 <- sum(aug20malariasite1$X1, na.rm=T)
asep03malariasite1 <- sum(sep03malariasite1$X1, na.rm=T)


#NONmalaria
ajun04nonmalariasite1 <- sum(jun04nonmalariasite1$X1, na.rm=T)
ajun12nonmalariasite1 <- sum(jun12nonmalariasite1$X1, na.rm=T)
ajun18nonmalariasite1 <- sum(jun18nonmalariasite1$X1, na.rm=T)
ajun25nonmalariasite1 <- sum(jun25nonmalariasite1$X1, na.rm=T)
ajul04nonmalariasite1 <- sum(jul04nonmalariasite1$X1, na.rm=T)
ajul10nonmalariasite1 <- sum(jul10nonmalariasite1$X1, na.rm=T)
ajul17nonmalariasite1 <- sum(jul17nonmalariasite1$X1, na.rm=T)
ajul24nonmalariasite1 <- sum(jul24nonmalariasite1$X1, na.rm=T)
ajul30nonmalariasite1 <- sum(jul30nonmalariasite1$X1, na.rm=T)
aaug06nonmalariasite1 <- sum(aug06nonmalariasite1$X1, na.rm=T)
aaug13nonmalariasite1 <- sum(aug13nonmalariasite1$X1, na.rm=T)
aaug20nonmalariasite1 <- sum(aug20nonmalariasite1$X1, na.rm=T)
asep03nonmalariasite1 <- sum(sep03nonmalariasite1$X1, na.rm=T)

#####################################
##### site2
#malaria
ajun04malariasite2 <- sum(jun04malariasite2$X2, na.rm=T)
ajun12malariasite2 <- sum(jun12malariasite2$X2, na.rm=T)
ajun18malariasite2 <- sum(jun18malariasite2$X2, na.rm=T)
ajun25malariasite2 <- sum(jun25malariasite2$X2, na.rm=T)
ajul04malariasite2 <- sum(jul04malariasite2$X2, na.rm=T)
ajul10malariasite2 <- sum(jul10malariasite2$X2, na.rm=T)
ajul17malariasite2 <- sum(jul17malariasite2$X2, na.rm=T)
ajul24malariasite2 <- sum(jul24malariasite2$X2, na.rm=T)
ajul30malariasite2 <- sum(jul30malariasite2$X2, na.rm=T)
aaug06malariasite2 <- sum(aug06malariasite2$X2, na.rm=T)
aaug13malariasite2 <- sum(aug13malariasite2$X2, na.rm=T)
aaug20malariasite2 <- sum(aug20malariasite2$X2, na.rm=T)
asep03malariasite2 <- sum(sep03malariasite2$X2, na.rm=T)


#NONmalaria
ajun04nonmalariasite2 <- sum(jun04nonmalariasite2$X2, na.rm=T)
ajun12nonmalariasite2 <- sum(jun12nonmalariasite2$X2, na.rm=T)
ajun18nonmalariasite2 <- sum(jun18nonmalariasite2$X2, na.rm=T)
ajun25nonmalariasite2 <- sum(jun25nonmalariasite2$X2, na.rm=T)
ajul04nonmalariasite2 <- sum(jul04nonmalariasite2$X2, na.rm=T)
ajul10nonmalariasite2 <- sum(jul10nonmalariasite2$X2, na.rm=T)
ajul17nonmalariasite2 <- sum(jul17nonmalariasite2$X2, na.rm=T)
ajul24nonmalariasite2 <- sum(jul24nonmalariasite2$X2, na.rm=T)
ajul30nonmalariasite2 <- sum(jul30nonmalariasite2$X2, na.rm=T)
aaug06nonmalariasite2 <- sum(aug06nonmalariasite2$X2, na.rm=T)
aaug13nonmalariasite2 <- sum(aug13nonmalariasite2$X2, na.rm=T)
aaug20nonmalariasite2 <- sum(aug20nonmalariasite2$X2, na.rm=T)
asep03nonmalariasite2 <- sum(sep03nonmalariasite2$X2, na.rm=T)

#####################################
##### site3
#malaria
ajun04malariasite3 <- sum(jun04malariasite3$X3, na.rm=T)
ajun12malariasite3 <- sum(jun12malariasite3$X3, na.rm=T)
ajun18malariasite3 <- sum(jun18malariasite3$X3, na.rm=T)
ajun25malariasite3 <- sum(jun25malariasite3$X3, na.rm=T)
ajul04malariasite3 <- sum(jul04malariasite3$X3, na.rm=T)
ajul10malariasite3 <- sum(jul10malariasite3$X3, na.rm=T)
ajul17malariasite3 <- sum(jul17malariasite3$X3, na.rm=T)
ajul24malariasite3 <- sum(jul24malariasite3$X3, na.rm=T)
ajul30malariasite3 <- sum(jul30malariasite3$X3, na.rm=T)
aaug06malariasite3 <- sum(aug06malariasite3$X3, na.rm=T)
aaug13malariasite3 <- sum(aug13malariasite3$X3, na.rm=T)
aaug20malariasite3 <- sum(aug20malariasite3$X3, na.rm=T)
asep03malariasite3 <- sum(sep03malariasite3$X3, na.rm=T)


#NONmalaria
ajun04nonmalariasite3 <- sum(jun04nonmalariasite3$X3, na.rm=T)
ajun12nonmalariasite3 <- sum(jun12nonmalariasite3$X3, na.rm=T)
ajun18nonmalariasite3 <- sum(jun18nonmalariasite3$X3, na.rm=T)
ajun25nonmalariasite3 <- sum(jun25nonmalariasite3$X3, na.rm=T)
ajul04nonmalariasite3 <- sum(jul04nonmalariasite3$X3, na.rm=T)
ajul10nonmalariasite3 <- sum(jul10nonmalariasite3$X3, na.rm=T)
ajul17nonmalariasite3 <- sum(jul17nonmalariasite3$X3, na.rm=T)
ajul24nonmalariasite3 <- sum(jul24nonmalariasite3$X3, na.rm=T)
ajul30nonmalariasite3 <- sum(jul30nonmalariasite3$X3, na.rm=T)
aaug06nonmalariasite3 <- sum(aug06nonmalariasite3$X3, na.rm=T)
aaug13nonmalariasite3 <- sum(aug13nonmalariasite3$X3, na.rm=T)
aaug20nonmalariasite3 <- sum(aug20nonmalariasite3$X3, na.rm=T)
asep03nonmalariasite3 <- sum(sep03nonmalariasite3$X3, na.rm=T)


#####################################
##### site4
#malaria
ajun04malariasite4 <- sum(jun04malariasite4$X4, na.rm=T)
ajun12malariasite4 <- sum(jun12malariasite4$X4, na.rm=T)
ajun18malariasite4 <- sum(jun18malariasite4$X4, na.rm=T)
ajun25malariasite4 <- sum(jun25malariasite4$X4, na.rm=T)
ajul04malariasite4 <- sum(jul04malariasite4$X4, na.rm=T)
ajul10malariasite4 <- sum(jul10malariasite4$X4, na.rm=T)
ajul17malariasite4 <- sum(jul17malariasite4$X4, na.rm=T)
ajul24malariasite4 <- sum(jul24malariasite4$X4, na.rm=T)
ajul30malariasite4 <- sum(jul30malariasite4$X4, na.rm=T)
aaug06malariasite4 <- sum(aug06malariasite4$X4, na.rm=T)
aaug13malariasite4 <- sum(aug13malariasite4$X4, na.rm=T)
aaug20malariasite4 <- sum(aug20malariasite4$X4, na.rm=T)
asep03malariasite4 <- sum(sep03malariasite4$X4, na.rm=T)


#NONmalaria
ajun04nonmalariasite4 <- sum(jun04nonmalariasite4$X4, na.rm=T)
ajun12nonmalariasite4 <- sum(jun12nonmalariasite4$X4, na.rm=T)
ajun18nonmalariasite4 <- sum(jun18nonmalariasite4$X4, na.rm=T)
ajun25nonmalariasite4 <- sum(jun25nonmalariasite4$X4, na.rm=T)
ajul04nonmalariasite4 <- sum(jul04nonmalariasite4$X4, na.rm=T)
ajul10nonmalariasite4 <- sum(jul10nonmalariasite4$X4, na.rm=T)
ajul17nonmalariasite4 <- sum(jul17nonmalariasite4$X4, na.rm=T)
ajul24nonmalariasite4 <- sum(jul24nonmalariasite4$X4, na.rm=T)
ajul30nonmalariasite4 <- sum(jul30nonmalariasite4$X4, na.rm=T)
aaug06nonmalariasite4 <- sum(aug06nonmalariasite4$X4, na.rm=T)
aaug13nonmalariasite4 <- sum(aug13nonmalariasite4$X4, na.rm=T)
aaug20nonmalariasite4 <- sum(aug20nonmalariasite4$X4, na.rm=T)
asep03nonmalariasite4 <- sum(sep03nonmalariasite4$X4, na.rm=T)

#####################################
##### site5
#malaria
ajun04malariasite5 <- sum(jun04malariasite5$X5, na.rm=T)
ajun12malariasite5 <- sum(jun12malariasite5$X5, na.rm=T)
ajun18malariasite5 <- sum(jun18malariasite5$X5, na.rm=T)
ajun25malariasite5 <- sum(jun25malariasite5$X5, na.rm=T)
ajul04malariasite5 <- sum(jul04malariasite5$X5, na.rm=T)
ajul10malariasite5 <- sum(jul10malariasite5$X5, na.rm=T)
ajul17malariasite5 <- sum(jul17malariasite5$X5, na.rm=T)
ajul24malariasite5 <- sum(jul24malariasite5$X5, na.rm=T)
ajul30malariasite5 <- sum(jul30malariasite5$X5, na.rm=T)
aaug06malariasite5 <- sum(aug06malariasite5$X5, na.rm=T)
aaug13malariasite5 <- sum(aug13malariasite5$X5, na.rm=T)
aaug20malariasite5 <- sum(aug20malariasite5$X5, na.rm=T)
asep03malariasite5 <- sum(sep03malariasite5$X5, na.rm=T)


#NONmalaria
ajun04nonmalariasite5 <- sum(jun04nonmalariasite5$X5, na.rm=T)
ajun12nonmalariasite5 <- sum(jun12nonmalariasite5$X5, na.rm=T)
ajun18nonmalariasite5 <- sum(jun18nonmalariasite5$X5, na.rm=T)
ajun25nonmalariasite5 <- sum(jun25nonmalariasite5$X5, na.rm=T)
ajul04nonmalariasite5 <- sum(jul04nonmalariasite5$X5, na.rm=T)
ajul10nonmalariasite5 <- sum(jul10nonmalariasite5$X5, na.rm=T)
ajul17nonmalariasite5 <- sum(jul17nonmalariasite5$X5, na.rm=T)
ajul24nonmalariasite5 <- sum(jul24nonmalariasite5$X5, na.rm=T)
ajul30nonmalariasite5 <- sum(jul30nonmalariasite5$X5, na.rm=T)
aaug06nonmalariasite5 <- sum(aug06nonmalariasite5$X5, na.rm=T)
aaug13nonmalariasite5 <- sum(aug13nonmalariasite5$X5, na.rm=T)
aaug20nonmalariasite5 <- sum(aug20nonmalariasite5$X5, na.rm=T)
asep03nonmalariasite5 <- sum(sep03nonmalariasite5$X5, na.rm=T)


#####################################
##### site6
#malaria
jun04malariasite6$X6 <- as.numeric(jun04malariasite6$X6)
jun12malariasite6$X6 <- as.numeric(jun12malariasite6$X6)
jun18malariasite6$X6 <- as.numeric(jun18malariasite6$X6)
jun25malariasite6$X6 <- as.numeric(jun25malariasite6$X6)
jul04malariasite6$X6 <- as.numeric(jul04malariasite6$X6)
jul10malariasite6$X6 <- as.numeric(jul10malariasite6$X6)
jul17malariasite6$X6 <- as.numeric(jul17malariasite6$X6)
jul24malariasite6$X6 <- as.numeric(jul24malariasite6$X6)
jul30malariasite6$X6 <- as.numeric(jul30malariasite6$X6)
aug06malariasite6$X6 <- as.numeric(aug06malariasite6$X6)
aug13malariasite6$X6 <- as.numeric(aug13malariasite6$X6)
aug20malariasite6$X6 <- as.numeric(aug20malariasite6$X6)
sep03malariasite6$X6 <- as.numeric(sep03malariasite6$X6)


ajun04malariasite6 <- sum(jun04malariasite6$X6, na.rm=T)
ajun12malariasite6 <- sum(jun12malariasite6$X6, na.rm=T)
ajun18malariasite6 <- sum(jun18malariasite6$X6, na.rm=T)
ajun25malariasite6 <- sum(jun25malariasite6$X6, na.rm=T)
ajul04malariasite6 <- sum(jul04malariasite6$X6, na.rm=T)
ajul10malariasite6 <- sum(jul10malariasite6$X6, na.rm=T)
ajul17malariasite6 <- sum(jul17malariasite6$X6, na.rm=T)
ajul24malariasite6 <- sum(jul24malariasite6$X6, na.rm=T)
ajul30malariasite6 <- sum(jul30malariasite6$X6, na.rm=T)
aaug06malariasite6 <- sum(aug06malariasite6$X6, na.rm=T)
aaug13malariasite6 <- sum(aug13malariasite6$X6, na.rm=T)
aaug20malariasite6 <- sum(aug20malariasite6$X6, na.rm=T)
asep03malariasite6 <- sum(sep03malariasite6$X6, na.rm=T)


#NONmalaria
jun04nonmalariasite6$X6 <- as.numeric(jun04nonmalariasite6$X6)
jun12nonmalariasite6$X6 <- as.numeric(jun12nonmalariasite6$X6)
jun18nonmalariasite6$X6 <- as.numeric(jun18nonmalariasite6$X6)
jun25nonmalariasite6$X6 <- as.numeric(jun25nonmalariasite6$X6)
jul04nonmalariasite6$X6 <- as.numeric(jul04nonmalariasite6$X6)
jul10nonmalariasite6$X6 <- as.numeric(jul10nonmalariasite6$X6)
jul17nonmalariasite6$X6 <- as.numeric(jul17nonmalariasite6$X6)
jul24nonmalariasite6$X6 <- as.numeric(jul24nonmalariasite6$X6)
jul30nonmalariasite6$X6 <- as.numeric(jul30nonmalariasite6$X6)
aug06nonmalariasite6$X6 <- as.numeric(aug06nonmalariasite6$X6)
aug13nonmalariasite6$X6 <- as.numeric(aug13nonmalariasite6$X6)
aug20nonmalariasite6$X6 <- as.numeric(aug20nonmalariasite6$X6)
sep03nonmalariasite6$X6 <- as.numeric(sep03nonmalariasite6$X6)


ajun04nonmalariasite6 <- sum(jun04nonmalariasite6$X6, na.rm=T)
ajun12nonmalariasite6 <- sum(jun12nonmalariasite6$X6, na.rm=T)
ajun18nonmalariasite6 <- sum(jun18nonmalariasite6$X6, na.rm=T)
ajun25nonmalariasite6 <- sum(jun25nonmalariasite6$X6, na.rm=T)
ajul04nonmalariasite6 <- sum(jul04nonmalariasite6$X6, na.rm=T)
ajul10nonmalariasite6 <- sum(jul10nonmalariasite6$X6, na.rm=T)
ajul17nonmalariasite6 <- sum(jul17nonmalariasite6$X6, na.rm=T)
ajul24nonmalariasite6 <- sum(jul24nonmalariasite6$X6, na.rm=T)
ajul30nonmalariasite6 <- sum(jul30nonmalariasite6$X6, na.rm=T)
aaug06nonmalariasite6 <- sum(aug06nonmalariasite6$X6, na.rm=T)
aaug13nonmalariasite6 <- sum(aug13nonmalariasite6$X6, na.rm=T)
aaug20nonmalariasite6 <- sum(aug20nonmalariasite6$X6, na.rm=T)
asep03nonmalariasite6 <- sum(sep03nonmalariasite6$X6, na.rm=T)


#####################################
##### site7
#malaria
ajun04malariasite7 <- sum(jun04malariasite7$X7, na.rm=T)
ajun12malariasite7 <- sum(jun12malariasite7$X7, na.rm=T)
ajun18malariasite7 <- sum(jun18malariasite7$X7, na.rm=T)
ajun25malariasite7 <- sum(jun25malariasite7$X7, na.rm=T)
ajul04malariasite7 <- sum(jul04malariasite7$X7, na.rm=T)
ajul10malariasite7 <- sum(jul10malariasite7$X7, na.rm=T)
ajul17malariasite7 <- sum(jul17malariasite7$X7, na.rm=T)
ajul24malariasite7 <- sum(jul24malariasite7$X7, na.rm=T)
ajul30malariasite7 <- sum(jul30malariasite7$X7, na.rm=T)
aaug06malariasite7 <- sum(aug06malariasite7$X7, na.rm=T)
aaug13malariasite7 <- sum(aug13malariasite7$X7, na.rm=T)
aaug20malariasite7 <- sum(aug20malariasite7$X7, na.rm=T)
asep03malariasite7 <- sum(sep03malariasite7$X7, na.rm=T)


#NONmalaria
ajun04nonmalariasite7 <- sum(jun04nonmalariasite7$X7, na.rm=T)
ajun12nonmalariasite7 <- sum(jun12nonmalariasite7$X7, na.rm=T)
ajun18nonmalariasite7 <- sum(jun18nonmalariasite7$X7, na.rm=T)
ajun25nonmalariasite7 <- sum(jun25nonmalariasite7$X7, na.rm=T)
ajul04nonmalariasite7 <- sum(jul04nonmalariasite7$X7, na.rm=T)
ajul10nonmalariasite7 <- sum(jul10nonmalariasite7$X7, na.rm=T)
ajul17nonmalariasite7 <- sum(jul17nonmalariasite7$X7, na.rm=T)
ajul24nonmalariasite7 <- sum(jul24nonmalariasite7$X7, na.rm=T)
ajul30nonmalariasite7 <- sum(jul30nonmalariasite7$X7, na.rm=T)
aaug06nonmalariasite7 <- sum(aug06nonmalariasite7$X7, na.rm=T)
aaug13nonmalariasite7 <- sum(aug13nonmalariasite7$X7, na.rm=T)
aaug20nonmalariasite7 <- sum(aug20nonmalariasite7$X7, na.rm=T)
asep03nonmalariasite7 <- sum(sep03nonmalariasite7$X7, na.rm=T)


#####################################
##### site8
#malaria
ajun04malariasite8 <- sum(jun04malariasite8$X8, na.rm=T)
ajun12malariasite8 <- sum(jun12malariasite8$X8, na.rm=T)
ajun18malariasite8 <- sum(jun18malariasite8$X8, na.rm=T)
ajun25malariasite8 <- sum(jun25malariasite8$X8, na.rm=T)
ajul04malariasite8 <- sum(jul04malariasite8$X8, na.rm=T)
ajul10malariasite8 <- sum(jul10malariasite8$X8, na.rm=T)
ajul17malariasite8 <- sum(jul17malariasite8$X8, na.rm=T)
ajul24malariasite8 <- sum(jul24malariasite8$X8, na.rm=T)
ajul30malariasite8 <- sum(jul30malariasite8$X8, na.rm=T)
aaug06malariasite8 <- sum(aug06malariasite8$X8, na.rm=T)
aaug13malariasite8 <- sum(aug13malariasite8$X8, na.rm=T)
aaug20malariasite8 <- sum(aug20malariasite8$X8, na.rm=T)
asep03malariasite8 <- sum(sep03malariasite8$X8, na.rm=T)


#NONmalaria
ajun04nonmalariasite8 <- sum(jun04nonmalariasite8$X8, na.rm=T)
ajun12nonmalariasite8 <- sum(jun12nonmalariasite8$X8, na.rm=T)
ajun18nonmalariasite8 <- sum(jun18nonmalariasite8$X8, na.rm=T)
ajun25nonmalariasite8 <- sum(jun25nonmalariasite8$X8, na.rm=T)
ajul04nonmalariasite8 <- sum(jul04nonmalariasite8$X8, na.rm=T)
ajul10nonmalariasite8 <- sum(jul10nonmalariasite8$X8, na.rm=T)
ajul17nonmalariasite8 <- sum(jul17nonmalariasite8$X8, na.rm=T)
ajul24nonmalariasite8 <- sum(jul24nonmalariasite8$X8, na.rm=T)
ajul30nonmalariasite8 <- sum(jul30nonmalariasite8$X8, na.rm=T)
aaug06nonmalariasite8 <- sum(aug06nonmalariasite8$X8, na.rm=T)
aaug13nonmalariasite8 <- sum(aug13nonmalariasite8$X8, na.rm=T)
aaug20nonmalariasite8 <- sum(aug20nonmalariasite8$X8, na.rm=T)
asep03nonmalariasite8 <- sum(sep03nonmalariasite8$X8, na.rm=T)


#####################################
##### site9
#malaria

jun04malariasite9$X9 <- as.numeric(jun04malariasite9$X9)
jun12malariasite9$X9 <- as.numeric(jun12malariasite9$X9)
jun18malariasite9$X9 <- as.numeric(jun18malariasite9$X9)
jun25malariasite9$X9 <- as.numeric(jun25malariasite9$X9)
jul04malariasite9$X9 <- as.numeric(jul04malariasite9$X9)
jul10malariasite9$X9 <- as.numeric(jul10malariasite9$X9)
jul17malariasite9$X9 <- as.numeric(jul17malariasite9$X9)
jul24malariasite9$X9 <- as.numeric(jul24malariasite9$X9)
jul30malariasite9$X9 <- as.numeric(jul30malariasite9$X9)
aug06malariasite9$X9 <- as.numeric(aug06malariasite9$X9)
aug13malariasite9$X9 <- as.numeric(aug13malariasite9$X9)
aug20malariasite9$X9 <- as.numeric(aug20malariasite9$X9)
sep03malariasite9$X9 <- as.numeric(sep03malariasite9$X9)


ajun04malariasite9 <- sum(jun04malariasite9$X9, na.rm=T)
ajun12malariasite9 <- sum(jun12malariasite9$X9, na.rm=T)
ajun18malariasite9 <- sum(jun18malariasite9$X9, na.rm=T)
ajun25malariasite9 <- sum(jun25malariasite9$X9, na.rm=T)
ajul04malariasite9 <- sum(jul04malariasite9$X9, na.rm=T)
ajul10malariasite9 <- sum(jul10malariasite9$X9, na.rm=T)
ajul17malariasite9 <- sum(jul17malariasite9$X9, na.rm=T)
ajul24malariasite9 <- sum(jul24malariasite9$X9, na.rm=T)
ajul30malariasite9 <- sum(jul30malariasite9$X9, na.rm=T)
aaug06malariasite9 <- sum(aug06malariasite9$X9, na.rm=T)
aaug13malariasite9 <- sum(aug13malariasite9$X9, na.rm=T)
aaug20malariasite9 <- sum(aug20malariasite9$X9, na.rm=T)
asep03malariasite9 <- sum(sep03malariasite9$X9, na.rm=T)


#NONmalaria
jun04nonmalariasite9$X9 <- as.numeric(jun04nonmalariasite9$X9)
jun12nonmalariasite9$X9 <- as.numeric(jun12nonmalariasite9$X9)
jun18nonmalariasite9$X9 <- as.numeric(jun18nonmalariasite9$X9)
jun25nonmalariasite9$X9 <- as.numeric(jun25nonmalariasite9$X9)
jul04nonmalariasite9$X9 <- as.numeric(jul04nonmalariasite9$X9)
jul10nonmalariasite9$X9 <- as.numeric(jul10nonmalariasite9$X9)
jul17nonmalariasite9$X9 <- as.numeric(jul17nonmalariasite9$X9)
jul24nonmalariasite9$X9 <- as.numeric(jul24nonmalariasite9$X9)
jul30nonmalariasite9$X9 <- as.numeric(jul30nonmalariasite9$X9)
aug06nonmalariasite9$X9 <- as.numeric(aug06nonmalariasite9$X9)
aug13nonmalariasite9$X9 <- as.numeric(aug13nonmalariasite9$X9)
aug20nonmalariasite9$X9 <- as.numeric(aug20nonmalariasite9$X9)
sep03nonmalariasite9$X9 <- as.numeric(sep03nonmalariasite9$X9)


ajun04nonmalariasite9 <- sum(jun04nonmalariasite9$X9, na.rm=T)
ajun12nonmalariasite9 <- sum(jun12nonmalariasite9$X9, na.rm=T)
ajun18nonmalariasite9 <- sum(jun18nonmalariasite9$X9, na.rm=T)
ajun25nonmalariasite9 <- sum(jun25nonmalariasite9$X9, na.rm=T)
ajul04nonmalariasite9 <- sum(jul04nonmalariasite9$X9, na.rm=T)
ajul10nonmalariasite9 <- sum(jul10nonmalariasite9$X9, na.rm=T)
ajul17nonmalariasite9 <- sum(jul17nonmalariasite9$X9, na.rm=T)
ajul24nonmalariasite9 <- sum(jul24nonmalariasite9$X9, na.rm=T)
ajul30nonmalariasite9 <- sum(jul30nonmalariasite9$X9, na.rm=T)
aaug06nonmalariasite9 <- sum(aug06nonmalariasite9$X9, na.rm=T)
aaug13nonmalariasite9 <- sum(aug13nonmalariasite9$X9, na.rm=T)
aaug20nonmalariasite9 <- sum(aug20nonmalariasite9$X9, na.rm=T)
asep03nonmalariasite9 <- sum(sep03nonmalariasite9$X9, na.rm=T)

#####################################
##### site10
#malaria
jun04malariasite10$X10 <- as.numeric(jun04malariasite10$X10)
jun12malariasite10$X10 <- as.numeric(jun12malariasite10$X10)
jun18malariasite10$X10 <- as.numeric(jun18malariasite10$X10)
jun25malariasite10$X10 <- as.numeric(jun25malariasite10$X10)
jul04malariasite10$X10 <- as.numeric(jul04malariasite10$X10)
jul10malariasite10$X10 <- as.numeric(jul10malariasite10$X10)
jul17malariasite10$X10 <- as.numeric(jul17malariasite10$X10)
jul24malariasite10$X10 <- as.numeric(jul24malariasite10$X10)
jul30malariasite10$X10 <- as.numeric(jul30malariasite10$X10)
aug06malariasite10$X10 <- as.numeric(aug06malariasite10$X10)
aug13malariasite10$X10 <- as.numeric(aug13malariasite10$X10)
aug20malariasite10$X10 <- as.numeric(aug20malariasite10$X10)
sep03malariasite10$X10 <- as.numeric(sep03malariasite10$X10)


ajun04malariasite10 <- sum(jun04malariasite10$X10, na.rm=T)
ajun12malariasite10 <- sum(jun12malariasite10$X10, na.rm=T)
ajun18malariasite10 <- sum(jun18malariasite10$X10, na.rm=T)
ajun25malariasite10 <- sum(jun25malariasite10$X10, na.rm=T)
ajul04malariasite10 <- sum(jul04malariasite10$X10, na.rm=T)
ajul10malariasite10 <- sum(jul10malariasite10$X10, na.rm=T)
ajul17malariasite10 <- sum(jul17malariasite10$X10, na.rm=T)
ajul24malariasite10 <- sum(jul24malariasite10$X10, na.rm=T)
ajul30malariasite10 <- sum(jul30malariasite10$X10, na.rm=T)
aaug06malariasite10 <- sum(aug06malariasite10$X10, na.rm=T)
aaug13malariasite10 <- sum(aug13malariasite10$X10, na.rm=T)
aaug20malariasite10 <- sum(aug20malariasite10$X10, na.rm=T)
asep03malariasite10 <- sum(sep03malariasite10$X10, na.rm=T)


#NONmalaria
jun04nonmalariasite10$X10 <- as.numeric(jun04nonmalariasite10$X10)
jun12nonmalariasite10$X10 <- as.numeric(jun12nonmalariasite10$X10)
jun18nonmalariasite10$X10 <- as.numeric(jun18nonmalariasite10$X10)
jun25nonmalariasite10$X10 <- as.numeric(jun25nonmalariasite10$X10)
jul04nonmalariasite10$X10 <- as.numeric(jul04nonmalariasite10$X10)
jul10nonmalariasite10$X10 <- as.numeric(jul10nonmalariasite10$X10)
jul17nonmalariasite10$X10 <- as.numeric(jul17nonmalariasite10$X10)
jul24nonmalariasite10$X10 <- as.numeric(jul24nonmalariasite10$X10)
jul30nonmalariasite10$X10 <- as.numeric(jul30nonmalariasite10$X10)
aug06nonmalariasite10$X10 <- as.numeric(aug06nonmalariasite10$X10)
aug13nonmalariasite10$X10 <- as.numeric(aug13nonmalariasite10$X10)
aug20nonmalariasite10$X10 <- as.numeric(aug20nonmalariasite10$X10)
sep03nonmalariasite10$X10 <- as.numeric(sep03nonmalariasite10$X10)


ajun04nonmalariasite10 <- sum(jun04nonmalariasite10$X10, na.rm=T)
ajun12nonmalariasite10 <- sum(jun12nonmalariasite10$X10, na.rm=T)
ajun18nonmalariasite10 <- sum(jun18nonmalariasite10$X10, na.rm=T)
ajun25nonmalariasite10 <- sum(jun25nonmalariasite10$X10, na.rm=T)
ajul04nonmalariasite10 <- sum(jul04nonmalariasite10$X10, na.rm=T)
ajul10nonmalariasite10 <- sum(jul10nonmalariasite10$X10, na.rm=T)
ajul17nonmalariasite10 <- sum(jul17nonmalariasite10$X10, na.rm=T)
ajul24nonmalariasite10 <- sum(jul24nonmalariasite10$X10, na.rm=T)
ajul30nonmalariasite10 <- sum(jul30nonmalariasite10$X10, na.rm=T)
aaug06nonmalariasite10 <- sum(aug06nonmalariasite10$X10, na.rm=T)
aaug13nonmalariasite10 <- sum(aug13nonmalariasite10$X10, na.rm=T)
aaug20nonmalariasite10 <- sum(aug20nonmalariasite10$X10, na.rm=T)
asep03nonmalariasite10 <- sum(sep03nonmalariasite10$X10, na.rm=T)



#####################
#
#
#
#
#
################################


#CREATE malariaS FOR PLOTTING
#Site 1
malaria1 <- c(ajun04malariasite1,
          ajun12malariasite1,
          ajun18malariasite1,
          ajun25malariasite1,
          ajul04malariasite1,
          ajul10malariasite1,
          ajul17malariasite1,
          ajul24malariasite1,
          ajul30malariasite1,
          aaug06malariasite1,
          aaug13malariasite1,
          aaug20malariasite1,
          asep03malariasite1)

#Site 2
malaria2 <- c(ajun04malariasite2,
          ajun12malariasite2,
          ajun18malariasite2,
          ajun25malariasite2,
          ajul04malariasite2,
          ajul10malariasite2,
          ajul17malariasite2,
          ajul24malariasite2,
          ajul30malariasite2,
          aaug06malariasite2,
          aaug13malariasite2,
          aaug20malariasite2,
          asep03malariasite2)


#Site 3
malaria3 <- c(ajun04malariasite3,
          ajun12malariasite3,
          ajun18malariasite3,
          ajun25malariasite3,
          ajul04malariasite3,
          ajul10malariasite3,
          ajul17malariasite3,
          ajul24malariasite3,
          ajul30malariasite3,
          aaug06malariasite3,
          aaug13malariasite3,
          aaug20malariasite3,
          asep03malariasite3)


#Site 4
malaria4 <- c(ajun04malariasite4,
          ajun12malariasite4,
          ajun18malariasite4,
          ajun25malariasite4,
          ajul04malariasite4,
          ajul10malariasite4,
          ajul17malariasite4,
          ajul24malariasite4,
          ajul30malariasite4,
          aaug06malariasite4,
          aaug13malariasite4,
          aaug20malariasite4,
          asep03malariasite4)


#Site 5
malaria5 <- c(ajun04malariasite5,
          ajun12malariasite5,
          ajun18malariasite5,
          ajun25malariasite5,
          ajul04malariasite5,
          ajul10malariasite5,
          ajul17malariasite5,
          ajul24malariasite5,
          ajul30malariasite5,
          aaug06malariasite5,
          aaug13malariasite5,
          aaug20malariasite5,
          asep03malariasite5)


#Site 6
malaria6 <- c(ajun04malariasite6,
          ajun12malariasite6,
          ajun18malariasite6,
          ajun25malariasite6,
          ajul04malariasite6,
          ajul10malariasite6,
          ajul17malariasite6,
          ajul24malariasite6,
          ajul30malariasite6,
          aaug06malariasite6,
          aaug13malariasite6,
          aaug20malariasite6,
          asep03malariasite6)


#Site 7
malaria7 <- c(ajun04malariasite7,
          ajun12malariasite7,
          ajun18malariasite7,
          ajun25malariasite7,
          ajul04malariasite7,
          ajul10malariasite7,
          ajul17malariasite7,
          ajul24malariasite7,
          ajul30malariasite7,
          aaug06malariasite7,
          aaug13malariasite7,
          aaug20malariasite7,
          asep03malariasite7)


#Site 8
malaria8 <- c(ajun04malariasite8,
          ajun12malariasite8,
          ajun18malariasite8,
          ajun25malariasite8,
          ajul04malariasite8,
          ajul10malariasite8,
          ajul17malariasite8,
          ajul24malariasite8,
          ajul30malariasite8,
          aaug06malariasite8,
          aaug13malariasite8,
          aaug20malariasite8,
          asep03malariasite8)


#Site 9
malaria9 <- c(ajun04malariasite9,
          ajun12malariasite9,
          ajun18malariasite9,
          ajun25malariasite9,
          ajul04malariasite9,
          ajul10malariasite9,
          ajul17malariasite9,
          ajul24malariasite9,
          ajul30malariasite9,
          aaug06malariasite9,
          aaug13malariasite9,
          aaug20malariasite9,
          asep03malariasite9)


#Site 10
malaria10 <- c(ajun04malariasite10,
           ajun12malariasite10,
           ajun18malariasite10,
           ajun25malariasite10,
           ajul04malariasite10,
           ajul10malariasite10,
           ajul17malariasite10,
           ajul24malariasite10,
           ajul30malariasite10,
           aaug06malariasite10,
           aaug13malariasite10,
           aaug20malariasite10,
           asep03malariasite10)



malariatotal <- malaria1+malaria2+malaria3+malaria4+malaria5+malaria6+malaria7+malaria8+malaria9+malaria10


##### dengue 6 - DENGUE
#SUBSET BY DATE and dengue
jun04dengue <- subset(mergeddengue, date2  == as.Date("2013-06-04"))
jun12dengue <- subset(mergeddengue, date2  == as.Date("2013-06-12"))
jun18dengue <- subset(mergeddengue, date2  == as.Date("2013-06-18"))
jun25dengue <- subset(mergeddengue, date2  == as.Date("2013-06-25"))
jul04dengue <- subset(mergeddengue, date2  == as.Date("2013-07-04"))
jul10dengue <- subset(mergeddengue, date2  == as.Date("2013-07-10"))
jul17dengue <- subset(mergeddengue, date2  == as.Date("2013-07-17"))
jul24dengue <- subset(mergeddengue, date2  == as.Date("2013-07-24"))
jul30dengue <- subset(mergeddengue, date2  == as.Date("2013-07-30"))
aug06dengue <- subset(mergeddengue, date2  == as.Date("2013-08-06"))
aug13dengue <- subset(mergeddengue, date2  == as.Date("2013-08-13"))
aug20dengue <- subset(mergeddengue, date2  == as.Date("2013-08-20"))
sep03dengue <- subset(mergeddengue, date2  == as.Date("2013-09-03"))


####CREATE SUBSETS for each day by site and type IN THE FORM OF
#DATE dengue SITE

#JUN 04 dengue
jun04denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-06-04"))
jun04denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-06-04"))
jun04denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-06-04"))
jun04denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-06-04"))
jun04denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-06-04"))
jun04denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-06-04"))
jun04denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-06-04"))
jun04denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-06-04"))
jun04denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-06-04"))
jun04denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-06-04"))


#jun12 dengue
jun12denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-06-12"))
jun12denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-06-12"))
jun12denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-06-12"))
jun12denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-06-12"))
jun12denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-06-12"))
jun12denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-06-12"))
jun12denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-06-12"))
jun12denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-06-12"))
jun12denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-06-12"))
jun12denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-06-12"))

#jun18 dengue
jun18denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-06-18"))
jun18denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-06-18"))
jun18denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-06-18"))
jun18denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-06-18"))
jun18denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-06-18"))
jun18denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-06-18"))
jun18denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-06-18"))
jun18denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-06-18"))
jun18denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-06-18"))
jun18denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-06-18"))

#jun25 dengue
jun25denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-06-25"))
jun25denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-06-25"))
jun25denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-06-25"))
jun25denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-06-25"))
jun25denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-06-25"))
jun25denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-06-25"))
jun25denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-06-25"))
jun25denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-06-25"))
jun25denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-06-25"))
jun25denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-06-25"))

#jul04 dengue
jul04denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-07-04"))
jul04denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-07-04"))
jul04denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-07-04"))
jul04denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-07-04"))
jul04denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-07-04"))
jul04denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-07-04"))
jul04denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-07-04"))
jul04denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-07-04"))
jul04denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-07-04"))
jul04denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-07-04"))

#jul10 dengue
jul10denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-07-10"))
jul10denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-07-10"))
jul10denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-07-10"))
jul10denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-07-10"))
jul10denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-07-10"))
jul10denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-07-10"))
jul10denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-07-10"))
jul10denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-07-10"))
jul10denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-07-10"))
jul10denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-07-10"))

#jul17 dengue
jul17denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-07-17"))
jul17denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-07-17"))
jul17denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-07-17"))
jul17denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-07-17"))
jul17denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-07-17"))
jul17denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-07-17"))
jul17denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-07-17"))
jul17denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-07-17"))
jul17denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-07-17"))
jul17denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-07-17"))

#jul24 dengue
jul24denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-07-24"))
jul24denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-07-24"))
jul24denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-07-24"))
jul24denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-07-24"))
jul24denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-07-24"))
jul24denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-07-24"))
jul24denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-07-24"))
jul24denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-07-24"))
jul24denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-07-24"))
jul24denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-07-24"))

#jul30 dengue
jul30denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-07-30"))
jul30denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-07-30"))
jul30denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-07-30"))
jul30denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-07-30"))
jul30denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-07-30"))
jul30denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-07-30"))
jul30denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-07-30"))
jul30denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-07-30"))
jul30denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-07-30"))
jul30denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-07-30"))

#aug06 dengue
aug06denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-08-06"))
aug06denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-08-06"))
aug06denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-08-06"))
aug06denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-08-06"))
aug06denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-08-06"))
aug06denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-08-06"))
aug06denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-08-06"))
aug06denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-08-06"))
aug06denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-08-06"))
aug06denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-08-06"))

#aug13 dengue
aug13denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-08-13"))
aug13denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-08-13"))
aug13denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-08-13"))
aug13denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-08-13"))
aug13denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-08-13"))
aug13denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-08-13"))
aug13denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-08-13"))
aug13denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-08-13"))
aug13denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-08-13"))
aug13denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-08-13"))

#aug20 dengue
aug20denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-08-20"))
aug20denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-08-20"))
aug20denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-08-20"))
aug20denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-08-20"))
aug20denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-08-20"))
aug20denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-08-20"))
aug20denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-08-20"))
aug20denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-08-20"))
aug20denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-08-20"))
aug20denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-08-20"))

#sep03 dengue
sep03denguesite1 <-subset(merged1dengue, date2  == as.Date("2013-09-03"))
sep03denguesite2 <-subset(merged2dengue, date2  == as.Date("2013-09-03"))
sep03denguesite3 <-subset(merged3dengue, date2  == as.Date("2013-09-03"))
sep03denguesite4 <-subset(merged4dengue, date2  == as.Date("2013-09-03"))
sep03denguesite5 <-subset(merged5dengue, date2  == as.Date("2013-09-03"))
sep03denguesite6 <-subset(merged6dengue, date2  == as.Date("2013-09-03"))
sep03denguesite7 <-subset(merged7dengue, date2  == as.Date("2013-09-03"))
sep03denguesite8 <-subset(merged8dengue, date2  == as.Date("2013-09-03"))
sep03denguesite9 <-subset(merged9dengue, date2  == as.Date("2013-09-03"))
sep03denguesite10 <-subset(merged10dengue, date2  == as.Date("2013-09-03"))

##### ALL SITES COMBINED
#dengue
ajun04denguetotal <- sum(jun04dengue$total, na.rm=T)
ajun12denguetotal <- sum(jun12dengue$total, na.rm=T)
ajun18denguetotal <- sum(jun18dengue$total, na.rm=T)
ajun25denguetotal <- sum(jun25dengue$total, na.rm=T)
ajul04denguetotal <- sum(jul04dengue$total, na.rm=T)
ajul10denguetotal <- sum(jul10dengue$total, na.rm=T)
ajul17denguetotal <- sum(jul17dengue$total, na.rm=T)
ajul24denguetotal <- sum(jul24dengue$total, na.rm=T)
ajul30denguetotal <- sum(jul30dengue$total, na.rm=T)
aaug06denguetotal <- sum(aug06dengue$total, na.rm=T)
aaug13denguetotal <- sum(aug13dengue$total, na.rm=T)
aaug20denguetotal <- sum(aug20dengue$total, na.rm=T)
asep03denguetotal <- sum(sep03dengue$total, na.rm=T)



#####################################
##### site1
#dengue
ajun04denguesite1 <- sum(jun04denguesite1$X1, na.rm=T)
ajun12denguesite1 <- sum(jun12denguesite1$X1, na.rm=T)
ajun18denguesite1 <- sum(jun18denguesite1$X1, na.rm=T)
ajun25denguesite1 <- sum(jun25denguesite1$X1, na.rm=T)
ajul04denguesite1 <- sum(jul04denguesite1$X1, na.rm=T)
ajul10denguesite1 <- sum(jul10denguesite1$X1, na.rm=T)
ajul17denguesite1 <- sum(jul17denguesite1$X1, na.rm=T)
ajul24denguesite1 <- sum(jul24denguesite1$X1, na.rm=T)
ajul30denguesite1 <- sum(jul30denguesite1$X1, na.rm=T)
aaug06denguesite1 <- sum(aug06denguesite1$X1, na.rm=T)
aaug13denguesite1 <- sum(aug13denguesite1$X1, na.rm=T)
aaug20denguesite1 <- sum(aug20denguesite1$X1, na.rm=T)
asep03denguesite1 <- sum(sep03denguesite1$X1, na.rm=T)


#NONdengue
ajun04nondenguesite1 <- sum(jun04nondenguesite1$X1, na.rm=T)
ajun12nondenguesite1 <- sum(jun12nondenguesite1$X1, na.rm=T)
ajun18nondenguesite1 <- sum(jun18nondenguesite1$X1, na.rm=T)
ajun25nondenguesite1 <- sum(jun25nondenguesite1$X1, na.rm=T)
ajul04nondenguesite1 <- sum(jul04nondenguesite1$X1, na.rm=T)
ajul10nondenguesite1 <- sum(jul10nondenguesite1$X1, na.rm=T)
ajul17nondenguesite1 <- sum(jul17nondenguesite1$X1, na.rm=T)
ajul24nondenguesite1 <- sum(jul24nondenguesite1$X1, na.rm=T)
ajul30nondenguesite1 <- sum(jul30nondenguesite1$X1, na.rm=T)
aaug06nondenguesite1 <- sum(aug06nondenguesite1$X1, na.rm=T)
aaug13nondenguesite1 <- sum(aug13nondenguesite1$X1, na.rm=T)
aaug20nondenguesite1 <- sum(aug20nondenguesite1$X1, na.rm=T)
asep03nondenguesite1 <- sum(sep03nondenguesite1$X1, na.rm=T)

#####################################
##### site2
#dengue
ajun04denguesite2 <- sum(jun04denguesite2$X2, na.rm=T)
ajun12denguesite2 <- sum(jun12denguesite2$X2, na.rm=T)
ajun18denguesite2 <- sum(jun18denguesite2$X2, na.rm=T)
ajun25denguesite2 <- sum(jun25denguesite2$X2, na.rm=T)
ajul04denguesite2 <- sum(jul04denguesite2$X2, na.rm=T)
ajul10denguesite2 <- sum(jul10denguesite2$X2, na.rm=T)
ajul17denguesite2 <- sum(jul17denguesite2$X2, na.rm=T)
ajul24denguesite2 <- sum(jul24denguesite2$X2, na.rm=T)
ajul30denguesite2 <- sum(jul30denguesite2$X2, na.rm=T)
aaug06denguesite2 <- sum(aug06denguesite2$X2, na.rm=T)
aaug13denguesite2 <- sum(aug13denguesite2$X2, na.rm=T)
aaug20denguesite2 <- sum(aug20denguesite2$X2, na.rm=T)
asep03denguesite2 <- sum(sep03denguesite2$X2, na.rm=T)


#NONdengue
ajun04nondenguesite2 <- sum(jun04nondenguesite2$X2, na.rm=T)
ajun12nondenguesite2 <- sum(jun12nondenguesite2$X2, na.rm=T)
ajun18nondenguesite2 <- sum(jun18nondenguesite2$X2, na.rm=T)
ajun25nondenguesite2 <- sum(jun25nondenguesite2$X2, na.rm=T)
ajul04nondenguesite2 <- sum(jul04nondenguesite2$X2, na.rm=T)
ajul10nondenguesite2 <- sum(jul10nondenguesite2$X2, na.rm=T)
ajul17nondenguesite2 <- sum(jul17nondenguesite2$X2, na.rm=T)
ajul24nondenguesite2 <- sum(jul24nondenguesite2$X2, na.rm=T)
ajul30nondenguesite2 <- sum(jul30nondenguesite2$X2, na.rm=T)
aaug06nondenguesite2 <- sum(aug06nondenguesite2$X2, na.rm=T)
aaug13nondenguesite2 <- sum(aug13nondenguesite2$X2, na.rm=T)
aaug20nondenguesite2 <- sum(aug20nondenguesite2$X2, na.rm=T)
asep03nondenguesite2 <- sum(sep03nondenguesite2$X2, na.rm=T)

#####################################
##### site3
#dengue
ajun04denguesite3 <- sum(jun04denguesite3$X3, na.rm=T)
ajun12denguesite3 <- sum(jun12denguesite3$X3, na.rm=T)
ajun18denguesite3 <- sum(jun18denguesite3$X3, na.rm=T)
ajun25denguesite3 <- sum(jun25denguesite3$X3, na.rm=T)
ajul04denguesite3 <- sum(jul04denguesite3$X3, na.rm=T)
ajul10denguesite3 <- sum(jul10denguesite3$X3, na.rm=T)
ajul17denguesite3 <- sum(jul17denguesite3$X3, na.rm=T)
ajul24denguesite3 <- sum(jul24denguesite3$X3, na.rm=T)
ajul30denguesite3 <- sum(jul30denguesite3$X3, na.rm=T)
aaug06denguesite3 <- sum(aug06denguesite3$X3, na.rm=T)
aaug13denguesite3 <- sum(aug13denguesite3$X3, na.rm=T)
aaug20denguesite3 <- sum(aug20denguesite3$X3, na.rm=T)
asep03denguesite3 <- sum(sep03denguesite3$X3, na.rm=T)


#NONdengue
ajun04nondenguesite3 <- sum(jun04nondenguesite3$X3, na.rm=T)
ajun12nondenguesite3 <- sum(jun12nondenguesite3$X3, na.rm=T)
ajun18nondenguesite3 <- sum(jun18nondenguesite3$X3, na.rm=T)
ajun25nondenguesite3 <- sum(jun25nondenguesite3$X3, na.rm=T)
ajul04nondenguesite3 <- sum(jul04nondenguesite3$X3, na.rm=T)
ajul10nondenguesite3 <- sum(jul10nondenguesite3$X3, na.rm=T)
ajul17nondenguesite3 <- sum(jul17nondenguesite3$X3, na.rm=T)
ajul24nondenguesite3 <- sum(jul24nondenguesite3$X3, na.rm=T)
ajul30nondenguesite3 <- sum(jul30nondenguesite3$X3, na.rm=T)
aaug06nondenguesite3 <- sum(aug06nondenguesite3$X3, na.rm=T)
aaug13nondenguesite3 <- sum(aug13nondenguesite3$X3, na.rm=T)
aaug20nondenguesite3 <- sum(aug20nondenguesite3$X3, na.rm=T)
asep03nondenguesite3 <- sum(sep03nondenguesite3$X3, na.rm=T)


#####################################
##### site4
#dengue
ajun04denguesite4 <- sum(jun04denguesite4$X4, na.rm=T)
ajun12denguesite4 <- sum(jun12denguesite4$X4, na.rm=T)
ajun18denguesite4 <- sum(jun18denguesite4$X4, na.rm=T)
ajun25denguesite4 <- sum(jun25denguesite4$X4, na.rm=T)
ajul04denguesite4 <- sum(jul04denguesite4$X4, na.rm=T)
ajul10denguesite4 <- sum(jul10denguesite4$X4, na.rm=T)
ajul17denguesite4 <- sum(jul17denguesite4$X4, na.rm=T)
ajul24denguesite4 <- sum(jul24denguesite4$X4, na.rm=T)
ajul30denguesite4 <- sum(jul30denguesite4$X4, na.rm=T)
aaug06denguesite4 <- sum(aug06denguesite4$X4, na.rm=T)
aaug13denguesite4 <- sum(aug13denguesite4$X4, na.rm=T)
aaug20denguesite4 <- sum(aug20denguesite4$X4, na.rm=T)
asep03denguesite4 <- sum(sep03denguesite4$X4, na.rm=T)


#NONdengue
ajun04nondenguesite4 <- sum(jun04nondenguesite4$X4, na.rm=T)
ajun12nondenguesite4 <- sum(jun12nondenguesite4$X4, na.rm=T)
ajun18nondenguesite4 <- sum(jun18nondenguesite4$X4, na.rm=T)
ajun25nondenguesite4 <- sum(jun25nondenguesite4$X4, na.rm=T)
ajul04nondenguesite4 <- sum(jul04nondenguesite4$X4, na.rm=T)
ajul10nondenguesite4 <- sum(jul10nondenguesite4$X4, na.rm=T)
ajul17nondenguesite4 <- sum(jul17nondenguesite4$X4, na.rm=T)
ajul24nondenguesite4 <- sum(jul24nondenguesite4$X4, na.rm=T)
ajul30nondenguesite4 <- sum(jul30nondenguesite4$X4, na.rm=T)
aaug06nondenguesite4 <- sum(aug06nondenguesite4$X4, na.rm=T)
aaug13nondenguesite4 <- sum(aug13nondenguesite4$X4, na.rm=T)
aaug20nondenguesite4 <- sum(aug20nondenguesite4$X4, na.rm=T)
asep03nondenguesite4 <- sum(sep03nondenguesite4$X4, na.rm=T)

#####################################
##### site5
#dengue
ajun04denguesite5 <- sum(jun04denguesite5$X5, na.rm=T)
ajun12denguesite5 <- sum(jun12denguesite5$X5, na.rm=T)
ajun18denguesite5 <- sum(jun18denguesite5$X5, na.rm=T)
ajun25denguesite5 <- sum(jun25denguesite5$X5, na.rm=T)
ajul04denguesite5 <- sum(jul04denguesite5$X5, na.rm=T)
ajul10denguesite5 <- sum(jul10denguesite5$X5, na.rm=T)
ajul17denguesite5 <- sum(jul17denguesite5$X5, na.rm=T)
ajul24denguesite5 <- sum(jul24denguesite5$X5, na.rm=T)
ajul30denguesite5 <- sum(jul30denguesite5$X5, na.rm=T)
aaug06denguesite5 <- sum(aug06denguesite5$X5, na.rm=T)
aaug13denguesite5 <- sum(aug13denguesite5$X5, na.rm=T)
aaug20denguesite5 <- sum(aug20denguesite5$X5, na.rm=T)
asep03denguesite5 <- sum(sep03denguesite5$X5, na.rm=T)


#NONdengue
ajun04nondenguesite5 <- sum(jun04nondenguesite5$X5, na.rm=T)
ajun12nondenguesite5 <- sum(jun12nondenguesite5$X5, na.rm=T)
ajun18nondenguesite5 <- sum(jun18nondenguesite5$X5, na.rm=T)
ajun25nondenguesite5 <- sum(jun25nondenguesite5$X5, na.rm=T)
ajul04nondenguesite5 <- sum(jul04nondenguesite5$X5, na.rm=T)
ajul10nondenguesite5 <- sum(jul10nondenguesite5$X5, na.rm=T)
ajul17nondenguesite5 <- sum(jul17nondenguesite5$X5, na.rm=T)
ajul24nondenguesite5 <- sum(jul24nondenguesite5$X5, na.rm=T)
ajul30nondenguesite5 <- sum(jul30nondenguesite5$X5, na.rm=T)
aaug06nondenguesite5 <- sum(aug06nondenguesite5$X5, na.rm=T)
aaug13nondenguesite5 <- sum(aug13nondenguesite5$X5, na.rm=T)
aaug20nondenguesite5 <- sum(aug20nondenguesite5$X5, na.rm=T)
asep03nondenguesite5 <- sum(sep03nondenguesite5$X5, na.rm=T)


#####################################
##### site6
#dengue
jun04denguesite6$X6 <- as.numeric(jun04denguesite6$X6)
jun12denguesite6$X6 <- as.numeric(jun12denguesite6$X6)
jun18denguesite6$X6 <- as.numeric(jun18denguesite6$X6)
jun25denguesite6$X6 <- as.numeric(jun25denguesite6$X6)
jul04denguesite6$X6 <- as.numeric(jul04denguesite6$X6)
jul10denguesite6$X6 <- as.numeric(jul10denguesite6$X6)
jul17denguesite6$X6 <- as.numeric(jul17denguesite6$X6)
jul24denguesite6$X6 <- as.numeric(jul24denguesite6$X6)
jul30denguesite6$X6 <- as.numeric(jul30denguesite6$X6)
aug06denguesite6$X6 <- as.numeric(aug06denguesite6$X6)
aug13denguesite6$X6 <- as.numeric(aug13denguesite6$X6)
aug20denguesite6$X6 <- as.numeric(aug20denguesite6$X6)
sep03denguesite6$X6 <- as.numeric(sep03denguesite6$X6)


ajun04denguesite6 <- sum(jun04denguesite6$X6, na.rm=T)
ajun12denguesite6 <- sum(jun12denguesite6$X6, na.rm=T)
ajun18denguesite6 <- sum(jun18denguesite6$X6, na.rm=T)
ajun25denguesite6 <- sum(jun25denguesite6$X6, na.rm=T)
ajul04denguesite6 <- sum(jul04denguesite6$X6, na.rm=T)
ajul10denguesite6 <- sum(jul10denguesite6$X6, na.rm=T)
ajul17denguesite6 <- sum(jul17denguesite6$X6, na.rm=T)
ajul24denguesite6 <- sum(jul24denguesite6$X6, na.rm=T)
ajul30denguesite6 <- sum(jul30denguesite6$X6, na.rm=T)
aaug06denguesite6 <- sum(aug06denguesite6$X6, na.rm=T)
aaug13denguesite6 <- sum(aug13denguesite6$X6, na.rm=T)
aaug20denguesite6 <- sum(aug20denguesite6$X6, na.rm=T)
asep03denguesite6 <- sum(sep03denguesite6$X6, na.rm=T)


#NONdengue
jun04nondenguesite6$X6 <- as.numeric(jun04nondenguesite6$X6)
jun12nondenguesite6$X6 <- as.numeric(jun12nondenguesite6$X6)
jun18nondenguesite6$X6 <- as.numeric(jun18nondenguesite6$X6)
jun25nondenguesite6$X6 <- as.numeric(jun25nondenguesite6$X6)
jul04nondenguesite6$X6 <- as.numeric(jul04nondenguesite6$X6)
jul10nondenguesite6$X6 <- as.numeric(jul10nondenguesite6$X6)
jul17nondenguesite6$X6 <- as.numeric(jul17nondenguesite6$X6)
jul24nondenguesite6$X6 <- as.numeric(jul24nondenguesite6$X6)
jul30nondenguesite6$X6 <- as.numeric(jul30nondenguesite6$X6)
aug06nondenguesite6$X6 <- as.numeric(aug06nondenguesite6$X6)
aug13nondenguesite6$X6 <- as.numeric(aug13nondenguesite6$X6)
aug20nondenguesite6$X6 <- as.numeric(aug20nondenguesite6$X6)
sep03nondenguesite6$X6 <- as.numeric(sep03nondenguesite6$X6)


ajun04nondenguesite6 <- sum(jun04nondenguesite6$X6, na.rm=T)
ajun12nondenguesite6 <- sum(jun12nondenguesite6$X6, na.rm=T)
ajun18nondenguesite6 <- sum(jun18nondenguesite6$X6, na.rm=T)
ajun25nondenguesite6 <- sum(jun25nondenguesite6$X6, na.rm=T)
ajul04nondenguesite6 <- sum(jul04nondenguesite6$X6, na.rm=T)
ajul10nondenguesite6 <- sum(jul10nondenguesite6$X6, na.rm=T)
ajul17nondenguesite6 <- sum(jul17nondenguesite6$X6, na.rm=T)
ajul24nondenguesite6 <- sum(jul24nondenguesite6$X6, na.rm=T)
ajul30nondenguesite6 <- sum(jul30nondenguesite6$X6, na.rm=T)
aaug06nondenguesite6 <- sum(aug06nondenguesite6$X6, na.rm=T)
aaug13nondenguesite6 <- sum(aug13nondenguesite6$X6, na.rm=T)
aaug20nondenguesite6 <- sum(aug20nondenguesite6$X6, na.rm=T)
asep03nondenguesite6 <- sum(sep03nondenguesite6$X6, na.rm=T)


#####################################
##### site7
#dengue
ajun04denguesite7 <- sum(jun04denguesite7$X7, na.rm=T)
ajun12denguesite7 <- sum(jun12denguesite7$X7, na.rm=T)
ajun18denguesite7 <- sum(jun18denguesite7$X7, na.rm=T)
ajun25denguesite7 <- sum(jun25denguesite7$X7, na.rm=T)
ajul04denguesite7 <- sum(jul04denguesite7$X7, na.rm=T)
ajul10denguesite7 <- sum(jul10denguesite7$X7, na.rm=T)
ajul17denguesite7 <- sum(jul17denguesite7$X7, na.rm=T)
ajul24denguesite7 <- sum(jul24denguesite7$X7, na.rm=T)
ajul30denguesite7 <- sum(jul30denguesite7$X7, na.rm=T)
aaug06denguesite7 <- sum(aug06denguesite7$X7, na.rm=T)
aaug13denguesite7 <- sum(aug13denguesite7$X7, na.rm=T)
aaug20denguesite7 <- sum(aug20denguesite7$X7, na.rm=T)
asep03denguesite7 <- sum(sep03denguesite7$X7, na.rm=T)


#NONdengue
ajun04nondenguesite7 <- sum(jun04nondenguesite7$X7, na.rm=T)
ajun12nondenguesite7 <- sum(jun12nondenguesite7$X7, na.rm=T)
ajun18nondenguesite7 <- sum(jun18nondenguesite7$X7, na.rm=T)
ajun25nondenguesite7 <- sum(jun25nondenguesite7$X7, na.rm=T)
ajul04nondenguesite7 <- sum(jul04nondenguesite7$X7, na.rm=T)
ajul10nondenguesite7 <- sum(jul10nondenguesite7$X7, na.rm=T)
ajul17nondenguesite7 <- sum(jul17nondenguesite7$X7, na.rm=T)
ajul24nondenguesite7 <- sum(jul24nondenguesite7$X7, na.rm=T)
ajul30nondenguesite7 <- sum(jul30nondenguesite7$X7, na.rm=T)
aaug06nondenguesite7 <- sum(aug06nondenguesite7$X7, na.rm=T)
aaug13nondenguesite7 <- sum(aug13nondenguesite7$X7, na.rm=T)
aaug20nondenguesite7 <- sum(aug20nondenguesite7$X7, na.rm=T)
asep03nondenguesite7 <- sum(sep03nondenguesite7$X7, na.rm=T)


#####################################
##### site8
#dengue
ajun04denguesite8 <- sum(jun04denguesite8$X8, na.rm=T)
ajun12denguesite8 <- sum(jun12denguesite8$X8, na.rm=T)
ajun18denguesite8 <- sum(jun18denguesite8$X8, na.rm=T)
ajun25denguesite8 <- sum(jun25denguesite8$X8, na.rm=T)
ajul04denguesite8 <- sum(jul04denguesite8$X8, na.rm=T)
ajul10denguesite8 <- sum(jul10denguesite8$X8, na.rm=T)
ajul17denguesite8 <- sum(jul17denguesite8$X8, na.rm=T)
ajul24denguesite8 <- sum(jul24denguesite8$X8, na.rm=T)
ajul30denguesite8 <- sum(jul30denguesite8$X8, na.rm=T)
aaug06denguesite8 <- sum(aug06denguesite8$X8, na.rm=T)
aaug13denguesite8 <- sum(aug13denguesite8$X8, na.rm=T)
aaug20denguesite8 <- sum(aug20denguesite8$X8, na.rm=T)
asep03denguesite8 <- sum(sep03denguesite8$X8, na.rm=T)


#NONdengue
ajun04nondenguesite8 <- sum(jun04nondenguesite8$X8, na.rm=T)
ajun12nondenguesite8 <- sum(jun12nondenguesite8$X8, na.rm=T)
ajun18nondenguesite8 <- sum(jun18nondenguesite8$X8, na.rm=T)
ajun25nondenguesite8 <- sum(jun25nondenguesite8$X8, na.rm=T)
ajul04nondenguesite8 <- sum(jul04nondenguesite8$X8, na.rm=T)
ajul10nondenguesite8 <- sum(jul10nondenguesite8$X8, na.rm=T)
ajul17nondenguesite8 <- sum(jul17nondenguesite8$X8, na.rm=T)
ajul24nondenguesite8 <- sum(jul24nondenguesite8$X8, na.rm=T)
ajul30nondenguesite8 <- sum(jul30nondenguesite8$X8, na.rm=T)
aaug06nondenguesite8 <- sum(aug06nondenguesite8$X8, na.rm=T)
aaug13nondenguesite8 <- sum(aug13nondenguesite8$X8, na.rm=T)
aaug20nondenguesite8 <- sum(aug20nondenguesite8$X8, na.rm=T)
asep03nondenguesite8 <- sum(sep03nondenguesite8$X8, na.rm=T)


#####################################
##### site9
#dengue

jun04denguesite9$X9 <- as.numeric(jun04denguesite9$X9)
jun12denguesite9$X9 <- as.numeric(jun12denguesite9$X9)
jun18denguesite9$X9 <- as.numeric(jun18denguesite9$X9)
jun25denguesite9$X9 <- as.numeric(jun25denguesite9$X9)
jul04denguesite9$X9 <- as.numeric(jul04denguesite9$X9)
jul10denguesite9$X9 <- as.numeric(jul10denguesite9$X9)
jul17denguesite9$X9 <- as.numeric(jul17denguesite9$X9)
jul24denguesite9$X9 <- as.numeric(jul24denguesite9$X9)
jul30denguesite9$X9 <- as.numeric(jul30denguesite9$X9)
aug06denguesite9$X9 <- as.numeric(aug06denguesite9$X9)
aug13denguesite9$X9 <- as.numeric(aug13denguesite9$X9)
aug20denguesite9$X9 <- as.numeric(aug20denguesite9$X9)
sep03denguesite9$X9 <- as.numeric(sep03denguesite9$X9)


ajun04denguesite9 <- sum(jun04denguesite9$X9, na.rm=T)
ajun12denguesite9 <- sum(jun12denguesite9$X9, na.rm=T)
ajun18denguesite9 <- sum(jun18denguesite9$X9, na.rm=T)
ajun25denguesite9 <- sum(jun25denguesite9$X9, na.rm=T)
ajul04denguesite9 <- sum(jul04denguesite9$X9, na.rm=T)
ajul10denguesite9 <- sum(jul10denguesite9$X9, na.rm=T)
ajul17denguesite9 <- sum(jul17denguesite9$X9, na.rm=T)
ajul24denguesite9 <- sum(jul24denguesite9$X9, na.rm=T)
ajul30denguesite9 <- sum(jul30denguesite9$X9, na.rm=T)
aaug06denguesite9 <- sum(aug06denguesite9$X9, na.rm=T)
aaug13denguesite9 <- sum(aug13denguesite9$X9, na.rm=T)
aaug20denguesite9 <- sum(aug20denguesite9$X9, na.rm=T)
asep03denguesite9 <- sum(sep03denguesite9$X9, na.rm=T)


#NONdengue
jun04nondenguesite9$X9 <- as.numeric(jun04nondenguesite9$X9)
jun12nondenguesite9$X9 <- as.numeric(jun12nondenguesite9$X9)
jun18nondenguesite9$X9 <- as.numeric(jun18nondenguesite9$X9)
jun25nondenguesite9$X9 <- as.numeric(jun25nondenguesite9$X9)
jul04nondenguesite9$X9 <- as.numeric(jul04nondenguesite9$X9)
jul10nondenguesite9$X9 <- as.numeric(jul10nondenguesite9$X9)
jul17nondenguesite9$X9 <- as.numeric(jul17nondenguesite9$X9)
jul24nondenguesite9$X9 <- as.numeric(jul24nondenguesite9$X9)
jul30nondenguesite9$X9 <- as.numeric(jul30nondenguesite9$X9)
aug06nondenguesite9$X9 <- as.numeric(aug06nondenguesite9$X9)
aug13nondenguesite9$X9 <- as.numeric(aug13nondenguesite9$X9)
aug20nondenguesite9$X9 <- as.numeric(aug20nondenguesite9$X9)
sep03nondenguesite9$X9 <- as.numeric(sep03nondenguesite9$X9)


ajun04nondenguesite9 <- sum(jun04nondenguesite9$X9, na.rm=T)
ajun12nondenguesite9 <- sum(jun12nondenguesite9$X9, na.rm=T)
ajun18nondenguesite9 <- sum(jun18nondenguesite9$X9, na.rm=T)
ajun25nondenguesite9 <- sum(jun25nondenguesite9$X9, na.rm=T)
ajul04nondenguesite9 <- sum(jul04nondenguesite9$X9, na.rm=T)
ajul10nondenguesite9 <- sum(jul10nondenguesite9$X9, na.rm=T)
ajul17nondenguesite9 <- sum(jul17nondenguesite9$X9, na.rm=T)
ajul24nondenguesite9 <- sum(jul24nondenguesite9$X9, na.rm=T)
ajul30nondenguesite9 <- sum(jul30nondenguesite9$X9, na.rm=T)
aaug06nondenguesite9 <- sum(aug06nondenguesite9$X9, na.rm=T)
aaug13nondenguesite9 <- sum(aug13nondenguesite9$X9, na.rm=T)
aaug20nondenguesite9 <- sum(aug20nondenguesite9$X9, na.rm=T)
asep03nondenguesite9 <- sum(sep03nondenguesite9$X9, na.rm=T)

#####################################
##### site10
#dengue
jun04denguesite10$X10 <- as.numeric(jun04denguesite10$X10)
jun12denguesite10$X10 <- as.numeric(jun12denguesite10$X10)
jun18denguesite10$X10 <- as.numeric(jun18denguesite10$X10)
jun25denguesite10$X10 <- as.numeric(jun25denguesite10$X10)
jul04denguesite10$X10 <- as.numeric(jul04denguesite10$X10)
jul10denguesite10$X10 <- as.numeric(jul10denguesite10$X10)
jul17denguesite10$X10 <- as.numeric(jul17denguesite10$X10)
jul24denguesite10$X10 <- as.numeric(jul24denguesite10$X10)
jul30denguesite10$X10 <- as.numeric(jul30denguesite10$X10)
aug06denguesite10$X10 <- as.numeric(aug06denguesite10$X10)
aug13denguesite10$X10 <- as.numeric(aug13denguesite10$X10)
aug20denguesite10$X10 <- as.numeric(aug20denguesite10$X10)
sep03denguesite10$X10 <- as.numeric(sep03denguesite10$X10)


ajun04denguesite10 <- sum(jun04denguesite10$X10, na.rm=T)
ajun12denguesite10 <- sum(jun12denguesite10$X10, na.rm=T)
ajun18denguesite10 <- sum(jun18denguesite10$X10, na.rm=T)
ajun25denguesite10 <- sum(jun25denguesite10$X10, na.rm=T)
ajul04denguesite10 <- sum(jul04denguesite10$X10, na.rm=T)
ajul10denguesite10 <- sum(jul10denguesite10$X10, na.rm=T)
ajul17denguesite10 <- sum(jul17denguesite10$X10, na.rm=T)
ajul24denguesite10 <- sum(jul24denguesite10$X10, na.rm=T)
ajul30denguesite10 <- sum(jul30denguesite10$X10, na.rm=T)
aaug06denguesite10 <- sum(aug06denguesite10$X10, na.rm=T)
aaug13denguesite10 <- sum(aug13denguesite10$X10, na.rm=T)
aaug20denguesite10 <- sum(aug20denguesite10$X10, na.rm=T)
asep03denguesite10 <- sum(sep03denguesite10$X10, na.rm=T)


#NONdengue
jun04nondenguesite10$X10 <- as.numeric(jun04nondenguesite10$X10)
jun12nondenguesite10$X10 <- as.numeric(jun12nondenguesite10$X10)
jun18nondenguesite10$X10 <- as.numeric(jun18nondenguesite10$X10)
jun25nondenguesite10$X10 <- as.numeric(jun25nondenguesite10$X10)
jul04nondenguesite10$X10 <- as.numeric(jul04nondenguesite10$X10)
jul10nondenguesite10$X10 <- as.numeric(jul10nondenguesite10$X10)
jul17nondenguesite10$X10 <- as.numeric(jul17nondenguesite10$X10)
jul24nondenguesite10$X10 <- as.numeric(jul24nondenguesite10$X10)
jul30nondenguesite10$X10 <- as.numeric(jul30nondenguesite10$X10)
aug06nondenguesite10$X10 <- as.numeric(aug06nondenguesite10$X10)
aug13nondenguesite10$X10 <- as.numeric(aug13nondenguesite10$X10)
aug20nondenguesite10$X10 <- as.numeric(aug20nondenguesite10$X10)
sep03nondenguesite10$X10 <- as.numeric(sep03nondenguesite10$X10)


ajun04nondenguesite10 <- sum(jun04nondenguesite10$X10, na.rm=T)
ajun12nondenguesite10 <- sum(jun12nondenguesite10$X10, na.rm=T)
ajun18nondenguesite10 <- sum(jun18nondenguesite10$X10, na.rm=T)
ajun25nondenguesite10 <- sum(jun25nondenguesite10$X10, na.rm=T)
ajul04nondenguesite10 <- sum(jul04nondenguesite10$X10, na.rm=T)
ajul10nondenguesite10 <- sum(jul10nondenguesite10$X10, na.rm=T)
ajul17nondenguesite10 <- sum(jul17nondenguesite10$X10, na.rm=T)
ajul24nondenguesite10 <- sum(jul24nondenguesite10$X10, na.rm=T)
ajul30nondenguesite10 <- sum(jul30nondenguesite10$X10, na.rm=T)
aaug06nondenguesite10 <- sum(aug06nondenguesite10$X10, na.rm=T)
aaug13nondenguesite10 <- sum(aug13nondenguesite10$X10, na.rm=T)
aaug20nondenguesite10 <- sum(aug20nondenguesite10$X10, na.rm=T)
asep03nondenguesite10 <- sum(sep03nondenguesite10$X10, na.rm=T)



#####################
#
#
#
#
#
################################


#CREATE dengueS FOR PLOTTING
#Site 1
dengue1 <- c(ajun04denguesite1,
          ajun12denguesite1,
          ajun18denguesite1,
          ajun25denguesite1,
          ajul04denguesite1,
          ajul10denguesite1,
          ajul17denguesite1,
          ajul24denguesite1,
          ajul30denguesite1,
          aaug06denguesite1,
          aaug13denguesite1,
          aaug20denguesite1,
          asep03denguesite1)

#Site 2
dengue2 <- c(ajun04denguesite2,
          ajun12denguesite2,
          ajun18denguesite2,
          ajun25denguesite2,
          ajul04denguesite2,
          ajul10denguesite2,
          ajul17denguesite2,
          ajul24denguesite2,
          ajul30denguesite2,
          aaug06denguesite2,
          aaug13denguesite2,
          aaug20denguesite2,
          asep03denguesite2)


#Site 3
dengue3 <- c(ajun04denguesite3,
          ajun12denguesite3,
          ajun18denguesite3,
          ajun25denguesite3,
          ajul04denguesite3,
          ajul10denguesite3,
          ajul17denguesite3,
          ajul24denguesite3,
          ajul30denguesite3,
          aaug06denguesite3,
          aaug13denguesite3,
          aaug20denguesite3,
          asep03denguesite3)


#Site 4
dengue4 <- c(ajun04denguesite4,
          ajun12denguesite4,
          ajun18denguesite4,
          ajun25denguesite4,
          ajul04denguesite4,
          ajul10denguesite4,
          ajul17denguesite4,
          ajul24denguesite4,
          ajul30denguesite4,
          aaug06denguesite4,
          aaug13denguesite4,
          aaug20denguesite4,
          asep03denguesite4)


#Site 5
dengue5 <- c(ajun04denguesite5,
          ajun12denguesite5,
          ajun18denguesite5,
          ajun25denguesite5,
          ajul04denguesite5,
          ajul10denguesite5,
          ajul17denguesite5,
          ajul24denguesite5,
          ajul30denguesite5,
          aaug06denguesite5,
          aaug13denguesite5,
          aaug20denguesite5,
          asep03denguesite5)


#Site 6
dengue6 <- c(ajun04denguesite6,
          ajun12denguesite6,
          ajun18denguesite6,
          ajun25denguesite6,
          ajul04denguesite6,
          ajul10denguesite6,
          ajul17denguesite6,
          ajul24denguesite6,
          ajul30denguesite6,
          aaug06denguesite6,
          aaug13denguesite6,
          aaug20denguesite6,
          asep03denguesite6)


#Site 7
dengue7 <- c(ajun04denguesite7,
          ajun12denguesite7,
          ajun18denguesite7,
          ajun25denguesite7,
          ajul04denguesite7,
          ajul10denguesite7,
          ajul17denguesite7,
          ajul24denguesite7,
          ajul30denguesite7,
          aaug06denguesite7,
          aaug13denguesite7,
          aaug20denguesite7,
          asep03denguesite7)


#Site 8
dengue8 <- c(ajun04denguesite8,
          ajun12denguesite8,
          ajun18denguesite8,
          ajun25denguesite8,
          ajul04denguesite8,
          ajul10denguesite8,
          ajul17denguesite8,
          ajul24denguesite8,
          ajul30denguesite8,
          aaug06denguesite8,
          aaug13denguesite8,
          aaug20denguesite8,
          asep03denguesite8)


#Site 9
dengue9 <- c(ajun04denguesite9,
          ajun12denguesite9,
          ajun18denguesite9,
          ajun25denguesite9,
          ajul04denguesite9,
          ajul10denguesite9,
          ajul17denguesite9,
          ajul24denguesite9,
          ajul30denguesite9,
          aaug06denguesite9,
          aaug13denguesite9,
          aaug20denguesite9,
          asep03denguesite9)


#Site 10
dengue10 <- c(ajun04denguesite10,
           ajun12denguesite10,
           ajun18denguesite10,
           ajun25denguesite10,
           ajul04denguesite10,
           ajul10denguesite10,
           ajul17denguesite10,
           ajul24denguesite10,
           ajul30denguesite10,
           aaug06denguesite10,
           aaug13denguesite10,
           aaug20denguesite10,
           asep03denguesite10)



denguetotal <- dengue1+dengue2+dengue3+dengue4+dengue5+dengue6+dengue7+dengue8+dengue9+dengue10

