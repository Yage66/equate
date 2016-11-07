setwd("E:/Doctorseminar")
load('merged.rdata')
head(mrs)
str(mrs)


# extract the item names for the four domains
isci <- read.csv("E:/Doctorseminar/science-sequence.csv")
colnames(isci) <- tolower(colnames(isci))
sdom <- isci[isci$item.usage == "OP", "standardformat"]
sqitem <- paste0("sq", 1:60)
sqitem


# extract science dataset, create p and q population
dat <- mrs[, c(sqitem, "gpa")]
dat <- dat[complete.cases(dat), ]
pindex <- sample((1:nrow(dat))[dat$gpa > 2.5], nrow(dat)/2, replace = F)
qindex <- (1:nrow(dat))[-pindex]
pvalue <- apply(dat[, sqitem], 2, mean, na.rm = T)
pvalue <- as.numeric(as.character(pvalue))

mean(dat$gpa)


domain <- cbind(sqitem, sdom, pvalue)
domain <- as.data.frame(domain)
head(domain)

# how many items in 4 each domain
table(domain$sdom)
table(domain$sqitem, domain$sdom)
table(domain$sqitem)


d1 <- subset(domain, sdom == 1)
d1 <- d1[order(d1$pvalue), ]
d1$pvalue <- as.numeric(as.character(d1$pvalue))
mean(d1$pvalue)

d2 <- subset(domain, sdom == 2)
d2 <- d2[order(d2$pvalue), ]
d2$pvalue <- as.numeric(as.character(d2$pvalue))
mean(d2$pvalue)

d3 <- subset(domain, sdom == 3)
d3 <- d3[order(d3$pvalue), ]
d3$pvalue <- as.numeric(as.character(d3$pvalue))
mean(d3$pvalue)


d4 <- subset(domain, sdom == 4)
d4 <- d4[order(d4$pvalue), ]
d4$pvalue <- as.numeric(as.character(d4$pvalue))
mean(d4$pvalue)

# create pseudo test form x and y(test length 20),two anchor test v(length 10) and w (length 20)

xitem <- sqitem[c(16, 58, 60, 36, 45, 17, 13, 9, 53, 46, 59, 49, 52, 48, 42, 39, 12, 43, 50, 21)]
yitem <- sqitem[c(20, 41, 29, 33, 27, 1, 5, 51, 15, 57, 32, 56, 26, 47, 18, 34, 55, 28, 23, 22)]
vitem <- sqitem[c(19, 3, 44, 8, 24, 14, 6, 10, 11, 54)]
witem <- sqitem[c(19, 3, 44, 8, 24, 14, 6, 10, 11, 54, 25, 7, 35, 40, 2, 30, 38, 31, 37,4)]
formx <- dat[c(xitem, vitem)]
formy <- dat[c(yitem, vitem)]

mean(apply(dat[pindex, sqitem], 2, mean, na.rm = T))



