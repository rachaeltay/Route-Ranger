
install.packages("mc2d")

library(mc2d)

435 to 480   7-8   500 
480 to 540   8-9   550
540 to 600   9-10  700
600 to 660   10-11 950
660 to 720   11-12 1000
720 to 780   12-1  1200
780 to 840   1-2   950
840 to 900   2-3   800
900 to 960   3-4   650
960 to 1020  4-5   500
1020 to 1080 5-6   500
1080 to 1140 6-7   450
1140 to 1200 7-8   400
1200 to 1260 8-9   350
1260 to 1320 9-10  300
1320 to 1380 10-11 200

#rigging
qpd7 <- rtriang(500, min=435, mode=465, max=480)
qpd8 <- rtriang(550, min=480, mode=525, max=540)
qpd9 <- rtriang(700, min=540, mode=685, max=600)
qpd10 <- rtriang(950, min=600, mode=645, max=660)
qpd11 <- rtriang(1000, min=660, mode=705, max=720)
qpd12 <- rtriang(1200, min=720, mode=765, max=780)
qpd13 <- rtriang(950, min=780, mode=825, max=840)
qpd14 <- rtriang(800, min=840, mode=885, max=900)
qpd15 <- rtriang(650, min=900, mode=945, max=960)
qpd16 <- rtriang(500, min=960, mode=1005, max=1020)
qpd17 <- rtriang(500, min=1020, mode=1065, max=1080)
qpd18 <- rtriang(450, min=1080, mode=1125, max=1140)
qpd19 <- rtriang(400, min=1140, mode=1185, max=1200)
qpd20 <- rtriang(350, min=1200, mode=1245, max=1260)
qpd21 <- rtriang(300, min=1260, mode=1305, max=1320)
qpd22 <- rtriang(200, min=1320, mode=1365, max=1380)
#end rigging




q7 <- data.frame("timeQ" = qpd7)
q8 <- data.frame("timeQ" = qpd8)
q9 <- data.frame("timeQ" = qpd9)
q10 <- data.frame("timeQ" = qpd10)
q11 <- data.frame("timeQ" = qpd11)
q12 <- data.frame("timeQ" = qpd12)
q13 <- data.frame("timeQ" = qpd13)
q14 <- data.frame("timeQ" = qpd14)
q15 <- data.frame("timeQ" = qpd15)
q16 <- data.frame("timeQ" = qpd16)
q17 <- data.frame("timeQ" = qpd17)
q18 <- data.frame("timeQ" = qpd18)
q19 <- data.frame("timeQ" = qpd19)
q20 <- data.frame("timeQ" = qpd20)
q21 <- data.frame("timeQ" = qpd21)
q22 <- data.frame("timeQ" = qpd22)


z1 <- rbind(q7,q8)
z1 <- rbind(z1,q9)
z1 <- rbind(z1,q10)
z1 <- rbind(z1,q11)
z1 <- rbind(z1,q12)
z1 <- rbind(z1,q13)
z1 <- rbind(z1,q14)
z1 <- rbind(z1,q15)
z1 <- rbind(z1,q16)
z1 <- rbind(z1,q17)
z1 <- rbind(z1,q18)
z1 <- rbind(z1,q19)
z1 <- rbind(z1,q20)
z1 <- rbind(z1,q21)
z1 <- rbind(z1,q22)



z1 <- sort(z1$timeQ,decreasing=FALSE)

hist(z1)

#[1] 1 4 2 5 3 6 
A.K. 




qpd <- rtriang(10000, min=7, mode=12, max=23)
qpd
hist(qpd)
# Generate 100 values
ret1 <- rtriang(10000, min=435, mode=720, max=1380)
df1 <- Vectorize(ret1)

ret2 <- rtriang(1000, min=0, mode=5, max=10)
df2 <- Vectorize(ret2)

dT <- data.frame(cbind(df1,df2))[order(df1),]

dT


typeof(dT)

data <- sample(seq(as.Time('10:00'), as.Date('2000/01/01'), by="day"), 12)
data
# plot a histogram
hist(returns)

set.seed(123)
# Generate 100 values
returns <- rnorm(1:1000, mean = 0, sd = 1)
returns
# plot a histogram
hist(returns, breaks=10)


