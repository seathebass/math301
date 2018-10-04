#### Test 1 answers

## Question 1
# LifeCycleSavings built-in data
x <- LifeCycleSavings$dpi #a x is dpi data
x
median(x) #b median of x
IQR(x) #c Interquartile range
summary(x)[5]-summary(x)[2] #c another way. just ignore the name
quantile(x,.75)-quantile(x,.25) #c another way
(ex <-mean(x)) #d sample mean
(s <- sd(x)) #e sample standard deviation
c(ex-1*s,ex+1*s) # interval within 1 sd of mean
xinside <- x[x>ex-1*s & x <ex + 1*s ]
tibble(x = x) %>% #the tidy way
  filter(x<ex+s,x>ex-s) %>%
  nrow()
38/length(x) #answer to f
length(xinside) # Number of elements withing 1 sd of mean
length(xinside)/length(x) #f proportion within 1 sd from mean
sqrt(sum(x)) #g square root of sum of values in x
max(x) #h maximum of x
             
## Question 2             
pa <- .3
pb <- .1
pc <- .2
(paandb<-pa*pb) #a probability of a and b multiply by independence
(pagb <- pa) #b By definition of conditional probability and independence
(paandbandc <-pa*pb*pc) #c P(a and b and c) - multiply because of independence
(paorb <- pa + pb - paandb) #d P(A union B) = P(A) + P(B) - P(A intersect B)
# by deMorgans law a' intersect b' = (A union B)' so
1 - paorb #e demorgans law and P((A intersect B)')

(l3<-(1-pa)*(1-pb)*(1-pc)) #f Probability of losing all 3 independent bets.
pa+pb+pc-pa*pb-pa*pc-pb*pc+pa*pb*pc #g. Formula for P(A union B union C)
1-l3 #g another way Want complement of losing all 3

### Question 3
#Bayes Theorem - Creative in math/music
# m creative in math nm Not creative in math
# u creative in music nu Not creative in music.
pu <- .29 # Probability of creative in music
(pnu<- 1-pu) # Probability of not creative in music
(pmgu <- .30) # Probability of cr. math given cr. music
(pmgnu<- .15) # Probability of cr. math given not cr. music
(pnmgu <- 1 - pmgu) # Probability of not cr. math given cr. music
(pnmgnu <- 1 - pmgnu) # Probability of not cr. math given not cr. music
(pm <- pmgu*pu + pmgnu*pnu)
# P(m) = P(m and u) + P(m and nu) = P(m|u)*P(u) + P(m|nu)*P(nu)v 
(pm <- pmgu*pu + pmgnu*pnu) #a) Law of total probability above
pmgu*pu #b P(m and u) = p(m|u)*pu : Note 1st term on right in a - multiplication principle

(pugm <- pmgu*pu / pm) # c Bayes Theorem p(u|m) = p(m and u)/p(m)

pu^5 #d Prob all 5 creative in music - multiply- independence
(1-pu)^5. #e None of the 5 creative in music.

### Question 4
m <- 6 # number of math books
n <- 9 # number statistics books
k<- 4 # number of books TA picks
dhyper(0,m,n,k) # a P(X=0)
phyper(1,m,n,k) # b P(X<2)
(p<-m/(m+n)) # proportion of math books on shelf
k*(m/(m+n))#expected value where k = n or the size of your sample.
#Variance of X if batch size is k=4
(vx <- ((m+n-k)/(m+n-1))*k*p*(1-p))
(ex <- k*p) # c expected value for hypergometric
(sdx<-sqrt(vx)) # d standard deviation of X
c(ex-sdx,ex+sdx) # e within 1sd
dhyper(1,m,n,k)+dhyper(2,m,n,k) #e

##Question 5
#Poisson boat arrival user 2.3 arrivals per 30 minutes
# x number of arrivals in an hour
# y = number of arrivals in a day.

(60/30)*2.3 #a Number of arrivals in 60 minutes. Convert to hour units
lambda <- (60/30)*2.3 #b Poisson parameter for X - arrivals in an hour is average arrivals per hour
(lambday<-24*lambda) #c parameter is proportional to time in Poisson
(sdx <- sqrt(lambda)) #d for Poisson, sd is square root of variance & variance = parameter
(vy <-lambday) #e variance is the parameter for poisson
dpois(0,lambda) #f probability of no boats arrive in hour
1 -ppois(8,lambda) #g prob more than 8 boats barrive in an hour
ppois(110,lambday)-ppois(89,lambday) #h P(90 <= Y <= 110)
sqrt(7)^2*lambda #i Variance of aX is a^2*V(X) and v(x) is lambda
(1 - dpois(0,lambda))^24 # j Use independence. True hour11 an hour2 and...and hour24

##Question 6
x <- c(8,10,12,14,16,18,20) # demandd for tickets
x
px <- c(.05,.10, .35, .25, .15, .05, .05) # must add to 1
px
sum(px)
rbind(x,px)
sum(px[5:7]) #a Probability X> 15 - p(16)+p(18)+p(20)
(ex <- sum(x*px)) #b expected value
(ex2<- sum(x^2*px)) #c Expected value of X^2
(vx <- ex2 - ex^2) #d variance is E(X^2) - E(X)^2
(sdx <- sqrt(vx)) #e standard deviation is sqrt of variance
7^2*vx #f V(aX+b) = a^2*V(X). our a= 7, V(X)=vx
Fx <- cumsum(px) # CDF of X
rbind(x,px,Fx)
Fx[4] # Probability x <=15 on a single week
Fx[4]^7 #g. By independence <15 on each of 7 days

## Question 7
# Binomial distribution - .29 probability of snowing on March 3
n <- 12. # The 12 years from 2019 - 2030
p <- .29 # probability of snowing on March 3
1-p # probability of not snowing on March 3
# x is number of times snowing March 3's, This is binomial(x, 12, .29)

(ex <- n*p) #a expected value of binomial
(vx <- n*p*(1-p)) #b variance of binomial
(sdx <- sqrt(n*p*(1-p))) #c standard deviation = sqrt of variance
5*ex +2 #d E(ax + b) = ae(x) + b
5^2*vx #e. V(ax+b) = a^2*v(x)
x011<-0:11
sum(x011^2*dbinom(x011,n,p)) #f E(x^2) using definition of expected value
dbinom(3, n, p) #g P( x = 3)
dbinom(0,n,p) #h P(x=0. No snow 2019-2030
#i Prob x within 1 sd of its expected value
c(ex - sdx, ex+sdx) # x must be in this interval
# We see x within 1 sd of expected value if x is 2 to 5
pbinom(5,n,p) - pbinom(1,n,p) #i
pbinom(ex+sdx,n,p)-pbinom(ex-sdx,n,p) #i another way. Does not work if ex-sdx is integer