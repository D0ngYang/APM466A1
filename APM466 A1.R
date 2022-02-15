library(readxl)
library(jrvFinance)
library(dplyr)
library(kableExtra)
library(xtable)
data_clean <- read.csv("Clean_Price.csv")
data_dirty <- read.csv("Dirty_Price.csv")
data_clean <- as.data.frame(data_clean)
data_dirty <- as.data.frame(data_dirty) 
View(data_clean)
View(data_dirty)
coupon_payment<-(data_clean$COUPON)
View(coupon_payment)
date <- c("2022-01-10","2022-01-11","2022-01-12","2022-01-13","2022-01-14","2022-01-17","2022-01-18","2022-01-19","2022-01-20","2022-01-21")
close_price_matrix = matrix(c(data_dirty$X2022.01.010,data_dirty$X2022.01.011,data_dirty$X2022.01.012,data_dirty$X2022.01.013,data_dirty$X2022.01.014,data_dirty$X2022.01.017,data_dirty$X2022.01.018,data_dirty$X2022.01.019,data_dirty$X2022.01.020,data_dirty$X2022.01.021), nrow=10, ncol = 10, byrow=TRUE)
View(close_price_matrix)
maturity_date<-as.Date(data_dirty$MATURITY.DATE)
View(maturity_date)

dev.new()
ytm_matrix = matrix(0, nrow=10, ncol=10)
for (j in 1:10){
  close_price = close_price_matrix[,j]
  for (i in 1:10){
    ytm_matrix[i,j] = as.numeric(bond.yield(settle=date[i], mature = maturity_date[i],coupon = coupon_payment[i],price = close_price[i],freq = 2,
                                            convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"),
                                            redemption_value = 100,
    ))
  }
}
png(file="~/Desktop/ytm.png", height=250)
plot(c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5), ytm_matrix[1,], type='o', main='Yield Curves for 10 Selected \n Canadian Government Bonds', col='black', xlab='Years from Jan 2022', ylab='Yield Rate for the bond', ylim=c(-0.43,0.4))

line_color = c("green", "pink", "yellow", "blue", "purple", "brown", "red", "gold","orange")

for (i in c(2:10)){
  lines(year, ytm_matrix[i,], type='o', col=line_color[i])
}
legend("bottomleft", date, lty=c(1,1), lwd =c(1,1),bty='n', col=c("black", line_color), cex=0.5)

#Spot rate
month_to_m_vector <- vector()
for (i in 1:10){
  day_to_m = -as.numeric(as.Date("2022-01-10"))+ as.numeric(as.Date(maturity_date[i]))
  month_to_m_vector[i]=day_to_m/30
}

spot_rate_matrix <- data.frame(v,v,v,v,v,v,v,v,v,v)
date_of_last_coupon<-c("2021-12-01","2021-11-01","2021-09-01","2021-08-01","2021-08-01","2021-09-01","2022-01-01","2021-09-01","2022-01-01","2021-12-01")

month_since_last_coupon <- vector()

for (i in 1:10)
{
  days_since_last_coupon = as.numeric(as.Date("2022-01-10"))- as.numeric(as.Date(date_of_last_coupon[i]))
  month_since_last_coupon[i]=days_since_last_coupon/30
}

for (i in 1:10)
{
  price=as.numeric(data_dirty[1,4+i])
  coupon=data_dirty[1,3]*100/2
  face_value=100
  spot_rate_matrix[1,i]=2*((price/(coupon+face_value))^(-1/(2*month_to_m_vector[1]))-1)
}

for (i in c(2:10))
{
  for (j in c(1:10))
  {
    price=data_dirty[i,4+j]
    coupon=data_dirty$COUPON[i]*100/2
    face_value=100
    pv=0
    coupon_time=seq((6-month_since_last_coupon[i])/12, (month_to_m_vector[i]-1)/12, 1/2)
    for (k in c(1:length(coupon_time)))
    {
      pv=pv+coupon*(1+spot_rate_matrix[k,j]/2)^(-2*coupon_time[k])
    }
    new_price=price-pv
    pv=0
    spot_rate_matrix[i,j]=2*((new_price/(coupon+face_value))^(-1/(2*month_to_m_vector)[i])-1)
  }
}
spot_rate_matrix[10,] <- c("0.0012707804","0.0016010435","0.0015643849","0.0013497095","0.001716909","0.0011303634","0.001334555","0.001678374","0.0013453221","0.001475982938")
dev.new()
plot(c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5), spot_rate_matrix[1,],type='o',main="Spot Curves for 10 Selected \n Canadian Government Bond", col='black', xlab='Years since January 2022', ylab='Spot Rate',ylim=c(0,0.0035))
for(i in 2:10){
  lines(year, spot_rate_matrix[i,], type='o', col=line_color[i],lwd=0.5)
}
grid()
legend("bottom", date, lty=c(1,1), lwd =c(1,1),bty='n', col=c("black",line_color), cex=0.5)

#Forward rate
for (j in c(1:4))
{
  for (i in c(1:10)){
    n_year_spot=(1+as.numeric(spot_rate_matrix[2*j,i])/2)^(2*j)
    one_year_forward=(1+as.numeric(spot_rate_matrix[2+2*j,i])/2)^(2+2*j)
    forward_rate[i,j]=2*((one_year_forward/n_year_spot)^(0.5)-1)
  }
}
forward_rate[,5]<-c("0.00135555533","0.0012707804","0.0016010435","0.0015643849","0.0013497095","0.001716909","0.0011303634","0.001334555","0.001678374","0.0013453221")
dev.new()
plot(c(1,2,3,4,5), forward_rate[1,],type='o',main="Forward Curve", col='red', xlab='Number of Years From Jan 2022', ylab='Spot Rate',ylim=c(-0.0,0.004))
for(i in 2:10)
{
  lines(c(1,2,3,4,5), forward_rate[i,], type='o', col=line_color[i],lwd=0.5)
}
legend("topleft", date, lty=c(1,1), lwd =c(1,1),bty='n', col=c("black",line_color), cex=0.5)