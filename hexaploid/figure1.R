library(RColorBrewer)
y <- c()
for(j in 1:6){
  
  times <- seq(0,20,1)
  N <- length(times)
  
  m <- numeric(N)
  n <- numeric(N)
  k <- numeric(N)
  e <- numeric(N)
  h <- numeric(N)
  f <- numeric(N)
  u <- numeric(N)
  
  m[1] <- 0.1 
  n[1] <- 0.05
  k[1] <- 0.2 
  e[1] <- 0.25
  h[1] <- 0.13
  f[1] <- 0.1 
  u[1] <- 0.17
  
  alpha=c(0,1/7,1/6,1/5,1/4,3/11)[j]
  for(t in 1:(N-1)){
    
    m[t+1] <- ((m[t]+1/2*n[t]+1/5*k[t]+1/20*e[t])+alpha*(1/6*n[t]+1/5*k[t]+3/20*e[t]+1/15*h[t]))^2
    
    n[t+1] <- (m[t]*n[t]+6/5*m[t]*k[t]+9/10*m[t]*e[t]+2/5*m[t]*h[t]+1/2*n[t]^2+4/5*n[t]*k[t]+1/2*n[t]*e[t]+1/5*n[t]*h[t]+6/25*k[t]^2+6/25*k[t]*e[t]+2/25*k[t]*h[t]+9/200*e[t]^2+1/50*e[t]*h[t])+alpha*(-2/3*m[t]*n[t]
                                                                                                                                                                                                       -2/3*m[t]*k[t]-3/10*m[t]*e[t]+2/15*m[t]*h[t]+1/3*m[t]*f[t]-1/6*n[t]^2-1/15*n[t]*k[t]+7/60*n[t]*e[t]+1/5*n[t]*h[t]+1/6*n[t]*f[t]+8/75*k[t]^2+4/15*k[t]*e[t]
                                                                                                                                                                                                       +14/75*k[t]*h[t]+1/15*k[t]*f[t]+3/25*e[t]^2+19/150*e[t]*h[t]+1/60*e[t]*f[t]+2/75*h[t]^2)+alpha^2*(-1/9*n[t]^2-11/45*n[t]*k[t]-3/20*n[t]*e[t]-1/45*n[t]*h[t]+1/18*n[t]*f[t]
                                                                                                                                                                                                                                                                                                         -2/15*k[t]^2-4/25*k[t]*e[t]-4/225*k[t]*h[t]+1/15*k[t]*f[t]-9/200*e[t]^2+1/20*e[t]*f[t]+2/225*h[t]^2+1/45*h[t]*f[t])
    
    k[t+1] <- (2/5*m[t]*k[t]+9/10*m[t]*e[t]+6/5*m[t]*h[t]+m[t]*f[t]+1/4*n[t]^2+4/5*n[t]*k[t]+9/10*n[t]*e[t]+4/5*n[t]*h[t]+1/2*n[t]*f[t]+11/25*k[t]^2+37/50*k[t]*e[t]+12/25*k[t]*h[t]+1/5*k[t]*f[t]
               +99/400*e[t]^2+6/25*e[t]*h[t]+1/20*e[t]*f[t]+1/25*h[t]^2)+alpha*(1/3*m[t]*n[t]+2/15*m[t]*k[t]-3/10*m[t]*e[t]-2/3*m[t]*h[t]-2/3*m[t]*f[t]-1/6*n[t]^2-8/15*n[t]*k[t]-13/30*n[t]*e[t]-1/5*n[t]*h[t]-22/75*k[t]^2
                                                                                -22/75*k[t]*e[t]+2/25*k[t]*h[t]+4/15*k[t]*f[t]-3/200*e[t]^2+31/150*e[t]*h[t]+4/15*e[t]*f[t]+8/75*h[t]^2+2/15*h[t]*f[t])+alpha^2*(1/6*n[t]^2+14/45*n[t]*k[t]+1/10*n[t]*e[t]-2/15*n[t]*h[t]-2/9*n[t]*f[t]
                                                                                                                                                                                                                 +31/225*k[t]^2+3/50*k[t]*e[t]-38/225*k[t]*h[t]-11/45*k[t]*f[t]-9/400*e[t]^2-7/50*e[t]*h[t]-3/20*e[t]*f[t]-9/225*h[t]^2-1/45*h[t]*f[t]+1/36*f[t]^2)
    
    
    e[t+1] <- (1/10*m[t]*e[t]+2/5*m[t]*h[t]+m[t]*f[t]+2*m[t]*u[t]+1/5*n[t]*k[t]+1/2*n[t]*e[t]+4/5*n[t]*h[t]+n[t]*f[t]+n[t]*u[t]+6/25*k[t]^2+37/50*k[t]*e[t]+22/25*k[t]*h[t]+4/5*k[t]*f[t]+2/5*k[t]*u[t]+41/100*e[t]^2
               +37/50*e[t]*h[t]+1/2*e[t]*f[t]+1/10*e[t]*u[t]+6/25*h[t]^2+1/5*h[t]*f[t])+alpha*(2/15*m[t]*k[t]+3/10*m[t]*e[t]+2/5*m[t]*h[t]+1/3*m[t]*f[t]+1/6*n[t]^2+1/5*n[t]*k[t]-2/15*n[t]*e[t]-2/5*n[t]*h[t]-1/3*n[t]*f[t]+1/3*n[t]*u[t]
                                                                                               -2/75*k[t]^2-59/150*k[t]*e[t]-44/75*k[t]*h[t]-2/5*k[t]*f[t]+2/5*k[t]*u[t]-6/25*e[t]^2-59/150*e[t]*h[t]-2/15*e[t]*f[t]+3/10*e[t]*u[t]-2/75*h[t]^2+1/5*h[t]*f[t]+2/15*h[t]*u[t]
                                                                                               +1/6*f[t]^2)+alpha^2*(-1/9*n[t]^2-2/15*n[t]*k[t]+1/10*n[t]*e[t]+14/45*n[t]*h[t]+1/3*n[t]*f[t]-4/225*k[t]^2+4/25*k[t]*e[t]+8/25*k[t]*h[t]
                                                                                                                     +14/45*k[t]*f[t]+9/100*e[t]^2+4/25*e[t]*h[t]+1/10*e[t]*f[t]-4/225*h[t]^2-2/15*h[t]*f[t]-1/9*f[t]^2)
    
    h[t+1] <- alpha^2*(1/36*n[t]^2-1/45*n[t]*k[t]-3/20*n[t]*e[t]-11/45*n[t]*h[t]-2/9*n[t]*f[t]-9/225*k[t]^2-7/50*k[t]*e[t]-38/225*k[t]*h[t]-2/15*k[t]*f[t]-9/400*e[t]^2+3/50*e[t]*h[t]+1/10*e[t]*f[t]+31/225*h[t]^2+14/45*h[t]*f[t]
                       +1/6*f[t]^2)+alpha*(2/15*n[t]*k[t]+4/15*n[t]*e[t]+4/15*n[t]*h[t]-2/3*n[t]*u[t]+8/75*k[t]^2+31/150*k[t]*e[t]+2/25*k[t]*h[t]-1/5*k[t]*f[t]-2/3*k[t]*u[t]-3/200*e[t]^2-22/75*e[t]*h[t]-13/30*e[t]*f[t]-3/10*e[t]*u[t]
                                           -22/75*h[t]^2-8/15*h[t]*f[t]+2/15*h[t]*u[t]-1/6*f[t]^2+1/3*f[t]*u[t])+(1/20*n[t]*e[t]+1/5*n[t]*h[t]+1/2*n[t]*f[t]+n[t]*u[t]+1/25*k[t]^2+6/25*k[t]*e[t]+12/25*k[t]*h[t]+4/5*k[t]*f[t]+6/5*k[t]*u[t]
                                                                                                                  +99/400*e[t]^2+37/50*e[t]*h[t]+9/10*e[t]*f[t]+9/10*e[t]*u[t]+11/25*h[t]^2+4/5*h[t]*f[t]+2/5*h[t]*u[t]+1/4*f[t]^2)
    
    f[t+1] <-alpha^2*(1/45*n[t]*k[t]+1/20*n[t]*e[t]+1/15*n[t]*h[t]+1/18*n[t]*f[t]+2/225*k[t]^2-4/225*k[t]*h[t]-1/45*k[t]*f[t]-9/200*e[t]^2-4/25*e[t]*h[t]-3/20*e[t]*f[t]-2/15*h[t]^2-11/45*h[t]*f[t]-1/9*f[t]^2)+alpha*(1/60*n[t]*e[t]+1/15*n[t]*h[t]
                                                                                                                                                                                                                        +1/6*n[t]*f[t]+1/3*n[t]*u[t]+2/75*k[t]^2+19/150*k[t]*e[t]+14/75*k[t]*h[t]+1/5*k[t]*f[t]+2/15*k[t]*u[t]+3/25*e[t]^2+4/15*e[t]*h[t]+7/60*e[t]*f[t]-3/10*e[t]*u[t]+8/75*h[t]^2-1/15*h[t]*f[t]-2/3*h[t]*u[t]-1/6*f[t]^2-2/3*f[t]*u[t])+(1/50*k[t]*e[t]
                                                                                                                                                                                                                                                                                                                                                                                                                                                            +2/25*k[t]*h[t]+1/5*k[t]*f[t]+2/5*k[t]*u[t]+9/200*e[t]^2+6/25*e[t]*h[t]+1/2*e[t]*f[t]+9/10*e[t]*u[t]+6/25*h[t]^2+4/5*h[t]*f[t]+6/5*h[t]*u[t]+1/2*f[t]^2+f[t]*u[t])
    
    u[t+1] <- ((1/20*e[t]+1/5*h[t]+1/2*f[t]+u[t])+alpha*(1/15*k[t]+3/20*e[t]+1/5*h[t]+1/6*f[t]))^2
  }
  
  y <- rbind(y,rbind(m,n,k,e,h,f,u))
  
}

colors_homozygote <- colorRampPalette(c("snow", "#b94919"))(18)
colors_heterozygote <- colorRampPalette(c("snow", "#e0a344"))(18)

#---------------------plotA-----------------------
#AAAAAA
par(mfrow=c(3,3),mar=c(3,2,3,2),oma=c(2,2,2,2))
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.1))
axis(1,seq(0,20,5),seq(0,20,5),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.1,0.02),seq(0,0.1,0.02),lwd=1,cex.axis=2,mgp = c(1, 1.5, 0))

lines(times,y[1,],type = "l",lwd=6,col=colors_homozygote[3])
lines(times,y[8,],type = "l",lwd=6,col=colors_homozygote[6])
lines(times,y[15,],type = "l",lwd=6,col=colors_homozygote[9])
lines(times,y[22,],type = "l",lwd=6,col=colors_homozygote[12])
lines(times,y[29,],type = "l",lwd=6,col=colors_homozygote[15])
lines(times,y[36,],type = "l",lwd=6,col=colors_homozygote[18])

#AAAAAa
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.15))
axis(1,seq(0,20,5),seq(0,20,5),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.15,0.03),seq(0,0.15,0.03),lwd=1,cex.axis=2,mgp = c(1, 1.5, 0))

lines(times,y[2,],type = "l",lwd=6,col=colors_heterozygote[3])
lines(times,y[9,],type = "l",lwd=6,col=colors_heterozygote[6])
lines(times,y[16,],type = "l",lwd=6,col=colors_heterozygote[9])
lines(times,y[23,],type = "l",lwd=6,col=colors_heterozygote[12])
lines(times,y[30,],type = "l",lwd=6,col=colors_heterozygote[15])
lines(times,y[37,],type = "l",lwd=6,col=colors_heterozygote[18])

#AAAAaa
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.3))
axis(1,seq(0,20,5),seq(0,20,5),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.3,0.1),seq(0,0.3,0.1),lwd=1,cex.axis=2,mgp = c(1, 1.5, 0))

lines(times,y[3,],type = "l",lwd=6,col=colors_heterozygote[3])
lines(times,y[10,],type = "l",lwd=6,col=colors_heterozygote[6])
lines(times,y[17,],type = "l",lwd=6,col=colors_heterozygote[9])
lines(times,y[24,],type = "l",lwd=6,col=colors_heterozygote[12])
lines(times,y[31,],type = "l",lwd=6,col=colors_heterozygote[15])
lines(times,y[38,],type = "l",lwd=6,col=colors_heterozygote[18])

#AAAaaa
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.4))
axis(1,seq(0,20,5),seq(0,20,5),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.4,0.1),seq(0,0.4,0.1),lwd=1,cex.axis=2,mgp = c(1, 1.5, 0))

lines(times,y[4,],type = "l",lwd=6,col=colors_heterozygote[3])
lines(times,y[11,],type = "l",lwd=6,col=colors_heterozygote[6])
lines(times,y[18,],type = "l",lwd=6,col=colors_heterozygote[9])
lines(times,y[25,],type = "l",lwd=6,col=colors_heterozygote[12])
lines(times,y[32,],type = "l",lwd=6,col=colors_heterozygote[15])
lines(times,y[39,],type = "l",lwd=6,col=colors_heterozygote[18])

#AAaaaa
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.3))
axis(1,seq(0,20,5),seq(0,20,5),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.3,0.1),seq(0,0.3,0.1),lwd=1,cex.axis=2,mgp = c(1, 1.5, 0))

lines(times,y[5,],type = "l",lwd=6,col=colors_heterozygote[3])
lines(times,y[12,],type = "l",lwd=6,col=colors_heterozygote[6])
lines(times,y[19,],type = "l",lwd=6,col=colors_heterozygote[9])
lines(times,y[26,],type = "l",lwd=6,col=colors_heterozygote[12])
lines(times,y[33,],type = "l",lwd=6,col=colors_heterozygote[15])
lines(times,y[40,],type = "l",lwd=6,col=colors_heterozygote[18])

#Aaaaaa
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.2))
axis(1,seq(0,20,5),seq(0,20,5),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.2,0.05),seq(0,0.2,0.05),lwd=1,cex.axis=2,mgp = c(1, 1.5, 0))

lines(times,y[6,],type = "l",lwd=6,col=colors_heterozygote[3])
lines(times,y[13,],type = "l",lwd=6,col=colors_heterozygote[6])
lines(times,y[20,],type = "l",lwd=6,col=colors_heterozygote[9])
lines(times,y[27,],type = "l",lwd=6,col=colors_heterozygote[12])
lines(times,y[34,],type = "l",lwd=6,col=colors_heterozygote[15])
lines(times,y[41,],type = "l",lwd=6,col=colors_heterozygote[18])

#aaaaaa
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.2))
axis(1,seq(0,20,5),seq(0,20,5),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.2,0.05),seq(0,0.2,0.05),lwd=1,cex.axis=2,mgp = c(1, 1.5, 0))

lines(times,y[7,],type = "l",lwd=6,col=colors_homozygote[3])
lines(times,y[14,],type = "l",lwd=6,col=colors_homozygote[6])
lines(times,y[21,],type = "l",lwd=6,col=colors_homozygote[9])
lines(times,y[28,],type = "l",lwd=6,col=colors_homozygote[12])
lines(times,y[35,],type = "l",lwd=6,col=colors_homozygote[15])
lines(times,y[42,],type = "l",lwd=6,col=colors_homozygote[18])

#==================================================================================
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,1))
axis(1,seq(0,20,5),seq(0,20,5),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,1,0.2),seq(0,1,0.2),lwd=1,cex.axis=2,mgp = c(1, 1.5, 0))

y11=y[1,]+y[7,];y12=y[8,]+y[14,];y13=y[15,]+y[21,];y14=y[22,]+y[28,];y15=y[29,]+y[35,];y16=y[36,]+y[42,];
lines(times,y11,type = "l",lwd=6,col=colors_homozygote[3])
lines(times,y12,type = "l",lwd=6,col=colors_homozygote[6])
lines(times,y13,type = "l",lwd=6,col=colors_homozygote[9])
lines(times,y14,type = "l",lwd=6,col=colors_homozygote[12])
lines(times,y15,type = "l",lwd=6,col=colors_homozygote[15])
lines(times,y16,type = "l",lwd=6,col=colors_homozygote[18])

y21=y[2,]+y[3,]+y[4,]+y[5,]+y[6,]; y22=y[9,]+y[10,]+y[11,]+y[12,]+y[13,]; y23=y[16,]+y[17,]+y[18,]+y[19,]+y[20,];
y24=y[23,]+y[24,]+y[25,]+y[26,]+y[27,];y25=y[30,]+y[31,]+y[32,]+y[33,]+y[34,];y26=y[37,]+y[38,]+y[39,]+y[40,]+y[41,]
lines(times,y21,type = "l",lwd=6,col=colors_heterozygote[3])
lines(times,y22,type = "l",lwd=6,col=colors_heterozygote[6])
lines(times,y23,type = "l",lwd=6,col=colors_heterozygote[9])
lines(times,y24,type = "l",lwd=6,col=colors_heterozygote[12])
lines(times,y25,type = "l",lwd=6,col=colors_heterozygote[15])
lines(times,y26,type = "l",lwd=6,col=colors_heterozygote[18])



#-----------------legend---------
layout(matrix(c(1:2),1,2))
par(mar=c(3,3,1,3))

nlev <- colorRampPalette(c( "#b94919","snow"))(50)
plot(y = c(1, (length(nlev) + 1)), x = c(1, 2),xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n',type = 'n', ann = F)
rect(xleft = rep(0, (length(nlev) + 1)),ybottom = 1:length(nlev),xright = rep(2, (length(nlev) + 1)),ytop = 2:(length(nlev) + 1),col = nlev,border = nlev)

nlev <- colorRampPalette(c( "#e0a344","snow"))(50)
plot(y = c(1, (length(nlev) + 1)), x = c(1, 2),xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n',type = 'n', ann = F)
rect(xleft = rep(0, (length(nlev) + 1)),ybottom = 1:length(nlev),xright = rep(2, (length(nlev) + 1)),ytop = 2:(length(nlev) + 1),col = nlev,border = nlev)


#================plot B==========================================

par(mfrow=c(3,3),mar=c(3,2,3,2),oma=c(2,2,2,2))

#AAAAAA
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,0.3),ylim=c(0,0.03))
axis(1,c(0,1/7,1/5,3/11),c(0,1/7,1/5,3/11),lwd=1,cex.axis=2,labels = c("0","1/7","1/5","3/11"),tick = TRUE,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.03,0.01),seq(0,0.03,0.01),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
y1 <- as.numeric(y[c(1,8,15,22,29,36),16])
lines(c(0,1/7,1/6,1/5,1/4,3/11),y1,type = "l",lwd=8,col="#336464")

#AAAAAa
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,0.3),ylim=c(0,0.1))
axis(1,c(0,1/7,1/5,3/11),c(0,1/7,1/5,3/11),lwd=1,cex.axis=2,labels = c("0","1/7","1/5","3/11"),tick = TRUE,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.1,0.05),seq(0,0.1,0.05),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
y2 <- as.numeric(y[c(1,8,15,22,29,36)+1,16])
lines(c(0,1/7,1/6,1/5,1/4,3/11),y2,type = "l",lwd=8,col="#1f9fa1")

#AAAAaa
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,0.3),ylim=c(0,0.3))
axis(1,c(0,1/7,1/5,3/11),c(0,1/7,1/5,3/11),lwd=1,cex.axis=2,labels = c("0","1/7","1/5","3/11"),tick = TRUE,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.3,0.1),seq(0,0.3,0.1),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
y3 <- as.numeric(y[c(1,8,15,22,29,36)+2,16])
lines(c(0,1/7,1/6,1/5,1/4,3/11),y3,type = "l",lwd=8,col="#1f9fa1")

#AAAaaa
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,0.3),ylim=c(0,0.4))
axis(1,c(0,1/7,1/5,3/11),c(0,1/7,1/5,3/11),lwd=1,cex.axis=2,labels = c("0","1/7","1/5","3/11"),tick = TRUE,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.4,0.1),seq(0,0.4,0.1),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
y4 <- as.numeric(y[c(1,8,15,22,29,36)+3,16])
lines(c(0,1/7,1/6,1/5,1/4,3/11),y4,type = "l",lwd=8,col="#1f9fa1")

#AAaaaa
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,0.3),ylim=c(0,0.3))
axis(1,c(0,1/7,1/5,3/11),c(0,1/7,1/5,3/11),lwd=1,cex.axis=2,labels = c("0","1/7","1/5","3/11"),tick = TRUE,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.3,0.1),seq(0,0.3,0.1),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
y5 <- as.numeric(y[c(1,8,15,22,29,36)+4,16])
lines(c(0,1/7,1/6,1/5,1/4,3/11),y5,type = "l",lwd=8,col="#1f9fa1")

#Aaaaaa
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,0.3),ylim=c(0,0.15))
axis(1,c(0,1/7,1/5,3/11),c(0,1/7,1/5,3/11),lwd=1,cex.axis=2,labels = c("0","1/7","1/5","3/11"),tick = TRUE,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.15,0.05),seq(0,0.15,0.05),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
y6 <- as.numeric(y[c(1,8,15,22,29,36)+5,16])
lines(c(0,1/7,1/6,1/5,1/4,3/11),y6,type = "l",lwd=8,col="#1f9fa1")

#aaaaaa
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,0.3),ylim=c(0,0.05))
axis(1,c(0,1/7,1/5,3/11),c(0,1/7,1/5,3/11),lwd=1,cex.axis=2,labels = c("0","1/7","1/5","3/11"),tick = TRUE,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.05,0.01),seq(0,0.05,0.01),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))
y7 <- as.numeric(y[c(1,8,15,22,29,36)+6,16])
lines(c(0,1/7,1/6,1/5,1/4,3/11),y7,type = "l",lwd=8,col="#336464")

#Double Reduction====
#Homozygote
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,0.3),ylim=c(0,0.1))
axis(1,c(0,1/7,1/5,3/11),c(0,1/7,1/5,3/11),lwd=1,cex.axis=2,labels = c("0","1/7","1/5","3/11"),tick = TRUE,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,0.1,0.05),seq(0,0.1,0.05),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))

y31 <- as.numeric(sum(y[1,16]+y[7,16]))
y32 <- as.numeric(sum(y[8,16]+y[14,16]))
y33 <- as.numeric(sum(y[15,16]+y[21,16]))
y34 <- as.numeric(sum(y[22,16]+y[28,16]))
y35 <- as.numeric(sum(y[29,16]+y[35,16]))
y36 <- as.numeric(sum(y[36,16]+y[42,16]))

yy3 <- c(y31,y32,y33,y34,y35,y36)
lines(c(0,1/7,1/6,1/5,1/4,3/11),yy3,type = "l",lwd=8,col="#336464")

#Heterozygote
plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,0.3),ylim=c(0,1))
axis(1,c(0,1/7,1/5,3/11),c(0,1/7,1/5,3/11),lwd=1,cex.axis=2,labels = c("0","1/7","1/5","3/11"),tick = TRUE,mgp = c(0.3, 1.5, 0))
axis(2,seq(0,1,0.2),seq(0,1,0.2),lwd=1,cex.axis=2,mgp = c(0.3, 1.5, 0))


y41 <- as.numeric(sum(y[2,16]+y[3,16]+y[4,16]+y[5,16]+y[6,16]))
y42 <- as.numeric(sum(y[9,16]+y[10,16]+y[11,16]+y[12,16]+y[13,16]))
y43 <- as.numeric(sum(y[16,16]+y[17,16]+y[18,16]+y[19,16]+y[20,16]))
y44 <- as.numeric(sum(y[23,16]+y[24,16]+y[25,16]+y[26,16]+y[27,16]))
y45 <- as.numeric(sum(y[30,16]+y[31,16]+y[32,16]+y[33,16]+y[34,16]))
y46 <- as.numeric(sum(y[37,16]+y[38,16]+y[39,16]+y[40,16]+y[41,16]))

yy4 <- c(y41,y42,y43,y44,y45,y46)
lines(c(0,1/7,1/6,1/5,1/4,3/11),yy4,type = "l",lwd=8,col="#1f9fa1")

