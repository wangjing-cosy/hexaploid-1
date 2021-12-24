#blank 1
rm(list=ls())
yww <- c( 29/120, 21/120 ,17/120, 10/120 ,10/120, 10/120 ,23/120 )

times <- seq(0,30,1)
N <- length(times)

m <- numeric(N)
n <- numeric(N)
k <- numeric(N)
e <- numeric(N)
h <- numeric(N)
f <- numeric(N)
u <- numeric(N)

m[1] <- as.numeric(yww[1])
n[1] <- as.numeric(yww[2])
k[1] <- as.numeric(yww[3])
e[1] <- as.numeric(yww[4])
h[1] <- as.numeric(yww[5])
f[1] <- as.numeric(yww[6])
u[1] <- as.numeric(yww[7])

alpha=0
freq <- c()
for(t in 1:(N-1)){
  
  m[t+1] <- (m[t]^2*1+2*m[t]*n[t]*(1/2+1/6*alpha)+2*m[t]*k[t]*(1/5+1/5*alpha)+2*m[t]*e[t]*(1/20+3/20*alpha)+2*m[t]*h[t]*(1/15*alpha)+2*m[t]*f[t]*0+2*m[t]*u[t]*0
             +n[t]^2*(1/4+1/36*alpha^2+1/6*alpha)+2*n[t]*k[t]*(1/10+1/30*alpha^2+2/15*alpha)+2*n[t]*e[t]*(1/40+1/40*alpha^2+1/12*alpha)+2*n[t]*h[t]*(1/90*alpha^2+1/30*alpha)+2*n[t]*f[t]*0+2*n[t]*u[t]*0
             +k[t]^2*(1/25+1/25*alpha^2+2/25*alpha)+2*k[t]*e[t]*(1/100+3/100*alpha^2+1/25*alpha)+2*k[t]*h[t]*(1/75*alpha^2+1/75*alpha)+2*k[t]*f[t]*0+2*k[t]*u[t]*0
             +e[t]^2*(1/400+9/400*alpha^2+3/200*alpha)+2*e[t]*h[t]*(1/100*alpha^2+1/300*alpha)+2*e[t]*f[t]*0+2*e[t]*u[t]*0
             +h[t]^2*(1/225*alpha^2)+2*h[t]*f[t]*0+2*h[t]*u[t]*0
             +f[t]^2*0+2*f[t]*u[t]*0
             +u[t]^2*0)
  
  n[t+1] <- (m[t]^2*0+2*m[t]*n[t]*(1/2-1/3*alpha)+2*m[t]*k[t]*(3/5-1/3*alpha)+2*m[t]*e[t]*(9/20-3/20*alpha)+2*m[t]*h[t]*(1/5+1/15*alpha)+2*m[t]*f[t]*(1/6*alpha)+2*m[t]*u[t]*0
             +n[t]^2*(1/2-1/9*alpha^2-1/6*alpha)+2*n[t]*k[t]*(2/5-11/90*alpha^2-1/30*alpha)+2*n[t]*e[t]*(1/4-3/40*alpha^2+7/120*alpha)+2*n[t]*h[t]*(1/10-1/90*alpha^2+1/10*alpha)+2*n[t]*f[t]*(1/36*alpha^2+1/12*alpha)+2*n[t]*u[t]*0
             +k[t]^2*(6/25-2/15*alpha^2+8/75*alpha)+2*k[t]*e[t]*(3/25-2/25*alpha^2+2/15*alpha)+2*k[t]*h[t]*(1/25-2/225*alpha^2+7/75*alpha)+2*k[t]*f[t]*(1/30*alpha^2+1/30*alpha)+2*k[t]*u[t]*0
             +e[t]^2*(9/200-9/200*alpha^2+3/25*alpha)+2*e[t]*h[t]*(1/100+19/300*alpha)+2*e[t]*f[t]*(1/40*alpha^2+1/120*alpha)+2*e[t]*u[t]*0
             +h[t]^2*(2/225*alpha^2+2/75*alpha)+2*h[t]*f[t]*(1/90*alpha^2)+2*h[t]*u[t]*0
             +f[t]^2*0+2*f[t]*u[t]*0
             +u[t]^2*0)  
  
  k[t+1] <- (m[t]^2*0+2*m[t]*n[t]*(1/6*alpha)+2*m[t]*k[t]*(1/5+1/15*alpha)+2*m[t]*e[t]*(9/20-3/20*alpha)+2*m[t]*h[t]*(3/5-1/3*alpha)+2*m[t]*f[t]*(1/2-1/3*alpha)+2*m[t]*u[t]*0
             +n[t]^2*(1/4+1/6*alpha^2-1/6*alpha)+2*n[t]*k[t]*(2/5+7/45*alpha^2-4/15*alpha)+2*n[t]*e[t]*(9/20+1/20*alpha^2-13/60*alpha)+2*n[t]*h[t]*(2/5-1/15*alpha^2-1/10*alpha)+2*n[t]*f[t]*(1/4-1/9*alpha^2)+2*n[t]*u[t]*0
             +k[t]^2*(11/25+31/225*alpha^2-22/75*alpha)+2*k[t]*e[t]*(37/100+3/100*alpha^2-11/75*alpha)+2*k[t]*h[t]*(6/25-19/225*alpha^2+1/25*alpha)+2*k[t]*f[t]*(1/10-11/90*alpha^2+2/15*alpha)+2*k[t]*u[t]*0
             +e[t]^2*(99/400-9/400*alpha^2-3/200*alpha)+2*e[t]*h[t]*(3/25-7/100*alpha^2+31/300*alpha)+2*e[t]*f[t]*(1/40-3/40*alpha^2+2/15*alpha)+2*e[t]*u[t]*0
             +h[t]^2*(1/25-9/225*alpha^2+8/75*alpha)+2*h[t]*f[t]*((-1/90)*alpha^2+1/15*alpha)+2*h[t]*u[t]*0
             +f[t]^2*(1/36*alpha^2)+2*f[t]*u[t]*0
             +u[t]^2*0)  
  
  e[t+1] <- (m[t]^2*0+2*m[t]*n[t]*0+2*m[t]*k[t]*(1/15*alpha)+2*m[t]*e[t]*(1/20+3/20*alpha)+2*m[t]*h[t]*(1/5+1/5*alpha)+2*m[t]*f[t]*(1/2+1/6*alpha)+2*m[t]*u[t]*1
             +n[t]^2*((-1/9)*alpha^2+1/6*alpha)+2*n[t]*k[t]*(1/10-1/15*alpha^2+1/10*alpha)+2*n[t]*e[t]*(1/4+1/20*alpha^2-1/15*alpha)+2*n[t]*h[t]*(2/5+7/45*alpha^2-1/5*alpha)+2*n[t]*f[t]*(1/2+1/6*alpha^2-1/6*alpha)+2*n[t]*u[t]*(1/2+1/6*alpha)
             +k[t]^2*(6/25-4/225*alpha^2-2/75*alpha)+2*k[t]*e[t]*(37/100+2/25*alpha^2-59/300*alpha)+2*k[t]*h[t]*(11/25+4/25*alpha^2-22/75*alpha)+2*k[t]*f[t]*(2/5+7/45*alpha^2-1/5*alpha)+2*k[t]*u[t]*(1/5+1/5*alpha)
             +e[t]^2*(41/100+9/100*alpha^2-6/25*alpha)+2*e[t]*h[t]*(37/100+2/25*alpha^2-59/300*alpha)+2*e[t]*f[t]*(1/4+1/20*alpha^2-1/15*alpha)+2*e[t]*u[t]*(1/20+3/20*alpha)
             +h[t]^2*(6/25-4/225*alpha^2-2/75*alpha)+2*h[t]*f[t]*(1/10-1/15*alpha^2+1/10*alpha)+2*h[t]*u[t]*(1/15*alpha)
             +f[t]^2*((-1/9)*alpha^2+1/6*alpha)+2*f[t]*u[t]*0
             +u[t]^2*0)  
  
  h[t+1] <- (m[t]^2*0+2*m[t]*n[t]*0+2*m[t]*k[t]*0+2*m[t]*e[t]*0+2*m[t]*h[t]*0+2*m[t]*f[t]*0+2*m[t]*u[t]*0
             +n[t]^2*(1/36*alpha^2)+2*n[t]*k[t]*((-1/90)*alpha^2+1/15*alpha)+2*n[t]*e[t]*(1/40-3/40*alpha^2+2/15*alpha)+2*n[t]*h[t]*(1/10-11/90*alpha^2+2/15*alpha)+2*n[t]*f[t]*(1/4-1/9*alpha^2)+2*n[t]*u[t]*(1/2-1/3*alpha)
             +k[t]^2*(1/25-9/225*alpha^2+8/75*alpha)+2*k[t]*e[t]*(3/25-7/100*alpha^2+31/300*alpha)+2*k[t]*h[t]*(6/25-19/225*alpha^2+1/25*alpha)+2*k[t]*f[t]*(2/5-1/15*alpha^2-1/10*alpha)+2*k[t]*u[t]*(3/5-1/3*alpha)
             +e[t]^2*(99/400-9/400*alpha^2-3/200*alpha)+2*e[t]*h[t]*(37/100+3/100*alpha^2-11/75*alpha)+2*e[t]*f[t]*(9/20+1/20*alpha^2-13/60*alpha)+2*e[t]*u[t]*(9/20-3/20*alpha)
             +h[t]^2*(11/25+31/225*alpha^2-22/75*alpha)+2*h[t]*f[t]*(2/5+7/45*alpha^2-4/15*alpha)+2*h[t]*u[t]*(1/5+1/15*alpha)
             +f[t]^2*(1/4+1/6*alpha^2-1/6*alpha)+2*f[t]*u[t]*(1/6*alpha)
             +u[t]^2*0) 
  
  f[t+1] <- (m[t]^2*0+2*m[t]*n[t]*0+2*m[t]*k[t]*0+2*m[t]*e[t]*0+2*m[t]*h[t]*0+2*m[t]*f[t]*0+2*m[t]*u[t]*0
             +n[t]^2*0+2*n[t]*k[t]*(1/90*alpha^2)+2*n[t]*e[t]*(1/40*alpha^2+1/120*alpha)+2*n[t]*h[t]*(1/30*alpha^2+1/30*alpha)+2*n[t]*f[t]*(1/36*alpha^2+1/12*alpha)+2*n[t]*u[t]*(1/6*alpha)
             +k[t]^2*(2/225*alpha^2+2/75*alpha)+2*k[t]*e[t]*(1/100+19/300*alpha)+2*k[t]*h[t]*(1/25-2/225*alpha^2+7/75*alpha)+2*k[t]*f[t]*(1/10-1/90*alpha^2+1/10*alpha)+2*k[t]*u[t]*(1/5+1/15*alpha)
             +e[t]^2*(9/200-9/200*alpha^2+3/25*alpha)+2*e[t]*h[t]*(3/25-2/25*alpha^2+2/15*alpha)+2*e[t]*f[t]*(1/4-3/40*alpha^2+7/120*alpha)+2*e[t]*u[t]*(9/20-3/20*alpha)
             +h[t]^2*(6/25-2/15*alpha^2+8/75*alpha)+2*h[t]*f[t]*(2/5-11/90*alpha^2-1/30*alpha)+2*h[t]*u[t]*(3/5-1/3*alpha)
             +f[t]^2*(1/2-1/9*alpha^2-1/6*alpha)+2*f[t]*u[t]*(1/2-1/3*alpha)
             +u[t]^2*0)
  
  u[t+1] <- (m[t]^2*0+2*m[t]*n[t]*0+2*m[t]*k[t]*0+2*m[t]*e[t]*0+2*m[t]*h[t]*0+2*m[t]*f[t]*0+2*m[t]*u[t]*0
             +n[t]^2*0+2*n[t]*k[t]*0+2*n[t]*e[t]*0+2*n[t]*h[t]*0+2*n[t]*f[t]*0+2*n[t]*u[t]*0
             +k[t]^2*(1/225*alpha^2)+2*k[t]*e[t]*(1/100*alpha^2+1/300*alpha)+2*k[t]*h[t]*(1/75*alpha^2+1/75*alpha)+2*k[t]*f[t]*(1/90*alpha^2+1/30*alpha)+2*k[t]*u[t]*(1/15*alpha)
             +e[t]^2*(1/400+9/400*alpha^2+3/200*alpha)+2*e[t]*h[t]*(1/100+3/100*alpha^2+1/25*alpha)+2*e[t]*f[t]*(1/40+1/40*alpha^2+1/12*alpha)+2*e[t]*u[t]*(1/20+3/20*alpha)
             +h[t]^2*(1/25+1/25*alpha^2+2/25*alpha)+2*h[t]*f[t]*(1/10+1/30*alpha^2+2/15*alpha)+2*h[t]*u[t]*(1/5+1/5*alpha)
             +f[t]^2*(1/4+1/36*alpha^2+1/6*alpha)+2*f[t]*u[t]*(1/2+1/6*alpha)
             +u[t]^2*1)
}
freq <- rbind(m,n,k,e,h,f,u)


#expect frequecy
R1 = m[8]
R2 = n[8]
R3 = k[8]
R4 = e[8]
R5 = h[8]
R6 = f[8]
R7 = u[8]

kf= (yww[1]-R1)^2/R1+(yww[2]-R2)^2/R2+(yww[3]-R3)^2/R3+(yww[4]-R4)^2/R4+(yww[5]-R5)^2/R5+(yww[6]-R6)^2/R6+(yww[7]-R7)^2/R7

#blank 2
rm(list=ls())
yww <- c()
for(j in 1){
  f3 <- c()
  f2 <- c()
  f1 <- c()
  f0 <- c()
  p3= 0.2; p2=0.35; p1=0.15; p0=0.3
  
  for(i in 1:30){
    p222=c(29/120 ,0.2 ,0.3 ,0.25,0.21,0.15)[j]
    p221=c(21/120,0.21,0.14,0.2 ,0.23,0.15)[j]
    p220=c(17/120,0.06,0.1 ,0.13,0.06,0.12)[j]
    p210=c(10/120,0.1 ,0.12,0.2 ,0.12,0.11)[j]
    p200=c(10/120,0.18,0.11,0.05,0.08,0.17)[j]
    p100=c(10/120,0.12,0.08,0.02,0.13,0.1)[j]
    p000=c(23/120,0.13,0.15,0.15,0.17,0.2)[j]
   
    fai6= 2*p3*p1/(p2^2+2*p3*p1)
    fai5= 2*p2^2/(p2^2+2*p3*p1)
    
    fai4= 2*p3*p0/(2*p3*p0+2*p2*p1)
    fai3= 2*p2*p1/(2*p3*p0+2*p2*p1)
    
    fai2= 2*p2*p0/(2*p2*p0+p1^2)
    fai1= 2*p1^2/(2*p2*p0+p1^2)
    
    p3=1/2*(2*p222+p221+fai6*p220+fai4*p210)
    p2=1/2*(p221+fai5*p220+fai3*p210+fai2*p200)
    p1=1/2*(fai6*p220+fai3*p210+fai1*p200+p100)
    p0=1/2*(fai4*p210+fai2*p200+p100+2*p000)
    
    f3 <- c(f3,p3)
    f2 <- c(f2,p2)
    f1 <- c(f1,p1)
    f0 <- c(f0,p0)
    
    p3 <- p3
    p2 <- p2
    p1 <- p1
    p0 <- p0
  }
  
  p3 <- f3[30];p2 <- f2[30];p1 <- f1[30];p0 <- f0[30]
  
  p222= (p3)^2
  p221=2*p3*p2
  p220=2*p3*p1+(p2)^2
  p210=2*p3*p0+2*p2*p1
  p200=2*p2*p0+(p1)^2
  p100=2*p1*p0
  p000=(p0)^2
  
  yww <-rbind(yww,c(p222,p221,p220,p210,p200,p100,p000))
}

times <- seq(0,20,1)
N <- length(times)

m <- numeric(N)
n <- numeric(N)
k <- numeric(N)
e <- numeric(N)
h <- numeric(N)
f <- numeric(N)
u <- numeric(N)

m[1] <- as.numeric(yww[1])
n[1] <- as.numeric(yww[2])
k[1] <- as.numeric(yww[3])
e[1] <- as.numeric(yww[4])
h[1] <- as.numeric(yww[5])
f[1] <- as.numeric(yww[6])
u[1] <- as.numeric(yww[7])

alpha=0

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
fre <- rbind(m,n,k,e,h,f,u)

obs=c( 29/120, 21/120 ,17/120, 10/120 ,10/120, 10/120 ,23/120 )
exp=as.numeric(fre[,8])
kf=(obs[1]-exp[1])^2/exp[1]+(obs[2]-exp[2])^2/exp[2]+(obs[3]-exp[3])^2/exp[3]+(obs[4]-exp[4])^2/exp[4]+(obs[5]-exp[5])^2/exp[5]+(obs[6]-exp[6])^2/exp[6]+(obs[7]-exp[7])^2/exp[7]

#blank3
rm(list = ls())
yww <- c()
for(j in 1){
  
  
  n00 <- 23
  n21 <- 21+17+10+10+10
  n22 <- 29
  
  sample_n <- n00+n21+n22
  
  obs_00 <- n00/sample_n
  obs_21 <- n21/sample_n
  obs_22 <- n22/sample_n
  
  p=(6*obs_22+3*obs_21)/6
  q=(6*obs_00+3*obs_21)/6
  
  fp <- c()
  fq <- c()
  
  allele <- c()
  for(i in 1:100){
    
    P222=c((n22/sample_n),0.04,0.13,0.2 ,0.14,0.1 ,0.15,0.06)[j]
    P000=c((n00/sample_n),0.14,0.15,0.15,0.17,0.2 ,0.11,0.08)[j]
    Phe  =c((n21/sample_n),0.82,0.72,0.65,0.69,0.7 ,0.74,0.86)[j]
    
    fai1 <- (30*p^5*q+60*p^4*q^2+60*p^3*q^3+30*p^2*q^4+6*p*q^5)/(6*p^5*q+15*p^4*q^2+20*p^3*q^3+15*p^2*q^4+6*p*q^5)
    fai2 <- (6*p^5*q+30*p^4*q^2+60*p^3*q^3+60*p^2*q^4+30*p*q^5)/(6*p^5*q+15*p^4*q^2+20*p^3*q^3+15*p^2*q^4+6*p*q^5)
    
    p=1/6*(6*P222+fai1*Phe)
    q=1/6*(fai2*Phe+6*P000)
    
    
    fp <- c(fp,p)
    fq <- c(fq,q)
    
    p <- p
    q <- q
    
    allele <- rbind(allele,c(p,q))
  }
  
  p <- allele[100,1];q <- allele[100,2]
  
  P222 <- p^6
  P221 <- 6*(p^5)*q
  P220 <- 15*(p^4)*(q^2)
  P210 <- 20*(p^3)*(q^3)
  P200 <- 15*(p^2)*(q^4)
  P100 <- 6*p*(q^5)
  P000 <- q^6
  
  Phe <- 6*(p^5)*q+15*(p^4)*(q^2)+20*(p^3)*(q^3)+15*(p^2)*(q^4)+6*p*(q^5)
  
  yww <-rbind(yww,c(P222,P221,P220,P210,P200,P100,P000))
  f<-rbind(p,q)
  
}

times <- seq(0,20,1)
N <- length(times)

m <- numeric(N)
n <- numeric(N)
k <- numeric(N)
e <- numeric(N)
h <- numeric(N)
f <- numeric(N)
u <- numeric(N)

m[1] <- as.numeric(yww[1])
n[1] <- as.numeric(yww[2])
k[1] <- as.numeric(yww[3])
e[1] <- as.numeric(yww[4])
h[1] <- as.numeric(yww[5])
f[1] <- as.numeric(yww[6])
u[1] <- as.numeric(yww[7])

alpha=0

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
fre <- rbind(m,n,k,e,h,f,u)

obs=c( 29/120, 21/120 ,17/120, 10/120 ,10/120, 10/120 ,23/120 )
exp=as.numeric(fre[,8])
kf=(obs[1]-exp[1])^2/exp[1]+(obs[2]-exp[2])^2/exp[2]+(obs[3]-exp[3])^2/exp[3]+(obs[4]-exp[4])^2/exp[4]+(obs[5]-exp[5])^2/exp[5]+(obs[6]-exp[6])^2/exp[6]+(obs[7]-exp[7])^2/exp[7]


