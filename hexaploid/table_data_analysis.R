geno_up3 <- list()
pos      <- NULL
col_names <- colnames(geno_up2)

for(i in 1:dim(geno_up2)[2])
{
  snp <- as.numeric(geno_up2[,i])
  n_na_index <- which(snp != -1)
  geno_up3   <- c(geno_up3, list(snp[n_na_index]))
  pos <- c(pos,i)
}
names(geno_up3) <- col_names

#------------------table1=recursive model----------------------

for(j in 1:length(geno_up3)){
  snp <- geno_up3[[m]]
  n00 <- length(which(snp == 2))
  n21 <- length(which(snp == 1))
  n22 <- length(which(snp == 0))
  
  sample_n <- n00+n21+n22
  
  obs_00 <- n00/sample_n
  obs_21 <- n21/sample_n
  obs_22 <- n22/sample_n
  yww <- c( obs_22, (obs_21/5) ,(obs_21/5), (obs_21/5) ,(obs_21/5), (obs_21/5) ,obs_00)
  
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
  
  R22 = m[9];
  R21 = n[9]+k[9]+e[9]+h[9]+f[9];
  R00 = u[9]

}

#------------------table2=allele-based model----------------------

P_allele <- c()
MP_allele <- c()
for(i in 1:1000 ){
  
  snp <- geno_up3[[m]]
  n00 <- length(which(snp == 2))
  n21 <- length(which(snp == 1))
  n22 <- length(which(snp == 0))
  
  sample_n <- n00+n21+n22
  
  obs_00 <- n00/sample_n
  obs_21 <- n21/sample_n
  obs_22 <- n22/sample_n
  
  p=(6*obs_22+3*obs_21)/6
  q=(6*obs_00+3*obs_21)/6
  
  P222 <- p^6
  Phe <- 6*p^5*q+15*p^4*q^2+20*p^3*q^3+15*p^2*q^4+6*p*q^5
  P000 <- q^6
  
  prob <- c(P222,Phe,P000)
  
  index <- sample(c(1,2,3),1000,replace = T,prob = prob)
  
  NN <- rep(0,3)
  st <- table(index)
  NNn <- as.numeric(names(st))
  NN[NNn] <- as.numeric(st)
  n3 <- NN[1]; n2 <- NN[2]; n1 <- NN[3]
  N=n3+n2+n1
  
  fai1 <- (30*p^5*q+60*p^4*q^2+60*p^3*q^3+30*p^2*q^4+6*p*q^5)/(6*p^5*q+15*p^4*q^2+20*p^3*q^3+15*p^2*q^4+6*p*q^5)
  fai2 <- (6*p^5*q+30*p^4*q^2+60*p^3*q^3+60*p^2*q^4+30*p*q^5)/(6*p^5*q+15*p^4*q^2+20*p^3*q^3+15*p^2*q^4+6*p*q^5)
  
  p=(6*n3+fai1*n2)/(6*sum(N))
  q=(fai2*n2+6*n1)/(6*sum(N))
  
  P_allele <- rbind(P_allele,c(p,q))
  
}   
MP_allele <- colMeans(P_allele)

exp_p <- MP_allele[1]
exp_q <- MP_allele[2]

exp_P222 <- exp_p^6
exp_Phe <- 6*exp_p^5*exp_q+15*exp_p^4*exp_q^2+20*exp_p^3*exp_q^3+15*exp_p^2*exp_q^4+6*exp_p*exp_q^5
exp_P000 <- exp_q^6

#=====================table3=double reduction==================

yww <- c()
for(j in 1){
  
  snp <- geno_up3[[m]]
  n00 <- length(which(snp == 2))
  n21 <- length(which(snp == 1))
  n22 <- length(which(snp == 0))
  
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
    
    P222=c((n22/sample_n))[j]
    P000=c((n00/sample_n))[j]
    Phe  =c((n21/sample_n))[j]
   
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

m <- as.numeric(yww[1])
n <- as.numeric(yww[2])
k <- as.numeric(yww[3])
e <- as.numeric(yww[4])
h <- as.numeric(yww[5])
f <- as.numeric(yww[6])
u <- as.numeric(yww[7])

alpha=0

R7 <- (m^2*1+2*m*n*(1/2+1/6*alpha)+2*m*k*(1/5+1/5*alpha)+2*m*e*(1/20+3/20*alpha)+2*m*h*(1/15*alpha)+2*m*f*0+2*m*u*0
       +n^2*(1/4+1/36*alpha^2+1/6*alpha)+2*n*k*(1/10+1/30*alpha^2+2/15*alpha)+2*n*e*(1/40+1/40*alpha^2+1/12*alpha)+2*n*h*(1/90*alpha^2+1/30*alpha)+2*n*f*0+2*n*u*0
       +k^2*(1/25+1/25*alpha^2+2/25*alpha)+2*k*e*(1/100+3/100*alpha^2+1/25*alpha)+2*k*h*(1/75*alpha^2+1/75*alpha)+2*k*f*0+2*k*u*0
       +e^2*(1/400+9/400*alpha^2+3/200*alpha)+2*e*h*(1/100*alpha^2+1/300*alpha)+2*e*f*0+2*e*u*0
       +h^2*(1/225*alpha^2)+2*h*f*0+2*h*u*0
       +f^2*0+2*f*u*0
       +u^2*0)

R6 <- (m^2*0+2*m*n*(1/2-1/3*alpha)+2*m*k*(3/5-1/3*alpha)+2*m*e*(9/20-3/20*alpha)+2*m*h*(1/5+1/15*alpha)+2*m*f*(1/6*alpha)+2*m*u*0
       +n^2*(1/2-1/9*alpha^2-1/6*alpha)+2*n*k*(2/5-11/90*alpha^2-1/30*alpha)+2*n*e*(1/4-3/40*alpha^2+7/120*alpha)+2*n*h*(1/10-1/90*alpha^2+1/10*alpha)+2*n*f*(1/36*alpha^2+1/12*alpha)+2*n*u*0
       +k^2*(6/25-2/15*alpha^2+8/75*alpha)+2*k*e*(3/25-2/25*alpha^2+2/15*alpha)+2*k*h*(1/25-2/225*alpha^2+7/75*alpha)+2*k*f*(1/30*alpha^2+1/30*alpha)+2*k*u*0
       +e^2*(9/200-9/200*alpha^2+3/25*alpha)+2*e*h*(1/100+19/300*alpha)+2*e*f*(1/40*alpha^2+1/120*alpha)+2*e*u*0
       +h^2*(2/225*alpha^2+2/75*alpha)+2*h*f*(1/90*alpha^2)+2*h*u*0
       +f^2*0+2*f*u*0
       +u^2*0)  

R5 <- (m^2*0+2*m*n*(1/6*alpha)+2*m*k*(1/5+1/15*alpha)+2*m*e*(9/20-3/20*alpha)+2*m*h*(3/5-1/3*alpha)+2*m*f*(1/2-1/3*alpha)+2*m*u*0
       +n^2*(1/4+1/6*alpha^2-1/6*alpha)+2*n*k*(2/5+7/45*alpha^2-4/15*alpha)+2*n*e*(9/20+1/20*alpha^2-13/60*alpha)+2*n*h*(2/5-1/15*alpha^2-1/10*alpha)+2*n*f*(1/4-1/9*alpha^2)+2*n*u*0
       +k^2*(11/25+31/225*alpha^2-22/75*alpha)+2*k*e*(37/100+3/100*alpha^2-11/75*alpha)+2*k*h*(6/25-19/225*alpha^2+1/25*alpha)+2*k*f*(1/10-11/90*alpha^2+2/15*alpha)+2*k*u*0
       +e^2*(99/400-9/400*alpha^2-3/200*alpha)+2*e*h*(3/25-7/100*alpha^2+31/300*alpha)+2*e*f*(1/40-3/40*alpha^2+2/15*alpha)+2*e*u*0
       +h^2*(1/25-9/225*alpha^2+8/75*alpha)+2*h*f*((-1/90)*alpha^2+1/15*alpha)+2*h*u*0
       +f^2*(1/36*alpha^2)+2*f*u*0
       +u^2*0)  

R4 <- (m^2*0+2*m*n*0+2*m*k*(1/15*alpha)+2*m*e*(1/20+3/20*alpha)+2*m*h*(1/5+1/5*alpha)+2*m*f*(1/2+1/6*alpha)+2*m*u*1
       +n^2*((-1/9)*alpha^2+1/6*alpha)+2*n*k*(1/10-1/15*alpha^2+1/10*alpha)+2*n*e*(1/4+1/20*alpha^2-1/15*alpha)+2*n*h*(2/5+7/45*alpha^2-1/5*alpha)+2*n*f*(1/2+1/6*alpha^2-1/6*alpha)+2*n*u*(1/2+1/6*alpha)
       +k^2*(6/25-4/225*alpha^2-2/75*alpha)+2*k*e*(37/100+2/25*alpha^2-59/300*alpha)+2*k*h*(11/25+4/25*alpha^2-22/75*alpha)+2*k*f*(2/5+7/45*alpha^2-1/5*alpha)+2*k*u*(1/5+1/5*alpha)
       +e^2*(41/100+9/100*alpha^2-6/25*alpha)+2*e*h*(37/100+2/25*alpha^2-59/300*alpha)+2*e*f*(1/4+1/20*alpha^2-1/15*alpha)+2*e*u*(1/20+3/20*alpha)
       +h^2*(6/25-4/225*alpha^2-2/75*alpha)+2*h*f*(1/10-1/15*alpha^2+1/10*alpha)+2*h*u*(1/15*alpha)
       +f^2*((-1/9)*alpha^2+1/6*alpha)+2*f*u*0
       +u^2*0)  

R3 <- (m^2*0+2*m*n*0+2*m*k*0+2*m*e*0+2*m*h*0+2*m*f*0+2*m*u*0
       +n^2*(1/36*alpha^2)+2*n*k*((-1/90)*alpha^2+1/15*alpha)+2*n*e*(1/40-3/40*alpha^2+2/15*alpha)+2*n*h*(1/10-11/90*alpha^2+2/15*alpha)+2*n*f*(1/4-1/9*alpha^2)+2*n*u*(1/2-1/3*alpha)
       +k^2*(1/25-9/225*alpha^2+8/75*alpha)+2*k*e*(3/25-7/100*alpha^2+31/300*alpha)+2*k*h*(6/25-19/225*alpha^2+1/25*alpha)+2*k*f*(2/5-1/15*alpha^2-1/10*alpha)+2*k*u*(3/5-1/3*alpha)
       +e^2*(99/400-9/400*alpha^2-3/200*alpha)+2*e*h*(37/100+3/100*alpha^2-11/75*alpha)+2*e*f*(9/20+1/20*alpha^2-13/60*alpha)+2*e*u*(9/20-3/20*alpha)
       +h^2*(11/25+31/225*alpha^2-22/75*alpha)+2*h*f*(2/5+7/45*alpha^2-4/15*alpha)+2*h*u*(1/5+1/15*alpha)
       +f^2*(1/4+1/6*alpha^2-1/6*alpha)+2*f*u*(1/6*alpha)
       +u^2*0) 

R2 <- (m^2*0+2*m*n*0+2*m*k*0+2*m*e*0+2*m*h*0+2*m*f*0+2*m*u*0
       +n^2*0+2*n*k*(1/90*alpha^2)+2*n*e*(1/40*alpha^2+1/120*alpha)+2*n*h*(1/30*alpha^2+1/30*alpha)+2*n*f*(1/36*alpha^2+1/12*alpha)+2*n*u*(1/6*alpha)
       +k^2*(2/225*alpha^2+2/75*alpha)+2*k*e*(1/100+19/300*alpha)+2*k*h*(1/25-2/225*alpha^2+7/75*alpha)+2*k*f*(1/10-1/90*alpha^2+1/10*alpha)+2*k*u*(1/5+1/15*alpha)
       +e^2*(9/200-9/200*alpha^2+3/25*alpha)+2*e*h*(3/25-2/25*alpha^2+2/15*alpha)+2*e*f*(1/4-3/40*alpha^2+7/120*alpha)+2*e*u*(9/20-3/20*alpha)
       +h^2*(6/25-2/15*alpha^2+8/75*alpha)+2*h*f*(2/5-11/90*alpha^2-1/30*alpha)+2*h*u*(3/5-1/3*alpha)
       +f^2*(1/2-1/9*alpha^2-1/6*alpha)+2*f*u*(1/2-1/3*alpha)
       +u^2*0)

R1 <- (m^2*0+2*m*n*0+2*m*k*0+2*m*e*0+2*m*h*0+2*m*f*0+2*m*u*0
       +n^2*0+2*n*k*0+2*n*e*0+2*n*h*0+2*n*f*0+2*n*u*0
       +k^2*(1/225*alpha^2)+2*k*e*(1/100*alpha^2+1/300*alpha)+2*k*h*(1/75*alpha^2+1/75*alpha)+2*k*f*(1/90*alpha^2+1/30*alpha)+2*k*u*(1/15*alpha)
       +e^2*(1/400+9/400*alpha^2+3/200*alpha)+2*e*h*(1/100+3/100*alpha^2+1/25*alpha)+2*e*f*(1/40+1/40*alpha^2+1/12*alpha)+2*e*u*(1/20+3/20*alpha)
       +h^2*(1/25+1/25*alpha^2+2/25*alpha)+2*h*f*(1/10+1/30*alpha^2+2/15*alpha)+2*h*u*(1/5+1/5*alpha)
       +f^2*(1/4+1/36*alpha^2+1/6*alpha)+2*f*u*(1/2+1/6*alpha)
       +u^2*1)

exp <- c(R7,R6,R5,R4,R3,R2,R1)

exp_22 <- exp[1]
exp_21 <- exp[2]+exp[3]+exp[4]+exp[5]+exp[6]
exp_00 <- exp[7]


