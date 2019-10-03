# americain 
payoff = function(S0,a,b,j,N,K){
  S = S0*((1+a)^j)*(1+b)^(N-j);
  return ((K-S)*(S<K))
}

S0 = 20;
a=-0.8;
b=0.8;
r=0.1;
N=2;
p=(b-r)/(b-a);
k = 18;
Ct = 0;


h <- matrix(nrow = N+1,ncol = N+1);

for(i in 1:(N+1)){
  for(j in 1:i){
    h[j,i] = payoff(S0,a,b,i-1,j-1,k)}}

U = matrix(nrow = N+1,ncol = N+1);
U[,N+1]<- h[,N+1]

for(i in seq(N,1,by = -1)){
  for(j in 1:i){
    U[j,i] = max((1/(1+r))*(p*U[j+1,i+1]+(1-p)*U[j,i+1]),h[j,i])
  }
}
U0 = U[1,1]
print(U)


Phi0 = matrix(nrow = N,ncol = N);
Phi1 = matrix(nrow = N,ncol = N);
for(i in seq(N,1,by = -1)){
  for(j in 1:i){
    Phi1[j,i] = (U[j+1,i+1]-U[j,i+1])/((a-b)*S0*((1+a)^(j-1))*(1+b)^(i-j))
    Phi0[j,i] = 1-Phi1[j,i]
  }
  
}
m =1;
callSim =seq(1,N+1, by = 1)
phiSim = seq(1,N, by = 1)
callSim[1] = U[1,1]
phiSim[1] = Phi1[1,1]
prixSim = seq(1,N+1, by = 1)
prixSim[1] = S0
for(i in 1:N){
  bern = rbinom(1,1,p);
  m=m+bern;
  prixSim[i+1] = (bern*(1+a)+(1-bern)*(1+b))*prixSim[i]
  callSim[i+1] = U[m,i+1]
  if(i<N){
    phiSim[i+1] = Phi1[m,i+1]
  }
}
matplot((1:N),cbind(prixSim[1:N],callSim[1:N],phiSim),type='l',col=c('blue','red','green'))