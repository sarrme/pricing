payoff = function(S0,a,b,j,N,K){
  S = S0*((1+a)^j)*(1+b)^(N-j);
  return ((S-K)*(S>K))
}

S0 = 10;
a=-0.1;
b=0.2;
r=0.1;
N=5;
p=(b-r)/(b-a);
k = 15;
Ct = 0;



  
  C = matrix(nrow = N+1,ncol = N+1);
  
  for(i in 1:(N+1)){
    C[i,N+1] = payoff(S0,a,b,i-1,N,k);
  }
  
  for(i in seq(N,1,by = -1)){
    for(j in 1:i){
      C[j,i] = (1/(1+r))*(p*C[j+1,i+1]+(1-p)*C[j,i+1]);
    }
  }
  #prime de l'option
  Ct = C[1,1];
  # Calcul de la matrice V
  print(C);

  #Stratégie de portefeuille
Phi0 = matrix(nrow = N,ncol = N);
Phi1 = matrix(nrow = N,ncol = N);
for(i in seq(N,1,by = -1)){
  for(j in 1:i){
    Phi1[j,i] = (C[j+1,i+1]-C[j,i+1])/((a-b)*S0*((1+a)^(j-1))*(1+b)^(i-j))
    Phi0[j,i] = 1-Phi1[j,i]
  }

}
  #Simulation
m =1;
callSim =seq(1,N+1, by = 1)
phiSim = seq(1,N, by = 1)
callSim[1] = C[1,1]
phiSim[1] = Phi1[1,1]
prixSim = seq(1,N+1, by = 1)
prixSim[1] = S0
for(i in 1:N){
  bern = rbinom(1,1,p);
  m=m+bern;
  prixSim[i+1] = (bern*(1+a)+(1-bern)*(1+b))*prixSim[i]
  callSim[i+1] = C[m,i+1]
  if(i<N){
  phiSim[i+1] = Phi1[m,i+1]
  }
}
matplot((1:N),cbind(prixSim[1:N],callSim[1:N],phiSim),type='l',col=c('blue','red','green'))
legend('topleft',legend = c("prix sous-jacent","valeur du call","Stratégie du portefeuille"),col=c('blue','red','green'),lty = 0:2, cex = 0.5)
  


