S0=10
a=-0.1
b=0.2
r=0.1
N=2
p=(b-r)/(b-a)
nsim=10000
hN=rep(0,nsim)
for (i in 1:nsim){
  s=rep(0,N+1)
  s[1]=S0
  for (k in 2:(N+1)){
    bern=rbinom(1,1,p)
    s[k]=s[k-1]*(bern*(1+a)+(1-bern)*(1+b))
  }
  hN[i]=s[N+1]-min(s)
}
price=1/(1+r)**N*mean(hN)

print(price)