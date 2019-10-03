S0 = 10;
a=-0.1;
b=0.2;
r=0.1;
N=5;
p=(b-r)/(b-a);
k = 15;
Ct = 0;
def call(n,S,a,b,r,N,k):
    if n==N: 
        return max(0,S-k)
    else:
        return (1/(1+r))*( p*call((n+1),(S*(1+a)),a,b,r,N,k) + (1-p)*call((n+1),(S*(1+b)),a,b,r,N,k) )
call(0,S0,a,b,r,N,k)
