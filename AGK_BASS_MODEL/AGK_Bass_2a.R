#BASS MODEL
FF = expression(p*(exp((p+q)*t)-1)/(p*exp((p+q)*t)+q))
print(FF)

## expression(p * (exp((p + q) * t) - 1)/(p * exp((p + q) * t) + 
##     q))

#Take derivative
ff = D(FF,"t")
print(ff)

## p * (exp((p + q) * t) * (p + q))/(p * exp((p + q) * t) + q) - 
##     p * (exp((p + q) * t) - 1) * (p * (exp((p + q) * t) * (p + 
##         q)))/(p * exp((p + q) * t) + q)^2

#SET UP THE FUNCTION
ff = function(p,q,t) {
  res = D(FF,"t")
}

#NOTE THE USE OF eval
m=100000; p=0.01; q=0.20; t=seq(1,20)
plot(t,m*eval(ff(p,q,t)),type="l",col="red",lwd=3)
grid(lwd=2)