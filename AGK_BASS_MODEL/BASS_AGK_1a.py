f = function(p,q,t) {
    res = (exp((p+q)*t)*p*(p+q)^2)/(p*exp((p+q)*t)+q)^2
}
t = seq(1,20)
m = 100000
p = 0.01
q = 0.20
plot(t,m*f(p,q,t),type="l",col="blue",lwd=3,xlab="Time (years)",ylab="Adoptions")
grid(lwd=2)