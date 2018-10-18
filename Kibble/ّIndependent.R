#-------------------------------TARA HARIRI --------------------------
#---------------------------------Kibble_Independent ----------------------------
ro=.75
alfa=2
#Y|X
kibble1=function(x,y) {(1/(1-ro))*((y/(x* ro))^((alfa-1)/2))*exp(-(ro*x+y)/(1-ro))*besselI((2*sqrt(ro*x*y))/(1-ro),(alfa-1))}
#X|y
kibble2=function(x,y){ (1/(1-ro))*((x/(y* ro))^((alfa-1)/2))*exp(-(ro*y+x)/(1-ro))*besselI((2*sqrt(ro*x*y))/(1-ro),(alfa-1))}
#----------------------------Y| in condition of x---------------------------
y.x=function(ee,x){
i=2
zz=c()
for(i in 1:1000){
X0=rgamma(1,2.8,1)
a=min((kibble1(x,X0)*dgamma(ee,3,1))/(kibble1(x,ee)*dgamma(X0,3,1)),1)
u=runif(1)
if(u<a){
zz[i]=X0
ee= X0
}
else{
zz[i]=ee
}
}
mean(zz[500:1000])
}
#----------------------------X| in condition of Y---------------------------

x.y=function(ee,v){
i=2
zz=c()
for(i in 1:1000){
X0 =rgamma(1,2.8,1)
a=min((kibble2(X0,v)*dgamma(ee,3,1))/(kibble2(ee,v)*dgamma(X0,3,1)),1)
u=runif(1)
if(u<a){
zz[i]=X0
ee= X0
}
else{
zz[i]=ee
}
}
mean(zz[500:1000])
}
#---------------------------Main function-----------------------------
f=function(n){
y=c()
x=c()
z=c()
y[1]=1
x[1]=x.y(0.5,y[1])
for(i in 2:n){
y[i]=y.x(y[i-1],x[i-1])
x[i]=x.y(x[i-1],y[i])
}
cbind(x[5000:n],y[5000:n])
}
z=f(10000)
cor(z)
acf(z)
