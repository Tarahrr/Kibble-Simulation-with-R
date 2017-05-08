#--------------------TARA HARIRI--------------------
#---------------------RANDOM WALK_KIBBLE----------------------
ro=.75
alfa=2
#Y|X
kibble1=function(x,y) {(1/(1-ro))*((y/(x* ro))^((alfa-1)/2))*exp(-(ro*x+y)/(1-ro))*besselI((2*sqrt(ro*x*y))/(1-ro),(alfa-1))}
#X|y
kibble2=function(x,y){ (1/(1-ro))*((x/(y* ro))^((alfa-1)/2))*exp(-(ro*y+x)/(1-ro))*besselI((2*sqrt(ro*x*y))/(1-ro),(alfa-1))}

#----------y| be sharte x --------------------
y.x=function(ee,x){
i=2
zz=c()
for(i in 1:1000){
y=abs(ee+rnorm(1,0,2))
a=min((kibble1(x,y))/(kibble1(x,ee)),1)
u=runif(1)
if(u<a ){
 zz[i]=y
 ee=y	
}
else{
zz[i]= ee

}
}
mean(zz[500:1000])
}
#----------x| be sharte y --------------------
y=rgamma(1,alfa,1)
x.y=function(ee,v){
i=2
zz=c()
for(i in 1:1000){
y=abs(ee+rnorm(1,0,2))
a=min((kibble2(y,v))/(kibble2(ee,v)),1)
u=runif(1)
if(u<a ){
 zz[i]=y
 ee=y	
}
else{
zz[i]= ee
}
}
mean(zz[500:1000])
}
#----------------------Main Function------------
f=function(n){
y=c()
x=c()
z=c()
y[1]=1
x[1]=x.y(1.5,y[1])
for(i in 2:n){
y[i]=y.x(y[i-1],x[i-1])
x[i]=x.y(x[i-1],y[i])
}
cbind(x[5000:n],y[5000:n])
}
z=f(10000)
cor(z)
acf(z)
