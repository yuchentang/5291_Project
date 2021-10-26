X = arima.sim(list(ar=-0.8, ma = 0.1),200)
plot.ts(X)
a = acf(X)
a


Y = arima.sim(list(ma = 0.8),200)
plot.ts(Y)
a = acf(Y)
a


t = 1:200  
Y = arima.sim(list(ar =0.5, ma = 0.5), 200)
X = 5 + 0.2* t + Y
plot.ts(X)
## mu_t = beta_0 + beta_1 * t
lm(X ~ 1 + t)



t = 1:200  
Y = arima.sim(list(ar =0.5, ma = 0.5), 200)
X = 10* sin(2* pi * 0.02* t) + Y
plot.ts(X)
## mu_t = alpha1 * sin(2*pi*w* t) +
##        alpha2 * cos(2*pi*w*t)
predictor1 = sin(2*pi*0.02 * t) 
predictor2 = cos(2*pi*0.02 * t) 

fit.sin_cos = lm(X ~ 0 + predictor1 + predictor2)
fit.sin_cos$fitted.values
Y.hat = X - fit.sin_cos$fitted.values
plot.ts(X)
points(t, fit.sin_cos$fitted.values, col='red')
plot.ts(Y.hat)


t = 1:200  
Y = arima.sim(list(ar =0.5, ma = 0.5), 200)
X = 100 + 0.2 *t +10* sin(2* pi * 0.02* t) + Y
plot.ts(X)
## mu_t = beta_0  + beta_1 *t
##        alpha1 * sin(2*pi*w* t) +
##        alpha2 * cos(2*pi*w*t)

predictor1 = sin(2*pi*0.02 * t) 
predictor2 = cos(2*pi*0.02 * t) 
predictor3 = t


fit  = lm(X ~ predictor1 + predictor2 + predictor3)
fit$fitted.values
fit
Y.hat = X - fit$fitted.values
plot.ts(X)
points(t, fit$fitted.values, col='red')
plot.ts(Y.hat)




t = 1:200  
Y = arima.sim(list(ar =0.5, ma = 0.5), 200)
X = 100 + 0.2 *t +10* sin(2* pi * 0.02* t) + Y
plot.ts(X)

### Moving-average smoother 
?filter
mas5 =filter(X, filter = rep(1/5,5), sides=2)
mas11 =filter(X, filter = rep(1/101,101), sides=2)
plot.ts(X)
points(mas5,col='red')
points(mas11,col='blue')
plot.ts(X-mas5)

### Kernel smoothing
?ksmooth
ks10 = ksmooth(t, X, kernel = 'normal', bandwidth = 10)
ks20 = ksmooth(t, X, kernel = 'normal', bandwidth = 20)
ks5 = ksmooth(t, X, kernel = 'normal', bandwidth = 5)

plot.ts(X)
points(ks10, col= 'red', cex = 0.4)
points(ks20, col= 'blue', cex = 0.4)
points(ks5, col= 'green', cex = 0.4)
