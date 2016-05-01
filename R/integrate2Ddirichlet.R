# integrate a dirichlet with small alphas
y = 1
x = 1
a1 = 1/60
a2 = 1/60
a3 = 1/60
integral = integrate(function(x) {
    sapply(x, function(x) {
        integrate(function(y) ((x)^(a1-1))*((y)^(a2-1))*((1-x-y)^(a3-1))*(gamma(a1+a2+a3)/(gamma(a1)*gamma(a2)*gamma(a3))) , lower = 0.0, upper = 1 - x)$value
        })
    }, lower = 0.0, upper = 1)
integral

