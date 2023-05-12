# Sea F(y) = 0 si y < 0, F(y) = y si 0 <= y <= 1, F(y) = 1 si y>1. 
#1) Hallar la f.d.p. f
# Para hallar f dada la función acumulada F(x) se deriva parte por parte y queda: 
# f(y) = 0 si y < 0 
# f(y) = 1 para 0 <= y <= 1
# f(y) = 0 para y > 1

# Para hallar E (Y) la Esperanza matemática de Y se integra y*f en el dominio. 
# Definimos la función a integrar: 
f1 = function(y) {y*f}
integrate(f1, 0, 1)
# Da por resultado 0.5

# Hallar la Varianza de Y (es decir, la V(Y) = E(Y-media)^2) = integral de (Y-media)^2*f en el dominio.
# Para eso definimos la función a integrar
f2 = function(y) {(y-.5)^2*1} 
integrate(f2, 0,1)

#2 Para hallar P(0.25 < Y < 0.5), como f(y) = 1 en [0,1] entonces, el área bajo la recta horizontal buscada es 
(0.5 - 0.25)*1
#0.25
# Percentil 90 es hallar la y tal que F(y) = 0.90. En este caso la respuesta es
y = 0.90
# porque la integral de 0 a 0.9 de la función f(y) = 1 da 0.90

# E(g(Y)) = integral de g(y)*f(y) en el dominio.
# Primero definimos la función a integrar:
y3 = function(y) {100*y^2*1}
integrate(y3, 0, 1)
# 33.33






