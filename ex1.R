# Question 1
# Fonction h
h <- function(x, S0, K) {
  payoff = max(S0*exp(x) - K, 0)
  return(payoff)
}
# Fonction f
f <- function(x, t, r, S0, K) {
  return(h(x + r*t, S0, K))
}
# Fonction g
g <- function(y, mu, delta, lambda) {
  numerateur = lambda * exp(-(y - mu)(y - mu) / (2 * delta * delta))
  quotient = numerateur / (delta * sqrt(2*pi))
  return(quotient)
}

# Question 2 

# Fonction de creation de matrices tridiagonales
tridiag <- function(taille, valeurUp, valeurDown, valeurDiag){
  M <- matrix(0, taille, taille)
  diag(M) <- valeurDiag
  for (i in 1:taille - 1) {
    M[i, i + 1] = valeurUp
    M[i+1, i] = valeurDown
  }
  return(M)
}

calculSchema <- function(nbPoints, Al, deltaX, deltaT) {
  r = 0.04
  maturity = 1
  spot = 1
  strike = 1
  sigma = 0.15
  # Initialisation du vecteur u
  u <- cbind(rep(0, nbPoints))
  for (i in nbPoints:1) {
    u[nbPoints-i] = h(Al + (i * deltaX), spot, strike)
  }
  # Creation de la matrice A 
  diagSup = -(sigma * sigma * deltaT) / (2 * deltaX * deltaX)
  diag = 1 - (2 * diagSup)
  A <- tridiag(nbPoints, diagSup, diagSup, diag)
  # Creation du vecteur B
  diagSup = ((sigma * sigma / 2) - r) * deltaT / deltaX
  diag = 1 - diagSup
  C <- tridiag(nbPoints, diagSup, 0, diag)
  # Resolution de Ax = B
  nbDates = maturity/deltaT
  for (j in 1:nbDates) {
    B = C %*% u
    t = solve(a = A, b = B)
    u = t
  }
  v = rev(u) * exp(-r*maturity)
  print(length(v))
  #x = c(seq(from = 0, by = 0.01, to = 0.99))
  plot(v, type="l")
}














