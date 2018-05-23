## On pose lambda = 3*lambdak

## taux d'utilisation
## <=> rho
tauxUtilisation = function(lambda,mu,M){
  return((lambda)/(M*mu))
}

## Mesures intermédiaires

## Probabilité qu'il y ait zéro requête dans le système
## <=> P0
pZeroRequete = function(lambda,mu,M){
  somme = 0;
  for (n in 0:(M-1)) {
    fact = factorial(n)
    up = (lambda/mu)
    up2 = up^n
    somme = somme + (up2/fact)
  }
  part2 = ((lambda/mu)^M)/(factorial(M)*(1-(lambda/(M*mu))))
  return(1/(somme+part2))
}

## Nombre moyen de requêtes en file
## <=> nl
requeteFile = function(lambda,mu,M,P0){
  return(((lambda*mu*((lambda/mu)^M))/(factorial(M-1)*((M*mu-lambda)^2)))*P0)
}

## A

A = function(lambda,rho,nl){
  return((lambda)/((1-rho)*nl))
}

## B
## B0 = 1

B = function(lambdak,mu,M,k){
  somme = 0;
  for (c in 1:k) {
    somme = somme + (lambdak/(M*mu)); 
  }
  return (1-somme)
}

##Temps moyen d'attente en file pour les requêtes de la priorité k
## <=> tk
## Bk1 = Bk-1
attenteFile = function(A,Bk,Bk1){
  return(1/(A*Bk1*Bk))
}

## Temps moyen d'attente dans le système pour les requêtes de la priorité k
## <=> ts
attenteSystème = function(tk,mu){
  return(tk + (1/mu))
}

## Nombre moyen de requêtes de la priorité k qui attendent en file
## <=> nk

requeteAttente = function(lambdak,tk){
  return(lambdak*tk)
}


## Programme principal
lambda = 5
lambda1 = 2
lambda2 = 2
M = 6
mu = 1
rho = tauxUtilisation(lambda,mu,M)
p0 = pZeroRequete(lambda,mu,M)
nl = requeteFile(lambda,mu,M,p0)
a = A(lambda,rho,nl)
b0 = 1
b1 = B(lambda1,mu,M,1)
b2 = B(lambda2,mu,M,2)
tk1 = attenteFile(a,b0,b1)
tk2 = attenteFile(a,b1,b2)
ts1 = attenteSystème(tk1,mu)
ts2 = attenteSystème(tk2,mu)
n1 = requeteAttente(lambda1,tk1)
n2 = requeteAttente(lambda2,tk2)

