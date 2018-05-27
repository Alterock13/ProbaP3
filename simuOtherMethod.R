## Simulation de l'arrivée des requêtes : 

## Simulation des services : 

simulationServeur2 = function(lambda,mu,N,p1,p2,p3,duration){
  ## N : capacité maximale de stockage (en service + dans la file d'attente)
  ## p1,p2,p3 : proportion de priorités (p1+p2+p3 = 1)
  ## lambda : intensité, lambda>0
  ## 1/mu = temps de traitement, mu>0
  
  ## Dur?e de l'exp?rience
  
  totalTime   = duration   # duration of simulation
  expTime = 0 # simulation time
  
  ## Pour les arrivees
  k1 = 0 # nombre de requetes dans la queue de P1
  k2 = 0 # nombre de requetes dans la queue de P2
  k3 = 0 # nombre de requetes dans la queue de P3
  k = 0 # nombre de requetes totales 
  
  k1succ = 0 # nombre des requetes 1 trait?es successivement
  k2succ = 0 # nombre des requetes 2 trait?es successivement
  
  tempsArrivee = 0 # Temps pour la r?ception suivante
  tempsService = 0 # Temps pour le service suivant
  
  paquetsRecus = 0
  paquetsTraites = 0
  paquetsPerdus = 0
  
  while(expTime<totalTime){
    if (tempsArrivee< tempsService || k == 0) {
      paquetsRecus = paquetsRecus + 1
      prochaineRequete = rexp(1,lambda)
      tempsArrivee = tempsArrivee + prochaineRequete
      
      ## On associe a la requete une priorite
      randomPriority = sample(1:100,1)
      
      if(randomPriority < p1*100){
        if(k==N){
          paquetsPerdus = paquetsPerdus + 1
        }
        else{
          k1 = k1 +1  
        }
      }
      else if( p1*100 <= randomPriority && randomPriority < (p1*100 + p2*100)){
        if(k==N){
          paquetsPerdus = paquetsPerdus + 1
        }
        else{
          k2 = k2 + 1
        }
      }
      else{
        if(k==N){
          paquetsPerdus = paquetsPerdus + 1
        }
        else{
          k3 = k3 + 1
        }
      }
      k = k1+k2+k3
      expTime = tempsArrivee
      
    
    }else
      {
        prochainService = rexp(1,mu)
        tempsService = tempsService + prochainService
      if (k1 > 0 && k1succ < 4){
        k1 = k1 - 1
        k1succ = k1succ + 1
        k2succ = 0
      }
      else if (k2 > 0 && k2succ < 4){
        k2 = k2 - 1
        k2succ = k2succ + 1
        k1succ = 0
      }
      else if (k3 > 0){
        k3 = k3 - 1
      }
        
      k = k1+k2+k3
      expTime = tempsService
      paquetsTraites = paquetsTraites + 1
    }
  }
  
  print ("#########")
  print(paquetsRecus)
  print(paquetsTraites)
  print(paquetsPerdus)
  print(k)
  
}

simulationServeur2(2,1,40,0.3,0.2,0.5,10^2)

