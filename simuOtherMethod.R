## Simulation de l'arrivÃ©e des requÃªtes : 

## Simulation des services : 

simulationServeur2 = function(lambda,mu,N,p1,p2,p3,duration){
  ## N : capacitÃ© maximale de stockage (en service + dans la file d'attente)
  ## p1,p2,p3 : proportion de prioritÃ©s (p1+p2+p3 = 1)
  ## lambda : intensitÃ©, lambda>0
  ## 1/mu = temps de traitement, mu>0
  
  ## Durée de l'expérience
  
  totalTime   = duration   # duration of simulation
  expTime = 0 # simulation time
  
  ## Pour les arrivees
  k1 = 0 # nombre de requetes dans la queue de P1
  k2 = 0 # nombre de requetes dans la queue de P2
  k3 = 0 # nombre de requetes dans la queue de P3
  k = 0 # nombre de requetes totales 
  
  k1succ = 0 # nombre des requetes 1 traitées successivement
  k2succ = 0 # nombre des requetes 2 traitées successivement
  
  tempsArrivee = 0 # Temps pour la réception suivante
  tempsService = 0 # Temps pour le service suivant
  
  paquetsRecus = 0
  paquetsTraites = 0
  paquetsPerdus = 0
  
  while(expTime<totalTime){
    if (tempsArrivee< tempsService || k == 0) {
      prochaineRequete = rexp(1,lambda)
      tempsArrivee = tempsArrivee + prochaineRequete
      
      ## On associe à la requête une priorité
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
      if (k>N){
        paquetsPerdus = paquetsPerdus + 1
        k=N
      }
      paquetsRecus = paquetsRecus + 1
      expTime = tempsArrivee
      tempsArrivee = tempsArrivee + rexp(1,lambda)
      
    
    }else
      {
      if (k1 > 0 && k1succ < 4){
        k1 = k1 - 1
        k1succ = k1succ + 1
        k2succ = 0
      }
      else if (k2 > 0 && k2succ < 4){
        k2 = k2 - 1
        k2succ = k2succ + 1
        k1succ = 0
        print(k2)

      }
      else if (k3 > 0){
        k3 = k3 - 1
      }
      k = k1+k2+k3
      expTime = expTime + tempsService
      tempsDepart = expTime + rexp(1,mu)
      paquetsTraites = paquetsTraites + 1
    }
  }
  
  print ("#########")
  print(paquetsRecus)
  print(paquetsTraites)
  print(paquetsPerdus)
  ## simulation de 20 services
  
  ## affichage du tableau arrivees
  #tableauArrivees<-data.frame(Tk,arrivee,priorite,etat)
  #print(tableauArrivees)
  
  ## affichage du tableau services
  #tableauServices<-data.frame(Si,service)
  #print(tableauServices)
  
}

simulationServeur2(1,1,40,0.3,0.2,0.5,10^2)

