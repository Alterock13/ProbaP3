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
    
    compteurk1 = 0 # nombre des requetes 1 trait?es successivement
    compteurk2 = 0 # nombre des requetes 2 trait?es successivement
    compteurk3 = 0 # 
    
    tempsArrivee = 0 # Temps pour la r?ception suivante
    tempsService = 0 # Temps pour le service suivant
    
    paquetsRecus = 0
    paquetsTraites = 0
    paquetsPerdus = 0
    
    while(expTime<totalTime){
      if (tempsArrivee< tempsService || k == 0) {
        paquetsRecus = paquetsRecus + 1
        ## On associe a la requete une priorite
        randomPriority = sample(1:100,1)
        
        if(randomPriority < p1*100){
          prochaineRequete = rexp(1,p1* lambda)
          if(k==N){
            paquetsPerdus = paquetsPerdus + 1
          }
          else{
            
            k1 = k1 +1  
          }
        }
        else if( p1*100 <= randomPriority && randomPriority < (p1*100 + p2*100)){
          prochaineRequete = rexp(1,p2* lambda)
          if(k==N){
            paquetsPerdus = paquetsPerdus + 1
          }
          else{
            k2 = k2 + 1
          }
        }
        else{
          prochaineRequete = rexp(1,p3* lambda)
          if(k==N){
            paquetsPerdus = paquetsPerdus + 1
          }
          else{
            k3 = k3 + 1
          }
        }
        tempsArrivee = tempsArrivee + prochaineRequete
        k = k1+k2+k3
        expTime = tempsArrivee
        
      
      }else
        {
          prochainService = rexp(1,mu)
          tempsService = tempsService + prochainService
        if (k1 > 0){
          k1 = k1 - 1
          paquetsTraites = paquetsTraites + 1
          compteurk1 = compteurk1 +1
        }
        else if (k2 > 0){
          k2 = k2 - 1
          paquetsTraites = paquetsTraites + 1
          compteurk2 = compteurk2+1
        }
        else if (k3 > 0){
          k3 = k3 - 1
          paquetsTraites = paquetsTraites + 1
          compteurk3 = compteurk3 +1
        }
          
        k = k1+k2+k3
        expTime = tempsService
        
      }
    }
    
    print ("#########")
    print(paquetsRecus)
    print(paquetsTraites)
    print(paquetsPerdus)
    print(paquetsTraites+paquetsPerdus+k)
    print ("#########")
    print(compteurk1)
    print(compteurk2)
    print(compteurk3)
    
  
  }
  
  simulationServeur2(1,1,20,0.5,0.3,0.2,10^4)
  
