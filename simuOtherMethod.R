  ## Simulation de l'arrivée des requêtes : 
  
  ## Simulation des services : 
  
  simulationServeur2 = function(lambda,mu,N,p1,p2,p3,duration,nbServeurs){
    ## N : capacité maximale de stockage (en service + dans la file d'attente)
    ## p1,p2,p3 : proportion de priorités (p1+p2+p3 = 1)
    ## lambda : intensité, lambda>0
    ## 1/mu : temps de traitement, mu>0
    ## nbServeurs : nombre de serveurs
    
    ## Duree de l'experience
    
    totalTime = duration   # duration of simulation
    expTime = 0 # simulation time
    
    N = N * nbServeurs # on augmente la taille de la file globale en fonction du nombre de serveurs dispos
    
    ## Pour les arrivees
    k1 = 0 # nombre de requetes dans la queue de P1
    k2 = 0 # nombre de requetes dans la queue de P2
    k3 = 0 # nombre de requetes dans la queue de P3
    k = 0 # nombre de requetes totales 
    
    compteurTraitesk1 = 0 # nombre des requetes 1 traitees successivement
    compteurTraitesk2 = 0 # nombre des requetes 2 traites successivement
    compteurTraitesk3 = 0 # nombre des requetes 1 traitees successivement
    
    compteurRecuesk1 = 0 # nombre des requetes 1 recues
    compteurRecuesk2 = 0 # nombre des requetes 2 recues
    compteurRecuesk3 = 0 # nombre des requetes 1 recues
    
    compteurPerduesk1 = 0 # nombre des requetes 1 perdues
    compteurPerduesk2 = 0 # nombre des requetes 2 perdues
    compteurPerduesk3 = 0 # nombre des requetes 1 perdues
    
    tempsArrivee = 0 # Temps pour la reception suivante
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
            compteurPerduesk1 = compteurPerduesk1+1
          }
          else{
            k1 = k1 +1  
          }
          compteurRecuesk1 = compteurRecuesk1 + 1
        }
        else if( p1*100 <= randomPriority && randomPriority < (p1*100 + p2*100)){
          prochaineRequete = rexp(1,p2* lambda)
          if(k==N){
            paquetsPerdus = paquetsPerdus + 1
            compteurPerduesk2 = compteurPerduesk2+1
          }
          else{
            
            k2 = k2 + 1
          }
          compteurRecuesk2 = compteurRecuesk2 + 1
        }
        else{
          prochaineRequete = rexp(1,p3* lambda)
          if(k==N){
            paquetsPerdus = paquetsPerdus + 1
            compteurPerduesk3 = compteurPerduesk3 + 1
          }
          else{
            k3 = k3 + 1
          }
          compteurRecuesk3 = compteurRecuesk3 + 1
          
        }
        tempsArrivee = tempsArrivee + prochaineRequete
        k = k1+k2+k3
        expTime = tempsArrivee
        
      
      }else
        {
          prochainService = rexp(1,mu)
          tempsService = tempsService + prochainService
          for (i in 0:nbServeurs) {
            if (k1 > 0){
              k1 = k1 - 1
              paquetsTraites = paquetsTraites + 1
              compteurTraitesk1 = compteurTraitesk1 +1
            }
            else if (k2 > 0){
              k2 = k2 - 1
              paquetsTraites = paquetsTraites + 1
              compteurTraitesk2 = compteurTraitesk2+1
            }
            else if (k3 > 0){
              k3 = k3 - 1
              paquetsTraites = paquetsTraites + 1
              compteurTraitesk3 = compteurTraitesk3 +1
            }
            
            k = k1+k2+k3
          }
          expTime = tempsService
      }
    }
    
    print ("#########")
    print(paquetsRecus)
    print(paquetsTraites)
    print(paquetsPerdus)
    print(paquetsTraites+paquetsPerdus+k)
    print ("#########")
    print(compteurRecuesk1)
    print(compteurRecuesk2)
    print(compteurRecuesk3)
    print(compteurRecuesk1+compteurRecuesk2+compteurRecuesk3)
    print ("#########")
    print(compteurTraitesk1)
    print(compteurTraitesk2)
    print(compteurTraitesk3)
    print ("#########")
    print(compteurPerduesk1)
    print(compteurPerduesk2)
    print(compteurPerduesk3)
    
    
  
  }
  
  simulationServeur2(6,1,20,0.5,0.3,0.2,10^4,1)
  simulationServeur2(6,1,20,0.5,0.3,0.2,10^4,2)
  simulationServeur2(6,1,20,0.5,0.3,0.2,10^4,4)
  simulationServeur2(6,1,20,0.5,0.3,0.2,10^4,8)
  
