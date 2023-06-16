install.packages ("TurtleGraphics") 
library ("TurtleGraphics")
library(grid) # Utiliser pour faire les etoiles et les planetes



Star <- function(NumberStar){                                                     
  # creer des étoiles 
  # chaque vecteur contient autant de coordonnées que le nombre d'etoiles donné en parametre
  
  rayon <- sample( c(0.003, 0.004, 0.005), size = NumberStar, replace = TRUE)         #  Creer un vecteur dans lequel on tire au hasard une valeur qui représentera la taille du rayon entre 0.003,0.004,0.005 puis on remet la valeur dans 'l'urne'    
  posx <-  sample( seq(from=0.01,to=0.99,by=0.01), size = NumberStar, replace = TRUE) #  Creer un vecteur dans lequel on tire au hasard une valeur qui représentera la coordonnée du x entre 0.01 et 0.99 par pas de 0.01 puis on remet la valeur dans 'l'urne'                  
  posy <-  sample( seq(from=0.01,to=0.99,by=0.01), size = NumberStar, replace = TRUE) #  Creer un vecteur dans lequel on tire au hasard une valeur qui représentera la coordonnée du y entre 0.01 et 0.99 par pas de 0.01 puis on remet la valeur dans 'l'urne'         
  color <- sample ( c('white', 'darkgoldenrod1'), size = NumberStar, replace = TRUE)  #  Creer un vecteur contenant 2 couleurs qui seront choisies séparément pour chaque étoile puis on remet la couleur dans 'l'urne'             
  grid.circle( x = posx, y = posy, r = rayon, gp = gpar(col=color, fill=color))       #  Cette fonction dessine les étoiles au fur et à mesure que les coordonnees sont choisies dans les vecteurs 
}


OtherPlanets <- function(NumberPlanet,couleur) {                                                                                                    
  # creer des planètes autour de la planete centrale 
  # chaque vecteur contient autant de coordonnees que le nombre de planètes donne en parametre
  
  rayon <- sample( seq(0.05,0.09,0.020), size = NumberPlanet, replace = TRUE)         #  Creer un vecteur dans lequel on tire au hasard une valeur qui representera la taille du rayon entre 0.05,0.09 avec un pas de 0.020 puis on remet la valeur dans 'l'urne'                           
  coordinates <- c(seq(0.1,0.3,0.03), seq(0.7,0.9,0.03))                              #  Creer un vecteur dans lequel on combine deux sequences, 1ere seq de 0.1 à 0.3 par pas de 0.03, 2eme seq de 0.7 à 0.9 par pas de 0.03       
  posx <-sample(coordinates, size = NumberPlanet, replace = TRUE)                     #  Creer un vecteur dans lequel on tire au hasard une valeur qui representera la coordonnee du x entre 1ere seq de 0.1 à 0.3 par pas de 0.03, 2eme seq de 0.7 à 0.9 par pas de 0.03 puis on remet la valeur dans 'l'urne'         
  posy <- sample(coordinates, size = NumberPlanet, replace = TRUE)                    #  Creer un vecteur dans lequel on tire au hasard une valeur qui representera la coordonnee du y entre 1ere seq de 0.1 à 0.3 par pas de 0.03, 2eme seq de 0.7 à 0.9 par pas de 0.03 puis on remet la valeur dans 'l'urne'         
  grid.circle( x = posx, y = posy, r = rayon, gp = gpar(col="black", fill=couleur))   #  Cette fonction dessine les planetes autour de la planete centrale au fur et à mesure que les coordonnees sont choises dans les vecteurs    
  
}



CentralPlanet <- function(){
  # creer une planète centrale
  grid.circle( x = 0.5, y = 0.5, r = 0.3, gp = gpar(col="lightgoldenrod3", fill="lightgoldenrod3"))  # Cette fonction dessine la planète centrale
  
}


TREE <- function(NumberTree,NumberRecTree){
  # cette fonction appelle la fonction DrawTree autant de fois que demandé en paramètre (avec le NumberTree)
  
  MaxY <- DrawSize - 400                                                  # Valeur max du y sera 600 
  MaxX <- DrawSize - 300                                                  # Valeur max du x sera 600
  MinX <- DrawSize - ( DrawSize - 300 )                                   # Valeur min du x sera 300
  MinY <- DrawSize - ( DrawSize - 300 )                                   # Valeur min du y sera 300
  
  for (i in 1:NumberTree) {                                               # Cette boucle appellera la fonction DrawTree 
    x <- sample (c ( MinX : MaxX ), size=1)                               # Créer un vecteur prenant une valeur entre le min et le max de x qui deviendra la coordonnée de x
    y <- sample (c ( MinY : DrawSize/2 ), size=1)                         # Créer un vecteur prenant une valeur entre le min et le max de y qui deviendra la coordonnée de y
    while( x > MaxX ||  y  > MaxY || x < MinX || y < MinY ){              # Ici on vérifie que les coordonnées soient bien comprises entre les bornes  
      x <- sample (c ( MinX : MaxX ), size=1)                             # Si les coordonnées ne sont pas comprises dans les bornes alors tant qu'elles ne sont pas dedans on change les valeurs du x et du y 
      y <- sample (c ( MinY : DrawSize/2 ), size=1)
    }  
    turtle_setpos (x,y)                                                   # Ici on assigne les valeurs du x et du y définis auparavant à notre Tortue
    turtle_lwd (2)                                                        # On définit l'épaisseur du trait de dessin à 2
    turtle_up ()                                                          
    turtle_down ()
    color <- sample ( c('darkgrey','darkorange4','darkorange3'), size=1)  # Créer un vecteur contenant les couleurs qui seront assignées aux arbres de manière aléatoire
    turtle_col (color)                                                    # On définit la couleur de la Tortue 
    DrawTree(DrawSize/12, NumberRecTree)                                  # Ici on appelle la fonction qui va dessiner les arbres 
    
  }
  
}


DrawTree <- function(length = 80, NumberRecTree = 2) {                            
  # Cette fonction dessine les arbres de manière récursive
  
  if (NumberRecTree <= 1) {                                    # On rentre dans le if seulement si 'NumberRecTree' est inférieur ou égal à 1           
    turtle_forward (length)                                    # On trace un trait de la longueur donnée en paramètre   
    turtle_up ()                                               # On lève le crayon de la Tortue
    turtle_backward (length)                                   # On recule de la longueur donnée en paramètre     
    turtle_down ()                                             # On repose ensuite le crayon de la Tortue
  }
  else {                                                                    
    turtle_forward (length)                                     # On trace le tronc de l'arbre                               
    ValueL <- runif (1, 5, 40)                                  # Cette variable prend une valeur aléatoire gràce à 'runif'
    turtle_left (ValueL)                                        # On tourne l'angle de la Tortue à gauche, le degré de l'angle correspond à la valeur de la variable 'ValueL'
    DrawTree (length * runif (1, 0.25, 1), NumberRecTree-1)     # On rappelle la fonction pour dessiner les branches 
    turtle_right (ValueL)                                       # On tourne l'angle de la Tortue à droite   
    ValueR <- runif (1, 5, 40)                                  # Cette variable prend une valeur aléatoire gràce à 'runif'
    turtle_right (ValueR)                                       # On tourne l'angle de la Tortue à droite, le degré de l'angle correspond à la valeur de la variable 'ValueR'
    DrawTree (length * runif (1, 0.25, 1), NumberRecTree-1)     # On rappelle la fonction pour dessiner les branches 
    turtle_left (ValueR)                                        # On tourne l'angle de la Tortue à gauche
    turtle_up ()                                                # On lève le crayon de la Tortue
    turtle_backward (length)                                    # On recule de la longueur donnée en paramètre 
    turtle_down ()                                              # On repose ensuite le crayon de la Tortue
    
  }
}



Galaxie <- function (DrawSize, NumberRecTree = 2,NumberTree = 5,NumberStar = 250, NumberPlanet = 7) {
  
  # La fonction est la fonction principale de notre programme
  # Elle appelle les fonctions afin de constituer notre représentation de la galaxie 
  
  # Tout d'abord on crée un fond noir qui représente le fond de notre galaxie :
  #  -Pour faire cela on utilise la 'turtle_col' afin de définir notre couleur pour le fond,
  #  -Puis 'turtle_lwd' pour définir l'épaisseur du trait qui fera le fond,
  #  -Enfin 'turlte_forward' permet de tracer le trait d'épaisseur et de couleur qui ont été définis auparavant par 'turtle_lwd' et 'turtle_col'.
  
  # Après avoir fait le fond la fonction Star() est appelée afin de dessiner les étoiles dont le nombre varie selon la valeur donnée en paramètre.
  
  # La variable color permet d'avoir différentes couleurs qui seront attribuées de manière aléatoire aux planètes dans la fonction OtherPlanets().
  
  # Ensuite, la fonction OtherPlanets() est appelée pour dessiner les planètes qui gravitent autour de la planète principale qui sera dessinée par la suite.
  
  # Après avoir dessiné les planètes qui gravitent dans la galaxie,
  # La fonction CentralPlanet() est appelée, elle dessine notre planète Terre.
  
  # Enfin, la fonction Tree() va appeler la fonction DrawTree() autant de fois que l'on a demandé d'arbres en paramètre,
  # ATTENTION, cependant il ne faut pas mettre plus de 10 arbres car la creation de l'arbre prend beacoup de temps.
  # Les arbres se dessinent sur la planète centrale qui a été dessinée auparavant par la fonction CentralPlanet().
  
  turtle_col('black')
  turtle_lwd(DrawSize)
  turtle_forward(1)               
  Star(NumberStar)                    
  color <- sample (c ("maroon1","slateblue3","green1","cyan2","yellow1","blue2","springgreen2"),size=NumberPlanet,replace=TRUE)
  OtherPlanets(NumberPlanet,color)                
  CentralPlanet()
  TREE(NumberTree,NumberRecTree)
}



DrawSize=1000  # Taille fenêtre de dessin 
# width et height sont égaux à DrawSize 
# Car la fenêtre est un carré donc la largeur et la hauteur sont égales
turtle_init (width = DrawSize, height = DrawSize, "clip")
turtle_hide()
turtle_do (Galaxie(DrawSize,NumberRecTree = 12,NumberTree = 4,NumberStar = 2000, NumberPlanet = 10))
