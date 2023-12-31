install.packages ("TurtleGraphics") 
library ("TurtleGraphics")
library(grid) # Utiliser pour faire les etoiles et les planetes



Star <- function(NumberStar){                                                     
  # creer des �toiles 
  # chaque vecteur contient autant de coordonn�es que le nombre d'etoiles donn� en parametre
  
  rayon <- sample( c(0.003, 0.004, 0.005), size = NumberStar, replace = TRUE)         #  Creer un vecteur dans lequel on tire au hasard une valeur qui repr�sentera la taille du rayon entre 0.003,0.004,0.005 puis on remet la valeur dans 'l'urne'    
  posx <-  sample( seq(from=0.01,to=0.99,by=0.01), size = NumberStar, replace = TRUE) #  Creer un vecteur dans lequel on tire au hasard une valeur qui repr�sentera la coordonn�e du x entre 0.01 et 0.99 par pas de 0.01 puis on remet la valeur dans 'l'urne'                  
  posy <-  sample( seq(from=0.01,to=0.99,by=0.01), size = NumberStar, replace = TRUE) #  Creer un vecteur dans lequel on tire au hasard une valeur qui repr�sentera la coordonn�e du y entre 0.01 et 0.99 par pas de 0.01 puis on remet la valeur dans 'l'urne'         
  color <- sample ( c('white', 'darkgoldenrod1'), size = NumberStar, replace = TRUE)  #  Creer un vecteur contenant 2 couleurs qui seront choisies s�par�ment pour chaque �toile puis on remet la couleur dans 'l'urne'             
  grid.circle( x = posx, y = posy, r = rayon, gp = gpar(col=color, fill=color))       #  Cette fonction dessine les �toiles au fur et � mesure que les coordonnees sont choisies dans les vecteurs 
}


OtherPlanets <- function(NumberPlanet,couleur) {                                                                                                    
  # creer des plan�tes autour de la planete centrale 
  # chaque vecteur contient autant de coordonnees que le nombre de plan�tes donne en parametre
  
  rayon <- sample( seq(0.05,0.09,0.020), size = NumberPlanet, replace = TRUE)         #  Creer un vecteur dans lequel on tire au hasard une valeur qui representera la taille du rayon entre 0.05,0.09 avec un pas de 0.020 puis on remet la valeur dans 'l'urne'                           
  coordinates <- c(seq(0.1,0.3,0.03), seq(0.7,0.9,0.03))                              #  Creer un vecteur dans lequel on combine deux sequences, 1ere seq de 0.1 � 0.3 par pas de 0.03, 2eme seq de 0.7 � 0.9 par pas de 0.03       
  posx <-sample(coordinates, size = NumberPlanet, replace = TRUE)                     #  Creer un vecteur dans lequel on tire au hasard une valeur qui representera la coordonnee du x entre 1ere seq de 0.1 � 0.3 par pas de 0.03, 2eme seq de 0.7 � 0.9 par pas de 0.03 puis on remet la valeur dans 'l'urne'         
  posy <- sample(coordinates, size = NumberPlanet, replace = TRUE)                    #  Creer un vecteur dans lequel on tire au hasard une valeur qui representera la coordonnee du y entre 1ere seq de 0.1 � 0.3 par pas de 0.03, 2eme seq de 0.7 � 0.9 par pas de 0.03 puis on remet la valeur dans 'l'urne'         
  grid.circle( x = posx, y = posy, r = rayon, gp = gpar(col="black", fill=couleur))   #  Cette fonction dessine les planetes autour de la planete centrale au fur et � mesure que les coordonnees sont choises dans les vecteurs    
  
}



CentralPlanet <- function(){
  # creer une plan�te centrale
  grid.circle( x = 0.5, y = 0.5, r = 0.3, gp = gpar(col="lightgoldenrod3", fill="lightgoldenrod3"))  # Cette fonction dessine la plan�te centrale
  
}


TREE <- function(NumberTree,NumberRecTree){
  # cette fonction appelle la fonction DrawTree autant de fois que demand� en param�tre (avec le NumberTree)
  
  MaxY <- DrawSize - 400                                                  # Valeur max du y sera 600 
  MaxX <- DrawSize - 300                                                  # Valeur max du x sera 600
  MinX <- DrawSize - ( DrawSize - 300 )                                   # Valeur min du x sera 300
  MinY <- DrawSize - ( DrawSize - 300 )                                   # Valeur min du y sera 300
  
  for (i in 1:NumberTree) {                                               # Cette boucle appellera la fonction DrawTree 
    x <- sample (c ( MinX : MaxX ), size=1)                               # Cr�er un vecteur prenant une valeur entre le min et le max de x qui deviendra la coordonn�e de x
    y <- sample (c ( MinY : DrawSize/2 ), size=1)                         # Cr�er un vecteur prenant une valeur entre le min et le max de y qui deviendra la coordonn�e de y
    while( x > MaxX ||  y  > MaxY || x < MinX || y < MinY ){              # Ici on v�rifie que les coordonn�es soient bien comprises entre les bornes  
      x <- sample (c ( MinX : MaxX ), size=1)                             # Si les coordonn�es ne sont pas comprises dans les bornes alors tant qu'elles ne sont pas dedans on change les valeurs du x et du y 
      y <- sample (c ( MinY : DrawSize/2 ), size=1)
    }  
    turtle_setpos (x,y)                                                   # Ici on assigne les valeurs du x et du y d�finis auparavant � notre Tortue
    turtle_lwd (2)                                                        # On d�finit l'�paisseur du trait de dessin � 2
    turtle_up ()                                                          
    turtle_down ()
    color <- sample ( c('darkgrey','darkorange4','darkorange3'), size=1)  # Cr�er un vecteur contenant les couleurs qui seront assign�es aux arbres de mani�re al�atoire
    turtle_col (color)                                                    # On d�finit la couleur de la Tortue 
    DrawTree(DrawSize/12, NumberRecTree)                                  # Ici on appelle la fonction qui va dessiner les arbres 
    
  }
  
}


DrawTree <- function(length = 80, NumberRecTree = 2) {                            
  # Cette fonction dessine les arbres de mani�re r�cursive
  
  if (NumberRecTree <= 1) {                                    # On rentre dans le if seulement si 'NumberRecTree' est inf�rieur ou �gal � 1           
    turtle_forward (length)                                    # On trace un trait de la longueur donn�e en param�tre   
    turtle_up ()                                               # On l�ve le crayon de la Tortue
    turtle_backward (length)                                   # On recule de la longueur donn�e en param�tre     
    turtle_down ()                                             # On repose ensuite le crayon de la Tortue
  }
  else {                                                                    
    turtle_forward (length)                                     # On trace le tronc de l'arbre                               
    ValueL <- runif (1, 5, 40)                                  # Cette variable prend une valeur al�atoire gr�ce � 'runif'
    turtle_left (ValueL)                                        # On tourne l'angle de la Tortue � gauche, le degr� de l'angle correspond � la valeur de la variable 'ValueL'
    DrawTree (length * runif (1, 0.25, 1), NumberRecTree-1)     # On rappelle la fonction pour dessiner les branches 
    turtle_right (ValueL)                                       # On tourne l'angle de la Tortue � droite   
    ValueR <- runif (1, 5, 40)                                  # Cette variable prend une valeur al�atoire gr�ce � 'runif'
    turtle_right (ValueR)                                       # On tourne l'angle de la Tortue � droite, le degr� de l'angle correspond � la valeur de la variable 'ValueR'
    DrawTree (length * runif (1, 0.25, 1), NumberRecTree-1)     # On rappelle la fonction pour dessiner les branches 
    turtle_left (ValueR)                                        # On tourne l'angle de la Tortue � gauche
    turtle_up ()                                                # On l�ve le crayon de la Tortue
    turtle_backward (length)                                    # On recule de la longueur donn�e en param�tre 
    turtle_down ()                                              # On repose ensuite le crayon de la Tortue
    
  }
}



Galaxie <- function (DrawSize, NumberRecTree = 2,NumberTree = 5,NumberStar = 250, NumberPlanet = 7) {
  
  # La fonction est la fonction principale de notre programme
  # Elle appelle les fonctions afin de constituer notre repr�sentation de la galaxie 
  
  # Tout d'abord on cr�e un fond noir qui repr�sente le fond de notre galaxie :
  #  -Pour faire cela on utilise la 'turtle_col' afin de d�finir notre couleur pour le fond,
  #  -Puis 'turtle_lwd' pour d�finir l'�paisseur du trait qui fera le fond,
  #  -Enfin 'turlte_forward' permet de tracer le trait d'�paisseur et de couleur qui ont �t� d�finis auparavant par 'turtle_lwd' et 'turtle_col'.
  
  # Apr�s avoir fait le fond la fonction Star() est appel�e afin de dessiner les �toiles dont le nombre varie selon la valeur donn�e en param�tre.
  
  # La variable color permet d'avoir diff�rentes couleurs qui seront attribu�es de mani�re al�atoire aux plan�tes dans la fonction OtherPlanets().
  
  # Ensuite, la fonction OtherPlanets() est appel�e pour dessiner les plan�tes qui gravitent autour de la plan�te principale qui sera dessin�e par la suite.
  
  # Apr�s avoir dessin� les plan�tes qui gravitent dans la galaxie,
  # La fonction CentralPlanet() est appel�e, elle dessine notre plan�te Terre.
  
  # Enfin, la fonction Tree() va appeler la fonction DrawTree() autant de fois que l'on a demand� d'arbres en param�tre,
  # ATTENTION, cependant il ne faut pas mettre plus de 10 arbres car la creation de l'arbre prend beacoup de temps.
  # Les arbres se dessinent sur la plan�te centrale qui a �t� dessin�e auparavant par la fonction CentralPlanet().
  
  turtle_col('black')
  turtle_lwd(DrawSize)
  turtle_forward(1)               
  Star(NumberStar)                    
  color <- sample (c ("maroon1","slateblue3","green1","cyan2","yellow1","blue2","springgreen2"),size=NumberPlanet,replace=TRUE)
  OtherPlanets(NumberPlanet,color)                
  CentralPlanet()
  TREE(NumberTree,NumberRecTree)
}



DrawSize=1000  # Taille fen�tre de dessin 
# width et height sont �gaux � DrawSize 
# Car la fen�tre est un carr� donc la largeur et la hauteur sont �gales
turtle_init (width = DrawSize, height = DrawSize, "clip")
turtle_hide()
turtle_do (Galaxie(DrawSize,NumberRecTree = 12,NumberTree = 4,NumberStar = 2000, NumberPlanet = 10))
