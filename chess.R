
#install.packages("png")
#install.packages("RCurl")
require(png)
require(RCurl)

# download piece images from wikipedia commons
{
  Bp <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/c/cd/Chess_pdt60.png"))
  Bb <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/8/81/Chess_bdt60.png"))
  Bt <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/a/a0/Chess_rdt60.png"))
  Bh <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/f/f1/Chess_ndt60.png"))
  Bq <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/a/af/Chess_qdt60.png"))
  Bk <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/e/e3/Chess_kdt60.png"))
  Wp <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/0/04/Chess_plt60.png"))
  Wb <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/9/9b/Chess_blt60.png"))
  Wh <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/2/28/Chess_nlt60.png"))
  Wt <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/5/5c/Chess_rlt60.png"))
  Wq <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/4/49/Chess_qlt60.png"))
  Wk <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/3/3b/Chess_klt60.png"))
}

# board drawing

button <- function(x,y, color=NA, text=NULL){
  X <- x + c(1,5,6,6,5,1,0,0,1)/3
  Y <- y + c(0,0,1,2,3,3,2,1,0)/3
  polygon(X,Y,col = color)
  if (!is.null(text)) text(x+1,y+0.5, labels = text)
}

square  <- function(x,y, color=NA){
  X <- x + c(0,0,1,1,0)
  Y <- y + c(0,1,1,0,0)
  polygon(X,Y,col = color)
}

graticule <- function(color=1){
  for (i in 0:3*2+1) {
    for (j in 0:3*2+1) {
      square(i,j, color = color)
      square(i+1,j+1, color = color)
    }
  }
}

board <- function(){
  par(mar = rep(0.4, 4))
  plot(NA, xaxt = "n", yaxt = "n", ylim = c(1,9), xlim = c(1,15),ann=FALSE)
  polygon(x=c(-2, 20, 20, -2, -2),
          y=c(-2, -2, 10, 10, -2), col="khaki", border = "khaki")
  lines(1+c(0,8,8,0,0), 1+c(0,0,8,8,0))
  graticule("sienna")
  points(1+1:8-0.5, 1+rep(-0.15,8), pch = c("A", "B", "C", "D", "E", "F", "G", "H"))
  points(1+1:8-0.5, 1+rep(8+0.15,8), pch = c("A", "B", "C", "D", "E", "F", "G", "H"))
  points(1+rep(-0.15,8), 1+1:8-0.5, pch = as.character(1:8))
  points(1+rep(8+0.15,8), 1+1:8-0.5, pch = as.character(1:8))
  button(13,8,"moccasin", "New Game")
  button(13,7,"moccasin", "End Game")
}
board()

# Initial piece position

BG <- matrix(c("Wt", "Wh", "Wb", "Wq", "Wk", "Wb", "Wh", "Wt",
              rep("Wp", 8),
              rep(NA, 4*8),
              rep("Bp", 8),
              "Bt", "Bb", "Bh", "Bq", "Bk", "Bb", "Bh", "Bt",
              rep(NA, 8)),
             byrow = T, ncol = 8)


# Piece drawing

pieces <- function(X){
  for (j in 1:8) {
    for (i in 1:8) {
      if(!is.na(X[i,j])) rasterImage(get(X[i,j]),j, i, j+1,i+1)

    }
  }
}


# Piece movement

move <- function(ini, fin, X){
  aux <- X[ini[1], ini[2]]
  X[ini[1], ini[2]] <- NA
  X[fin[1], fin[2]] <- aux
  X
}


# Piece movement rules

rules <- function(ini, fin, X){
  p <- X[ini[2], ini[1]]
  res <- TRUE
  if(grepl("p", p)){
    if(grepl("W", p)){
      if(sum(fin != ini + c(1,0))==0 | sum(fin !=  ini + c(2,0))==0){
        print("Invalid movement")
        res <- FALSE
      }
    } else{
      if(sum(fin != ini - c(0,1)) | sum(fin != ini - c(0,2))){
        print("Invalid movement")
        res <- FALSE
      }
    }
  }
  else if(grepl("t", p)){}
  else if(grepl("h", p)){}
  else if(grepl("b", p)){}
  else if(grepl("q", p)){}
  else if(grepl("k", p)){}
  else if(grepl("p", p)){}
  res
}


#  Movement posibilities

inside <- function(ini){
  res <- FALSE
  if(ini[1]>0 & ini[1]<9 & ini[2]>0 & ini[2]<9) res <- TRUE
  res
}

movements <- function(ini, X){
  clr <- "slategray1"
  p <- X[ini[2], ini[1]]
  if(grepl("p", p)){
    board()
    if(grepl("W", p)){square(ini[1], ini[2]+1, clr); square(ini[1], ini[2]+2, clr)}
    if(grepl("B", p)){square(ini[1], ini[2]-1, clr); square(ini[1], ini[2]-2, clr)}
    pieces(G)
  }
  else if(grepl("t", p)){
    board()
    for (i in c(1:8)[-ini[1]]) square(i, ini[2], clr)
    for (i in c(1:8)[-ini[2]]) square(ini[1], i, clr)
    pieces(G)
  }
  else if(grepl("h", p)){
    aux <- matrix(c(1,2,2,1,2,-1,1,-2,-1,-2,-2,-1,-2,1,-1,2), byrow = T, ncol = 2)
    board()
    for (i in 1:8) if(inside(ini+aux[i,])) square(ini[1]+aux[i,1], ini[2]+aux[i,2], clr)
    pieces(G)
  }
  else if(grepl("b", p)){
    board()
    for (i in c(-7:7)[-8]) if(inside(c(ini[1]-i, ini[2]+i))) square(ini[1]-i, ini[2]+i, clr)
    for (i in c(-7:7)[-8]) if(inside(c(ini[1]+i, ini[2]+i))) square(ini[1]+i, ini[2]+i, clr)
    pieces(G)
  }
  else if(grepl("q", p)){
    board()
    for (i in c(1:8)[-ini[1]]) square(i, ini[2], clr)
    for (i in c(1:8)[-ini[2]]) square(ini[1], i, clr)
    for (i in c(-7:7)[-8]) if(inside(c(ini[1]-i, ini[2]+i))) square(ini[1]-i, ini[2]+i, clr)
    for (i in c(-7:7)[-8]) if(inside(c(ini[1]+i, ini[2]+i))) square(ini[1]+i, ini[2]+i, clr)
    pieces(G)
  }
  else if(grepl("k", p)){
    board()
    if(inside(c(ini[1]+1, ini[2]))) square(ini[1]+1, ini[2], clr)
    if(inside(c(ini[1]-1, ini[2]))) square(ini[1]-1, ini[2], clr)
    if(inside(c(ini[1], ini[2]+1))) square(ini[1], ini[2]+1, clr)
    if(inside(c(ini[1], ini[2]-1))) square(ini[1], ini[2]-1, clr)
    if(inside(c(ini[1]+1, ini[2]+1))) square(ini[1]+1, ini[2]+1, clr)
    if(inside(c(ini[1]-1, ini[2]+1))) square(ini[1]-1, ini[2]+1, clr)
    if(inside(c(ini[1]+1, ini[2]-1))) square(ini[1]+1, ini[2]-1, clr)
    if(inside(c(ini[1]-1, ini[2]-1))) square(ini[1]-1, ini[2]-1, clr)
    pieces(G)
  }
}


# Test

{
  board()
  pieces(BG)
  ini <- fin <- 1
  G <- BG
  game <- data.frame(ini = NULL, fin=NULL)
  turn = 0
}
while(sum(ini)>0){
  board()
  pieces(G)
  ini <- floor(as.vector(unlist(locator(n=1))))
  if((ini[2]>1 & ini[2]<9) & (ini[1]>1 & ini[1]<9)){
    p <- G[ini[2], ini[1]]
    #print(G[ini[2], ini[1]])
    #print(paste0(letters[ini[1]],ini[2]))
    while(grepl("B", p) & turn == 0){
      print("Please, let the other player make their move")
      ini <- floor(as.vector(unlist(locator(n=1))))
      p <- G[ini[2], ini[1]]
    }
    while(grepl("W", p) & turn == 1){
      print("Please, let the other player make their move")
      ini <- floor(as.vector(unlist(locator(n=1))))
      p <- G[ini[2], ini[1]]
    }
    movements(ini, G)
    fin <- floor(as.vector(unlist(locator(n=1))))
    #print(paste0(letters[fin[1]],fin[2]))
    game <- rbind(game,
                  data.frame(ini=paste0(letters[ini[1]], ini[2]),
                             fin = paste0(letters[fin[1]], fin[2])))
    G <- move(ini[2:1], fin[2:1], G)
    board()
    pieces(G)
    turn <- (turn +1) %% 2
  }
  if((ini[2]==8) & (ini[1]<16 & ini[1]>12)) G <- BG
  if((ini[2]==7) & (ini[1]<16 & ini[1]>12)) ini <- -1
  #print(ini)
}

game










