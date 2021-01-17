library("dplyr")

##### function #####

delta <- list(as.raw(c(0xc3, 0xef, 0xe9, 0xdb)), as.raw(c(0x44, 0x62, 0x6b, 0x02)), as.raw(c(0x79, 0xe2, 0x7c, 0x8a)), as.raw(c(0x78 ,0xdf, 0x30, 0xec)))


ROR <- function(n, i){
  bitwXor(lag(n, i, default = 0), lead(n, 32-i, default = 0))
}

ROL <- function(n, i){
  bitwXor(lag(n, 32-i, default = 0), lead(n, i, default = 0))
}

split_n <- function(a, n){
  split(a, ceiling(seq_along(a)/n))
}

raw_to_bin <- function(n){
  a<-as.integer(rawToBits(n))
  b<-split_n(a, 8)
  c<-list()
  for(i in 1:(length(a)/8)){
    c<-append(c, rev(split_n(b[[i]], 4)))
  }
  d<-vector()
  for(i in 1:(length(a)/4)){
    d<-append(d, rev(c[[i]]))
  }
  d
}

bin_to_dec <- function(n){
  dec = 0
  for(i in 1:length(n)){
    dec = dec + n[length(n)-i+1]*(2^(i-1))
  }
  dec
}

bin_to_raw<-function(a){
  b<-split_n(a, 8)
  c<-vector()
  for(i in 1:(length(a)/8)){
    c<-append(c, bin_to_dec(b[[i]]))
  }
  as.raw(c)
}

Plus <- function(a,b){
  P<-a+b
  for(i in 0:31){
    if(P[32-i] == 2){
      P[32-i] <- 0
      P[32-i-1] <- P[32-i-1] + 1
    }
    if(P[32-i] == 3){
      P[32-i] <- 1
      P[32-i-1] <- P[32-i-1] + 1
    }
  }
  P
}

Minus<-function(a,b){
  M<-a-b
  for(i in 0:31){
    if(M[32-i] == -1){
      M[32-i] <- 1
      M[32-i-1] <- (M[32-i-1] - 1) 
    }
    if(M[32-i] == -2){
      M[32-i] <- 0
      M[32-i-1] <- M[32-i-1] - 1
    }
  }
  M
}

##### KeyShedule #####

lea_key <- as.raw(0:15)
lea_key

T<-split_n(lea_key, 4)
T<-lapply(T, function(n) raw_to_bin(rev(n)))
RK <- list()

for(i in 0:23){
  T[[1]] <- ROL(Plus(T[[1]], ROL(raw_to_bin(delta[[i%%4+1]]),i)),1)
  T[[2]] <- ROL(Plus(T[[2]], ROL(raw_to_bin(delta[[i%%4+1]]),i+1)),3)
  T[[3]] <- ROL(Plus(T[[3]], ROL(raw_to_bin(delta[[i%%4+1]]),i+2)),6)
  T[[4]] <- ROL(Plus(T[[4]], ROL(raw_to_bin(delta[[i%%4+1]]),i+3)),11)
  RK[[i*6+1]]<-T[[1]]
  RK[[i*6+2]]<-T[[2]]
  RK[[i*6+3]]<-T[[3]]
  RK[[i*6+4]]<-T[[2]]
  RK[[i*6+5]]<-T[[4]]
  RK[[i*6+6]]<-T[[2]]
}

##### LEA_Encrypt #####

msg_chr <- "Hong kong has been rocked by pro-democracy, anti-government demonstrations for months, with escalating anger and violence on all sides. CNN takes a look at how the protests evolved over the course of the year"
msg_raw <- charToRaw(msg_chr)
msg_raw

Plain_text <- split_n(msg_raw, 16)
len <- length(Plain_text)
Cipher_text <- list()

for(j in 1:len){
  X_Round <- split_n(Plain_text[[j]], 4)
  X_Round <- lapply(X_Round, function(n) raw_to_bin(rev(n)))
  X_NextRound <- list()
  
  for(i in 0:23){
    X_NextRound[[1]] <- ROL(Plus(bitwXor(X_Round[[1]], RK[[i*6+1]]), bitwXor(X_Round[[2]], RK[[i*6+2]])), 9)
    X_NextRound[[2]] <- ROR(Plus(bitwXor(X_Round[[2]], RK[[i*6+3]]), bitwXor(X_Round[[3]], RK[[i*6+4]])), 5)
    X_NextRound[[3]] <- ROR(Plus(bitwXor(X_Round[[3]], RK[[i*6+5]]), bitwXor(X_Round[[4]], RK[[i*6+6]])), 3)
    X_NextRound[[4]] <- X_Round[[1]]
    
    X_Round <- lapply(X_NextRound, function(x) x)
  }
  block_text <- as.vector(sapply(X_Round, function(n) rev(bin_to_raw(n))))
  Cipher_text <- unlist(append(Cipher_text, block_text))
}
Cipher_text

##### LEA_Decrypt #####

Cipher_text <- split_n(Cipher_text, 16)
Decrypt_text <- list()

for(j in 1:len){
  X_Round <- split_n(Cipher_text[[j]], 4)
  X_Round <- lapply(X_Round, function(n) raw_to_bin(rev(n)))
  X_NextRound <- list()
  
  for(i in 0:23){
    X_NextRound[[1]] <- X_Round[[4]]
    X_NextRound[[2]] <- 
      bitwXor(Minus(ROR(X_Round[[1]], 9), 
                    bitwXor(X_NextRound[[1]], RK[[((24-i-1)*6)+1]])), 
              RK[[((24-i-1)*6) + 2]])
    X_NextRound[[3]] <- 
      bitwXor(Minus(ROL(X_Round[[2]], 5), 
                    bitwXor(X_NextRound[[2]], RK[[((24-i-1)*6)+3]])), 
              RK[[((24-i-1)*6) + 4]])
    X_NextRound[[4]] <- 
      bitwXor(Minus(ROL(X_Round[[3]], 3), 
                    bitwXor(X_NextRound[[3]], RK[[((24-i-1)*6)+5]])), 
              RK[[((24-i-1)*6)+6]])
    
    X_Round <- lapply(X_NextRound, function(x) x)
  }
  block_text <- as.vector(sapply(X_Round, function(n) rev(bin_to_raw(n))))
  Decrypt_text <- unlist(append(Decrypt_text, block_text))
}
Decrypt_text

Plain_text <- rawToChar(Decrypt_text)
Plain_text