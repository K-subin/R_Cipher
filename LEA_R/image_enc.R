library("digest")
library("png")
library("imager")
library("keras")

my_img <- readPNG("C:/what2.png")
my_img
dim(my_img)

my_img_array <- array_reshape(my_img[,,2], c(1,23*50))
image(my_img_array)
my_img_raw <- as.raw(my_img_array)
my_img_raw

key <- as.raw(0:15)
iv <- sample(0:255, 16, replace = TRUE)

aes <- AES(key, mode="CTR", iv)
cipher_img_raw <- aes$encrypt(my_img_raw)
cipher_img_raw
cipher_img <- array_reshape(cipher_img_raw, c(23, 50))
image(cipher_img)

aes <- AES(key, mode="CTR", iv)
dec_img_raw <- aes$decrypt(cipher_img_raw, raw=TRUE)
dec_img <- array_reshape(dec_img_raw, c(23, 50))
image(dec_img)
