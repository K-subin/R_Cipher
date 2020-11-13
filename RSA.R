#=========================================
# RSA (using openssl built-in function)
#=========================================

library("openssl")
library("digest")

## (1) Message
msg <- "Welcome to R world"

## (2) RSA key generation (public_key(encryption), private_key(decryption))
rsa_key <- rsa_keygen(512) # key material (all about keys)
pubkey <- rsa_key$pubkey # public to anyone (public key information)

## (3) (Anyone) RSA encryption (Message to Alice)
ciphertext <- rsa_encrypt(charToRaw(msg), pubkey)
ciphertext

## (4) Alice : RSA decryption with private_key
decrypted_msg <- rsa_decrypt(ciphertext, rsa_key)
decrypted_msg
rawToChar(decrypted_msg)