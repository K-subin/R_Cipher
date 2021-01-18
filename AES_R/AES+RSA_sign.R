#===================================================
# Message Encryption (AES) + Digital signature(RSA-sign)
#===================================================
library("openssl")
library("digest")

## (1) Plaintext Message
plaintext_msg <- "Hello R-world !!"

## (2) key setup (AES, RSA-sign)
aes_key <- as.raw(0:15)
rsa_key <- rsa_keygen(512)
n <- rsa_key$data$n # n = pq
sign_key_d <- rsa_key$data$d # sign (private key)
verify_key_e <- rsa_key$data$e # verification (public key)

## (3) AES encryption
aes <- AES(aes_key, mode = "ECB")
ciphertext_raw <- aes$encrypt(charToRaw(plaintext_msg))

## (4) Hashed Message (Hash tag) plaintext ---> Hash tag
hash_tag <- sha256(plaintext_msg)

## (5) RSA-sign : Hash tag ---> signature
rsa_signature <- bignum_mod_exp(bignum(charToRaw(hash_tag)), sign_key_d, n) # S = M^d mod n

## (6) Signed Message : (ciphertext_raw, rsa_signature)  

## (7) Bob receives : (ciphertext_raw, rsa_signature)  

## (8) Bob knows aes_key (for AES decryption)
##               verify_key (for verifying RSA-signature)

## (9) Decrypt ciphertext
bob_aes <- AES(aes_key, mode = "ECB")
bob_plaintext <- bob_aes$decrypt(ciphertext_raw)
bob_plaintext
bob_hasg_tag <- sha256(bob_plaintext)
calculated_hash_tag <- rawToChar(bignum_mod_exp(rsa_signature, verify_key_e, n))#M = S^e mod n
bob_hasg_tag
calculated_hash_tag

