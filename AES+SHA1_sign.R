#=========================================
# AES (using digest built-in function)
#=========================================

library("digest")

msg_chr <- "Hello! Our Team name is 4. This is LEA Encrypt !"
msg_raw <- charToRaw(msg_chr)
msg_raw

#AES Algoritm Setup
aes_key <- as.raw(0:15)
aes <- AES(aes_key, mode = "ECB")

#Call AES Encrypt
cipher_raw <- aes$encrypt(msg_raw)

#Generate HMAC Tag
hmac_key <- as.raw(16:31)
hmac_tag <- hmac(hmac_key, msg_chr, "sha1")
hmac_tag

### ciphertext packet = ( cipher_raw, hmac_tag ) ---> [Bob]
aes_key_bob <- as.raw(0:15)
aes_bob <- AES(aes_key_bob, mode = "ECB")

msg_bob_raw <- aes_bob$decrypt(cipher_raw, raw=TRUE)
msg_bob_chr <- rawToChar(msg_bob_raw)
msg_bob_chr

hmac_bob_key <- as.raw(16:31)
hmac_bob_tag <- hmac(hmac_bob_key, msg_bob_chr, "sha1")
hmac_bob_tag

stopifnot(identical(hmac_tag, hmac_bob_tag))
