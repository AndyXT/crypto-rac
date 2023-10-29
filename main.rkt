#lang racket
(require openssl/sha1)
(require base64)

(define (hex-string-to-b64 hex-string)
  (let* ([bytes (hex-string->bytes hex-string)]
         [b64-str (bytes->string/utf-8(base64-encode bytes))])
    b64-str)) 

(define challenge1-1
  (let* ([hex-string "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"]
         [b64-ans "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"]
         [b64-str (hex-string-to-b64 hex-string)])
    (if (string=? b64-ans b64-str)
      (displayln "challenge1-1 passed!")
      (displayln "error string incorrect!"))))

(define (bytes-xor bytes1 bytes2)
  (let* ([len (min (bytes-length bytes1) (bytes-length bytes2))]
         [bytes3 (make-bytes len)])
    (for ([i (in-range len)])
      (bytes-set! bytes3 i (bitwise-xor (bytes-ref bytes1 i) (bytes-ref bytes2 i))))
    bytes3))

(define challenge1-2
  (let* ([hex-string1 "1c0111001f010100061a024b53535009181c"]
         [hex-string2 "686974207468652062756c6c277320657965"]
         [ans "746865206b696420646f6e277420706c6179"]
         [bytes1 (hex-string->bytes hex-string1)]
         [bytes2 (hex-string->bytes hex-string2)]
         [bytes3 (bytes-xor bytes1 bytes2)]
         [hex-str (bytes->hex-string bytes3)])
    (if (string=? ans hex-str)
      (displayln "challenge1-2 passed!")
      (displayln "error string incorrect!"))))
