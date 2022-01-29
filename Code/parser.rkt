#lang racket
;Hello grader or proffesor this is my scanner at the top and parser at the bottom. I know it not the best but for awhile it was trial and error a LOT in the beginning.
;as I got more used to it I got better at writing and reading it and you might be able to see that progression as the further you go
;down the project that is rougly the order I wrote all this code in. Also I hope there are not to many comments but I bet my code is
;bad at best so hopefully lots of comments explain what the hell I was thinking. It helped me as I went back.

;gets a character from an input file and returns that char. There used to be more to this fucntion but not anymore
(define (get-char input)
 (read-char input))

;Uses Ascii to check if a char is a valid symbol in the language if not stated here it is not valid. returns true or false
(define (valid-symbol? char)
  (let ([c (char->integer char)])                       ;uses get-char to get a char and convert to AsciI int
    (cond
      ((or (eq? c 10) (eq? c 13) (eq? c 32) (eq? c 36)) ;new line(10), return(13), space(32), $(36)
       #t)
      ((and (>= c 40) (<= c 43))                        ; ((40), )(41), *(42), +(43)
       #t)
      ((and (>= c 45) (<= c 58))                        ; -(45), .(46), /(47), 0-9(48-47), :(58)
       #t)
      ((eq? c 61)                                       ; =(61)
       #t)
      ((and (>= c 65) (<= c 90))                        ;A-Z(65-90)
       #t)
      ((and (>= c 97) (<= c 122))                       ;a-z(97-122)
       #t)
      (else
       #f))))

;Checks if char is a single token so no look ahead is needed. returns true or false
(define (simple-symbol? char)
  (let ([c (char->integer char)])
    (cond
      ((or (eq? c 43) (eq? c 45)) ; +(43), -(45)
       #t)
      ((or (eq? c 42) (eq? c 47)) ; *(42), /(47)
       #t)
      ((or (eq? c 40) (eq? c 41)) ; ((40), )(41)
       #t)
      (else                       ;returns false if it is not a simple symbol
       #f))))

;this returns the token of a simple symbol(ss) and yes this is not good pratice but I had trouble making simple-symbol? return two values that the conditional could handle.
(define (get-ss char)
  (let ([c (char->integer char)])
    (cond
      ((or (eq? c 43) (eq? c 45)) ; +(43), -(45) if it is either of those it returns add_op instead of the sign
       "add_op")
      ((or (eq? c 42) (eq? c 47)) ; *(42), /(47) if it is either of those it returns mult_op instead of the sign
       "mult_op")
      ((or (eq? c 40) (eq? c 41)) ; ((40), )(41) if it is either a parentheses it returns it in a string
       (string char))
      (else                       ;returns an empty string if nothing matches but that should not happen
       ""))))

;This function looks to see if the symbol : or $ have the correct follow up. I now realize that this function is dumb but I am not messing with it
(define (lookahead char file)
  (let* ([input (get-char file)]                  ;get next input
        [char-num (char->integer char)]           ;get the Ascii number of the passed char
        [input-num (char->integer input)])        ;get the Ascii number of the next char
    (cond
      ((and (eq? char-num 58) (eq? input-num 61)) ;is it the equal sign := 
       #t)

      ((and (eq? char-num 36) (eq? input-num 36)) ;is it the end sign $$
       #t)      

      (else                                       
       #f))))

;this is used for when the scanner sees a new number I think it uses tail recursion with temp-string. returns a list with the token
(define (num-rec file temp-string)
  (let* ([input (get-char file)]                                  ;get next input
         [char-num (char->integer input)])                        ;get the Ascii number of the char
    (cond
      ((and (>= char-num 48) (<= char-num 57))                    ;is next char a number
       (num-rec file (string-append temp-string (string input)))) ;add to string and get next input

      ((or (eq? char-num 13) (eq? char-num 32))                   ;is next char a return or space
       (list "number"))                                           ;return a list that only contains the string

      ((eq? (char->integer input) 41)                             ;is next char a )
       (list ")" "number"))                                       ;return a list that has the string and ) so as to not lose the )

      (else                                                       ;Some error happened such as having 9a you can't have a number with letters
       (error "Scanner error: invlaid numnber")))))              

;This is used for when the scanner sees a new letter it could either be a id or keyword. Also does use recursion and returns a list withe the token
(define (alp-rec file temp-string)
  (let* ([input (get-char file)]                                                            ;get next input
        [char-num (char->integer input)])                                                   ;get the Ascii number of the char
    (cond
      ((and (>= char-num 48) (<= char-num 57))                                              ;is next char a number
       (alp-rec file (string-append temp-string (string input))))                           ;Combine the ID string and the new number. Then iterate again with recusion
      
      ((or (and (>= char-num 65)(<= char-num 90)) (and (>= char-num 97)(<= char-num 122)))  ;is next char a capatilize or lower case letter
       (alp-rec file (string-append temp-string (string input))))                           ;Combine the ID string with the new letter. Then iterate again with recursion

      ((eq? char-num 41)                                                                    ;is next char a )
       (list ")" "ID"))                                                              ;return a list that has the ID and the ) so we do not lose the )
       
      ((or (eq? char-num 13) (eq? char-num 32))                                             ;is next char a return or space
       (cond
         ((string=? temp-string "read") (list "read"))                                      ;Before returning the list check to make sure it not a keyword
         ((string=? temp-string "write") (list "write"))                                    ;Before returning the list check to make sure it not a keyword
         (else (list "ID"))))                                                               ;The string is not a keyword and is valid so just return ID for the parser
                                       
      (else                                                                                 ;Some error happened such as having AC! you can't use a !
       (error "Scanner error: invlaid ID")))))

;This is the scanner that will look at a character and determine which function to go to. Will return a list with all the tokens 
(define (scanner file list)
  (let* ([input (get-char file)]                                                                ;gets the next char from the file
        [char-num (char->integer input)])                                                       ;I used the Ascii number a lot so i saved it as a variable
     (if (valid-symbol? input)                                                                  ;checks if it is a symbol in the language if not throw error
         (cond
           ((simple-symbol? input)                                                              ;is it a simple that needs no look ahead need like +
            (scanner file (cons (get-ss input) list)))                                          ;gets the token and adds to list

           ((eq? char-num 58)                                                                   ;is char a :
            (if (lookahead input file)                                                          ;need to look ahead one to see if next char is =
                (scanner file (cons ":=" list))                                                 ;if next char is = add := token to the list 
                (error "Scanner error: expected = after :")))                                   ;if not error

           ((and (>= char-num 48)  (<= char-num 57))                                            ;is char a number error checking is done in num-rec
            (scanner file (append (num-rec file (string input)) list)))                         ;add number to list           

           ((or (and (>= char-num 65)(<= char-num 90)) (and (>= char-num 97)(<= char-num 122))) ;is char a letter error checking is done in alp-rec
            (scanner file (append (alp-rec file (string input)) list)))                         ;add id or keyword to list

           ((or (eq? char-num 10) (eq? char-num 13) (eq? char-num 32))                          ;is char a newline, return, or space
            (scanner file list))                                                                ;then continue with program and don't add to list
            
           ((eq? char-num 36)                                                                   ;is char $
            (if (lookahead input file)                                                          ;need to look ahead one to see if next char is $
                (reverse (cons "$$" list))                                                      ;if next char is $ return list in correct order for parser 
                (error "Scanner error: expected $ after $")))                                   ;if not error
           
           (else                                                                                ;Most errors should be covered but if not it will throw an error
            (error "Scanner error: You used a bad scanner"))) 

         (error "Scanner error: invalid symbol detected: "(string input)))))                    ;This is the if statment if symbol is not valid throw error

;How the user can start the program if they only want to scan it calls the scanner to open the file and give an empty list for the scanner
(define (scan input-file)
  (if (string? input-file) (scanner (open-input-file (string-append "./Input/" input-file)) (list))
      (display"please use \"'s when typeing in the file like \"input01.txt\"")))

; ^^^^^SCANNER^^^^^SCANNER^^^^^SCANNER^^^^^SCANNER^^^^^SCANNER^^^^^SCANNER^^^^^SCANNER^^^^^
; -----------------------------------------------------------------------------------------
; -----------------------------------------------------------------------------------------
; vvvvvPARSERvvvvvPARSERvvvvvPARSERvvvvvPARSERvvvvvPARSERvvvvvPARSERvvvvvPARSERvvvvvPARSERv

;I had trouble for awhile as I knew how to parse the project but I was thinking in a Von Neumann language where I could make
;a list that I could edit as I work along the project. I now know that is a side effect and not the point of racket
;so I had to convert my sudo code into racket which called functions to check if there was a correct token then return the rest
;of the list it can get confusing as there are many nested function calls but that is how a parser has to work

;How the user starts the program that opens the file and checks if it can parse correctly
(define (parse input-file)
  (if (string? input-file) (program? (scanner (open-input-file (string-append "./Input/" input-file)) (list)))
      (display"please use \"'s when typeing in the file like \"input01.txt\"")))

;program predict rule needs to check if first token is a stmt_list or $$
(define (program? list)
  (cond                                                                 
    ((string=? "ID" (first list))                                          ;needs to go to statment list
     ($$?(stmt_list? list)))

    ((string=? "read" (first list))                                        ;needs to go to statment list
     ($$? (stmt_list? list)))

    ((string=? "write" (first list))                                       ;needs to go to statment list
     ($$?(stmt_list? list)))

    ((string=? "$$" (first list))                                          ;If the last token is $$ it pasrsed
     (display "Program parsed and scanned correctly"))

    (else (error "Parser error: program? expected ID, read, or write"))))

;stmt_list predict rule
(define (stmt_list? list)
  (cond
    ((string=? "ID" (first list))                                           ;first needs to check if there is a stament then check if there is a statment list
     (stmt_list? (stmt? list)))

    ((string=? "read" (first list))                                         ;first needs to check if there is a stament then check if there is a statment list
     (stmt_list? (stmt? list)))

    ((string=? "write" (first list))                                        ;first needs to check if there is a stament then check if there is a statment list
     (stmt_list? (stmt? list)))

    ((string=? "$$" (first list))                                           ; can just return list
     list)

    (else (error "Parser error: stmt_list? expected ID, read, or write"))))

;stmt predict rule
(define (stmt? list)
  (cond
    ((string=? "ID" (first list))                                           ;if token is ID check that it is id := expr
     (expr?(:=?(ID? list))))

    ((string=? "read" (first list))                                         ;if token is read check that it is read id 
     (ID?(read? list))) 

    ((string=? "write" (first list))                                        ;if token is write check if it is write expr
     (expr?(write? list)))

    (else (error "Parser error: stmt? expected ID, read, or write"))))

;expr predict rule
(define (expr? list)
  (cond
    ((string=? "(" (first list))                                            ;if token is term then check term tail
     (term_tail?(term? list)))

    ((string=? "ID" (first list))                                           ;if token is term then check term tail
     (term_tail?(term? list)))

    ((string=? "number" (first list))                                       ;if token is term then check term tail
     (term_tail?(term? list)))

    (else (error "Parser error: expr? expected (, ID, or number"))))

;term_tail predict rule
(define (term_tail? list)
  (cond
    ((string=? "add_op" (first list))                                       ;if token is add_op then check if it is add_op term term_tail
     (term_tail?(term? (add_op? list))))

    ((string=? ")" (first list))                                            ;if token is epsilon return token
     list)

    ((string=? "ID" (first list))                                           ;if token is epsilon return token
     list)

    ((string=? "read" (first list))                                         ;if token is epsilon return token
     list)

    ((string=? "write" (first list))                                        ;if token is epsilon return token
     list)

    ((string=? "$$" (first list))                                           ;if token is epsilon return token
     list)

    (else (error "Parser error: term_tail? expected add_op, ), ID, read, write, $$"))))

;term predict rule
(define (term? list)
  (cond
    ((string=? "(" (first list))                                             ;if token is a certain token check if it is a factor then factor tail
     (factor_tail?(factor? list)))

    ((string=? "ID" (first list))                                            ;if token is a certain token check if it is a factor then factor tail
     (factor_tail?(factor? list)))

    ((string=? "number" (first list))                                        ;if token is a certain token check if it is a factor then factor tail
     (factor_tail?(factor? list)))

    (else (error "Parser error: term? expected (, ID, or number"))))

;factor_tail perdict rule
(define (factor_tail? list)
  (cond
    ((string=? "mult_op" (first list))                                       ;check if token is mult_op that it is mult_op factor factor_tail
     (factor_tail?(factor? (mult_op? list))))

    ((string=? "add_op" (first list))                                        ;check if token is epsilon then return list
     list)

    ((string=? ")" (first list))                                             ;check if token is epsilon then return list
     list)

    ((string=? "ID" (first list))                                            ;check if token is epsilon then return list
     list)

    ((string=? "read" (first list))                                          ;check if token is epsilon then return list
     list)

    ((string=? "write" (first list))                                         ;check if token is epsilon then return list
     list)

    ((string=? "$$" (first list))                                            ;check if token is epsilon then return list
     list)

    (else (error "Parser error: factor_tail? expected mult_op, add_op, ), ID, read, write, or $$"))))

;factor predict rule
(define (factor? list)
  (cond
    ((string=? "(" (first list))                                             ;check if token is ( then check if it is ( expr )
     (rightpar?(expr?(leftpar? list))))

    ((string=? "ID" (first list))                                            ;check if token is ID then return list
     (ID? list)) 

    ((string=? "number" (first list))                                        ;check if token is number then return list
     (number? list))

    (else (error "Parser error: factor? expected (, ID, or number"))))

;add_op predict rule
(define (add_op? list)
  (if (string=? "add_op" (first list))        ;since my scanner makes + and - the same it only needs to check if string match then return rest of list
     (rest list)
     (error "Parser error: add_op? expected add_op")))

;mult_op predict rule
(define (mult_op? list)
  (if (string=? "mult_op" (first list))        ;since my scanner makes * and / the same it only needs to check if string match then return rest of list
     (rest list)
     (error "Parser error: mult_op? expected mult_op")))

;to check if a token is an ID then return the rest of the list
(define (ID? list)
  (if (string=? "ID" (first list))
      (rest list)
      (error "Parser error: ID? expected ID")))

;to check if a token is a number then return the rest of the list
(define (number? list)
  (if (string=? "number" (first list))
      (rest list)
      (error "Parser error: number? expected number")))

;to check if a token is a := then return the rest of the list
(define (:=? list)
  (if (string=? ":=" (first list))
      (rest list)
      (error "Parser error: :=? expected :=")))

;to check if a token is a read then return the rest of the list
(define (read? list)
  (if (string=? "read" (first list))
      (rest list)
      (error "Parser error: read? expected read")))

;to check if a token is a write then return the rest of the list
(define (write? list)
  (if (string=? "write" (first list))
      (rest list)
      (error "Parser error: write? expected write")))

;to check if a token is a ( then return the rest of the list
(define (leftpar? list)
  (if (string=? "(" (first list))
      (rest list)
      (error "Parser error: leftpar? expected (")))

;to check if a token is a ) then return the rest of the list
(define (rightpar? list)
  (if (string=? ")" (first list))
      (rest list)
      (error "Parser error: rightpar? expected )")))

;to check if a token is a $$ then return the rest of the list
(define ($$? list)
  (if (string=? "$$" (first list))
      (display "Program parsed and scanned correctly!")
      (error "Parser error: $$? expected $$")))

;displays a message for the user so they know what to do
(display "Hello to a start a parse type in the procedure parse with one argument that is a file name in a string. example: (parse \"input.txt\")")
(newline)
(display "To a start a scan type in the procedure scan with one argument that is a file name in a string. example: (scan \"input.txt\")")