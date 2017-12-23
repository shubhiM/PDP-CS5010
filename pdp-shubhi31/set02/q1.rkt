;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q1.rkt: provides implemention for a non graphical text editor. The edit
;; function allows manipulation of text in the editor based on the key event
;; passed.

(require rackunit)
(require 2htdp/universe)
(require "extras.rkt")

(provide make-editor)
(provide editor-pre)
(provide editor-post)
(provide editor?)
(provide edit)

;; constants

(define TAB "\t")
(define BACKSPACE "\b")
(define RETURN "\r")
(define LEFT "left")
(define RIGHT "right")


;; String helper functions

; DATA DEFINITIONS: none
; one-string? : String -> Boolean
; GIVEN: A string of any length
; RETURNS: true if the given string's length equals to one
; EXAMPLES:
; -- (one-string? "name") = false
; -- (one-string? "") = false
; -- (one-string? "n") = true
; DESIGN STRATEGY: combine simpler functions

(define (one-string? s)
  (= (string-length s) 1))


; DATA DEFINITIONS: none
; string-first : String -> String
; GIVEN: a string of any length
; RETURNS: first character of the input string
; EXAMPLES:
; -- (string-first "name") = "n"
; -- (string-first "n") = "n"
; DESIGN STRATEGY: combine simpler functions

(define (string-first s)
  (if (> (string-length s) 0) (string-ith s 0) s))


; DATA DEFINITIONS: none
; string-last : String -> String
; GIVEN: a string of any length
; RETURNS: last character of the given string.
; EXAMPLES:
; -- (string-last "name") = "e"
; -- (string-last "n") = "n"
; -- (string-last "") = ""
; DESIGN STRATEGY: combine simpler functions

(define (string-last s)
  (if (> (string-length s) 0) (string-ith s (- (string-length s) 1)) s))


; DATA DEFINITIONS: none
; string-rest : String -> String
; GIVEN: a string of any length
; RETURNS: the given string after removing first character from it
; EXAMPLES:
; -- (string-rest "name") = "ame"
; -- (string-rest "n") = ""
; -- (string-last "") = ""
; DESIGN STRATEGY: combine simpler functions

(define (string-rest s)
  (if (> (string-length s) 0) (substring s 1) s))


; DATA DEFINITIONS: none
; string-remove-last : String -> String
; GIVEN: a string of any length
; RETURNS: the given string after removing last character from it
; EXAMPLES:
; -- (string-remove-last "name") = "nam"
; -- (string-remove-last "n") = ""
; -- (string-remove-last "") = ""
; DESIGN STRATEGY: combine simpler functions

(define (string-remove-last s)
  (if (> (string-length s) 0) (substring s 0 (- (string-length s) 1)) s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Editor specific functions

; DATA DEFINITIONS

(define-struct editor (pre post))

; An Editor is a
; -- (make-editor String String)
; INTERPRETATION :
; -- pre represents text to the left of the cursor
; -- post represents text to the right of the cursor

;; Template
; editor-fn : Editor -> ??
#|
 (define (editor-fn (e)
   (...
      (editor-pre e) 
      (editor-post e)))
|#


; A KeyEvent is a predefined scalar data type in 2htdp/universe in Racket.
; It represents key board events as strings. All key events are strings
; but all strings are not key events.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; move-left : Editor -> Editor
; GIVEN: any editor
; RETURNS: A new editor with cursor position changed to one step towards left
; EXAMPLES:
; -- (move-left (make-editor "ft" " name")) = (make-editor "f" "t name")
; -- (move-left (make-editor "" "name")) -> (make-editor "" "name")
; -- (move-left (make-editor "" "")) -> (make-editor "" "")
; DESIGN STRATEGY: combine simpler functions

(define (move-left e)
  (make-editor
   (string-remove-last (editor-pre e))
   (string-append (string-last (editor-pre e)) (editor-post e))))



; move-right : Editor -> Editor
; GIVEN: any editor
; RETURNS: A new editor with cursor position changed to one step towards
;  right.
; EXAMPLES:
; -- (move-right (make-editor "f" " name")) = (make-editor "f " "name")
; -- (move-right (make-editor "" "name")) = (make-editor "n" "ame")
; -- (move-right (make-editor "" "")) = (make-editor "" "")
; -- (move-right (make-editor "name" "")) = (make-editor "name" "")
; DESIGN STRATEGY: combine simpler functions

(define (move-right e)
  (make-editor
   (string-append (editor-pre e) (string-first (editor-post e)))
   (string-rest (editor-post e))))


; insert-text : Editor String -> Editor
; GIVEN : Any editor and a string representing one character on the keyboard.
; RETURNS: A new editor with the string input added to the left of the
;  cursor's position
; EXAMPLES:
; -- (insert-text (make-editor "f" "name") ":") = (make-editor "f:" "name")
; -- (insert-text (make-editor "" "name") "f") = (make-editor "f" "ame")
; -- (insert-text (make-editor "" "") "a") = (make-editor "a" "")
; -- (insert-text (make-editor "name" "") "f") = (make-editor "namef" "")
; DESIGN STRATEGY: combine simpler functions
(define (insert-text e s)
  (make-editor
   (string-append (editor-pre e) s)
   (editor-post e)))



; move-back-and-delete : Editor -> Editor
; GIVEN: any editor
; RETURNS: A new editor with the one character removed from the left of the
;   cursor's position
; EXAMPLES:
; -- (move-back-and-delete (make-editor "fooo" "")) = (make-editor "foo" "")
; -- (move-back-and-delete (make-editor "" "name")) = (make-editor "" "name")
; -- (move-back-and-delete (make-editor "" "")) = (make-editor "" "")
; -- (move-back-and-delete (make-editor "name" "")) = (make-editor "nam" "")
; DESIGN STRATEGY: combine simpler functions

(define (move-back-and-delete e)
  (make-editor
   (string-remove-last (editor-pre e))
   (editor-post e)))



; edit : Editor KeyEvent -> Editor
; GIVEN: An editor ed and a key event ke.
; RETURNS: The new editor with key event applied on it.
; EXAMPLES:
#|-- (edit (make-editor "a" "") "\t") = (make-editor "a" "")
  -- (edit (make-editor "" "") "\r") = (make-editor "" "")
  -- (edit (make-editor "" "a") "\b") = (make-editor "" "a")
  -- (edit (make-editor "first" "name") " ") = (make-editor "first " "name")
  -- (edit (make-editor "first" "name") "b") = (make-editor "firstb" "name")
  -- (edit (make-editor "first" "name") "left") = (make-editor "firs" "tname")
  -- (edit (make-editor "f" "name") "right") = (make-editor "fn" "ame")
  -- (edit (make-editor "first" "name") "up") = (make-editor "first" "name")
|#   
; STRATEGY: divide into cases based on the Key Event

(define (edit ed ke)
  (cond [ (one-string? ke)
         (cond [ (or (key=? TAB ke) (key=? RETURN ke)) ed]
               [ (key=? BACKSPACE ke) (move-back-and-delete ed)]
               [else (insert-text ed ke)])]
        [else
         (cond [ (key=? LEFT ke) (move-left ed)]
               [ (key=? RIGHT ke) (move-right ed)]
               [ else ed])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(begin-for-test
  (check-equal? 
   (edit (make-editor "a" "") "\t")
   (make-editor "a" "")              
   "Editor value should remain same as '\t' event needs to be ignored")

  (check-equal? 
   (edit (make-editor "a" "") "\r")
   (make-editor "a" "")              
   "Editor value should remain same as '\r' event needs to be ignored")

  (check-equal? 
   (edit (make-editor "a" "") "\b")
   (make-editor "" "")              
   "The value 'a' before cursor, should be removed from the editor.")

  (check-equal? 
   (edit (make-editor "a" "") ",")
   (make-editor "a," "")              
   "The value ',' should be added before cursor in the editor")

  (check-equal? 
   (edit (make-editor "a" "") " ")
   (make-editor "a " "")              
   "The value ' ' should be added before cursor in the editor")
  
  (check-equal? 
   (edit (make-editor "a" "") "b")
   (make-editor "ab" "")              
   "The value 'b' should be added before cursor in the editor")
  
  (check-equal? 
   (edit (make-editor "a" "") "left")
   (make-editor "" "a")              
   "The cursor should move one step left in the editor.")

  (check-equal? 
   (edit (make-editor "a" "") "right")
   (make-editor "a" "")              
   "Editor value should remain same as cursor cannot move to right
      until there is alteast one character to the cursor's right.")

  (check-equal? 
   (edit (make-editor "a" "") "up")
   (make-editor "a" "")              
   "editor value should remain the same as multicharacter events excluding
      'left' and 'right' needs to be ignored ")

  (check-equal? 
   (edit (make-editor "" "") "\t")
   (make-editor "" "")              
   "Editor value should remain same as '\t' event needs to be ignored")

  (check-equal? 
   (edit (make-editor "" "") "\r")
   (make-editor "" "")              
   "Editor value should remain same as '\r' event needs to be ignored")

  (check-equal? 
   (edit (make-editor "" "") "\b")
   (make-editor "" "")              
   "Editor value should remain same as there is nothing to the left of
       editor that can be deleted")

  (check-equal? 
   (edit (make-editor "" "") ",")
   (make-editor "," "")              
   "The value ',' should be added before cursor in the editor")

  (check-equal? 
   (edit (make-editor "" "") " ")
   (make-editor " " "")              
   "The value ' ' should be added before cursor in the editor")
  
  (check-equal? 
   (edit (make-editor "" "") "b")
   (make-editor "b" "")              
   "The value 'b' should be added before cursor in the editor")
  
  (check-equal? 
   (edit (make-editor "" "") "left")
   (make-editor "" "")              
   "Editor value should remain same as cursor cannot be moved to
       left as there is nothing to the left of the editor.")

  (check-equal? 
   (edit (make-editor "" "") "right")
   (make-editor "" "")              
   "Editor value should remain same as cursor cannot be moved to
       right as there is nothing to the right of the editor.")

  (check-equal? 
   (edit (make-editor "" "") "up")
   (make-editor "" "")              
   "editor value should remain the same as multicharacter events excluding
      'left' and 'right' needs to be ignored ")

  (check-equal? 
   (edit (make-editor "" "a") "\t")
   (make-editor "" "a")              
   "Editor value should remain same as '\t' event needs to be ignored")

  (check-equal? 
   (edit (make-editor "" "a") "\r")
   (make-editor "" "a")              
   "Editor value should remain same as '\r' event needs to be ignored")

  (check-equal? 
   (edit (make-editor "" "a") "\b")
   (make-editor "" "a")              
   "Editor value should remain same as there is nothing to the left of
       editor that can be deleted")

  (check-equal? 
   (edit (make-editor "" "a") ",")
   (make-editor "," "a")              
   "The value ',' should be added before cursor in the editor")

  (check-equal? 
   (edit (make-editor "" "a") " ")
   (make-editor " " "a")              
   "The value ' ' should be added before cursor in the editor")
  
  (check-equal? 
   (edit (make-editor "" "a") "b")
   (make-editor "b" "a")              
   "The value 'b' should be added before cursor in the editor")
  
  (check-equal? 
   (edit (make-editor "" "a") "left")
   (make-editor "" "a")              
   "Editor value should remain same as cursor cannot be moved to
       left as there is nothing to the left of the editor.")

  (check-equal? 
   (edit (make-editor "" "a") "right")
   (make-editor "a" "")              
   "The cursor should move one step right in the editor")

  (check-equal? 
   (edit (make-editor "" "a") "up")
   (make-editor "" "a")              
   "editor value should remain the same as multicharacter events excluding
      'left' and 'right' needs to be ignored ")

  (check-equal? 
   (edit (make-editor "first" "name") "\t")
   (make-editor "first" "name")              
   "Editor value should remain same as '\t' event needs to be ignored")

  (check-equal? 
   (edit (make-editor "first" "name") "\r")
   (make-editor "first" "name")              
   "Editor value should remain same as '\r' event needs to be ignored")

  (check-equal? 
   (edit (make-editor "first" "name") "\b")
   (make-editor "firs" "name")              
   "The cursor should remove 't' from first which is one step left
       to the editor.")

  (check-equal? 
   (edit (make-editor "first" "name") ",")
   (make-editor "first," "name")              
   "The value ',' should be added before cursor in the editor")

  (check-equal? 
   (edit (make-editor "first" "name") " ")
   (make-editor "first " "name")              
   "The value ' ' should be added before cursor in the editor")
  
  (check-equal? 
   (edit (make-editor "first" "name") "b")
   (make-editor "firstb" "name")              
   "The value 'b' should be added before cursor in the editor")
  
  (check-equal? 
   (edit (make-editor "first" "name") "left")
   (make-editor "firs" "tname")              
   "The cursor should move one step left in the editor.")

  (check-equal? 
   (edit (make-editor "first" "name") "right")
   (make-editor "firstn" "ame")              
   "The cursor should move one step right in the editor")

  (check-equal? 
   (edit (make-editor "first" "name") "up")
   (make-editor "first" "name")              
   "editor value should remain the same as multicharacter events excluding
      'left' and 'right' needs to be ignored ")
  )