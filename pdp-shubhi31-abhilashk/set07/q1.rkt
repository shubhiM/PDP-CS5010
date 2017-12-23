;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; This file contains the solution of Question 1 of Problem Set 07

;; Flight Scheduling Problem

(require rackunit)
(require "extras.rkt")
(require "flight.rkt")

(check-location "07" "q1.rkt")

(provide can-get-there?
         fastest-itinerary
         travel-time)

;; CONSTANT DEFINITIONS

(define MINUTES-IN-AN-HOUR 60)
(define HOURS-IN-A-DAY 24)
(define TRAVELTIME-FOR-SAME-AIRPORT 0)

;; DATA DEFINITIONS

(define-struct airport (name flights-taken next-airports))

;; An Airport is a (make-airport String ListOfFlight ListOfAirport)
;;
;; INTERPRETATION:
;; name is the name of the airport
;; flights-taken is the list of flights taken to arrive at the current airport
;; next-airports is the list of directly connecting airports
;;
;; airport-fn : Airport -> ??
;;
;; TEMPLATE:
;; (define (airport-fn a)
;;  (... (airport-name a)
;;       (airport-flights-taken a)
;;       (loa-fn (airport-next-airports a))))
;;

(define-struct route (time flights))

;; An Route is a (make-route NonNegInt ListOfFlight)
;;
;; INTERPRETATION:
;; time is the total time of travel of the flights.
;; flights is the list of flights taken to go from point A to B
;;
;; route-fn : Route -> ??
;;
;; TEMPLATE:
;; (define (route-fn r)
;;  (... (route-time r)
;;       (route-flights r)))
;;

;; ListOfAirport (LOA) is one of
;; -- empty
;; -- (cons Airport LOA)
;; loa-fn : LOA -> ??
;;
;;(define (loa-fn as)
;;  (cond
;;    [(empty? as) ...]
;;    [else (... (airport-fn (first as))
;;               (loa-fn (rest as)))]))

;; FUNCTIONS

;; can-get-there? : String String ListOfFlight -> Boolean
;; GIVEN          : the names of two airports, airport-one and airport-two
;;                  (respectively), and a ListOfFlight that describes all of
;;                  the flights a traveller is willing to consider taking.
;; WHERE          : there are no non-trivial round trips
;;                  (Once you depart from an airport, you can't get back to it)
;; RETURNS        : true if and only if it is possible to fly from the
;;                  airport-one to the airport-two using only the given flights
;; EXAMPLES:
;;     (can-get-there? "06N" "LAX" deltaFlights)  =>  false
;;     (can-get-there? "LAX" "06N" deltaFlights)  =>  false
;;     (can-get-there? "LGA" "PDX" deltaFlights)  =>  true
;; STRATEGY       : Combine simple functions


(define
  (can-get-there? from to flights)
  (or (string=? from to)
      (not (empty? (possible-flights from to flights)))))

(begin-for-test
  (check-equal? (can-get-there? "06N" "LAX" deltaFlights)
                #false
                "No path available from 06N to LAX")
  (check-equal? (can-get-there? "LAX" "06N" deltaFlights)
                #false
                "No path available from LAX to 06N")

  (check-equal? (can-get-there? "LGA" "PDX" deltaFlights)
                #true
                "Path available from LGA to PDX")

  (check-equal? (can-get-there? "LGA" "LGA" deltaFlights)
                #true
                "Path available from LGA to LGA"))


;; fastest-itinerary : String String ListOfFlight -> ListOfFlight
;; GIVEN             : the names of two airports, airport-one and airport-two
;;                     (respectively), and a ListOfFlight that describes all of
;;                     the flights a traveller is willing to consider taking.
;; WHERE             : there are no non-trivial round trips, and it is possible
;;                     to fly from the airport-one to the airport-two using
;;                     only the given flights
;; RETURNS           : a list of flights that tells how to fly from the
;;                     airport-one to the airport-two in the least possible
;;                     time, using only the given flights
;; EXAMPLES          :
;;     (fastest-itinerary "LGA" "PDX" deltaFlights)
;; =>  (list (make-flight "Delta 0121" "LGA" "MSP"
;;                    (make-UTC 11 00) (make-UTC 14 09))
;;       (make-flight "Delta 2163" "MSP" "PDX"
;;                    (make-UTC 15 00) (make-UTC 19 02))
;; STRATEGY          : Combining Simpler Functions

(define (fastest-itinerary from to flights)
  (if (string=? from to)
      empty
      (route-flights (fastest-route from to flights))))


(begin-for-test
  (check-equal?
   (itinerary=?
    (fastest-itinerary "LGA" "PDX" deltaFlights)
    (list (make-flight "Delta 0121" "LGA" "MSP"
                       (make-UTC 11 00) (make-UTC 14 09))
          (make-flight "Delta 2163" "MSP" "PDX"
                       (make-UTC 15 00) (make-UTC 19 02))))
   true
   "The Fastest Itinerary from LGA to PDX is Delta0121 & Delta 2163")
  (check-equal?
   (itinerary=? (fastest-itinerary "LGA" "LGA" deltaFlights) empty)
   true
   "The Fastest Itinerary is empty"))


;; travel-time      : String String ListOfFlight -> NonNegInt
;; GIVEN            : the names of two airports, airport-one and airport-two
;;                    (respectively), and a ListOfFlight that describes all of
;;                    the flights a traveller is willing to consider taking.
;; WHERE            : there are no non-trivial round trips, and
;;                    it is possible to fly from the airport-one to the
;;                    airport-two using only the given flights
;; RETURNS          : the number of minutes it takes to fly from airport-one to
;;                    the airport-two, including any layovers, by the fastest
;;                    possible route that uses only the given flights
;; EXAMPLES:
;;     (travel-time "JFK" "JFK" panAmFlights)  =>  0
;;     (travel-time "LGA" "PDX" deltaFlights)  =>  482
;; STRATEGY         : Combining Simpler Functions

(define (travel-time from to flights)
  (if (string=? from to)
      TRAVELTIME-FOR-SAME-AIRPORT
      (route-time (fastest-route from to flights))))

(begin-for-test
  (check-equal? (travel-time "LGA" "PDX" deltaFlights) 482
                "travel time should be 482")
  (check-equal? (travel-time "LGA" "LGA" deltaFlights) 0
                "travel time should be 0"))


;; HELPER FUNCTIONS


;; fastest-route    : String String ListOfFlight -> Route
;; GIVEN            : the names of two airports, airport-one and airport-two
;;                    (respectively), and a ListOfFlight that describes all of
;;                    the flights a traveller is willing to consider taking.
;; RETURNS          : fastest route for the given input
;; EXAMPLES:
;;   (fastest-route "LGA" "PDX" deltaFlights)  =>
;;(make-route
;; 482
;; (list (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0)
;;                    (make-UTC 14 9))
;;       (make-flight "Delta 2163" "MSP" "PDX" (make-UTC 15 0)
;;                    (make-UTC 19 2))))
;; STRATEGY         : Using HOF Foldr on possible routes

(define (fastest-route from to flights)
  (let ((u (possible-routes from to flights)))
    (foldr
     ;; Route Route -> Route
     ;; GIVEN    : a Route and the Current Fastest Route
     ;; RETURNS  : The Fastest Route
     (λ (route fast-route)
       (cond
         [(< (route-time fast-route) (route-time route)) fast-route]
         [else route]))
     (first u)
     u)))


;; connecting-flights   : String ListOfFlight -> ListOfFlight
;; GIVEN                : A departure airport and ListOfFlight
;; RETURNS              : ListOfFlight containing all possible depart airports
;; EXAMPLES             :
;; (connecting-flights "LGA" deltaFlights) =>
;; (list (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9)))
;; STRATEGY             : Using HOF on Flights 

(define
  (connecting-flights airport-name flights)
  (filter
   ;; Flight -> Boolean
   ;; GIVEN    : a Flight
   ;; RETURNS  : true iff the airport name matches with depart
   (λ (flight) (string=? airport-name (departs flight))) flights))


;; airport-tree         : String ListOfFlight -> ListOfFlight
;; GIVEN                : Departure Airport and List of flights
;; RETURNS              : Tree of the given Flights
;; EXAMPLES             :
;;(airport-tree "DEN" deltaFlights) =>
;;(make-airport
;; "DEN" '()
;; (list (make-airport
;;        "LAX"
;;        (list
;;         (make-flight
;;          "Delta 5703" "DEN" "LAX" (make-UTC 14 4) (make-UTC 17 15)))
;;        (list
;;         (make-airport
;;          "PDX"
;;          (list (make-flight
;;                 "Delta 5703" "DEN" "LAX" (make-UTC 14 4) (make-UTC 17 15))
;;                (make-flight
;;                 "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9)))
;;          '())))))
;; STRATEGY             : Using Template of airport

(define (airport-tree from flights)
  (make-airport from empty (build-airport-tree from empty flights)))


;; build-airport-tree   : String ListOfFlight ListOfFlight -> ListOfAirport
;; GIVEN                : Current Airport Name, flights taken to get there and
;;                        ListOfAirport
;; RETURNS              : Airport
;; EXAMPLES             :
;; (build-airport-tree "DEN" empty deltaFlights)
;; (make-airport
;;  "LAX"
;;  (list
;;    (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4) (make-UTC 17 15)))
;;  (list
;;   (make-airport
;;    "PDX"
;;    (list
;;     (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4) (make-UTC 17 15))
;;     (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9)))
;;    '()))))
;; HALTING MEASURE      : Stops when ListOfFlight (flights) is empty
;; STRATEGY             : Using HOF map on ListOfFlight

(define (build-airport-tree name flights-taken flights)
  (map
   ;; Flight -> Flight
   ;; GIVEN    : a Flight
   ;; RETURNS  : Airports for the given flight
   (λ (flight)
     (make-airport (arrives flight)
                   (append flights-taken (list flight))
                   (build-airport-tree (arrives flight)
                                       (append flights-taken (list flight))
                                       flights)))
   (connecting-flights name flights)))


;; airport-descendants  : Airport -> ListOfAirports
;; GIVEN                : a Airport
;; RETURNS              : ListOfAirports
;; EXAMPLES             :
;; (airport-descendants (airport-tree "DEN" deltaFlights)) =>
;;(list
;; (make-airport
;;  "LAX"
;;  (list (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4)
;;                     (make-UTC 17 15)))
;;  (list
;;   (make-airport
;;    "PDX"
;;    (list
;;     (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4)
;;                  (make-UTC 17 15))
;;     (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35)
;;                  (make-UTC 20 9)))
;;    '())))
;; (make-airport
;;  "PDX"
;;  (list
;;   (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4) (make-UTC 17 15))
;;   (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9)))
;;  '()))
;; STRATEGY             : Using Template of Airport

(define (airport-descendants a)
  (append
   (airport-next-airports a)
   (airports-descendants (airport-next-airports a))))


;; airports-descendants : ListOfAirport -> Airport
;; GIVEN                : a ListOfAirports
;; RETURNS              : Airport
;; STRATEGY             : Using Template of ListOfAirports

(define (airports-descendants loa)
  (cond
    [(empty? loa) empty]
    [else (append
           (airport-descendants (first loa))
           (airports-descendants (rest loa)))]))


;; arriving-airports    : Airport String -> ListOfFlight
;; GIVEN                : A Airport and Destination Name
;; RETURNS              : List of Airport that arrive at Destination
;; EXAMPLES             :
;; (arriving-airports (airport-tree "DEN" deltaFlights) "LAX")    =>
;; (make-airport
;;  "LAX"
;;  (list (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4)
;;                     (make-UTC 17 15)))
;;  (list
;;   (make-airport
;;    "PDX"
;;    (list
;;     (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4) (make-UTC 17 15))
;;     (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9)))
;;    '())))
;; STRATEGY             : Using HOF filter on ListOfFlight

(define (arriving-airports airport arrives)
  (filter
   ;; Airport -> Boolean
   ;; GIVEN    : a ListOfAirport
   ;; RETURNS  : true iff the airport name matches arrives
   (λ (subtree)
     (string=? (airport-name subtree) arrives))
   (airport-descendants airport)))


;; possible-flights     : String String ListOfFlight -> ListOfFlight
;; GIVEN                : A departs, arrives airport and list of all flights
;; RETURNS              : List of Flights which can be taken to go from A to B
;; EXAMPLES             :
;; (possible-flights  "LGA" "LAX" deltaFlights)   =>
;;(list
;; (list
;;  (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9))
;;  (make-flight "Delta 1609" "MSP" "DEN" (make-UTC 20 35) (make-UTC 22 52))
;;  (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4) (make-UTC 17 15))))
;; STRATEGY             : Using HOF map on ListOfFlight

(define (possible-flights departs arrives flights)
  (map
   ;; Airport -> ListOfFlight
   ;; GIVEN    : a Airport
   ;; RETURNS  : flights taken for a given route
   (λ (a) (airport-flights-taken a))
   (arriving-airports (airport-tree departs flights) arrives)))


;; time-in-minutes      : UTC -> NonNegInt
;; GIVEN                : time in UTC
;; RETURNS              : time in Minutes
;; EXAMPLES             :
;; (time-in-minutes (make-UTC 15 06))   =>   906
;; STRATEGY             : Combining Simpler Functions

(define (time-in-minutes utc-time)
  (+ (* (UTC-hour utc-time) MINUTES-IN-AN-HOUR) (UTC-minute utc-time)))


;; time2-is-less?       : UTC UTC -> Boolean
;; GIVEN                : times in UTC
;; RETURNS              : true iff time2 is less than time1
;; EXAMPLES             :
;;  (time2-is-less? (make-UTC 15 06) (make-UTC 18 26))  =>  #false
;; STRATEGY             : Combining Simpler Functions

(define (time2-is-less? time1 time2)
  (< (time-in-minutes time2) (time-in-minutes time1)))


;; time-difference      : UTC UTC -> NonNegInt
;; GIVEN                : times in UTC
;; RETURNS              : difference between give time
;; EXAMPLES             :
;; (time-difference (make-UTC 15 06) (make-UTC 18 26))    => 200
;; STRATEGY             : Combining Simpler Functions

(define (time-difference time1 time2)
  (cond
    [(time2-is-less? time1 time2)
     (- (add-one-day-layover time2) (time-in-minutes time1))]
    [else (- (time-in-minutes time2) (time-in-minutes time1))]))


;; add-one-day-layover  : UTC -> NonNegInt
;; GIVEN                : time in UTC
;; RETURNS              : time in minutes with layover
;; EXAMPLES             :
;; (add-one-day-layover (make-UTC 15 06))  =>  2346
;; STRATEGY             : Combining Simpler Functions

(define (add-one-day-layover time2)
  (+ (time-in-minutes time2) (* HOURS-IN-A-DAY MINUTES-IN-AN-HOUR)))


;; journey-time         : Flight -> NonNegInt
;; GIVEN                : a Flight
;; RETURNS              : total journey time of the flight
;; EXAMPLES             :
;; (journey-time  (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0)
;;                             (make-UTC 14 9)))    => 189
;; STRATEGY             : Combining Simpler Functions

(define (journey-time flight)
  (time-difference (departs-at flight) (arrives-at flight)))


;; layover-time         : Flight Flight -> NonNegInt
;; GIVEN                : two flights
;; RETURNS              : layover time between flights
;; EXAMPLES             : 
;; (layover-time  (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0)
;;                             (make-UTC 14 9))
;;                (make-flight "Delta 1609" "MSP" "DEN" (make-UTC 20 35)
;;                             (make-UTC 22 52)))
;;                      => 386
;; STRATEGY             : Combining Simpler Functions

(define (layover-time flight1 flight2)
  (time-difference (arrives-at flight1)(departs-at flight2)))


;; total-time           : ListOfFlight -> NonNegInt
;; GIVEN                : a list of flights 
;; RETURNS              : total duration of the journey
;; EXAMPLES             :
;; (total-time (list
;; (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9))
;; (make-flight "Delta 1609" "MSP" "DEN" (make-UTC 20 35) (make-UTC 22 52))
;; (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4) (make-UTC 17 15))))
;;            => 1815
;; HALTING MEASURE      : When length of ListOfFlight is 1
;; STRATEGY             : Combining Simpler Functions

(define (total-time lof)
  (cond
    [(empty? (rest lof)) (journey-time (first lof))]
    [else (+ (journey-time (first lof))
             (layover-time (first lof) (first (rest lof)))
             (total-time (rest lof)))]))


;; possible-routes         : String String ListOfFlights -> ListOfRoutes
;; GIVEN                   : Departs and Arrives Airport and list all possible
;;                           flights
;; RETURNS                 : List of Flights with their total time
;; EXAMPLES                :
;; (possible-routes "LGA" "PDX" deltaFlights) =>
;; (list
;; (make-route
;;  482
;;  (list
;;   (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9))
;;   (make-flight "Delta 2163" "MSP" "PDX" (make-UTC 15 0) (make-UTC 19 2))))
;; (make-route
;;  1989
;;  (list
;;   (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9))
;;   (make-flight "Delta 1609" "MSP" "DEN" (make-UTC 20 35) (make-UTC 22 52))
;;   (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4) (make-UTC 17 15))
;;   (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9)))))
;; STRATEGY                : Using HOF map on ListOfListOfFlight

(define (possible-routes from to flights)
  (map
   ;; ListOfFlight -> Route
   ;; GIVEN    : a ListOfFlight
   ;; RETURNS  : Route for a given set of flights
   (lambda (lof)
     (make-route (total-time lof) lof))
   (possible-flights from to flights)))

;; itinerary=?    : ListOfFlight ListOfFlight -> Boolean
;; GIVEN          : two lists of flights
;; RETURNS        : true if both lists of flights are same
;; EXAMPLES       :
;;
;;(itinerary=?
;;(fastest-itinerary "LGA" "PDX" deltaFlights)
;; (list (make-flight "Delta 0121" "LGA" "MSP"
;;                     (make-UTC 11 00) (make-UTC 14 09))
;;       (make-flight "Delta 2163" "MSP" "PDX"
;;                    (make-UTC 15 00) (make-UTC 19 02))))
;;
;; STRATEGY       : Use HOF andmap and map

(define (itinerary=? it1 it2)
  (and (= (length it1) (length it2))
       (andmap (lambda (bool) bool)
               (map flight=? it1 it2))))


;; Testing examples

(define deltaFlights
  (let ((u make-UTC))
    (list
     (make-flight "Delta 0121" "LGA" "MSP" (u 11 00) (u 14 09))
     (make-flight "Delta 1609" "MSP" "DEN" (u 20 35) (u 22 52))
     (make-flight "Delta 5703" "DEN" "LAX" (u 14 04) (u 17 15))
     (make-flight "Delta 2077" "LAX" "PDX" (u 17 35) (u 20 09))
     (make-flight "Delta 2163" "MSP" "PDX" (u 15 00) (u 19 02)))))

(define flight-lyst
  (list (make-flight "Delta123" "A" "B" (make-UTC 10 01) (make-UTC 10 01))
        (make-flight "Delta124" "A" "C" (make-UTC 10 01) (make-UTC 10 01))
        (make-flight "Delta125" "A" "D" (make-UTC 10 01) (make-UTC 10 01))
        (make-flight "Delta126" "B" "C" (make-UTC 10 01) (make-UTC 10 01))
        (make-flight "Delta127" "B" "E" (make-UTC 10 01) (make-UTC 10 01))
        (make-flight "Delta128" "E" "F" (make-UTC 10 01) (make-UTC 10 01))))


(define syntheticFlights
  (list (make-flight "1010" "UVW" "XYZ" (make-UTC 23 59) (make-UTC  4 20))
        (make-flight "2020" "UVW" "XYZ" (make-UTC  3 00) (make-UTC  7 15))
        (make-flight "3030" "UVW" "XYZ" (make-UTC 19 45) (make-UTC  0  0))
        (make-flight "0100" "RST" "XYZ" (make-UTC 10 00) (make-UTC 17 55))
        (make-flight "0101" "RST" "UVW" (make-UTC  7 10) (make-UTC 12 05))
        (make-flight "0102" "RST" "UVW" (make-UTC 11 59) (make-UTC  4 50))
        (make-flight "0103" "RST" "UVW" (make-UTC 15 15) (make-UTC 20 20))
        (make-flight "0201" "OPQ" "RST" (make-UTC  6 20) (make-UTC  9 00))
        (make-flight "0202" "OPQ" "RST" (make-UTC  9 05) (make-UTC 11 30))
        (make-flight "0203" "OPQ" "RST" (make-UTC 14 30) (make-UTC 17 01))
        (make-flight "0204" "OPQ" "UVW" (make-UTC 10 00) (make-UTC 16 58))
        (make-flight "0301" "LMN" "OPQ" (make-UTC  8 19) (make-UTC 11 30))
        (make-flight "0302" "LMN" "OPQ" (make-UTC 23 00) (make-UTC  2 15))
        (make-flight "0401" "IJK" "LMN" (make-UTC  5 45) (make-UTC 11 35))
        (make-flight "0402" "IJK" "LMN" (make-UTC  9 35) (make-UTC 15 25))
        (make-flight "0403" "IJK" "LMN" (make-UTC 13 30) (make-UTC 19 15))
        (make-flight "0404" "IJK" "LMN" (make-UTC 23 20) (make-UTC  5 45))
        (make-flight "0501" "FGH" "IJK" (make-UTC 10 48) (make-UTC 12 15))
        (make-flight "0502" "FGH" "IJK" (make-UTC 14 20) (make-UTC 15 45))
        (make-flight "0503" "FGH" "IJK" (make-UTC 18 30) (make-UTC 19 50))
        (make-flight "0504" "FGH" "UVW" (make-UTC 20 30) (make-UTC 13 00))
        (make-flight "0601" "CDE" "FGH" (make-UTC  7 45) (make-UTC  8 40))
        (make-flight "0602" "CDE" "FGH" (make-UTC  9 45) (make-UTC 10 44))
        (make-flight "0603" "CDE" "FGH" (make-UTC 16 45) (make-UTC 17 50))
        (make-flight "0604" "CDE" "FGH" (make-UTC 18 45) (make-UTC 19 48))))