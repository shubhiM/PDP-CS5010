;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; This file contains the solution of Question 1 of Problem Set 08

;; Flight Scheduling Problem

(require rackunit)
(require "extras.rkt")
(require "flight.rkt")

(check-location "08" "q1.rkt")

(provide can-get-there?
         fastest-itinerary
         travel-time)

;; CONSTANT DEFINITIONS

(define MINUTES-IN-AN-HOUR 60)
(define HOURS-IN-A-DAY 24)
(define TRAVELTIME-FOR-SAME-AIRPORT 0)


;; DATA DEFINITIONS


;; A ListOfFlight (LOF) is a list of given flights where flight is an Abstract
;; data type.

(define-struct node (flight flights cost))

;; A Node is a (make-node Flight ListOfFlight NonNegInt)
;;
;; INTERPRETATION:
;; flight is the current flight
;; flights is the list of flights taken to reach current node including
;; current flight
;; cost is the total time taken in minutes to get to current node. 
;;
;; node-fn : Node -> ??
;;
;; TEMPLATE:
#;(define (node-fn n)
  (... (node-flight n)
       (node-flights n)
       (node-cost n)))


;; ListOfNode (LON) is one of
;; -- empty
;; -- (cons Node LON)
;; lon-fn : LON -> ??
;;
#;(define (lon-fn ns)
  (cond
    [(empty? ns) ...]
    [else (... (node-fn (first ns))
               (lon-fn (rest ns)))]))


;; ListOfListOfNode (LOLON) is one of
;; -- empty
;; -- (cons LON LOLON)
;; lolon-fn: LOLON -> ??

#;(define (lolon-fn lons)
  (cond
    [(empty? lons) ..]
    [else (... (lon-fn (first lons))
               (lolon-fn (rest lons)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Testing examples

(define deltaFlights
  (let ((u make-UTC))
    (list
     (make-flight "Delta 0121" "LGA" "MSP" (u 11 00) (u 14 09))
     (make-flight "Delta 1609" "MSP" "DEN" (u 20 35) (u 22 52))
     (make-flight "Delta 5703" "DEN" "LAX" (u 14 04) (u 17 15))
     (make-flight "Delta 2077" "LAX" "PDX" (u 17 35) (u 20 09))
     (make-flight "Delta 2163" "MSP" "PDX" (u 15 00) (u 19 02)))))

(define testNode (make-node
  (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9))
  (list (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9)))
  154))

(define deltaNodes (list
 (make-node
  (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9))
  (list (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9)))
  189)
 (make-node
  (make-flight "Delta 2163" "MSP" "PDX" (make-UTC 15 0) (make-UTC 19 2))
  (list
   (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9))
   (make-flight "Delta 2163" "MSP" "PDX" (make-UTC 15 0) (make-UTC 19 2)))
  482)
 (make-node
  (make-flight "Delta 1609" "MSP" "DEN" (make-UTC 20 35) (make-UTC 22 52))
  (list
   (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9))
   (make-flight "Delta 1609" "MSP" "DEN" (make-UTC 20 35) (make-UTC 22 52)))
  712)
 (make-node
  (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4) (make-UTC 17 15))
  (list
   (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9))
   (make-flight "Delta 1609" "MSP" "DEN" (make-UTC 20 35) (make-UTC 22 52))
   (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4) (make-UTC 17 15)))
  1815)
 (make-node
  (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9))
  (list
   (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9))
   (make-flight "Delta 1609" "MSP" "DEN" (make-UTC 20 35) (make-UTC 22 52))
   (make-flight "Delta 5703" "DEN" "LAX" (make-UTC 14 4) (make-UTC 17 15))
   (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9)))
  1989)))

(define sample-flights (list
 (make-flight "NoWays 6" "AP3" "AP4" (make-UTC 9 9) (make-UTC 21 51))
 (make-flight "NoWays 7" "AP3" "AP4" (make-UTC 9 3) (make-UTC 21 9))
 (make-flight "NoWays 4" "AP2" "AP3" (make-UTC 22 26) (make-UTC 14 34))
 (make-flight "NoWays 5" "AP2" "AP3" (make-UTC 14 42) (make-UTC 14 6))
 (make-flight "NoWays 2" "AP1" "AP2" (make-UTC 11 43) (make-UTC 7 17))
 (make-flight "NoWays 3" "AP1" "AP2" (make-UTC 19 21) (make-UTC 7 3))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (not (empty? (fastest-node from to flights)))))

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
                "Path available from LGA to LGA")
  (check-equal? (can-get-there? "ABC" "PQR" deltaFlights)
                #false
                "Path not available from ABC to PQR")
  (check-equal? (can-get-there? "AP2" "AP1" sample-flights)
                #false
                "Path not available from AP2 to AP1"))


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
      (node-flights (fastest-node from to flights))))

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
      (node-cost (fastest-node from to flights))))



(begin-for-test
  (check-equal? (travel-time "LGA" "PDX" deltaFlights) 482
                "travel time should be 482")
  (check-equal? (travel-time "LGA" "LGA" deltaFlights) 0
                "travel time should be 0"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS


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
;; STRATEGY       : Use HOF andmap

(define (itinerary=? it1 it2)
  (and (= (length it1) (length it2))
       (andmap flight=? it1 it2)))


;; sp-set-is-full? : ListOfNode NonNegInt -> Boolean
;; GIVEN           : sp-set is the list of visited nodes and count is the
;;            number of flights that need to considered for shortest path.
;; RETURNS         : true if the shortest-path contains all the flights.
;; EXAMPLES        :
;; (sp-set-is-full? deltaNodes 5) = #true
;; (sp-set-is-full? empty 5) = #false 
;; STRATEGY        : combine simple functions

(define (sp-set-is-full? sp-set count)
  (= (length sp-set) count))


;; unvisited-nodes  : ListOfNode ListOfNode -> ListOfNode
;; GIVEN            : sp-set is the list of visited nodes and frontier-nodes is
;;                    the list of nodes that need to be considered for shortest
;;                    path
;; RETURNS          : list of unvisited nodes, nodes that are in frontier nodes
;;                    but not in the sp-set
;; EXAMPLES:
;;
;; STRATEGY: use HOF Filter on frontier nodes

(define (unvisited-nodes sp-set frontier-nodes)
  (filter
   ;; Node -> Boolean
   ;; RETURNS : true if the given node is not in the frontier set
   (lambda (node) (not (member? node sp-set))) frontier-nodes))

;; min-cost-node: ListOfNode -> Node
;; GIVEN        : list of nodes
;; RETURNS      : node with the minimum cost from the given list of nodes
;; EXAMPLES:
;;
;; STRATEGY     : use HOF foldr on the given list of nodes
(define (min-cost-node nodes)
  (foldr
   ;; Node Node -> Node
   ;; GIVEN   : current node and previous minimum node
   ;; RETURNS : the node with minimum cost
   (lambda (node f-node)
           (if (< (node-cost node) (node-cost f-node))
               node
               f-node))
         (first nodes)
         nodes))

;; next-node-to-visit  : ListOfNode ListOfNode -> Node
;; GIVEN               : sp-set is the list of visited nodes and frontier-nodes
;;                       is the list of the nodes to be considered for shortest
;;                       path
;; RETURNS             : minimum cost node from frontier-nodes that is not
;;                       already visited
;; EXAMPLES            :
;;
;; STRATEGY            : combine simpler functions
(define (next-node-to-visit sp-set frontier-nodes)
  (min-cost-node (unvisited-nodes sp-set frontier-nodes)))


;; connecting-flights-to : Flight ListOfFlight -> ListOfFlight
;; GIVEN                 : A Flight and List of FLights
;; RETURNS               : The list of connecting flights from the given flight
;;                         and the given list of flights
;; EXAMPLES              :
;; STRATEGY              : use HOF Filter on flights

(define (connecting-flights-to flight flights)
  (filter
   ;; Flight -> Boolean
   ;; GIVEN    : a Flight
   ;; RETURNS  : true if departing airport of f is same as arriving airport
   ;; of flight
   (lambda (f) (string=? (arrives flight) (departs f))) flights))


;; total-time  : Flight Flight -> NonNegInt
;; GIVEN       : Two Flights
;; RETURNS     : sum of the flying time of first flight and layover between
;;               first and second flight
;; EXAMPLES
;; STRATEGY    : combine simple functions
(define (total-time f1 f2)
  (+ (journey-time f2) (layover-time f1 f2)))


;; adjacent-nodes : Node ListOfFlights -> ListOfNode
;; GIVEN          : A Node and List of Flights
;; RETURNS        : list of nodes from the given list of flights which are
;;                  adjacent to the given node
;; EXAMPLES       : (adjacent-nodes testNode deltaFlights) => '()
;; STRATEGY       : use HOF map on list of flights
(define (adjacent-nodes node flights)
  (map
   ;; FLight -> Node
   ;; RETURNS    : the node corresponding to the current flight
   (lambda (f)
         (make-node f (append (node-flights node) (list f))
                    (+ (node-cost node) (total-time (node-flight node) f)))) 
       (connecting-flights-to (node-flight node) flights)))


;; shortest-path-tree : Node NonNegInt ListOfFlight
;;         ListOfNode ListOfNode -> ListOfNode
;; GIVEN:
;; -- source-node: is the starting node for which shortest-path-tree needs to be
;; created
;; -- count is the number of the flights which needs to be considered for the
;; shortest path tree
;; -- flights is the given list of flights
;; -- sp-set is the shortest-path-tree with list of nodes with minimum cost from
;; starting node
;; -- frontier-nodes is the list of nodes which are to be considered for
;; creating shortest path tree for the source node

;; RETURNS          : list of nodes with all the nodes updated with mimum cost
;;                    from the source node

;; EXAMPLES:   (shortest-path-tree (make-node
;;(make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9))
;;(list (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9)))
;;154) 5 deltaFlights empty empty)     =>
;; (list
;; (make-node
;;  (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9))
;;  (list
;;   (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9)))
;;  154))
;; HALTING MEASURE  : Length of Sp-se or Unvisited Nodes 
;: STRATEGY         : cases on sp-set and frontier nodes

(define (shortest-path-tree source-node count flights sp-set frontier-nodes)
  (cond [(sp-set-is-full? sp-set count) sp-set]
        [(empty? sp-set) (shortest-path-tree
                          source-node count flights
                          (append sp-set (list source-node))
                          (append frontier-nodes
                                  (adjacent-nodes source-node flights)))]
        [(empty? frontier-nodes) sp-set]
        [(empty? (unvisited-nodes sp-set frontier-nodes)) sp-set]
        [else
         (let ((next-node (next-node-to-visit sp-set frontier-nodes)))
           (shortest-path-tree source-node count flights
                               (append sp-set (list next-node))
                               (append frontier-nodes
                                       (adjacent-nodes next-node flights))))]))


;; start-nodes : String ListOfFlight -> ListofNode
;; GIVEN       : name of the starting aiport and list of flights
;; RETURNS     : the list of the possible starting nodes for the given airport
;;               name
;; EXAMPLES    : (start-nodes "LGA" deltaFlights)  => 
;;(list
;; (make-node
;;  (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9))
;;  (list
;;   (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9)))
;;  189))
;; STRATEGY    : use HOF map and filter on the list of flights

(define (start-nodes from flights)
  (map
   ;; FLight -> Node
   ;; Returns Node corresponding to the flights
   (λ (flight)
       (make-node flight (list flight) (journey-time flight)))
       (filter
        ;; Flight -> Boolean
        ;; RETURNS: true if the flight f departs from the given aiport name
        (lambda (f) (string=? from (departs f))) flights)))


;; filter-node : String String ListOfFlight -> ListOfNode
;; GIVEN       : Departs, Arrives and List Of Flights that can be taken.
;; RETURNS     : list of nodes with current flight arriving at the 'to' airport
;;               from possible source nodes
;; EXAMPLES    : (filter-nodes "LGA" "MSP" deltaFlights) =>
;;(list (make-node
;;  (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9))
;;  (list
;;   (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9)))
;;  189))
;; STRATEGY    : use HOF filter

(define (filter-nodes from to flights)
  (filter
   ;; Node -> Boolean
   ;; GIVEN      : A Node
   ;; RETURNS    : true iff the node contains current flight that arrives at the
   ;; 'to' airport
   (λ (node) (string=? to (arrives (node-flight node))))
   (flatten-list (shortest-path-trees from flights))))


;; flatten-list : ListOfListOfX -> ListOfX
;; RETURNS      : flattens the list of any type
;; EXAMPLES     : (flatten-list (list (list 1 2 3) (list 4 5 6))) =>
;;                 (list 1 2 3 4 5 6)
;; STRATEGY: use HOF foldr on the given list

(define (flatten-list lolox)
  (foldr
   ;; ListOfX ListOfX -> ListOfX
   ;; RETURNS: a combined list of X
   (λ (lox lyst) (append lox lyst))
   empty
   lolox))


;; fastest-node : String String ListOfFlight -> Node
;; GIVEN        : from is the airport name of the source airport
;;                to is the airport name of the destination airport
;;                flights is the list of flights
;; RETURNS      : the node with minmum cost to reach from source airport to
;;                destination airport.
;; EXAMPLES     : (fastest-node "LGA" "PDX" deltaFlights)    => 
;;(make-node
;; (make-flight "Delta 2163" "MSP" "PDX" (make-UTC 15 0) (make-UTC 19 2))
;; (list
;;  (make-flight "Delta 0121" "LGA" "MSP" (make-UTC 11 0) (make-UTC 14 9))
;;  (make-flight "Delta 2163" "MSP" "PDX" (make-UTC 15 0) (make-UTC 19 2)))
;; 482)
;; STRATEGY     : Use HOF foldr

(define (fastest-node from to flights)
  (let ((filtered (filter-nodes from to flights)))
    (if (empty? filtered)
        empty
        (foldr
         ;; Node Node -> Node
         ;; GIVEN     : current node and the minimum node
         ;; RETURNS   : the node with minimum cost amongst the given nodes
         (λ (node min)
           (cond 
             [(> (node-cost node) (node-cost min)) min]
             [else node]))
         (first filtered)
         filtered))))


;; flight-count-for-graph: String ListOfFlight -> NonNegInt
;; GIVEN                 : airport-name of the departing airport and
;;                         list of flights
;; RETURNS               : the count of the flight to be considered for the
;;                         creating shortest path
;; EXAMPLES              : (flight-count-for-graph "LGA" deltaFlights) => 5
;; STRATEGY              : combine simple functions

(define (flight-count-for-graph from flights)
  (- (length flights) (length (start-nodes from flights)) -1))


;; shortest-path-trees: String ListOfFlight -> ListOfListOfNode
;; GIVEN              : name of the departing airport and list of flight
;; RETURNS            : the shortest path trees of each of the possible
;;                      starting nodes, shortest path trees are list of nodes
;; EXAMPLES           :  (shortest-path-trees "LAX" deltaFlights)  =>
;;(list
;; (list
;;  (make-node
;;   (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9))
;;   (list
;;    (make-flight "Delta 2077" "LAX" "PDX" (make-UTC 17 35) (make-UTC 20 9)))
;;   154)))
;; STRATEGY           : use HOF map

(define (shortest-path-trees from flights)
  (let ((snodes (start-nodes from flights))
        (count (flight-count-for-graph from flights)))
    (map
     ;; Node -> ListOfNode
     ;; RETURNS  : the list of node with minimum cost from the given node
   (λ (n)
     (shortest-path-tree n count flights empty empty))
   snodes)))