#lang racket

(require rackunit)
(require rackunit/text-ui)
(require racket/sandbox)
(require "flight.rkt")
(require "q2.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Black-box tests.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tests based on flights used to test Problem Set 00.

;;; flight-to-list : Flight -> List
;;; RETURNS: a readable representation of the flight as a list

(define (flight-to-list flight)
  (list 'make-flight
        (flight-name flight)
        (departs flight)
        (arrives flight)
        (list 'make-UTC
              (UTC-hour (departs-at flight))
              (UTC-minute (departs-at flight)))
        (list 'make-UTC
              (UTC-hour (arrives-at flight))
              (UTC-minute (arrives-at flight)))))

(define (makeFlight name ap1 ap2 t1 t2)
  (make-flight name
               ap1
               ap2
               (make-UTC (quotient t1 100) (remainder t1 100))
               (make-UTC (quotient t2 100) (remainder t2 100))))

;;; Pan American is no longer flying.

(define panAmFlights '())

;;; From 1 December 2016 to 15 January 2017,
;;; Delta's Worldwide Timetable is 1562 pages long (53.5 megabytes).
;;;
;;; We'll use only a small number of those flights for testing.

(define deltaFlights
  (list (makeFlight "Delta 0121" "LGA" "MSP" 1100 1409)
        (makeFlight "Delta 2163" "MSP" "PDX" 1500 1902)
        (makeFlight "Delta 2079" "BOS" "DTW" 1035 1259)
        (makeFlight "Delta 1523" "BOS" "DTW" 2158 0020)
        (makeFlight "Delta 0058" "BOS" "LHR" 0044 0720)
        (makeFlight "Delta 2531" "BOS" "LAX" 1317 2020)
        (makeFlight "Delta 2532" "BOS" "LAX" 2250 0555)
        (makeFlight "Delta 1959" "BOS" "MSP" 1050 1417)
        (makeFlight "Delta 1894" "BOS" "MSP" 1355 1730)
        (makeFlight "Delta 2391" "BOS" "MSP" 2135 0105)
        (makeFlight "Delta 2734" "BOS" "LGA" 1100 1230)
        (makeFlight "Delta 3550" "BZN" "LAX" 2020 2302)
        (makeFlight "Delta 1601" "DEN" "DTW" 1305 1611)
        (makeFlight "Delta 0916" "DEN" "DTW" 2332 0219)
        (makeFlight "Delta 0010" "DEN" "LHR" 2030 0945)
        (makeFlight "Delta 5703" "DEN" "LAX" 1404 1715)
        (makeFlight "Delta 5743" "DEN" "LAX" 0034 0331)
        (makeFlight "Delta 2437" "DTW" "BOS" 1345 1546)
        (makeFlight "Delta 0158" "DTW" "BOS" 1700 1855)
        (makeFlight "Delta 1700" "DTW" "BOS" 2240 0042)
        (makeFlight "Delta 1511" "DTW" "DEN" 1330 1651)
        (makeFlight "Delta 1645" "DTW" "DEN" 1711 2038)
        (makeFlight "Delta 1706" "DTW" "LAX" 1320 1845)
        (makeFlight "Delta 0249" "DTW" "MSP" 1500 1707)
        (makeFlight "Delta 2359" "DTW" "MSP" 1715 1920)
        (makeFlight "Delta 2476" "DTW" "MSP" 0110 0318)
        (makeFlight "Delta 0059" "LHR" "BOS" 0920 1726)
        (makeFlight "Delta 4378" "LHR" "BOS" 1645 0020)
        (makeFlight "Delta 0011" "LHR" "DEN" 1255 0220)
        (makeFlight "Delta 0302" "LAX" "BOS" 1625 2214)
        (makeFlight "Delta 5732" "LAX" "BZN" 0030 0318)
        (makeFlight "Delta 4574" "LAX" "DEN" 1735 2007)
        (makeFlight "Delta 5700" "LAX" "DEN" 0010 0245)
        (makeFlight "Delta 2077" "LAX" "PDX" 1735 2009)
        (makeFlight "Delta 1728" "MSP" "BOS" 1600 1851)
        (makeFlight "Delta 2305" "MSP" "BZN" 0221 0513)
        (makeFlight "Delta 1609" "MSP" "DEN" 2035 2252)
        (makeFlight "Delta 1836" "MSP" "DTW" 1224 1415)
        (makeFlight "Delta 1734" "MSP" "DTW" 1755 1941)
        (makeFlight "Delta 0592" "MSP" "LGA" 1730 2017)
        (makeFlight "Delta 2734" "LGA" "BOS" 1100 1208)
        (makeFlight "Delta 1294" "LGA" "DEN" 1310 1754)
        (makeFlight "Delta 0879" "LGA" "DTW" 1410 1620)
        (makeFlight "Delta 1422" "LGA" "MSP" 1500 1822)
        (makeFlight "Delta 0950" "PDX" "LAX" 1418 1655)
        (makeFlight "Delta 2077" "PDX" "LAX" 2045 2314)
        (makeFlight "Delta 2831" "PDX" "LAX" 2346 0225)
        (makeFlight "Delta 2167" "PDX" "MSP" 2200 0120)))

;;; A long cycle of Delta flights that has only "BOS" in common
;;; with the airports served by the deltaFlights example above.

(define deltaCycle
  (list (makeFlight "Delta 0105" "BOS" "ATL" 1950 2259)
        (makeFlight "Delta 1895" "ATL" "PHL" 1505 1705)
        (makeFlight "Delta 0926" "PHL" "SLC" 1059 1615)
        (makeFlight "Delta 5828" "SLC" "DFW" 1813 2056)
        (makeFlight "Delta 8122" "DFW" "MEX" 0132 0435)
        (makeFlight "Delta 8028" "MEX" "LAS" 1800 2228)
        (makeFlight "Delta 2837" "LAS" "MKC" 0215 0505)
        (makeFlight "Delta 3337" "MKC" "ORL" 2000 2250)
        (makeFlight "Delta 3617" "ORL" "BNA" 1735 1936)
        (makeFlight "Delta 4811" "BNA" "CVG" 1215 1333)
        (makeFlight "Delta 6207" "CVG" "IAH" 1850 2131)
        (makeFlight "Delta 0108" "IAH" "MAD" 2006 0715)
        (makeFlight "Delta 6775" "MAD" "MIA" 1425 2350)
        (makeFlight "Delta 7199" "MIA" "YTO" 2055 0006)
        (makeFlight "Delta 7037" "YTO" "BOS" 2215 0005)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; All airports served by one of the flights in the given list.

(define (all-airports flights)
  (all-airports-loop (append (map departs flights)
                             (map arrives flights))
                     empty))

(define (all-airports-loop names airports)
  (if (empty? names)
      (list-sort string<? airports)
      (all-airports-loop (rest names)
                         (if (member (first names) airports)
                             airports
                             (cons (first names) airports)))))

(define (list-sort < xs)
  (sort xs <))

;;; Is it possible to get from every airport to every other airport?

(define (strongly-connected? flights)
  (let ((airports (all-airports flights)))
    (andmap (lambda (ap1)
              (andmap (lambda (ap2)
                        (can-get-there? ap1 ap2 flights))
                      airports))
            airports)))

;;; Fastest itinerary for all pairs of airports.

(define (visit-all-pairs-of-airports visitor flights)
  (let ((airports (all-airports flights)))
    (apply append
           (map (lambda (ap1)
                  (map (lambda (ap2)
                         (visitor ap1 ap2 flights))
                       airports))
                airports))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define time-limit-s 20)
(define long-time-limit-s 900)
(define memory-limit-mb 128)

(define tests
  (test-suite
   "q2"
   
   (test-case
    "Test #1"
    (with-limits
     time-limit-s memory-limit-mb
     (check-equal? (can-get-there? "ROT" "ROT" panAmFlights)
                   true
                   "but you're already there!")
     ))
   (test-case
    "Test #2"
    (with-limits
     time-limit-s memory-limit-mb
     (check-equal? (can-get-there? "ROT" "BOS" deltaFlights)
                   false
                   "but ROT isn't served by Delta")
     ))
   (test-case
    "Test #3"
    (with-limits
     time-limit-s memory-limit-mb
     
     (check-equal? (can-get-there? "BOS" "TEN" deltaFlights)
                   false
                   "but TEN isn't served by Delta")
     ))
   (test-case
    "Test #4"
    (with-limits
     time-limit-s memory-limit-mb
     (check-equal? (strongly-connected? panAmFlights)
                   true
                   "but the empty graph is strongly connected")
     ))
   (test-case
    "Test #5"
    (with-limits
     long-time-limit-s memory-limit-mb
     (check-equal? (strongly-connected? deltaFlights)
                   true
                   "but deltaFlights is strongly connected")
     ))
   (test-case
    "Test #6"
    (with-limits
     long-time-limit-s memory-limit-mb
     (check-equal? (strongly-connected? deltaCycle)
                   true
                   "but deltaCycle is strongly connected")
     ))
   (test-case
    "Test #7"
    (with-limits
     long-time-limit-s memory-limit-mb
     (check-equal? (strongly-connected? (rest deltaCycle))
                   false
                   "but deltaCycle is not strongly connected without BOS")
     ))
   (test-case
    "Test #8"
    (with-limits
     time-limit-s memory-limit-mb
     (check-equal? (fastest-itinerary "DEN" "DEN" panAmFlights)
                   empty
                   "wrong itinerary for DEN to DEN via Pan Am")
     ))
   (test-case
    "Test #9"
    (with-limits
     time-limit-s memory-limit-mb
     (check-equal? (fastest-itinerary "BAL" "BAL" deltaFlights)
                   empty
                   "wrong itinerary for BAL to BAL via Delta")
     ))
   (test-case
    "Test #10"
    (with-limits
     time-limit-s memory-limit-mb
     (check-equal? (map flight-to-list
                        (fastest-itinerary "DEN" "DTW" deltaFlights))
                   (list (list 'make-flight "Delta 0916" "DEN" "DTW"
                               (list 'make-UTC 23 32) (list 'make-UTC 2 19)))
                   "wrong itinerary for DEN to DTW via Delta")
     ))
   (test-case
    "Test #11"
    (with-limits
     time-limit-s memory-limit-mb
     (check-equal? (travel-time "BOS" "DTW" deltaFlights)
                   142
                   "wrong travel time for BOS to DTW")
     ))
   (test-case
    "Test #12"
    (with-limits
     time-limit-s memory-limit-mb
     (check-equal? (map flight-to-list
                        (fastest-itinerary "PDX" "LHR" deltaFlights))
                   (list
                    (list 'make-flight "Delta 0950" "PDX" "LAX"
                          (list 'make-UTC 14 18) (list 'make-UTC 16 55))
                    (list 'make-flight "Delta 4574" "LAX" "DEN"
                          (list 'make-UTC 17 35) (list 'make-UTC 20 7))
                    (list 'make-flight "Delta 0010" "DEN" "LHR"
                          (list 'make-UTC 20 30) (list 'make-UTC 9 45)))
                   "wrong itinerary for PDX to LHR via Delta")
     ))
   (test-case
    "Test #13"
    (with-limits
     time-limit-s memory-limit-mb
     (check-equal? (travel-time "BZN" "LGA" deltaFlights)
                   2410
                   "wrong travel time for BZN to LGA")
     ))
   (test-case
    "Test #14"
    (with-limits
     time-limit-s memory-limit-mb
     (check-equal? (travel-time "BZN" "LHR" deltaFlights)
                   2100
                   "wrong travel time for BZN to LHR")
     ))
   (test-case
    "Test #15"
    (with-limits
     long-time-limit-s memory-limit-mb
     (check-equal? (visit-all-pairs-of-airports
                    (lambda (ap1 ap2 flights)
                      (list ap1 ap2 (travel-time ap1 ap2 flights)))
                    deltaCycle)
                   (list
                    (list "ATL" "ATL" 0)
                    (list "ATL" "BNA" 6031)
                    (list "ATL" "BOS" 13500)
                    (list "ATL" "CVG" 7108)
                    (list "ATL" "DFW" 1791)
                    (list "ATL" "IAH" 7586)
                    (list "ATL" "LAS" 3323)
                    (list "ATL" "MAD" 9610)
                    (list "ATL" "MEX" 2250)
                    (list "ATL" "MIA" 10605)
                    (list "ATL" "MKC" 3720)
                    (list "ATL" "ORL" 4785)
                    (list "ATL" "PHL" 120)
                    (list "ATL" "SLC" 1510)
                    (list "ATL" "YTO" 12061)
                    (list "BNA" "ATL" 7844)
                    (list "BNA" "BNA" 0)
                    (list "BNA" "BOS" 6470)
                    (list "BNA" "CVG" 78)
                    (list "BNA" "DFW" 10601)
                    (list "BNA" "IAH" 556)
                    (list "BNA" "LAS" 12133)
                    (list "BNA" "MAD" 2580)
                    (list "BNA" "MEX" 11060)
                    (list "BNA" "MIA" 3575)
                    (list "BNA" "MKC" 12530)
                    (list "BNA" "ORL" 13595)
                    (list "BNA" "PHL" 8930)
                    (list "BNA" "SLC" 10320)
                    (list "BNA" "YTO" 5031)
                    (list "BOS" "ATL" 189)
                    (list "BOS" "BNA" 7186)
                    (list "BOS" "BOS" 0)
                    (list "BOS" "CVG" 8263)
                    (list "BOS" "DFW" 2946)
                    (list "BOS" "IAH" 8741)
                    (list "BOS" "LAS" 4478)
                    (list "BOS" "MAD" 10765)
                    (list "BOS" "MEX" 3405)
                    (list "BOS" "MIA" 11760)
                    (list "BOS" "MKC" 4875)
                    (list "BOS" "ORL" 5940)
                    (list "BOS" "PHL" 1275)
                    (list "BOS" "SLC" 2665)
                    (list "BOS" "YTO" 13216)
                    (list "CVG" "ATL" 7449)
                    (list "CVG" "BNA" 14446)
                    (list "CVG" "BOS" 6075)
                    (list "CVG" "CVG" 0)
                    (list "CVG" "DFW" 10206)
                    (list "CVG" "IAH" 161)
                    (list "CVG" "LAS" 11738)
                    (list "CVG" "MAD" 2185)
                    (list "CVG" "MEX" 10665)
                    (list "CVG" "MIA" 3180)
                    (list "CVG" "MKC" 12135)
                    (list "CVG" "ORL" 13200)
                    (list "CVG" "PHL" 8535)
                    (list "CVG" "SLC" 9925)
                    (list "CVG" "YTO" 4636)
                    (list "DFW" "ATL" 12807)
                    (list "DFW" "BNA" 3964)
                    (list "DFW" "BOS" 11433)
                    (list "DFW" "CVG" 5041)
                    (list "DFW" "DFW" 0)
                    (list "DFW" "IAH" 5519)
                    (list "DFW" "LAS" 1256)
                    (list "DFW" "MAD" 7543)
                    (list "DFW" "MEX" 183)
                    (list "DFW" "MIA" 8538)
                    (list "DFW" "MKC" 1653)
                    (list "DFW" "ORL" 2718)
                    (list "DFW" "PHL" 13893)
                    (list "DFW" "SLC" 15283)
                    (list "DFW" "YTO" 9994)
                    (list "IAH" "ATL" 5933)
                    (list "IAH" "BNA" 12930)
                    (list "IAH" "BOS" 4559)
                    (list "IAH" "CVG" 14007)
                    (list "IAH" "DFW" 8690)
                    (list "IAH" "IAH" 0)
                    (list "IAH" "LAS" 10222)
                    (list "IAH" "MAD" 669)
                    (list "IAH" "MEX" 9149)
                    (list "IAH" "MIA" 1664)
                    (list "IAH" "MKC" 10619)
                    (list "IAH" "ORL" 11684)
                    (list "IAH" "PHL" 7019)
                    (list "IAH" "SLC" 8409)
                    (list "IAH" "YTO" 3120)
                    (list "LAS" "ATL" 11324)
                    (list "LAS" "BNA" 2481)
                    (list "LAS" "BOS" 9950)
                    (list "LAS" "CVG" 3558)
                    (list "LAS" "DFW" 14081)
                    (list "LAS" "IAH" 4036)
                    (list "LAS" "LAS" 0)
                    (list "LAS" "MAD" 6060)
                    (list "LAS" "MEX" 14540)
                    (list "LAS" "MIA" 7055)
                    (list "LAS" "MKC" 170)
                    (list "LAS" "ORL" 1235)
                    (list "LAS" "PHL" 12410)
                    (list "LAS" "SLC" 13800)
                    (list "LAS" "YTO" 8511)
                    (list "MAD" "ATL" 4834)
                    (list "MAD" "BNA" 11831)
                    (list "MAD" "BOS" 3460)
                    (list "MAD" "CVG" 12908)
                    (list "MAD" "DFW" 7591)
                    (list "MAD" "IAH" 13386)
                    (list "MAD" "LAS" 9123)
                    (list "MAD" "MAD" 0)
                    (list "MAD" "MEX" 8050)
                    (list "MAD" "MIA" 565)
                    (list "MAD" "MKC" 9520)
                    (list "MAD" "ORL" 10585)
                    (list "MAD" "PHL" 5920)
                    (list "MAD" "SLC" 7310)
                    (list "MAD" "YTO" 2021)
                    (list "MEX" "ATL" 11819)
                    (list "MEX" "BNA" 2976)
                    (list "MEX" "BOS" 10445)
                    (list "MEX" "CVG" 4053)
                    (list "MEX" "DFW" 14576)
                    (list "MEX" "IAH" 4531)
                    (list "MEX" "LAS" 268)
                    (list "MEX" "MAD" 6555)
                    (list "MEX" "MEX" 0)
                    (list "MEX" "MIA" 7550)
                    (list "MEX" "MKC" 665)
                    (list "MEX" "ORL" 1730)
                    (list "MEX" "PHL" 12905)
                    (list "MEX" "SLC" 14295)
                    (list "MEX" "YTO" 9006)
                    (list "MIA" "ATL" 3004)
                    (list "MIA" "BNA" 10001)
                    (list "MIA" "BOS" 1630)
                    (list "MIA" "CVG" 11078)
                    (list "MIA" "DFW" 5761)
                    (list "MIA" "IAH" 11556)
                    (list "MIA" "LAS" 7293)
                    (list "MIA" "MAD" 13580)
                    (list "MIA" "MEX" 6220)
                    (list "MIA" "MIA" 0)
                    (list "MIA" "MKC" 7690)
                    (list "MIA" "ORL" 8755)
                    (list "MIA" "PHL" 4090)
                    (list "MIA" "SLC" 5480)
                    (list "MIA" "YTO" 191)
                    (list "MKC" "ATL" 10259)
                    (list "MKC" "BNA" 1416)
                    (list "MKC" "BOS" 8885)
                    (list "MKC" "CVG" 2493)
                    (list "MKC" "DFW" 13016)
                    (list "MKC" "IAH" 2971)
                    (list "MKC" "LAS" 14548)
                    (list "MKC" "MAD" 4995)
                    (list "MKC" "MEX" 13475)
                    (list "MKC" "MIA" 5990)
                    (list "MKC" "MKC" 0)
                    (list "MKC" "ORL" 170)
                    (list "MKC" "PHL" 11345)
                    (list "MKC" "SLC" 12735)
                    (list "MKC" "YTO" 7446)
                    (list "ORL" "ATL" 8964)
                    (list "ORL" "BNA" 121)
                    (list "ORL" "BOS" 7590)
                    (list "ORL" "CVG" 1198)
                    (list "ORL" "DFW" 11721)
                    (list "ORL" "IAH" 1676)
                    (list "ORL" "LAS" 13253)
                    (list "ORL" "MAD" 3700)
                    (list "ORL" "MEX" 12180)
                    (list "ORL" "MIA" 4695)
                    (list "ORL" "MKC" 13650)
                    (list "ORL" "ORL" 0)
                    (list "ORL" "PHL" 10050)
                    (list "ORL" "SLC" 11440)
                    (list "ORL" "YTO" 6151)
                    (list "PHL" "ATL" 13680)
                    (list "PHL" "BNA" 4837)
                    (list "PHL" "BOS" 12306)
                    (list "PHL" "CVG" 5914)
                    (list "PHL" "DFW" 597)
                    (list "PHL" "IAH" 6392)
                    (list "PHL" "LAS" 2129)
                    (list "PHL" "MAD" 8416)
                    (list "PHL" "MEX" 1056)
                    (list "PHL" "MIA" 9411)
                    (list "PHL" "MKC" 2526)
                    (list "PHL" "ORL" 3591)
                    (list "PHL" "PHL" 0)
                    (list "PHL" "SLC" 316)
                    (list "PHL" "YTO" 10867)
                    (list "SLC" "ATL" 13246)
                    (list "SLC" "BNA" 4403)
                    (list "SLC" "BOS" 11872)
                    (list "SLC" "CVG" 5480)
                    (list "SLC" "DFW" 163)
                    (list "SLC" "IAH" 5958)
                    (list "SLC" "LAS" 1695)
                    (list "SLC" "MAD" 7982)
                    (list "SLC" "MEX" 622)
                    (list "SLC" "MIA" 8977)
                    (list "SLC" "MKC" 2092)
                    (list "SLC" "ORL" 3157)
                    (list "SLC" "PHL" 14332)
                    (list "SLC" "SLC" 0)
                    (list "SLC" "YTO" 10433)
                    (list "YTO" "ATL" 1484)
                    (list "YTO" "BNA" 8481)
                    (list "YTO" "BOS" 110)
                    (list "YTO" "CVG" 9558)
                    (list "YTO" "DFW" 4241)
                    (list "YTO" "IAH" 10036)
                    (list "YTO" "LAS" 5773)
                    (list "YTO" "MAD" 12060)
                    (list "YTO" "MEX" 4700)
                    (list "YTO" "MIA" 13055)
                    (list "YTO" "MKC" 6170)
                    (list "YTO" "ORL" 7235)
                    (list "YTO" "PHL" 2570)
                    (list "YTO" "SLC" 3960)
                    (list "YTO" "YTO" 0))
                   "incorrect travel time using deltaCycle")
     ))
   ))

(run-tests tests 'verbose)
