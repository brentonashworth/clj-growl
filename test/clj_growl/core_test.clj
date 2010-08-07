;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns clj-growl.core-test
  (use (clojure test)
       (clj-growl core)))

(def reg
     [1 0 0 5 2 2 77 121 65 112 112 0 7 83 117 99 99 101 115 115 0 7 70 97
      105 108 117 114 101 0 1 48 27 -94 3 -42 85 -20 18 -73 -17 47 27 44
      -48 -95 112])

(def reg-pwd
     [1 0 0 5 2 2 77 121 65 112 112 0 7 83 117 99 99 101 115 115 0 7 70 97
      105 108 117 114 101 0 1 32 -6 102 -59 -21 -94 -13 34 109 39 124 -117
      -79 -110 -62 65])

(def notif
     [1 1 0 0 0 7 0 5 0 7 0 5 83 117 99 99 101 115 115 84 105 116 108 101 77
      101 115 115 97 103 101 77 121 65 112 112 -82 -42 -63 -24 -23 19 23 94
      26 55 -124 123 53 97 -122 26])

(def notif-pwd
     [1 1 0 0 0 7 0 5 0 7 0 5 83 117 99 99 101 115 115 84 105 116 108 101 77
      101 115 115 97 103 101 77 121 65 112 112 -57 -84 96 -35 -86 54 -60 77
      -69 21 -106 -103 97 -29 -127 -79])

(deftest test-registration-packet
  (is (= (seq (registration-packet "MyApp" ["Success" true "Failure" true]))
         reg))
  (is (= (seq (registration-packet "password"
                                   "MyApp"
                                   ["Success" true "Failure" true]))
         reg-pwd)))

(deftest test-notification-packet
  (is (= (seq (notification-packet "MyApp" "Success" "Title" "Message"))
         notif))
  (is (= (seq (notification-packet "password"
                                   "MyApp"
                                   "Success"
                                   "Title"
                                   "Message"))
         notif-pwd)))
