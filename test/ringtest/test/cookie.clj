(ns ringtest.test.cookie
  (:use [ringtest.cookie :reload true]
        [ringtest.core :reload true]
        [clojure.test])
  (:require [ring.util.codec :as codec]
            [cheshire.core :as json]))

(deftest test-cookie-for
  (is (cookie-for (make-cookie "localhost" "/" "" "") "localhost" "/"))
  (is (cookie-for (make-cookie "localhost" "/" "" "") "127.0.0.1" "/"))
  (is (cookie-for (make-cookie "localhost" "/" "" "") "a.b.localhost" "/"))
  (is (cookie-for (make-cookie "localhost" "/" "" "") "a.b.localhost" nil))
  (is (not (cookie-for (make-cookie "localhost" "/a" "" "") "localhost" "/")))
  (is (not (cookie-for (make-cookie "localhost" "/" "" "") "localhost1" "/"))))

(deftest test-parse-set-cookies
  (is (= (make-cookie "abc" "/xyz" "name" "value")
         (parse-set-cookie "localhost""name=value;Path=/xyz;Domain=abc")))
  (is (= (make-cookie "localhost" "/a" "name" "value")
         (parse-set-cookie "localhost" "name=value;Path=/a")))
  (is (= (make-cookie "localhost" "/" "name" "value")
         (parse-set-cookie "localhost" "name=value")))
  (is (= (make-cookie "localhost" "/" "name" "value")
         (parse-set-cookie "127.0.0.1" "name=value"))))

(deftest test-set-cookies
  (let [cookies (atom {})]
    (set-cookies
     {:headers {"Set-Cookie" '("a=xxx;Path=/" "b=yyy;Path=/")}}
     "localhost"     
     cookies)
    (is (= (make-cookie "localhost" "/" "a" "xxx") (@cookies "a")))
    (is (= (make-cookie "localhost" "/" "b" "yyy") (@cookies "b")))
    (set-cookies
     {:headers {"Set-Cookie" '("a=xxxyyy;Path=/uv" "c=efg;Path=/uv;Domain=xyz")}}
     "localhost"     
     cookies)    
    (is (= (make-cookie "localhost" "/uv" "a" "xxxyyy") (@cookies "a")))
    (is (= (make-cookie "localhost" "/" "b" "yyy") (@cookies "b")))
    (is (= (make-cookie "xyz" "/uv" "c" "efg") (@cookies "c")))))

(deftest test-with-cookies
  (let [req (with-cookies {:server-name "abc.localhost" :uri "/abc"}
              {"a" (make-cookie "localhost" "/" "a" "xx")
               "b" (make-cookie "localhost" "/" "b" "yy")
               "c" (make-cookie "abc" "/" "c" "yy")
               "d" (make-cookie "localhost" "/def" "d" "uvw")})]
    (is (= "xx" (get-in req [:cookies "a" :value])))
    (is (= "yy" (get-in req [:cookies "b" :value])))
    (is (nil? (get-in req [:cookies "c"])))
    (is (nil? (get-in req [:cookies "d"])))))
