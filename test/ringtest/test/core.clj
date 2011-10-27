(ns ringtest.test.core
  (:use [ringtest.core :reload true]
        [ringtest.cookie :reload true]
        [clojure.test])
  (:require [ring.util.codec :as codec]
            [cheshire.core :as json]))

(deftest test-with-default
  (let [ops (with-default {:server-port 8081 :remote-addr "localhost" :use-json false})]
    (is (= :http (:scheme ops)))
    (is (= "localhost" (:remote-addr ops)))
    (is (= 8081 (:server-port ops)))
    (is (= :get (:request-method ops)))
    (is (= "utf-8" (:character-encoding ops)))
    (is (not (:use-json :ops)))))

(deftest test-encode-nvpair
  (is (= "a=b" (encode-nvpair "utf-8" "a=b")))
  (is (= (str "name=" (codec/url-encode "苏奎彰" "utf-8"))
         (encode-nvpair "utf-8" "name=苏奎彰"))))
(deftest test-encode-query-string
  (is (= (str "a=b&c=d&name=" (codec/url-encode "苏奎彰" "utf-8"))
         (encode-query-string "utf-8" "a=b&c=d&name=苏奎彰"))))

(deftest test-with-query-string
  (let [req {:character-encoding "utf-8"}] nil))


(deftest test-build-req
  (let [req (build-req default-ops "security/login" "password=111aaa" {})]
    (is (= "security/login" (:uri req)))
    (is (= "password=111aaa" (:query-string req)))
    (is (= "127.0.0.1" (:remote-addr req)))
    (is (= "127.0.0.1" (:server-name req)))
    (is (= :get (:request-method req)))
    (is (nil? (:use-json req))))
  (let [req (build-req
             default-ops
             "security/loginStart" "username=苏奎彰"
             {:request-method :post :character-encoding "GBK"})
        _ (println req)]
    (is (= "security/loginStart" (:uri req)))
    (is (= (str "username=" (codec/url-encode "苏奎彰" "GBK")) (:query-string req)))
    (is (= "127.0.0.1" (:remote-addr req)))
    (is (= "127.0.0.1" (:server-name req)))
    (is (= :post (:request-method req)))
    (is (= "GBK" (:character-encoding req)))))

(deftest test-read-body
  (let [resp {:a "xyz" :body (json/generate-string {:data "123"})}
        value (read-body resp true)]
    (is (= {"data" "123"} value))))

(deftest test-cookies
  (let [h (fn [req] {:headers {"Set-Cookie" '("a=abc;Path=/")}
                    :body (:cookies req)})
        client (client-factory {:use-json false})
        resp1 (client h nil nil)
        resp2 (client h nil nil)]
    (is (empty? resp1))
    (is (= "abc" (get-in resp2 ["a" :value])))))



