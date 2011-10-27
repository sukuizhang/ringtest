(ns ringtest.core
  "用来测试ring的handler，对基于ring的web app，测试handler是比较简易和接近真实的。
   step1: 用client-factory方法创建一个或多个client，创建client时需要传入初始的ops。
   step2: 用client去执行请求
          (client handler uri query-string & other-ops)
        返回值是handler返回的:body中的值，
        对于:use-json=true的情况，已经把json parse成消息。
         一个client相当于一个浏览器，维护自己的cookie列表。
   step3:可以用多个client顺次执行多个请求，并且对请求的返回值进行检查。
   :todo 对于跨多个app的情形，我们实际上需要测试几个handler，但此时的一个问题是:如果app之间有相互调用，那么我们需要有办法方便的把她们之间的相互调用切换成handler之间的调用。"
  (:use [ring.middleware.cookies])
  (:require [ringtest.cookie :as cookie]
            [ring.util.codec :as codec]            
            [cheshire.core :as json]
            [clojure.string :as string])
  (:import java.io.File
           (java.net URLEncoder URLDecoder)))

(def default-ops
     "构造request的默认选项"
     {:scheme :http
      :remote-addr "127.0.0.1"
      :server-name "127.0.0.1"
      :server-port 8080      
      :request-method :get      
      :character-encoding "utf-8"
      :use-json true})

(defn with-default
  "把输入的选项和默认的选项合并"
  [ops]
  (merge default-ops ops))

(defn encode-nvpair
  [encoding pair]
  (let [pair (clojure.string/split pair #"=")
        pair (map #(codec/url-encode % encoding) pair)]
    (clojure.string/join "=" pair)))

(defn encode-query-string
  [encoding value]
  (let [value (clojure.string/split value #"&")
        value (map (partial encode-nvpair encoding) value)]
    (clojure.string/join "&" value)))

(defn with-query-string
  [req value]
  (if value 
    (assoc req :query-string (encode-query-string (req :character-encoding) value))
    req))

(defn build-req
  "构造一个请求
   ops:构造client时设置的ops，已经和default-ops合并过
   uri:请求的uri
   query-string: 请求的query string
   c-ops:当前请求的ops，可以覆盖创建client时设置的值"
  [ops uri query-string & [c-ops]]
  (-> (dissoc ops :use-json)      
      (merge c-ops)     
      (assoc :uri uri)        
      (with-query-string query-string)))

(defn read-body
  "从handler返回的response中读取测试关心的数据"
  [resp use-json]
  (let [body (:body resp)]
    (if use-json (json/parse-string body)
        body)))

(defn client-factory
  "根据指定的ops创建一个client"
  [ops]
  (let [ops (with-default ops)
        cookies (atom {})]
    (fn [handler uri query-string & [c-ops]]
      (let [req (build-req ops uri query-string c-ops)]
        (-> req 
            (cookie/with-cookies @cookies)        
            (handler)            
            (cookie/set-cookies (:server-name req) cookies)
            (read-body (:use-json ops)))))))

(defn client-data
  "包装了client，返回值是第一个消息的data"
  [client]
  (fn [handler uri query-string & [ops]]
    ((first (client handler uri query-string ops)) "data")))

(defn client-success
  "包装了client，返回值是第一个消息的isSuccess"
  [client]
  (fn [handler uri query-string & [ops]]
    ((first (client handler uri query-string ops)) "isSuccess")))

(defn print-req-and-resp
  "一个用来打印req和resp的middleware。"
  [h]
   (fn [req]
    (let [resp (h req)]
      (if (:query-string req)
        (println (str "req:" req "\n" "resp:" resp "\n")))
      resp)))
