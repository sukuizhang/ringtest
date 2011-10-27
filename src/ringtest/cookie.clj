(ns ringtest.cookie
  "维护cookie列表"
  (:use [ring.middleware.cookies])
  (:require [ring.util.codec :as codec]
            [cheshire.core :as json]
            [clojure.string :as string])
  (:import java.io.File
           (java.net URLEncoder URLDecoder)))

(defrecord Cookie [domain path name value])
(defn make-cookie [domain path name value] (Cookie. domain path name value))

(defn- translate-host [host] (if (= "127.0.0.1" host) "localhost" host))

(defn cookie-for
  ([cookie req]
     (cookie-for cookie (:server-name req) (:uri req)))
  ([cookie host uri]
     (and (.endsWith (translate-host host) (:domain cookie))       
          (.startsWith (or uri "/") (:path cookie)))))

(defn ^Cookie parse-set-cookie
  "从一个标准的set-cookie: name=value;Path=/xyz;Domain=abc 获得一个Cookie"
  [host set-cookie]
  (let [k-v (map #(string/split %1 #"=") (string/split set-cookie #";"))
        [name value] (nth k-v 0)
        k-v (into {} (rest k-v))
        path (or (k-v "Path") "/")
        domain (or (k-v "Domain") (translate-host host))]
    (make-cookie domain path name value)))

(defn with-cookies
  "把client端的cookies加入到请求中"
  [req cookies]
  (assoc req :cookies
         (into {} (->> cookies
                       (filter #(cookie-for (second %) req))
                       (map (fn [[n v]] [n {:value (:value v)}]))))))

(defn set-cookies
  "从handler返回的response中读取设置的cookies，把她们保存到client。"
  [resp host cookies]
  (if-let [set-cookies (get-in resp [:headers "Set-Cookie"])]
    (doseq [set-cookie set-cookies]
      (let [cookie (parse-set-cookie host set-cookie)]
        (swap! cookies assoc (:name cookie) cookie))))
  resp)
