;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns clj-growl.core
  "Implementation of GrowlTalk protocol in Clojure. This implementation
   currently does not support flags."
  (:import (java.net InetAddress
                     DatagramSocket
                     DatagramPacket)
           (java.nio ByteBuffer)
           (java.security MessageDigest)))

(def growl-udp-port 9887)
(def growl-protocol-version 1)
(def growl-type-registration 0)
(def growl-type-notification 1)

(defn utf8 [s]
  (.getBytes s "UTF8"))

(defn md5
  "Create a message digest including the message and the password if there is
   one."
  [bytes password]
  (let [md (MessageDigest/getInstance "MD5")]
    (do (.update md bytes)
        (if (and password
                 (not (empty? password)))
          (.update md (utf8 password))))
    (.digest md)))

(defn- defaults-seq [notifications]
  (map last
       (filter first
               (map vector
                    (map last notifications)
                    (iterate inc 0)))))

(defn- notifications-seq [notifications]
  (map #(let [n (utf8 %)]
          [(count n) n])
       (map first notifications)))

(defn- reg-buffer-size [application notifications defaults]
  (apply +
         6
         (count application)
         (count defaults)
         (map #(+ 2 (first %)) notifications)))

(defn- add-md5 [buff password]
  (let [packet (seq (.array buff))]
    (byte-array (concat packet
                        (md5 (byte-array packet) password)))))

(defn registration-packet
  "Create a Growl registration packet that can be sent with send-datagram.
   e.g. (registration-packet \"X\" [\"N1\" true \"N2\" true]) or
        (registration-packet \"password\" \"X\" [\"N1\" true \"N2\" true])
   where X is the name of the application and N1 and N2 are notification
   names. The boolean value indicates that the notification is enabled by
   default."
  ([application notifications]
     (registration-packet nil application notifications))
  ([password application notifications]
     (let [application (utf8 application)
           notifications (partition 2 notifications)
           defaults (defaults-seq notifications)
           notifications (notifications-seq notifications)]
       (let [buff (doto (ByteBuffer/allocate (reg-buffer-size application
                                                              notifications
                                                              defaults))
                    (.put (byte growl-protocol-version))
                    (.put (byte growl-type-registration))
                    (.putShort (count application))
                    (.put (byte (count notifications)))
                    (.put (byte (count defaults)))
                    (.put application))]
         (doseq [n notifications]
           (doto buff
             (.putShort (first n))
             (.put (last n))))
         (doseq [n defaults]
           (doto buff
             (.put (byte n))))
         (add-md5 buff password)))))

(defn- notif-buffer-size [& fields]
  (apply +
         12
         (map count fields)))

(defn notification-packet
  "Create a Growl notification packet that can be sent using send-datagram.
   Flags are not implemented."
  ([app notif title message]
     (notification-packet nil app notif title message))
  ([pwd app notif title message]
     (let [app (utf8 app)
           notif (utf8 notif)
           title (utf8 title)
           message (utf8 message)
           buff (doto (ByteBuffer/allocate (notif-buffer-size notif
                                                              title
                                                              message
                                                              app))
                  (.put (byte growl-protocol-version))
                  (.put (byte growl-type-notification))
                  (.putShort 0)
                  (.putShort (count notif))
                  (.putShort (count title))
                  (.putShort (count message))
                  (.putShort (count app))
                  (.put notif)
                  (.put title)
                  (.put message)
                  (.put app))]
       (add-md5 buff pwd))))

(defn send-datagram
  "Send a datagram to Growl."
  ([bytes]
     (send-datagram "localhost" bytes))
  ([host bytes]
     (let [socket (DatagramSocket.)
           address (InetAddress/getByName host)
           packet (DatagramPacket. bytes (count bytes) address growl-udp-port)]
       (.send socket packet)
       (.close socket))))

(defn make-growler
  "Register this application and return a function that will create and send
   growl notifications. Encapsulates the common use case."
  ([pwd app]
     (make-growler pwd app nil))
  ([pwd app notifications]
     (do (when notifications
           (send-datagram
            (registration-packet pwd app notifications)))
         (fn [notification title message]
           (send-datagram
            (notification-packet pwd
                                 app
                                 notification
                                 title
                                 message))))))

;;
;; Send Growl notifictions via the command growlnotify.
;; This is an alternative way of sending notifications. It is easier
;; to use but not as reliable.
;;

(defn- execute-command [command]
  (do (println command)
      (try
        (let [proc (.exec (Runtime/getRuntime) command)
              exit-val (.waitFor proc)])
        (catch Exception e (println e)))))

(defn- options-seq [opts]
  (mapcat #(cond (and (= (key %) :type)
                      (= (val %) :sticky))
                 ["-s"]
                 (= (key %) :image)
                 ["--image" (val %)]
                 :else [])
       opts))

(defn growlnotify
  ([app title message]
     (growlnotify app {:type :none} title message))
  ([app opts title message]
     (let [opts (options-seq opts)]
       (execute-command (into-array
                         (concat ["growlnotify"]
                                 ["-n" app
                                  "-t" title
                                  "-m" message]
                                 opts))))))
