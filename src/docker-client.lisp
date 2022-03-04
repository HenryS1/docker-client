(defpackage :docker-client
  (:use :cl :usocket
        :flexi-streams
        :monad
        :either
        :trivia
        :trivia.ppcre)
  (:export :start-container :attach-container 
           :detach :attach-socket :close-connection
           :get-stdin :get-stdout :get-stderr))

(in-package :docker-client)

(defclass status ()
  ((version :reader version :initarg :version)
   (code :reader code :initarg :code)
   (reason-phrase :reader reason-phrase :initarg :reason-phrase)))

(defclass header ()
  ((header-name :reader header-name :initarg :header-name)
   (header-value :reader header-value :initarg :header-value)))

(defclass http-response ()
  ((http-status :reader http-status :initarg :http-status)
   (http-headers :reader http-headers :initarg :http-headers)))

(defun parse-status (status-line)
  (match status-line
    ((ppcre "([^\\s]+) (\\d+) (.+)" version (read code) reason-phrase)
     (right (make-instance 'status :version version :code code :reason-phrase reason-phrase)))
    (otherwise (left (format nil "Unable to parse http status line ~a~%" status-line)))))

(defun parse-header (header-line)
  (match header-line
    ((ppcre "([^\\s]+): ([^\\s]+)" header-name header-value)
     (right (make-instance 'header :header-name header-name :header-value header-value)))
    (otherwise (left (format nil "Unable to parse http header ~a~%" header-line)))))

(defun read-headers (socket-stream)
  (let ((line (read-line socket-stream nil nil)))
    (match line
      (nil (left "End of input"))
      ((ppcre "\\s+") (right nil))
      (otherwise (mdo (first (parse-header line))
                      (rest (read-headers socket-stream))
                      (yield (cons first rest)))))))

(defun read-http-response (socket-stream)
  (let ((status-line (read-line socket-stream nil nil)))
    (if (not status-line)
        (left "End of input")
        (mdo (status (parse-status status-line))
             (headers (read-headers socket-stream))
             (yield (make-instance 'http-response :http-status status :http-headers headers))))))

(defclass attached-container ()
  ((identitifer :reader identifier :initarg :identifier)
   (container-stdin :reader container-stdin :initarg :container-stdin)
   (container-stdout :reader container-stdout :initarg :container-stdout)
   (container-stderr :reader container-stderr :initarg :container-stderr)))

(defclass container-connection ()
  ((identifier :reader identifier :initarg :identifier)
   (connection :accessor connection :initarg :connection)))

(defmethod close-connection ((connection container-connection))
  (handler-case
      (socket-close (connection connection)) 
    (error (e) (format *error-output* "Error while closing socket ~a~%" e))))

(defmethod detach ((attachment attached-container))
  (close-connection (container-stdin attachment))
  (close-connection (container-stdout attachment))
  (close-connection (container-stderr attachment)))

(defmethod get-stdin ((attachment attached-container))
  (socket-stream (connection (container-stdin attachment))))

(defmethod get-stdout ((attachment attached-container))
  (socket-stream (connection (container-stdout attachment))))

(defmethod get-stderr ((attachment attached-container))
  (socket-stream (connection (container-stderr attachment))))

(defmethod start-container (identifier)
  (let ((sock (socket-connect "localhost" 2375 :element-type 'character)))
    (unwind-protect 
         (progn 
           (format (socket-stream sock)
                   "POST /containers/~a/start HTTP/1.0~a~a~a~a"
                   identifier #\return #\newline #\return #\newline)
           (force-output (socket-stream sock))
           (flatmap (lambda (response) 
                   (cond ((= (code (http-status response)) 500)
                          (left (format nil "Error from docker daemon: 500 ~a" 
                                        (reason-phrase response))))
                         ((= (code (http-status response)) 404)
                          (left (format nil "Container does not exist: 404 ~a"
                                        (reason-phrase response))))
                         ((= (code (http-status response)) 304)
                          (right "Container already started"))
                         ((= (code (http-status response)) 204)
                          (right "Started container"))
                         (t (left (format nil "Unexpected response from docker daemon: ~a ~a" 
                                          (code (http-status response)) (reason-phrase response))))))
                 (read-http-response (socket-stream sock))))
      (socket-close sock))))

(defun attach-socket (identifier attach-type)
  (let ((attach-query (case attach-type
                        (:stdin (right "stdin=1"))
                        (:stdout (right "stdout=1"))
                        (:stderr (right "stderr=1"))
                        (otherwise (left (format nil "unknown attach-type ~a" attach-type))))))
    (flatmap (lambda (q) 
            (let ((sock (socket-connect "localhost" 2375 :element-type 'character)))
              (handler-case 
                  (progn (setf (socket-option sock :tcp-keepalive) t)
                         (format (socket-stream sock)
                                 "POST /containers/~a/attach?stream=1&~a HTTP/1.0~a~aUpgrade: tcp~a~aConnection: Upgrade~a~a~a~a" 
                                 identifier
                                 q
                                 #\return #\newline #\return #\newline 
                                 #\return #\newline #\return #\newline)
                         (force-output (socket-stream sock))
                         (flatmap (lambda (response)
                                 (if (and (/= (code (http-status response)) 101)
                                          (/= (code (http-status response)) 200))
                                     (progn (socket-close sock)
                                            (left (format nil "Failed to attach to container ~a ~a~%" 
                                                          (code (http-status response))
                                                          (reason-phrase (http-status response)))))
                                     (right (make-instance 'container-connection
                                                           :identifier identifier 
                                                           :connection sock))))
                               (read-http-response (socket-stream sock))))
                (error (e) (format *error-output* "Error attaching socket to container ~a~%" e)
                       (socket-close sock)))))
          attach-query)))

(defun attach-container (identifier)
  (let ((stdin (attach-socket identifier :stdin))
        (stdout (attach-socket identifier :stdout))
        (stderr (attach-socket identifier :stderr)))
    (let ((result (mdo (in stdin)
                       (out stdout)
                       (err stderr)
                       (yield (make-instance 
                               'attached-container 
                               :identifier identifier
                               :container-stdin in
                               :container-stdout out
                               :container-stderr err)))))
      (match result
        ((type right) result)
        ((type left) 
         (fmap #'close-connection stdin)
         (fmap #'close-connection stdout)
         (fmap #'close-connection stderr)
         result)))))


;; (defun compute-payload-size (buffer)
;;   (loop for i from 4 to 7
;;      for size = (aref buffer i) then (+ (ash size 8) (aref buffer i))
;;      finally (return size)))

;; (defun determine-header-type (buffer)
;;   (case (aref buffer 0)
;;     (0 (right :stdin))
;;     (1 (right :stdout))
;;     (2 (right :stdout))
;;     (otherwise (left (format nil "unknown frame type ~a" (aref buffer 0))))))

;; (defun read-frame-header (socket-stream)
;;   (let ((buffer (make-array 8 :element-type '(unsigned-byte 8))))
;;     (loop for i from 0 to 7
;;        do (setf (aref buffer i) (read-byte socket-stream)))
;;     (fmap (lamdba (type) 
;;                   (make-frame-header :type type 
;;                                      :payload-size (compute-payload-size buffer)))
;;           (determine-header-type buffer))))

;; (defmethod read-frame ((connection attached-container))
;;   (match (read-frame-header)
;;     ((left-err e) (format *error-output* "Failed to read frame ~a" e))
;;     ((right-value h) ())))
