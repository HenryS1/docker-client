(defpackage :docker-client
  (:use :cl 
        :flexi-streams
        :monad
        :either
        :alexandria
        :herodotus
        :trivia
        :trivia.ppcre)
  (:export :start-container :attach-container 
           :detach :attach-socket :close-connection
           :container-stdin :container-stdout :container-stderr 
           :stop-container :stdin-socket
           :host-config
           :docker-config
           :*default-docker-config*
           :pause-container :unpause-container
           :remove-container
           :create-container
           :connect-docker-socket
           :docker-stream
           :close-docker-socket
           :paused 
           :running
           :state
           :container-input-stream
           :inspect-container))

(in-package :docker-client)

(define-json-model container-state ((paused () "Paused") (running () "Running")))
(define-json-model inspect-result ((state container-state "State")))

(defclass status ()
  ((version :reader version :initarg :version)
   (code :reader code :initarg :code)
   (reason-phrase :reader reason-phrase :initarg :reason-phrase)))

(defclass header ()
  ((header-name :reader header-name :initarg :header-name)
   (header-value :reader header-value :initarg :header-value)))

(defclass http-response ()
  ((http-status :reader http-status :initarg :http-status)
   (http-headers :reader http-headers :initarg :http-headers)
   (http-body :reader http-body :initarg :http-body :initform nil)))

(defclass docker-socket ()
  ((docker-connection :reader socket-connection :initarg :docker-connection)
   (docker-stream :reader docker-stream :initarg :docker-stream)))

(defun connect-docker-socket (&key (element-type 'character))
  (handler-case (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
                  (handler-case (progn 
                                  (sb-bsd-sockets:socket-connect socket "/var/run/docker.sock")
                                  (let ((stream (sb-bsd-sockets:socket-make-stream 
                                                 socket :input t :output t 
                                                 :element-type element-type)))
                                    (right (make-instance 'docker-socket
                                                          :docker-connection socket
                                                          :docker-stream stream))))
                    (error (e) (left (format nil "Error while connecting docker socket ~a" e)))))
    (error (e) (left (format nil "Error while constructing docker socket ~a" e)))))

(defmethod close-docker-socket ((socket docker-socket))
  (close (docker-stream socket)))

(defun parse-status (status-line)
  (match status-line
    ((ppcre "([^\\s]+) (\\d+) (.+)" version (read code) reason-phrase)
     (right (make-instance 'status :version version :code code :reason-phrase reason-phrase)))
    (otherwise (left (format nil "Unable to parse http status line ~a" status-line)))))

(defun parse-header (header-line)
  (match header-line
    ((ppcre "([^\\s]+): ([^\\s]+)" header-name header-value)
     (right (make-instance 'header :header-name header-name :header-value header-value)))
    (otherwise (left (format nil "Unable to parse http header ~a" header-line)))))

(defun read-headers (socket-stream)
  (let ((line (read-line socket-stream nil nil)))
    (match line
      (nil (left "End of input"))
      ((ppcre "^\\s+$") (right nil))
      (otherwise (mdo (first (parse-header line))
                      (rest (read-headers socket-stream))
                      (yield (cons first rest)))))))

(defun read-http-response (socket-stream &key (body-parser nil))
  (let ((status-line (read-line socket-stream nil nil)))
    (if (not status-line)
        (left "End of input")
        (mdo (status (parse-status status-line))
             (headers (read-headers socket-stream))
             (body (if body-parser (funcall body-parser socket-stream status headers) (right nil)))
             (yield (make-instance 
                     'http-response
                     :http-status status
                     :http-headers headers
                     :http-body body))))))

(defclass frame-reader (sb-gray:fundamental-character-input-stream)
  ((buffer :reader buffer :initform (make-array 2000000 :element-type 'character))
   (read-pointer :accessor read-pointer :initform 0)
   (write-pointer :accessor write-pointer :initform 0)
   (frame-stream :reader frame-stream :initarg :frame-stream)))

(defmethod stream-element-type ((stream frame-reader))
  (stream-element-type (docker-stream (frame-stream stream))))

(defmethod close ((stream frame-reader) &key abort)
  (declare (ignore abort))
  (close-docker-socket (frame-stream stream)))

(defmethod ensure-available ((stream frame-reader))
  (when (= (read-pointer stream) (write-pointer stream))
    (read-frame stream)))

(defmethod sb-gray:stream-read-char ((stream frame-reader))
  (ensure-available stream)
  (let ((c (aref (buffer stream) (read-pointer stream))))
    (setf (read-pointer stream) (mod (+ (read-pointer stream) 1)
                                     (length (buffer stream))))
    c))

(defmethod sb-gray:stream-unread-char ((stream frame-reader) c)
  (declare (ignore c))
  (setf (read-pointer stream) 
        (mod (+ (read-pointer stream) (length (buffer stream)) (- 1)) 
             (length (buffer stream)))))

(defmethod sb-gray:stream-read-char-no-hang ((stream frame-reader))
  (when (or (/= (read-pointer stream) (write-pointer stream)) 
            (listen (docker-stream (frame-stream stream))))
    (read-char stream)))

(defclass attached-container ()
  ((identitifer :reader identifier :initarg :identifier)
   (container-stdin :reader container-stdin :initarg :container-stdin)
   (container-stdout :reader container-stdout :initarg :container-stdout)
   (container-stderr :reader container-stderr :initarg :container-stderr)))

(defmethod container-input-stream ((attached-container attached-container))
  (stdin-stream (container-stdin attached-container)))

(defclass stdin-writer (sb-gray:fundamental-character-output-stream)
  ((stdin-socket :reader stdin-socket :initarg :stdin-socket)
   (stdin-stream :reader stdin-stream :initarg :stdin-stream)))

(defmethod sb-gray:stream-write-char ((stream stdin-writer) c)
  (write-char c (stdin-stream stream)))

(defmethod sb-gray:stream-finish-output ((stream stdin-writer))
  (finish-output (stdin-stream stream)))

(defmethod sb-gray:stream-force-output ((stream stdin-writer))
  (force-output (stdin-stream stream)))

(defmethod close ((stream stdin-writer) &key abort) 
  (declare (ignore abort))
  (close-docker-socket (stdin-socket stream)))

(defmethod detach ((attachment attached-container))
  (close (container-stdin attachment))
  (close (container-stdout attachment))
  (close (container-stderr attachment)))

(defclass http-header ()
  ((header-name :reader header-name :initarg :header-name)
   (header-value :reader header-value :initarg :header-value)))

(defclass http-request ()
  ((request-method :reader request-method :initarg :request-method)
   (request-uri :reader request-uri :initarg :request-uri)
   (request-headers :reader request-headers :initarg :request-headers :initform nil)
   (request-body :reader request-body :initarg :request-body :initform nil)))

(defmethod send-http-request-over-socket ((http-request http-request) socket-stream 
                                          &key (response-body-parser nil))
  (handler-case (progn (format socket-stream "~a ~a HTTP/1.0~a~a"
                               (request-method http-request) 
                               (request-uri http-request)
                               #\return #\newline)
                       (loop for header in (request-headers http-request)
                          do (format socket-stream "~a: ~a~a~a" 
                                     (header-name header) (header-value header) 
                                     #\return #\newline))
                       (when (request-body http-request)
                         (format socket-stream "Content-Length: ~a~a~a"
                                 (length (request-body http-request)) #\return #\newline))
                       (format socket-stream "~a~a" #\return #\newline)
                       (when (request-body http-request)
                         (format socket-stream (request-body http-request)))
                       (finish-output socket-stream)
                       (read-http-response socket-stream :body-parser response-body-parser))
    (error (e) (left (format nil "Error sending http-request ~a" e)))))

(defmethod send-http-request ((http-request http-request) response-handler 
                              &key (response-body-parser nil))
  (handler-case 
      (let ((socket-connection (connect-docker-socket)))
        (unwind-protect 
             (mdo (sock socket-connection)
                  (response (send-http-request-over-socket 
                             http-request
                             (docker-stream sock)
                             :response-body-parser response-body-parser))
                  (result (funcall response-handler response))
                  (yield result))
          (fmap #'close-docker-socket socket-connection)))
    (error (e) (left (format nil "Failed to send http request: ~a" e)))))

(defmethod start-container (identifier)
  (send-http-request (make-instance 
                      'http-request
                      :request-method :post
                      :request-uri (format nil "/v1.41/containers/~a/start" identifier))
                     (lambda (response)
                        (cond ((= (code (http-status response)) 500)
                               (left (format nil "Error from docker daemon: 500 ~a" 
                                             (reason-phrase (http-status response)))))
                              ((= (code (http-status response)) 404)
                               (left (format nil "Container does not exist: 404 ~a"
                                             (reason-phrase (http-status response)))))
                              ((= (code (http-status response)) 304)
                               (right "Container already started"))
                              ((= (code (http-status response)) 204)
                               (right "Started container"))
                              (t (left (format nil
                                               "Unexpected response from docker daemon: ~a ~a" 
                                               (code (http-status response))
                                               (reason-phrase 
                                                (http-status response)))))))))

(defmethod inspect-container (identifier)
  (send-http-request (make-instance
                      'http-request
                      :request-method :get
                      :request-uri (format nil "/v1.41/containers/~a/json" identifier))
                     (lambda (response)
                       (cond ((= (code (http-status response)) 200)
                              (right (http-body response)))
                             ((= (code (http-status response)) 404)
                              (left (format nil "Container not found: ~a ~a~%"
                                            (code (http-status response))
                                            (reason-phrase (http-status response)))))
                             ((= (code (http-status response)) 500)
                              (left (format nil "Docker daemon error: ~a ~a~%"
                                            (code (http-status response))
                                            (reason-phrase (http-status response)))))))
                     :response-body-parser (lambda (stream status headers)
                                             (declare (ignore status headers))
                                             (handler-case
                                                 (right (inspect-result-json:from-json stream))
                                               (error (e) 
                                                 (format 
                                                  nil
                                                  "Error parsing respones json ~a" e))))))

(defmethod pause-container (identifier)
  (send-http-request (make-instance 
                      'http-request
                      :request-method :post
                      :request-uri (format nil "/v1.41/containers/~a/pause" identifier))
                     (lambda (response)
                       (cond ((= (code (http-status response)) 500)
                              (left (format nil "Error from docker daemon: 500 ~a" 
                                            (reason-phrase (http-status response)))))
                             ((= (code (http-status response)) 404)
                              (left (format nil "Container does not exist: 404 ~a"
                                            (reason-phrase (http-status response)))))
                             ((= (code (http-status response)) 204)
                              (right "Paused container"))
                             (t (left (format nil
                                              "Unexpected response from docker daemon: ~a ~a" 
                                              (code (http-status response))
                                              (reason-phrase (http-status response)))))))))

(defmethod unpause-container (identifier)
  (send-http-request (make-instance
                      'http-request
                      :request-method :post
                      :request-uri (format nil "/v1.41/containers/~a/unpause" identifier))
                     (lambda (response)
                       (cond ((= (code (http-status response)) 500)
                                 (left (format nil "Error from docker daemon: 500 ~a" 
                                               (reason-phrase (http-status response)))))
                                ((= (code (http-status response)) 404)
                                 (left (format nil "Container does not exist: 404 ~a"
                                               (reason-phrase (http-status response)))))
                                ((= (code (http-status response)) 204)
                                 (right "Unpaused container"))
                                (t (left (format nil
                                                 "Unexpected response from docker daemon: ~a ~a" 
                                                 (code (http-status response))
                                                 (reason-phrase (http-status response)))))))))

(defmethod stop-container (identifier &key (kill-wait 10))
  (send-http-request (make-instance
                      'http-request
                      :request-method :post
                      :request-uri (format nil "/v1.41/containers/~a/stop?t=~a" identifier kill-wait))
                     (lambda (response)
                       (cond ((= (code (http-status response)) 500)
                              (left (format nil "Error from docker daemon: 500 ~a" 
                                            (reason-phrase (http-status response)))))
                             ((= (code (http-status response)) 404)
                              (left (format nil "Container does not exist: 404 ~a"
                                            (reason-phrase (http-status response)))))
                             ((= (code (http-status response)) 304)
                              (right "Container already stopped"))
                             ((= (code (http-status response)) 204)
                              (right "Stopped container"))
                             (t (left (format nil
                                              "Unexpected response from docker daemon: ~a ~a" 
                                              (code (http-status response))
                                              (reason-phrase (http-status response)))))))))

(defmethod remove-container (identifier)
  (send-http-request (make-instance
                      'http-request
                      :request-method :delete
                      :request-uri (format nil "/v1.41/containers/~a" identifier))
                     (lambda (response)
                       (cond ((= (floor (code (http-status response)) 100) 5)
                              (left (format nil "Error from docker daemon: ~a ~a" 
                                            (code (http-status response))
                                            (reason-phrase (http-status response)))))
                             ((= (floor (code (http-status response)) 100) 4)
                              (left (format nil "Client error: ~a ~a"
                                            (code (http-status response))
                                            (reason-phrase (http-status response)))))
                             ((= (code (http-status response)) 204)
                              (right "Removed container"))
                             (t (left (format nil
                                              "Unexpected response from docker daemon: ~a ~a" 
                                              (code (http-status response))
                                              (reason-phrase (http-status response)))))))))

(define-json-model host-config ((binds () "Binds")))

(define-json-model docker-config ((image) 
                                  (command)
                                  (entrypoint)
                                  (open-stdin)
                                  (volumes)
                                  (host-config host-config)) :pascal-case)

(defparameter *blah-config* (make-instance 'docker-config 
                                           :image "bot-match/lisp-base"
                                           :command (list "ros +Q -- /bots/input-bot.lisp")
                                           :entrypoint (list "")
                                           :open-stdin t
                                           :volumes (alist-hash-table (list (cons "/Users/henrysteere/wip/lisp/bot-match/runtime/tests/" (alist-hash-table (list) :test 'equal))) :test 'equal)
                                           :host-config (make-instance 'host-config
                                                                       :binds (list "/Users/henrysteere/wip/lisp/bot-match/runtime/tests/:/bots"))))

(defparameter *default-docker-config* (make-instance 'docker-config 
                                                     :image "ubuntu:21.04"
                                                     :command "date"
                                                     :entrypoint nil
                                                     :open-stdin t
                                                     :volumes nil
                                                     :host-config nil))

(define-json-model bad-request (message))

(defmethod create-container (identifier &key (docker-config *default-docker-config*))
  (send-http-request (make-instance 
                      'http-request
                      :request-method :post
                      :request-uri (format nil "/v1.41/containers/create?name=~a" identifier)
                      :request-headers (list (make-instance 'http-header 
                                                            :header-name "Content-Type"
                                                            :header-value "application/json"))
                      :request-body (to-json docker-config))
                     (lambda (response)
                       (cond ((= (floor (code (http-status response)) 100) 5)
                              (left (format nil "Error from docker daemon: ~a ~a" 
                                            (code (http-status response))
                                            (reason-phrase (http-status response)))))
                             ((= (floor (code (http-status response)) 100) 4)
                              (left (format nil "Client error: ~a ~a"
                                            (code (http-status response))
                                            (reason-phrase (http-status response)))))
                             ((= (code (http-status response)) 201)
                              (right "Created container"))
                             (t (left (format nil
                                              "Unexpected response from docker daemon: ~a ~a" 
                                              (code (http-status response))
                                              (reason-phrase (http-status response)))))))
                     ;; :response-body-parser (lambda (stream status headers)
                     ;;                         (declare (ignore status headers))
                     ;;                         (handler-case
                     ;;                             (right (inspect-result-json:from-json stream))
                     ;;                           (error (e) 
                     ;;                             (format 
                     ;;                              nil
                     ;;                              "Error parsing respones json ~a" e))))
                     ))

(defun attach-socket (identifier attach-type)
  (handler-case 
   (let ((socket-connection (connect-docker-socket :element-type '(unsigned-byte 8)))
         (attach-query (case attach-type
                         (:stdin (right "stdin=1"))
                         (:stdout (right "stdout=1"))
                         (:stderr (right "stderr=1"))
                         (otherwise (left (format nil "Unknown attach-type ~a" attach-type))))))
     (handler-case 
         (mdo (sock socket-connection)
              (query attach-query)
              (let (char-stream (make-flexi-stream (docker-stream sock))))
              (response
               (send-http-request-over-socket 
                (make-instance
                 'http-request 
                 :request-method :post
                 :request-uri (format nil "/v1.41/containers/~a/attach?stream=1&~a" identifier query)
                 :request-headers (list (make-instance 
                                      'http-header 
                                      :header-name "Upgrade"
                                      :header-value "tcp")
                                     (make-instance 
                                      'http-header
                                      :header-name "Connection"
                                      :header-value "Upgrade")))
                char-stream))
              (result (if (and (/= (code (http-status response)) 101)
                               (/= (code (http-status response)) 200))
                          (progn (close-docker-socket sock)
                                 (left (format nil 
                                               "Failed to attach to container ~a ~a" 
                                               (code (http-status response))
                                               (reason-phrase (http-status response)))))
                          (right sock)))
              (yield result))
       (error (e) (fmap #'close-docker-socket socket-connection)
              (left (format nil "Error attaching socket ~a" e)))))))

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
                               :container-stdin (make-instance 
                                                 'stdin-writer 
                                                 :stdin-socket in
                                                 :stdin-stream (make-flexi-stream 
                                                                (docker-stream in)
                                                                :element-type 'character))
                               :container-stdout (make-instance 'frame-reader :frame-stream out)
                               :container-stderr (make-instance 'frame-reader :frame-stream err))))))
      (match result
        ((type right) result)
        ((type left) 
         (fmap #'close-docker-socket stdin)
         (fmap #'close-docker-socket stdout)
         (fmap #'close-docker-socket stderr)
         result)))))

(defstruct frame-header type payload-size)

(defun compute-payload-size (buffer)
  (loop for i from 4 to 7
     for size = (aref buffer i) then (+ (ash size 8) (aref buffer i))
     finally (return size)))

(defun determine-header-type (buffer)
  (case (aref buffer 0)
    (0 (right :stdin))
    (1 (right :stdout))
    (2 (right :stdout))
    (otherwise (left (format nil "unknown frame type ~a" (aref buffer 0))))))

(defun read-frame-header (stream)
  (let ((buffer (make-array 8 :element-type '(unsigned-byte 8))))
    (loop for i from 0 to 7
       do (setf (aref buffer i) (read-byte stream)))
    (fmap (lambda (type) 
                  (make-frame-header :type type 
                                     :payload-size (compute-payload-size buffer)))
          (determine-header-type buffer))))

(defmethod read-frame ((frame-reader frame-reader))
  (match 
    (handler-case (flatmap 
                   (lambda (header)
                     (let ((size (frame-header-payload-size header))
                           (buffer (buffer frame-reader))
                           (char-stream (make-flexi-stream 
                                         (docker-stream (frame-stream frame-reader)))))
                       (loop for i from 0 to (- size 1)
                          do (setf (aref buffer (write-pointer frame-reader))
                                   (read-char char-stream))
                            (setf (write-pointer frame-reader) 
                                  (mod (+ (write-pointer frame-reader) 1) (length buffer))))))

                   (read-frame-header (docker-stream (frame-stream frame-reader))))
      (error (e) (left (format nil "Error while reading frame ~a" e))))))
