(defpackage :docker-client
  (:use :cl :usocket
        :flexi-streams
        :monad
        :either
        :trivia
        :trivia.ppcre)
  (:export :start-container :attach-container 
           :detach :attach-socket :close-connection
           :container-stdin :container-stdout :container-stderr 
           :stop-container :stdin-socket))

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

(defun read-http-response (socket-stream)
  (let ((status-line (read-line socket-stream nil nil)))
    (if (not status-line)
        (left "End of input")
        (mdo (status (parse-status status-line))
             (headers (read-headers socket-stream))
             (yield (make-instance 'http-response :http-status status :http-headers headers))))))

(defclass frame-reader (sb-gray:fundamental-character-input-stream)
  ((buffer :reader buffer :initform (make-array 2000000 :element-type 'character))
   (read-pointer :accessor read-pointer :initform 0)
   (write-pointer :accessor write-pointer :initform 0)
   (frame-stream :reader frame-stream :initarg :frame-stream)))

(defmethod stream-element-type ((stream frame-reader))
  (stream-element-type (socket-stream (frame-stream stream))))

(defmethod close ((stream frame-reader) &key abort)
  (declare (ignore abort))
  (socket-close (frame-stream stream)))

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
            (listen (socket-stream (frame-stream stream))))
    (read-char stream)))

(defclass attached-container ()
  ((identitifer :reader identifier :initarg :identifier)
   (container-stdin :reader container-stdin :initarg :container-stdin)
   (container-stdout :reader container-stdout :initarg :container-stdout)
   (container-stderr :reader container-stderr :initarg :container-stderr)))

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
  (socket-close (stdin-socket stream)))

(defmethod detach ((attachment attached-container))
  (close (container-stdin attachment))
  (close (container-stdout attachment))
  (close (container-stderr attachment)))

(defmethod start-container (identifier)
  (handler-case 
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
                                (t (left (format nil
                                                 "Unexpected response from docker daemon: ~a ~a" 
                                                 (code (http-status response))
                                                 (reason-phrase response))))))
                        (read-http-response (socket-stream sock))))
          (socket-close sock)))
    (error (e) (left (format nil "Failed to start docker container: ~a" e)))))

(defmethod stop-container (identifier)
  (handler-case 
      (let ((sock (socket-connect "localhost" 2375 :element-type 'character)))
        (unwind-protect 
             (progn 
               (format (socket-stream sock)
                       "POST /containers/~a/stop HTTP/1.0~a~a~a~a"
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
                                 (right "Container already stopped"))
                                ((= (code (http-status response)) 204)
                                 (right "Stopped container"))
                                (t (left (format nil
                                                 "Unexpected response from docker daemon: ~a ~a" 
                                                 (code (http-status response))
                                                 (reason-phrase response))))))
                        (read-http-response (socket-stream sock))))
          (socket-close sock)))
    (error (e) (left (format nil "Failed to stop docker container: ~a" e)))))

(defun attach-socket (identifier attach-type)
  (let ((attach-query (case attach-type
                        (:stdin (right "stdin=1"))
                        (:stdout (right "stdout=1"))
                        (:stderr (right "stderr=1"))
                        (otherwise (left (format nil "Unknown attach-type ~a" attach-type))))))
    (flatmap (lambda (q) 
            (handler-case 
                (let* ((sock (socket-connect "localhost" 2375 :element-type '(unsigned-byte 8)))
                       (char-stream (make-flexi-stream (socket-stream sock) :element-type 'character)))
                  (handler-case 
                      (progn (setf (socket-option sock :tcp-keepalive) t)
                             (format char-stream
                                     "POST /containers/~a/attach?stream=1&~a HTTP/1.0~a~aUpgrade: tcp~a~aConnection: Upgrade~a~a~a~a" 
                                     identifier
                                     q
                                     #\return #\newline #\return #\newline 
                                     #\return #\newline #\return #\newline)
                             (force-output char-stream)
                             (flatmap (lambda (response)
                                        (if (and (/= (code (http-status response)) 101)
                                                 (/= (code (http-status response)) 200))
                                            (progn (socket-close sock)
                                                   (left (format nil 
                                                                 "Failed to attach to container ~a ~a" 
                                                                 (code (http-status response))
                                                                 (reason-phrase (http-status response)))))
                                            (right sock)))
                                      (read-http-response char-stream)))
                    (error (e) (format *error-output* "Error attaching socket to container ~a" e)
                           (socket-close sock))))
              (error (e) (left (format nil "Error connecting socket to docker container ~a" e)))))
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
                               :container-stdin (make-instance 
                                                 'stdin-writer :stdin-socket in
                                                 :stdin-stream (make-flexi-stream 
                                                                (socket-stream in)
                                                                :element-type 'character))
                               :container-stdout (make-instance 'frame-reader :frame-stream out)
                               :container-stderr (make-instance 'frame-reader :frame-stream err))))))
      (match result
        ((type right) result)
        ((type left) 
         (fmap #'socket-close stdin)
         (fmap #'socket-close stdout)
         (fmap #'socket-close stderr)
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
                                         (socket-stream (frame-stream frame-reader)))))
                       (loop for i from 0 to (- size 1)
                          do (setf (aref buffer (write-pointer frame-reader))
                                   (read-char char-stream))
                            (setf (write-pointer frame-reader) 
                                  (mod (+ (write-pointer frame-reader) 1) (length buffer))))))
                   (read-frame-header (socket-stream (frame-stream frame-reader))))
      (error (e) (left (format nil "Error while reading frame ~a" e))))))
