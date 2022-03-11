(defsystem "docker-client"
  :version "0.1.0"
  :license "MIT"
  :depends-on ("arrow-macros"
               "anaphora"
               "metabang-bind"
               "yason"
               "flexi-streams"
               "local-time"
               "salmon"
               "trivia"
               "unix-opts"
               "cl-fad"
               "herodotus"
               "alexandria"
               "trivia.ppcre")
  :components ((:module "src"
                :components
                ((:file "docker-client"))))
  :description "A game called docker-client"
  :in-order-to ((test-op (test-op "docker-client/tests"))))

(defsystem "docker-client/tests"
  :license "MIT"
  :depends-on ("docker-client"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "docker-client"))))
  :description "Test system for docker-client"
  :perform (test-op (op c) (symbol-call :rove :run c)))
