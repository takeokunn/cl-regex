;;;; cl-regex.asd - ASDF システム定義ファイル

(defsystem "cl-regex"
  :description "世界最高峰の性能を目指す純Lisp正規表現エンジン"
  :version "0.1.0"
  :author "Your Name"
  :license "MIT"
  :depends-on ("bordeaux-threads"
               "alexandria"
               "trivial-utf-8"
               "cl-ppcre"  ; ベンチマーク比較用
               "lparallel") ; 並列処理
  :components ((:module "src"
                :components
                ((:file "package")
                 (:module "core"
                  :depends-on ("package")
                  :components
                  ((:file "types")
                   (:file "errors")
                   (:file "utils")))
                 (:module "compiler"
                  :depends-on ("core")
                  :components
                  ((:file "parser")
                   (:file "ast")
                   (:file "optimizer")
                   (:file "code-generator")))
                 (:module "matcher"
                  :depends-on ("compiler")
                  :components
                  ((:file "nfa")
                   (:file "dfa")
                   (:file "hybrid")
                   (:file "parallel")))
                 (:module "optimizer"
                  :depends-on ("matcher")
                  :components
                  ((:file "cache")
                   (:file "jit")
                   (:file "simd-emulation")))
                 (:module "prolog"
                  :depends-on ("core")
                  :components
                  ((:file "unification")
                   (:file "rules")
                   (:file "dcg")))
                 (:module "api"
                  :depends-on ("matcher" "prolog")
                  :components
                  ((:file "match")
                   (:file "replace")
                   (:file "macros"))))))
  :in-order-to ((test-op (test-op "cl-regex-test"))))

(defsystem "cl-regex-test"
  :description "CL-Regexのテストスイート"
  :depends-on ("cl-regex"
               "fiveam"
               "cl-quickcheck")
  :components ((:module "tests"
                :components
                ((:file "test-suite")
                 (:file "unit-tests")
                 (:file "property-tests")
                 (:file "performance-tests"))))
  :perform (test-op (op c) (symbol-call :fiveam :run! 'cl-regex-test:all-tests)))

(defsystem "cl-regex-benchmark"
  :description "CL-Regexのベンチマークスイート"
  :depends-on ("cl-regex"
               "cl-ppcre"
               "trivial-benchmark")
  :components ((:module "benchmarks"
                :components
                ((:file "benchmark-suite")
                 (:file "comparison")))))