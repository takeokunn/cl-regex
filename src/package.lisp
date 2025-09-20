;;;; package.lisp - パッケージ定義

(defpackage :cl-regex
  (:use :cl :alexandria)
  (:nicknames :clr :regex)
  (:export
   ;; コア関数
   #:match
   #:match-all
   #:find-all
   #:replace
   #:replace-all
   #:compile-regex

   ;; マクロDSL
   #:regex
   #:defpattern
   #:when-match
   #:with-match

   ;; 文字クラス
   #:digit
   #:alpha
   #:alphanumeric
   #:word-char
   #:whitespace
   #:any
   #:newline
   #:tab

   ;; 量指定子
   #:optional
   #:exactly
   #:repeat

   ;; グループ
   #:group
   #:non-capturing
   #:atomic
   #:possessive

   ;; アサーション
   #:look-ahead
   #:not-ahead
   #:look-behind
   #:not-behind
   #:word-boundary
   #:not-word-boundary

   ;; Match オブジェクトAPI
   #:match-string
   #:match-start
   #:match-end
   #:group-ref
   #:group-count
   #:match-groups

   ;; Prolog API
   #:rule
   #:fact
   #:query
   #:dcg-rule

   ;; 設定
   #:*default-engine*
   #:*regex-cache-size*
   #:*parallel-threshold*
   #:with-regex-configuration

   ;; デバッグ
   #:trace-pattern
   #:explain-pattern
   #:analyze-pattern
   #:benchmark-pattern))

(in-package :cl-regex)