;;;; types.lisp - 型定義

(in-package :cl-regex)

;;; 基本型定義

(deftype pattern-string () 'string)
(deftype text-position () '(integer 0 *))
(deftype match-result () '(or null match-object))

;;; パターン表現

(defstruct pattern
  "正規表現パターンの内部表現"
  (source nil :type (or string list))
  (ast nil :type (or null ast-node))
  (compiled nil :type (or null compiled-pattern))
  (flags nil :type list)
  (metadata nil :type list))

(defstruct compiled-pattern
  "コンパイル済みパターン"
  (engine :hybrid :type (member :nfa :dfa :hybrid))
  (states nil :type (or null vector))
  (transitions nil :type (or null hash-table))
  (start-state 0 :type fixnum)
  (accept-states nil :type list)
  (groups nil :type list))

;;; マッチ結果

(defclass match-object ()
  ((text :initarg :text
         :reader match-text
         :type string
         :documentation "マッチ対象テキスト")
   (start :initarg :start
          :reader match-start
          :type fixnum
          :documentation "マッチ開始位置")
   (end :initarg :end
        :reader match-end
        :type fixnum
        :documentation "マッチ終了位置")
   (groups :initarg :groups
           :initform nil
           :reader match-groups
           :type list
           :documentation "キャプチャグループ"))
  (:documentation "マッチ結果を表現するオブジェクト"))

(defmethod print-object ((match match-object) stream)
  (print-unreadable-object (match stream :type t)
    (format stream "~S :start ~D :end ~D"
            (match-string match)
            (match-start match)
            (match-end match))))

(defmethod match-string ((match match-object))
  "マッチした文字列を返す"
  (subseq (match-text match)
          (match-start match)
          (match-end match)))

(defmethod group-ref ((match match-object) group)
  "グループ参照"
  (cond
    ((integerp group)
     (nth group (match-groups match)))
    ((symbolp group)
     (cdr (assoc group (match-groups match))))
    (t (error "Invalid group reference: ~S" group))))

;;; AST ノード

(defstruct ast-node
  "抽象構文木のノード"
  (type nil :type symbol)
  (value nil :type t)
  (children nil :type list)
  (properties nil :type list))

(defstruct (literal-node (:include ast-node))
  "リテラル文字列ノード")

(defstruct (char-class-node (:include ast-node))
  "文字クラスノード"
  (negated nil :type boolean))

(defstruct (quantifier-node (:include ast-node))
  "量指定子ノード"
  (min 0 :type fixnum)
  (max nil :type (or null fixnum))
  (greedy t :type boolean))

(defstruct (group-node (:include ast-node))
  "グループノード"
  (name nil :type (or null symbol))
  (capturing t :type boolean))

(defstruct (anchor-node (:include ast-node))
  "アンカーノード")

(defstruct (lookaround-node (:include ast-node))
  "先読み・後読みノード"
  (direction :ahead :type (member :ahead :behind))
  (positive t :type boolean))

;;; エンジン状態

(defstruct nfa-state
  "NFA状態"
  (id 0 :type fixnum)
  (transitions nil :type list)
  (epsilon-transitions nil :type list)
  (accepting nil :type boolean))

(defstruct dfa-state
  "DFA状態"
  (id 0 :type fixnum)
  (transitions nil :type hash-table)
  (accepting nil :type boolean)
  (nfa-states nil :type list))

;;; 最適化メタデータ

(defstruct pattern-analysis
  "パターン分析結果"
  (has-backreferences nil :type boolean)
  (has-lookaround nil :type boolean)
  (is-anchored nil :type boolean)
  (literal-prefix nil :type (or null string))
  (minimum-length 0 :type fixnum)
  (maximum-length nil :type (or null fixnum))
  (complexity-score 0 :type fixnum))

;;; キャッシュエントリ

(defstruct cache-entry
  "キャッシュエントリ"
  (key nil :type t)
  (value nil :type t)
  (hits 0 :type fixnum)
  (timestamp 0 :type fixnum))

;;; 並列処理用

(defstruct work-unit
  "並列処理の作業単位"
  (pattern nil :type compiled-pattern)
  (text nil :type string)
  (start 0 :type fixnum)
  (end nil :type (or null fixnum))
  (result nil :type (or null match-object)))