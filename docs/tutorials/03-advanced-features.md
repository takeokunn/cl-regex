# 高度な機能の活用

CL-Regexの強力な機能を活用して、複雑なパターンマッチングを実現する方法を学びます。

## 目次

1. [名前付きキャプチャグループ](#名前付きキャプチャグループ)
2. [前方・後方先読み](#前方後方先読み)
3. [条件付きマッチング](#条件付きマッチング)
4. [再帰パターン](#再帰パターン)
5. [マクロベースDSL](#マクロベースdsl)
6. [CLOS統合](#clos統合)
7. [並列処理](#並列処理)

## 名前付きキャプチャグループ

CL-Regexでは、キャプチャグループに名前を付けて、より分かりやすいコードを書くことができます。

```lisp
;; 基本的な名前付きキャプチャ
(defparameter *email-regex*
  (regex "(?P<user>[a-zA-Z0-9._%+-]+)@(?P<domain>[a-zA-Z0-9.-]+\\.(?P<tld>[a-zA-Z]{2,}))"))

(let ((match (match *email-regex* "user@example.com")))
  (when match
    (format t "User: ~A~%" (capture-named match "user"))
    (format t "Domain: ~A~%" (capture-named match "domain"))
    (format t "TLD: ~A~%" (capture-named match "tld"))))
```

## 前方・後方先読み

複雑な条件を満たすパターンを効率的にマッチングできます。

```lisp
;; パスワード強度チェック（前方先読み）
(defparameter *strong-password*
  (regex "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[@$!%*?&])[A-Za-z\\d@$!%*?&]{8,}$"))

;; HTMLタグの除外（後方先読み）
(defparameter *non-html-word*
  (regex "\\b\\w+(?!</?[a-zA-Z])"))

(defun validate-password (password)
  "パスワードの強度を検証"
  (if (match *strong-password* password)
      :strong
      :weak))
```

## 条件付きマッチング

CL-Regexの条件付きマッチング機能を使用して、動的なパターンを作成できます。

```lisp
;; 条件付きマッチングのマクロ定義
(defmacro conditional-regex (condition true-pattern false-pattern)
  `(if ,condition
       (regex ,true-pattern)
       (regex ,false-pattern)))

;; 使用例：日付形式の動的切り替え
(defun date-regex (use-iso-format)
  (conditional-regex use-iso-format
    "\\d{4}-\\d{2}-\\d{2}"         ; ISO形式
    "\\d{2}/\\d{2}/\\d{4}"))       ; US形式

;; 実際の使用
(let ((iso-matcher (date-regex t))
      (us-matcher (date-regex nil)))
  (match iso-matcher "2024-03-15")    ; マッチ
  (match us-matcher "03/15/2024"))    ; マッチ
```

## 再帰パターン

S式やネストした構造をマッチングするための再帰パターンです。

```lisp
;; 括弧の対応をチェックする再帰パターン
(defclass recursive-matcher ()
  ((pattern :initarg :pattern :accessor pattern)
   (depth :initform 0 :accessor depth)))

(defmethod match-recursive ((matcher recursive-matcher) text &optional (start 0))
  "再帰的にネストした括弧をマッチング"
  (let ((current-depth 0)
        (pos start))
    (loop for char across (subseq text start) do
      (case char
        (#\\( (incf current-depth))
        (#\\) (decf current-depth)
              (when (< current-depth 0)
                (return-from match-recursive nil))))
      (incf pos))
    (zerop current-depth)))

;; S式パターンマッチング
(defparameter *sexp-pattern*
  (make-instance 'recursive-matcher
                 :pattern "\\([^()]*(?:[^()]*\\([^()]*\\)[^()]*)*\\)"))

(defun validate-sexp (text)
  "S式の構文検証"
  (match-recursive *sexp-pattern* text))
```

## マクロベースDSL

CL-Regexの真の力は、マクロシステムを活用した独自DSLの構築にあります。

```lisp
;; 正規表現構築のためのマクロDSL
(defmacro regex-dsl (&body forms)
  "直感的な正規表現構築DSL"
  `(concatenate 'string
     ,@(mapcar #'compile-regex-form forms)))

(defun compile-regex-form (form)
  "DSL形式を正規表現文字列に変換"
  (case (first form)
    (:literal (second form))
    (:char-class (format nil "[~A]" (second form)))
    (:optional (format nil "(?:~A)?" (compile-regex-form (second form))))
    (:one-or-more (format nil "(?:~A)+" (compile-regex-form (second form))))
    (:zero-or-more (format nil "(?:~A)*" (compile-regex-form (second form))))
    (:group (format nil "(~A)" (compile-regex-form (second form))))
    (:or (format nil "(?:~{~A~^|~})"
                 (mapcar #'compile-regex-form (rest form))))))

;; DSL使用例
(defparameter *email-dsl*
  (regex-dsl
    (:one-or-more (:char-class "a-zA-Z0-9._%+-"))
    (:literal "@")
    (:one-or-more (:char-class "a-zA-Z0-9.-"))
    (:literal "\\.")
    (:group (:one-or-more (:char-class "a-zA-Z")))))
```

## CLOS統合

CLOSの力を活用して、拡張可能な正規表現システムを構築します。

```lisp
;; 基底マッチャクラス
(defclass base-matcher ()
  ((pattern :initarg :pattern :accessor pattern)
   (flags :initarg :flags :initform nil :accessor flags)
   (compiled :initform nil :accessor compiled)))

;; 特化マッチャ
(defclass unicode-matcher (base-matcher)
  ((encoding :initarg :encoding :initform :utf-8 :accessor encoding)))

(defclass performance-matcher (base-matcher)
  ((cache :initform (make-hash-table :test 'equal) :accessor cache)
   (jit-compiled :initform nil :accessor jit-compiled)))

;; ジェネリック関数
(defgeneric compile-pattern (matcher)
  (:documentation "パターンをコンパイル"))

(defgeneric match-text (matcher text)
  (:documentation "テキストをマッチング"))

;; 実装
(defmethod compile-pattern ((matcher base-matcher))
  "基本的なパターンコンパイル"
  (setf (compiled matcher)
        (cl-regex:compile-regex (pattern matcher) (flags matcher))))

(defmethod compile-pattern ((matcher unicode-matcher))
  "Unicode対応パターンコンパイル"
  (setf (compiled matcher)
        (cl-regex:compile-regex (pattern matcher)
                               (append (flags matcher) '(:unicode)))))

(defmethod match-text ((matcher performance-matcher) text)
  "キャッシュ機能付きマッチング"
  (let ((cached-result (gethash text (cache matcher))))
    (if cached-result
        cached-result
        (setf (gethash text (cache matcher))
              (cl-regex:match (compiled matcher) text)))))
```

## 並列処理

大量のテキスト処理を並列化して高速化します。

```lisp
(require 'lparallel)

;; 並列マッチング設定
(defparameter *kernel* (lparallel:make-kernel 4))
(setf lparallel:*kernel* *kernel*)

(defun parallel-match-all (patterns text-list)
  "複数パターンを複数テキストに並列適用"
  (lparallel:pmap 'list
    (lambda (text)
      (mapcar (lambda (pattern)
                (cons pattern (match pattern text)))
              patterns))
    text-list))

;; ファイル並列処理
(defun parallel-grep (pattern file-paths)
  "複数ファイルを並列でGrep処理"
  (lparallel:pmap 'list
    (lambda (file-path)
      (with-open-file (stream file-path :direction :input)
        (let ((matches '())
              (line-number 0))
          (loop for line = (read-line stream nil nil)
                while line do
                  (incf line-number)
                  (when (match pattern line)
                    (push (list line-number line) matches)))
          (cons file-path (nreverse matches)))))
    file-paths))

;; 使用例
(let ((patterns (list (regex "error") (regex "warning") (regex "info")))
      (log-files '("app.log" "system.log" "debug.log")))
  (parallel-grep (first patterns) log-files))
```

## パフォーマンス最適化

高度な最適化テクニックを適用します。

```lisp
;; JITコンパイル対応マッチャ
(defclass jit-matcher (base-matcher)
  ((optimization-level :initarg :optimization-level :initform 3 :accessor optimization-level)
   (compile-threshold :initarg :compile-threshold :initform 100 :accessor compile-threshold)
   (execution-count :initform 0 :accessor execution-count)))

(defmethod match-text :around ((matcher jit-matcher) text)
  "JITコンパイル付きマッチング"
  (incf (execution-count matcher))
  (when (and (not (jit-compiled matcher))
             (>= (execution-count matcher) (compile-threshold matcher)))
    (jit-compile-pattern matcher))
  (call-next-method))

(defun jit-compile-pattern (matcher)
  "パターンをJITコンパイル"
  (setf (jit-compiled matcher) t)
  (setf (compiled matcher)
        (cl-regex:jit-compile (pattern matcher)
                             :optimization (optimization-level matcher))))
```

## まとめ

CL-Regexの高度な機能を活用することで：

- **表現力の向上**: 複雑なパターンを直感的に記述
- **保守性の向上**: CLOSによる構造化された設計
- **性能の向上**: JITコンパイルと並列処理
- **拡張性の確保**: マクロベースDSLによる柔軟性

次のステップでは、[カスタム拡張の構築](./04-custom-extensions.md)について学習します。