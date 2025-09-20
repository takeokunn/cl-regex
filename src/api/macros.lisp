;;;; macros.lisp - 可読性の高いマクロDSL

(in-package :cl-regex)

;;; 基本マクロ

(defmacro regex (&body forms)
  "S式で正規表現パターンを構築"
  `(build-pattern ',forms))

(defmacro defpattern (name &body pattern-spec)
  "名前付きパターンを定義"
  (multiple-value-bind (pattern options)
      (if (keywordp (car (last pattern-spec)))
          (values (butlast pattern-spec) (last pattern-spec))
          (values pattern-spec nil))
    `(progn
       (defparameter ,name
         (compile-regex (regex ,@pattern) ,@options))
       ',name)))

;;; マッチングマクロ

(defmacro when-match ((var pattern text) &body body)
  "パターンがマッチした時に実行"
  `(let ((,var (match ,pattern ,text)))
     (when ,var
       ,@body)))

(defmacro with-match ((var pattern text) &body body)
  "マッチ結果をバインドして実行"
  (let ((gtext (gensym "TEXT-")))
    `(let* ((,gtext ,text)
            (,var (match ,pattern ,gtext)))
       (if ,var
           (progn ,@body)
           (error "Pattern did not match: ~S" ,gtext)))))

(defmacro do-matches ((var pattern text) &body body)
  "全てのマッチに対して繰り返し"
  (let ((gmatches (gensym "MATCHES-")))
    `(let ((,gmatches (match-all ,pattern ,text)))
       (dolist (,var ,gmatches)
         ,@body))))

;;; パターン構築ヘルパー

(defmacro group (name-or-index &body expr)
  "名前付きまたは番号付きキャプチャグループ"
  `(make-group-node :name ,name-or-index
                    :capturing t
                    :children (list ,@expr)))

(defmacro non-capturing (&body expr)
  "非キャプチャグループ"
  `(make-group-node :capturing nil
                    :children (list ,@expr)))

(defmacro atomic (&body expr)
  "アトミックグループ（バックトラッキングなし）"
  `(make-group-node :capturing nil
                    :properties '(:atomic t)
                    :children (list ,@expr)))

;;; 量指定子マクロ

(defmacro optional (expr)
  "0または1回"
  `(make-quantifier-node :min 0 :max 1
                         :children (list ,expr)))

(defmacro exactly (n expr)
  "正確にn回"
  `(make-quantifier-node :min ,n :max ,n
                         :children (list ,expr)))

(defmacro repeat (min max-or-expr &optional expr)
  "min回以上max回以下"
  (if expr
      `(make-quantifier-node :min ,min :max ,max-or-expr
                             :children (list ,expr))
      `(make-quantifier-node :min ,min :max nil
                             :children (list ,max-or-expr))))

;;; 文字クラスマクロ

(defmacro char-class (&rest ranges)
  "文字クラスを定義"
  `(make-char-class-node :value (list ,@ranges)))

(defmacro not-char (&rest chars)
  "否定文字クラス"
  `(make-char-class-node :value (list ,@chars)
                         :negated t))

;;; アサーションマクロ

(defmacro look-ahead (expr)
  "肯定先読み"
  `(make-lookaround-node :direction :ahead
                         :positive t
                         :children (list ,expr)))

(defmacro not-ahead (expr)
  "否定先読み"
  `(make-lookaround-node :direction :ahead
                         :positive nil
                         :children (list ,expr)))

(defmacro look-behind (expr)
  "肯定後読み"
  `(make-lookaround-node :direction :behind
                         :positive t
                         :children (list ,expr)))

(defmacro not-behind (expr)
  "否定後読み"
  `(make-lookaround-node :direction :behind
                         :positive nil
                         :children (list ,expr)))

;;; 最適化マクロ

(defmacro with-optimization ((&key (level 2) jit cache parallel) &body body)
  "最適化コンテキスト内で実行"
  `(let ((*optimization-level* ,level)
         (*use-jit* ,jit)
         (*use-cache* ,cache)
         (*use-parallel* ,parallel))
     ,@body))

(defmacro compile-time-pattern (pattern-string)
  "コンパイル時にパターンを完全に展開"
  (let ((compiled (compile-regex pattern-string :optimize :extreme)))
    `(load-time-value ,compiled)))

;;; デバッグマクロ

(defmacro trace-match (pattern text)
  "マッチングプロセスをトレース"
  `(let ((*trace-matching* t))
     (match ,pattern ,text)))

(defmacro benchmark-match ((&key iterations) pattern text)
  "マッチングのベンチマーク"
  `(progn
     (format t "~&Benchmarking ~S~%" ',pattern)
     (time
      (dotimes (i (or ,iterations 10000))
        (match ,pattern ,text)))))

;;; 高度なDSLマクロ

(defmacro define-pattern-dsl (name &body clauses)
  "カスタムパターンDSLを定義"
  `(defmacro ,name (&body specs)
     (let ((pattern-parts '()))
       ,@(loop for (keyword . body) in clauses
               collect `(when (member ,keyword specs)
                         (push ,@body pattern-parts)))
       `(regex ,@(nreverse pattern-parts)))))

;;; 使用例：SQLインジェクション検出パターン
(defmacro sql-injection-pattern ()
  "SQLインジェクション検出用パターン"
  `(regex
     (or "'"
         "--"
         (group 'keyword
           (or "union" "select" "insert" "update" "delete" "drop"))
         "/*"
         "*/"
         (group 'hex "%27" "%22"))))

;;; 使用例：メールアドレスパターン
(defmacro email-pattern ()
  "メールアドレスマッチングパターン"
  `(regex
     (group 'local
       (+ (or alphanumeric "." "_" "%" "+" "-")))
     "@"
     (group 'domain
       (+ (or alphanumeric "." "-")))
     "."
     (group 'tld
       (repeat 2 4 alpha))))