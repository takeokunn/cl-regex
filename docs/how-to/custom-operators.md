# カスタム演算子の実装

CL-Regexでカスタム演算子を作成し、独自の正規表現構文を拡張する方法を説明します。

## 概要

カスタム演算子により、以下を実現できます：

- 特定ドメイン向けの専用構文
- 複雑なパターンの単純化
- パフォーマンス特化の演算子
- 他言語との互換性向上

## 基本的なカスタム演算子

### ステップ1: 演算子クラスの定義

```lisp
;; 基底クラスを継承したカスタム演算子
(defclass email-operator (custom-operator)
  ((allow-subdomains :initarg :allow-subdomains
                     :initform t
                     :accessor allow-subdomains)
   (strict-tld :initarg :strict-tld
               :initform nil
               :accessor strict-tld))
  (:default-initargs
   :symbol :email
   :arity 0
   :precedence 10))

(defmethod compile-operator ((op email-operator) &rest args)
  "メールアドレスパターンを生成"
  (declare (ignore args))
  (let ((user-pattern "[a-zA-Z0-9._%+-]+")
        (domain-pattern (if (allow-subdomains op)
                            "[a-zA-Z0-9.-]+"
                            "[a-zA-Z0-9]+"))
        (tld-pattern (if (strict-tld op)
                         "(?:com|org|net|edu|gov)"
                         "[a-zA-Z]{2,}")))
    (format nil "~A@~A\\.~A" user-pattern domain-pattern tld-pattern)))
```

### ステップ2: 演算子の登録と使用

```lisp
;; 演算子を登録
(register-operator (make-instance 'email-operator))

;; DSL内での使用
(defpattern email-finder
  (regex (:email)))

;; より詳細な設定
(defpattern strict-email-finder
  (regex (:email :allow-subdomains nil :strict-tld t)))
```

## 高度な演算子の実装

### パラメータ化演算子

```lisp
;; 可変長引数を持つ演算子
(defclass repeat-operator (custom-operator)
  ()
  (:default-initargs
   :symbol :repeat
   :arity :variable  ; 可変長引数
   :precedence 15))

(defmethod compile-operator ((op repeat-operator) pattern &optional (min 1) (max nil))
  "パターンの繰り返し回数を指定"
  (cond
    ((and min max)
     (format nil "(?:~A){~A,~A}" pattern min max))
    (min
     (format nil "(?:~A){~A,}" pattern min))
    (t
     (format nil "(?:~A)*" pattern))))

;; 使用例
(defpattern word-boundaries
  (regex (:repeat (:word-char) 3 10)))  ; 3-10文字の単語
```

### 条件付き演算子

```lisp
;; 実行時条件に基づくパターン生成
(defclass conditional-operator (custom-operator)
  ((condition-function :initarg :condition :accessor condition-function))
  (:default-initargs
   :symbol :when
   :arity 2
   :precedence 5))

(defmethod compile-operator ((op conditional-operator) condition pattern)
  "条件に基づいてパターンを生成"
  (if (funcall (condition-function op) condition)
      pattern
      "(?!)"))  ; 常に失敗するパターン

;; 使用例
(defparameter *debug-mode* t)

(register-operator
  (make-instance 'conditional-operator
                 :condition (lambda (cond)
                             (case cond
                               (:debug *debug-mode*)
                               (:production (not *debug-mode*))
                               (t nil)))))

(defpattern debug-log-pattern
  (regex (:when :debug "DEBUG:.*")))
```

## ドメイン特化演算子

### 日本語テキスト処理演算子

```lisp
;; 日本語文字クラス演算子群
(defclass hiragana-operator (custom-operator)
  ()
  (:default-initargs :symbol :hiragana :arity 0 :precedence 10))

(defmethod compile-operator ((op hiragana-operator) &rest args)
  (declare (ignore args))
  "[\\u3040-\\u309F]")

(defclass katakana-operator (custom-operator)
  ()
  (:default-initargs :symbol :katakana :arity 0 :precedence 10))

(defmethod compile-operator ((op katakana-operator) &rest args)
  (declare (ignore args))
  "[\\u30A0-\\u30FF]")

(defclass kanji-operator (custom-operator)
  ()
  (:default-initargs :symbol :kanji :arity 0 :precedence 10))

(defmethod compile-operator ((op kanji-operator) &rest args)
  (declare (ignore args))
  "[\\u4E00-\\u9FAF]")

;; 複合演算子
(defclass japanese-word-operator (custom-operator)
  ()
  (:default-initargs :symbol :jp-word :arity 0 :precedence 10))

(defmethod compile-operator ((op japanese-word-operator) &rest args)
  (declare (ignore args))
  "[\\u3040-\\u309F\\u30A0-\\u30FF\\u4E00-\\u9FAF]+")

;; 使用例
(mapc #'register-operator
      (list (make-instance 'hiragana-operator)
            (make-instance 'katakana-operator)
            (make-instance 'kanji-operator)
            (make-instance 'japanese-word-operator)))

(defpattern japanese-sentence
  (regex (:jp-word) "。"))
```

### 数値処理特化演算子

```lisp
;; 数値パターン演算子
(defclass number-operator (custom-operator)
  ((integer-only :initarg :integer-only :initform nil :accessor integer-only)
   (allow-negative :initarg :allow-negative :initform t :accessor allow-negative)
   (decimal-places :initarg :decimal-places :initform nil :accessor decimal-places))
  (:default-initargs
   :symbol :number
   :arity 0
   :precedence 10))

(defmethod compile-operator ((op number-operator) &rest args)
  (declare (ignore args))
  (let ((sign-part (if (allow-negative op) "[+-]?" ""))
        (integer-part "[0-9]+")
        (decimal-part (cond
                        ((integer-only op) "")
                        ((decimal-places op)
                         (format nil "\\.[0-9]{~A}" (decimal-places op)))
                        (t "(?:\\.[0-9]+)?"))))
    (format nil "~A~A~A" sign-part integer-part decimal-part)))

;; 特化バリエーション
(defclass currency-operator (number-operator)
  ((currency-symbol :initarg :currency :initform "$" :accessor currency-symbol))
  (:default-initargs
   :symbol :currency
   :decimal-places 2))

(defmethod compile-operator ((op currency-operator) &rest args)
  (declare (ignore args))
  (format nil "\\~A~A"
          (currency-symbol op)
          (call-next-method)))
```

## パフォーマンス最適化演算子

### 先読み最適化演算子

```lisp
;; 先読みを活用した効率的なパターン
(defclass optimized-word-boundary-operator (custom-operator)
  ()
  (:default-initargs
   :symbol :word-boundary-opt
   :arity 1
   :precedence 12))

(defmethod compile-operator ((op optimized-word-boundary-operator) pattern)
  "単語境界の最適化バージョン"
  (format nil "(?<=\\W)~A(?=\\W)" pattern))

;; 量詞最適化
(defclass greedy-prevention-operator (custom-operator)
  ()
  (:default-initargs
   :symbol :non-greedy
   :arity 1
   :precedence 11))

(defmethod compile-operator ((op greedy-prevention-operator) pattern)
  "非貪欲マッチングの強制"
  (regex-replace-all "\\*" pattern "*?"))
```

## 演算子の組み合わせと合成

### 演算子合成システム

```lisp
;; 演算子合成機能
(defclass composite-operator (custom-operator)
  ((component-operators :initarg :components :accessor component-operators))
  (:default-initargs
   :symbol :composite
   :arity :variable))

(defmethod compile-operator ((op composite-operator) &rest args)
  "複数演算子の組み合わせ"
  (let ((results '()))
    (loop for component in (component-operators op)
          for arg in args do
            (push (compile-operator component arg) results))
    (format nil "(?:~{~A~^|~})" (nreverse results))))

;; 使用例：複数言語対応
(defparameter *multilang-word*
  (make-instance 'composite-operator
                 :components (list (make-instance 'japanese-word-operator)
                                  (make-instance 'english-word-operator)
                                  (make-instance 'korean-word-operator))))
```

## テストとデバッグ

### 演算子のユニットテスト

```lisp
;; テストフレームワーク統合
(defun test-operator (operator test-cases)
  "演算子のテストケース実行"
  (loop for (input expected) in test-cases do
    (let ((pattern (compile-operator operator input))
          (result (match-pattern compiled-pattern expected)))
      (assert result (result)
              "演算子 ~A のテストが失敗: ~A"
              (operator-symbol operator) input))))

;; テストケースの定義
(defparameter *email-test-cases*
  '(("" "user@example.com")
    ("" "invalid.email")
    ("" "user@sub.domain.com")))

(test-operator (make-instance 'email-operator) *email-test-cases*)
```

### デバッグ支援

```lisp
;; デバッグ情報付き演算子
(defclass debug-operator (custom-operator)
  ((debug-mode :initarg :debug :initform nil :accessor debug-mode)
   (original-operator :initarg :original :accessor original-operator))
  (:default-initargs
   :symbol :debug))

(defmethod compile-operator ((op debug-operator) &rest args)
  "デバッグ情報付きパターン生成"
  (let ((pattern (apply #'compile-operator (original-operator op) args)))
    (if (debug-mode op)
        (format nil "(?P<debug_~A>~A)"
                (operator-symbol (original-operator op))
                pattern)
        pattern)))
```

## ベストプラクティス

### 1. 演算子の設計原則

```lisp
;; 良い設計例
(defclass url-operator (custom-operator)
  ((protocol :initarg :protocol :initform '("http" "https") :accessor protocol)
   (validate-tld :initarg :validate-tld :initform t :accessor validate-tld))
  (:default-initargs
   :symbol :url
   :arity 0))

;; 悪い設計例（避けるべき）
(defclass bad-operator (custom-operator)
  ((complex-state :initform (make-hash-table) :accessor complex-state)
   (global-dependency :initform *some-global* :accessor global-dependency)))
```

### 2. エラーハンドリング

```lisp
(defmethod compile-operator :around ((op custom-operator) &rest args)
  "演算子のエラーハンドリング"
  (handler-case (call-next-method)
    (error (condition)
      (error 'operator-compilation-error
             :operator op
             :args args
             :condition condition))))

(define-condition operator-compilation-error (error)
  ((operator :initarg :operator :reader error-operator)
   (args :initarg :args :reader error-args)
   (condition :initarg :condition :reader error-condition)))
```

### 3. ドキュメンテーション

```lisp
(defmethod describe-operator ((op custom-operator))
  "演算子の説明を生成"
  (format nil "演算子: ~A~%引数数: ~A~%優先順位: ~A~%説明: ~A"
          (operator-symbol op)
          (operator-arity op)
          (operator-precedence op)
          (operator-description op)))
```

## まとめ

カスタム演算子により、CL-Regexの表現力を効果的に拡張できます：

- **ドメイン特化**: 業務領域に特化した構文
- **パフォーマンス**: 最適化されたパターン生成
- **保守性**: 複雑なパターンの抽象化
- **再利用性**: 演算子の組み合わせと合成

次は [Unicode対応](./unicode-support.md) や [CLOSとの統合](./clos-integration.md) について学習しましょう。