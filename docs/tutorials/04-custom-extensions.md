# カスタム拡張の構築

CL-Regexのアーキテクチャを拡張して、独自の機能を実装する方法を学習します。

## 目次

1. [拡張アーキテクチャの理解](#拡張アーキテクチャの理解)
2. [カスタム演算子の実装](#カスタム演算子の実装)
3. [プラグインシステム](#プラグインシステム)
4. [Prolog統合の活用](#prolog統合の活用)
5. [最適化エンジンの拡張](#最適化エンジンの拡張)
6. [カスタムバックエンドの実装](#カスタムバックエンドの実装)

## 拡張アーキテクチャの理解

CL-Regexは、MOPパターンとCLOSを活用した高度に拡張可能なアーキテクチャを提供します。

```lisp
;; 拡張ポイントの定義
(defclass extension-point ()
  ((name :initarg :name :accessor extension-name)
   (description :initarg :description :accessor extension-description)
   (priority :initarg :priority :initform 0 :accessor extension-priority)
   (enabled :initarg :enabled :initform t :accessor extension-enabled-p)))

;; 拡張レジストリ
(defparameter *extension-registry* (make-hash-table :test 'eq))

(defgeneric register-extension (extension-type extension)
  (:documentation "拡張機能を登録"))

(defgeneric load-extension (extension-type name)
  (:documentation "拡張機能をロード"))

(defgeneric unload-extension (extension-type name)
  (:documentation "拡張機能をアンロード"))

;; 拡張基底クラス
(defclass regex-extension (extension-point)
  ((hooks :initform '() :accessor extension-hooks)
   (metadata :initform (make-hash-table) :accessor extension-metadata)))
```

## カスタム演算子の実装

独自の正規表現演算子を実装して、表現力を拡張します。

```lisp
;; カスタム演算子基底クラス
(defclass custom-operator ()
  ((symbol :initarg :symbol :accessor operator-symbol)
   (arity :initarg :arity :accessor operator-arity)
   (precedence :initarg :precedence :accessor operator-precedence)
   (associativity :initarg :associativity :initform :left :accessor operator-associativity)))

;; 日本語文字クラス演算子の実装
(defclass japanese-char-operator (custom-operator)
  ()
  (:default-initargs
   :symbol :jp-char
   :arity 0
   :precedence 10))

(defmethod compile-operator ((op japanese-char-operator) &rest args)
  "日本語文字のパターンを生成"
  (declare (ignore args))
  "[\\u3040-\\u309F\\u30A0-\\u30FF\\u4E00-\\u9FAF\\u3400-\\u4DBF]")

;; 繰り返し回数指定演算子
(defclass count-operator (custom-operator)
  ((min-count :initarg :min :accessor min-count)
   (max-count :initarg :max :accessor max-count))
  (:default-initargs
   :symbol :count
   :arity 3
   :precedence 15))

(defmethod compile-operator ((op count-operator) pattern min max)
  "指定回数の繰り返しパターンを生成"
  (format nil "(?:~A){~A,~A}" pattern min max))

;; 演算子レジストリ
(defparameter *custom-operators* (make-hash-table :test 'eq))

(defun register-operator (operator)
  "カスタム演算子を登録"
  (setf (gethash (operator-symbol operator) *custom-operators*)
        operator))

;; 使用例
(register-operator (make-instance 'japanese-char-operator))
(register-operator (make-instance 'count-operator))

;; マクロでの活用
(defmacro defoperator (name (&rest params) &body body)
  "カスタム演算子定義マクロ"
  `(progn
     (defclass ,name (custom-operator)
       ()
       (:default-initargs :symbol ,(intern (symbol-name name) :keyword)))
     (defmethod compile-operator ((op ,name) ,@params)
       ,@body)
     (register-operator (make-instance ',name))))

;; 使用例：URL演算子
(defoperator url-operator ()
  "https?://[a-zA-Z0-9.-]+(?:\\.[a-zA-Z]{2,})+(?:/[^\\s]*)?")
```

## プラグインシステム

動的にロード可能なプラグインシステムを構築します。

```lisp
;; プラグイン基底クラス
(defclass regex-plugin (regex-extension)
  ((version :initarg :version :accessor plugin-version)
   (author :initarg :author :accessor plugin-author)
   (dependencies :initarg :dependencies :initform '() :accessor plugin-dependencies)
   (load-function :initarg :load-function :accessor plugin-load-function)
   (unload-function :initarg :unload-function :accessor plugin-unload-function)))

;; プラグインマネージャ
(defclass plugin-manager ()
  ((loaded-plugins :initform '() :accessor loaded-plugins)
   (plugin-paths :initform '() :accessor plugin-paths)
   (dependency-graph :initform (make-hash-table) :accessor dependency-graph)))

(defparameter *plugin-manager* (make-instance 'plugin-manager))

(defgeneric load-plugin (manager plugin-name)
  (:documentation "プラグインをロード"))

(defgeneric unload-plugin (manager plugin-name)
  (:documentation "プラグインをアンロード"))

(defgeneric discover-plugins (manager path)
  (:documentation "指定パスからプラグインを発見"))

;; プラグイン実装例：Unicode拡張
(defclass unicode-plugin (regex-plugin)
  ()
  (:default-initargs
   :name :unicode-extended
   :description "拡張Unicode文字クラスサポート"
   :version "1.0.0"
   :author "CL-Regex Team"))

(defmethod plugin-load-function ((plugin unicode-plugin))
  "Unicodeプラグインのロード処理"
  (lambda ()
    (register-operator (make-instance 'unicode-category-operator))
    (register-operator (make-instance 'unicode-block-operator))
    (format t "Unicode拡張プラグインがロードされました~%")))

(defmethod plugin-unload-function ((plugin unicode-plugin))
  "Unicodeプラグインのアンロード処理"
  (lambda ()
    (unregister-operator :unicode-category)
    (unregister-operator :unicode-block)
    (format t "Unicode拡張プラグインがアンロードされました~%")))

;; プラグイン設定ファイルの例（plugin.lisp）
(defplugin "unicode-extended"
  :version "1.0.0"
  :author "CL-Regex Team"
  :description "拡張Unicode文字クラスサポート"
  :dependencies ("core-regex")
  :operators (unicode-category-operator unicode-block-operator)
  :load-hook setup-unicode-tables
  :unload-hook cleanup-unicode-tables)
```

## Prolog統合の活用

S式Prologエンジンを活用した高度なパターンマッチングを実装します。

```lisp
;; Prolog統合エンジン
(defclass prolog-regex-engine ()
  ((knowledge-base :initform '() :accessor knowledge-base)
   (rules :initform '() :accessor rules)
   (facts :initform '() :accessor facts)))

;; Prolog規則の定義
(defmacro defrule (head &body body)
  "Prolog規則を定義"
  `(push '(rule ,head ,@body) (rules *prolog-engine*)))

(defmacro defact (fact)
  "Prolog事実を定義"
  `(push ',fact (facts *prolog-engine*)))

;; パターンマッチング規則の例
(defrule (valid-email ?email)
  (has-at-symbol ?email)
  (valid-username ?email)
  (valid-domain ?email))

(defrule (has-at-symbol ?email)
  (contains ?email "@"))

(defrule (valid-username ?username)
  (not (starts-with ?username "."))
  (not (ends-with ?username "."))
  (alphanumeric-or-special ?username))

;; 実行エンジン
(defun prolog-match (pattern text)
  "Prologエンジンを使用したマッチング"
  (let ((bindings (make-hash-table)))
    (unify pattern text bindings)))

;; 統合マッチャ
(defclass prolog-enhanced-matcher (base-matcher)
  ((prolog-engine :initarg :prolog-engine :accessor prolog-engine)
   (hybrid-mode :initarg :hybrid-mode :initform t :accessor hybrid-mode)))

(defmethod match-text ((matcher prolog-enhanced-matcher) text)
  "Prologルールと正規表現のハイブリッドマッチング"
  (if (hybrid-mode matcher)
      (and (call-next-method)  ; 通常の正規表現マッチング
           (prolog-validate matcher text))  ; Prologによる検証
      (prolog-match (pattern matcher) text)))
```

## 最適化エンジンの拡張

カスタム最適化パスを実装して、特定用途向けの最適化を行います。

```lisp
;; 最適化パス基底クラス
(defclass optimization-pass ()
  ((name :initarg :name :accessor pass-name)
   (description :initarg :description :accessor pass-description)
   (enabled :initarg :enabled :initform t :accessor pass-enabled)
   (dependencies :initarg :dependencies :initform '() :accessor pass-dependencies)))

;; 最適化パスの実装例：反復削除最適化
(defclass redundant-repetition-pass (optimization-pass)
  ()
  (:default-initargs
   :name :redundant-repetition
   :description "冗長な反復パターンを削除"))

(defmethod apply-optimization ((pass redundant-repetition-pass) ast)
  "冗長な反復を最適化"
  (transform-ast ast
    ;; a*a* -> a*
    ((repetition (repetition ?pattern ?min1 ?max1) ?min2 ?max2)
     (repetition ?pattern
                 (max ?min1 ?min2)
                 (if (and ?max1 ?max2) (max ?max1 ?max2) nil)))

    ;; a+a* -> a+
    ((sequence (repetition ?pattern 1 nil) (repetition ?pattern 0 nil))
     (repetition ?pattern 1 nil))))

;; カスタム最適化パイプライン
(defclass custom-optimizer ()
  ((passes :initform '() :accessor optimizer-passes)
   (statistics :initform (make-hash-table) :accessor optimizer-statistics)))

(defmethod add-pass ((optimizer custom-optimizer) pass)
  "最適化パスを追加"
  (push pass (optimizer-passes optimizer)))

(defmethod run-optimization ((optimizer custom-optimizer) ast)
  "最適化パイプラインを実行"
  (reduce (lambda (current-ast pass)
            (if (pass-enabled pass)
                (apply-optimization pass current-ast)
                current-ast))
          (reverse (optimizer-passes optimizer))
          :initial-value ast))

;; JIT最適化の実装
(defclass jit-optimization-pass (optimization-pass)
  ((runtime-statistics :initform (make-hash-table) :accessor runtime-statistics)
   (hot-threshold :initarg :hot-threshold :initform 1000 :accessor hot-threshold)))

(defmethod apply-optimization ((pass jit-optimization-pass) ast)
  "実行時統計に基づく最適化"
  (let ((hot-patterns (find-hot-patterns pass)))
    (optimize-hot-patterns ast hot-patterns)))
```

## カスタムバックエンドの実装

独自の実行エンジンバックエンドを実装します。

```lisp
;; バックエンド基底クラス
(defclass regex-backend ()
  ((name :initarg :name :accessor backend-name)
   (capabilities :initarg :capabilities :initform '() :accessor backend-capabilities)
   (optimization-level :initarg :optimization-level :initform 2 :accessor optimization-level)))

;; GPU並列バックエンド（概念実装）
(defclass gpu-backend (regex-backend)
  ((device-id :initarg :device-id :initform 0 :accessor device-id)
   (memory-limit :initarg :memory-limit :initform (* 1024 1024 1024) :accessor memory-limit)
   (thread-blocks :initarg :thread-blocks :initform 256 :accessor thread-blocks))
  (:default-initargs
   :name :gpu-parallel
   :capabilities '(:parallel :simd :large-text)))

(defmethod compile-pattern ((backend gpu-backend) pattern)
  "GPUカーネル用にパターンをコンパイル"
  ;; 実際の実装では、CUDAやOpenCLカーネルを生成
  (generate-gpu-kernel pattern))

(defmethod execute-match ((backend gpu-backend) compiled-pattern text)
  "GPU上でマッチングを実行"
  ;; GPUメモリ転送、カーネル実行、結果取得
  (with-gpu-context ((device-id backend))
    (let ((gpu-text (transfer-to-gpu text))
          (gpu-pattern (transfer-to-gpu compiled-pattern)))
      (execute-gpu-kernel gpu-pattern gpu-text))))

;; メモリ最適化バックエンド
(defclass memory-optimized-backend (regex-backend)
  ((memory-pool :initform (make-memory-pool) :accessor memory-pool)
   (gc-threshold :initarg :gc-threshold :initform (* 10 1024 1024) :accessor gc-threshold))
  (:default-initargs
   :name :memory-optimized
   :capabilities '(:low-memory :incremental-gc)))

(defmethod execute-match ((backend memory-optimized-backend) compiled-pattern text)
  "メモリ効率最適化実行"
  (with-memory-management ((memory-pool backend))
    (streaming-match compiled-pattern text)))

;; バックエンドセレクタ
(defun select-optimal-backend (pattern text-size requirements)
  "要件に基づいて最適なバックエンドを選択"
  (cond
    ((and (> text-size (* 100 1024 1024))
          (member :parallel requirements))
     (make-instance 'gpu-backend))

    ((member :low-memory requirements)
     (make-instance 'memory-optimized-backend))

    (t (make-instance 'regex-backend))))
```

## テスト駆動拡張開発

拡張機能のテスト戦略を実装します。

```lisp
;; 拡張テストフレームワーク
(defclass extension-test-suite ()
  ((extension-name :initarg :extension-name :accessor extension-name)
   (test-cases :initform '() :accessor test-cases)
   (benchmarks :initform '() :accessor benchmarks)
   (property-tests :initform '() :accessor property-tests)))

(defmacro defextension-test (extension-name test-name &body body)
  "拡張機能のテストケース定義"
  `(let ((suite (find-test-suite ,extension-name)))
     (push (make-test-case ',test-name ,@body)
           (test-cases suite))))

;; プロパティベーステスト
(defun property-test-custom-operator (operator)
  "カスタム演算子のプロパティテスト"
  (quickcheck
    (forall (pattern (arbitrary-pattern))
      (let ((compiled (compile-operator operator pattern)))
        (implies (valid-pattern-p pattern)
                 (valid-regex-p compiled))))))

;; パフォーマンステスト
(defun benchmark-extension (extension text-samples)
  "拡張機能のベンチマーク"
  (let ((baseline-times '())
        (extension-times '()))
    (dolist (sample text-samples)
      (push (benchmark-match-without-extension sample) baseline-times)
      (push (benchmark-match-with-extension extension sample) extension-times))
    (compute-performance-impact baseline-times extension-times)))
```

## まとめ

CL-Regexの拡張システムにより、以下が実現可能です：

- **カスタム演算子**: 独自の正規表現構文の追加
- **プラグインシステム**: 動的な機能拡張
- **Prolog統合**: 論理的パターンマッチング
- **最適化エンジン**: 用途特化の最適化
- **バックエンド拡張**: 実行環境の最適化

これらの技術を組み合わせることで、高品質の性能と柔軟性を持つ正規表現エンジンを構築できます。

次は [ハウツーガイド](../how-to/) で具体的な実装テクニックを学習しましょう。