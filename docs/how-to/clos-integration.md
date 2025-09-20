# CLOSとの統合

CL-RegexとCommon Lisp Object System (CLOS)を深く統合し、オブジェクト指向の利点を最大限活用する方法を説明します。

## 概要

CLOS統合により実現される機能：

- **ジェネリック関数**: 柔軟なディスパッチ
- **メソッド合成**: 動的な機能拡張
- **MOPパターン**: メタオブジェクトプロトコル活用
- **マルチメソッド**: 複数引数での特殊化

## 基本的なCLOS統合

### ジェネリック関数ベースの設計

```lisp
;; 基本的なジェネリック関数群
(defgeneric compile-pattern (compiler pattern &key &allow-other-keys)
  (:documentation "パターンをコンパイル"))

(defgeneric match-against (matcher text &key start end)
  (:documentation "テキストに対してマッチング実行"))

(defgeneric extract-captures (match-result &key named-only)
  (:documentation "キャプチャグループを抽出"))

(defgeneric optimize-pattern (optimizer pattern level)
  (:documentation "パターンを最適化"))

;; 基底クラス階層
(defclass regex-component ()
  ((metadata :initform (make-hash-table) :accessor component-metadata)
   (created-at :initform (get-universal-time) :accessor created-at)
   (version :initarg :version :initform "1.0" :accessor component-version)))

(defclass pattern-element (regex-component)
  ((element-type :initarg :type :accessor element-type)
   (precedence :initarg :precedence :initform 0 :accessor element-precedence)))

(defclass quantified-element (pattern-element)
  ((min-count :initarg :min :initform 0 :accessor min-count)
   (max-count :initarg :max :initform nil :accessor max-count)
   (greedy :initarg :greedy :initform t :accessor greedy-p)))
```

### 特殊化メソッドの実装

```lisp
;; 文字クラス特殊化
(defclass character-class (pattern-element)
  ((character-set :initarg :characters :accessor character-set)
   (negated :initarg :negated :initform nil :accessor negated-p)
   (case-insensitive :initarg :case-insensitive :initform nil :accessor case-insensitive-p))
  (:default-initargs :type :character-class))

(defmethod compile-pattern ((compiler standard-compiler) (element character-class) &key)
  "文字クラス要素のコンパイル"
  (let ((chars (character-set element))
        (prefix (if (negated-p element) "[^" "["))
        (suffix "]"))
    (format nil "~A~A~A" prefix chars suffix)))

;; リテラル文字特殊化
(defclass literal-character (pattern-element)
  ((character :initarg :char :accessor literal-character)
   (case-sensitive :initarg :case-sensitive :initform t :accessor case-sensitive-p))
  (:default-initargs :type :literal))

(defmethod compile-pattern ((compiler standard-compiler) (element literal-character) &key)
  "リテラル文字のコンパイル"
  (let ((char (literal-character element)))
    (if (case-sensitive-p element)
        (string char)
        (format nil "[~C~C]" (char-upcase char) (char-downcase char)))))

;; グループ特殊化
(defclass capture-group (pattern-element)
  ((group-name :initarg :name :initform nil :accessor group-name)
   (group-index :initarg :index :accessor group-index)
   (inner-pattern :initarg :pattern :accessor inner-pattern))
  (:default-initargs :type :group))

(defmethod compile-pattern ((compiler standard-compiler) (element capture-group) &key)
  "キャプチャグループのコンパイル"
  (let ((inner (compile-pattern compiler (inner-pattern element)))
        (name (group-name element)))
    (if name
        (format nil "(?P<~A>~A)" name inner)
        (format nil "(~A)" inner))))
```

## 高度なCLOS機能の活用

### メソッド合成（Method Combination）

```lisp
;; カスタムメソッド合成の定義
(define-method-combination regex-processing ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after))
   (cleanup (:cleanup)))
  "正規表現処理用のメソッド合成"
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let ((form (if (or before after around cleanup)
                    `(multiple-value-prog1
                       (progn ,@(call-methods before)
                              (call-method ,(first primary)))
                       ,@(call-methods (reverse after)))
                    `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around)
                        (,@(rest around)
                         (make-method ,form)))
          (if cleanup
              `(unwind-protect ,form
                 ,@(call-methods cleanup))
              form)))))

;; メソッド合成の使用例
(defgeneric process-match (processor pattern text)
  (:method-combination regex-processing))

(defmethod process-match :before ((processor logging-processor) pattern text)
  "マッチング前のログ出力"
  (log-info "Starting match: ~A against ~A" pattern text))

(defmethod process-match :after ((processor logging-processor) pattern text)
  "マッチング後のログ出力"
  (log-info "Completed match processing"))

(defmethod process-match :cleanup ((processor resource-manager) pattern text)
  "リソースのクリーンアップ"
  (cleanup-resources processor))

(defmethod process-match ((processor standard-processor) pattern text)
  "メインのマッチング処理"
  (match-pattern pattern text))
```

### MOPを活用したメタクラス

```lisp
;; 正規表現要素用メタクラス
(defclass regex-element-metaclass (standard-class)
  ((element-registry :initform (make-hash-table) :allocation :class)))

(defmethod validate-superclass ((class regex-element-metaclass)
                               (superclass standard-class))
  t)

(defmethod finalize-inheritance :after ((class regex-element-metaclass))
  "クラス定義の完了時に要素を登録"
  (when (and (slot-boundp class 'element-type)
             (slot-value class 'element-type))
    (register-element-type (slot-value class 'element-type) class)))

;; 自動プロパティ生成
(defclass auto-property-metaclass (standard-class)
  ())

(defmethod compute-slots :around ((class auto-property-metaclass))
  "自動的にアクセサメソッドを生成"
  (let ((slots (call-next-method)))
    (dolist (slot slots)
      (when (getf (slot-definition-initargs slot) :auto-accessor)
        (ensure-accessor-methods class slot)))
    slots))

;; 使用例
(defclass optimized-pattern (pattern-element)
  ((optimization-level :initarg :optimization-level
                       :auto-accessor t
                       :documentation "最適化レベル")
   (cache-enabled :initarg :cache-enabled
                  :auto-accessor t
                  :initform t
                  :documentation "キャッシュの有効/無効"))
  (:metaclass auto-property-metaclass))
```

### マルチメソッドの活用

```lisp
;; 複数ディスパッチによる最適化
(defgeneric optimize-combination (optimizer left-element right-element)
  (:documentation "隣接する要素の組み合わせ最適化"))

;; 文字 + 文字の最適化
(defmethod optimize-combination ((optimizer sequence-optimizer)
                                (left literal-character)
                                (right literal-character))
  "連続する文字をまとめる"
  (make-instance 'literal-string
                 :string (concatenate 'string
                                     (string (literal-character left))
                                     (string (literal-character right)))))

;; 文字クラス + 文字クラスの最適化
(defmethod optimize-combination ((optimizer sequence-optimizer)
                                (left character-class)
                                (right character-class))
  "文字クラスをマージ"
  (when (and (not (negated-p left)) (not (negated-p right)))
    (make-instance 'character-class
                   :characters (union (character-set left)
                                     (character-set right)))))

;; 量詞の最適化
(defmethod optimize-combination ((optimizer sequence-optimizer)
                                (left quantified-element)
                                (right quantified-element))
  "同じ要素の量詞をまとめる"
  (when (pattern-equal-p (inner-pattern left) (inner-pattern right))
    (make-instance 'quantified-element
                   :pattern (inner-pattern left)
                   :min (+ (min-count left) (min-count right))
                   :max (when (and (max-count left) (max-count right))
                          (+ (max-count left) (max-count right))))))
```

## ドメイン固有のCLOS設計

### テンプレートメソッドパターン

```lisp
;; 抽象マッチングエンジン
(defclass abstract-matching-engine ()
  ((preprocessing-enabled :initarg :preprocessing :initform t :accessor preprocessing-enabled)
   (postprocessing-enabled :initarg :postprocessing :initform t :accessor postprocessing-enabled)))

(defgeneric execute-match (engine pattern text)
  (:documentation "マッチング実行のテンプレートメソッド"))

(defmethod execute-match ((engine abstract-matching-engine) pattern text)
  "マッチング実行のテンプレート"
  (let ((preprocessed-text (if (preprocessing-enabled engine)
                               (preprocess-text engine text)
                               text))
        (compiled-pattern (compile-pattern-for-engine engine pattern)))
    (let ((raw-result (perform-match engine compiled-pattern preprocessed-text)))
      (if (postprocessing-enabled engine)
          (postprocess-result engine raw-result)
          raw-result))))

;; 抽象メソッド（サブクラスで実装必須）
(defgeneric preprocess-text (engine text)
  (:documentation "テキストの前処理"))

(defgeneric compile-pattern-for-engine (engine pattern)
  (:documentation "エンジン固有のパターンコンパイル"))

(defgeneric perform-match (engine pattern text)
  (:documentation "実際のマッチング処理"))

(defgeneric postprocess-result (engine result)
  (:documentation "結果の後処理"))

;; 具象実装例：NFA エンジン
(defclass nfa-engine (abstract-matching-engine)
  ((state-cache :initform (make-hash-table) :accessor state-cache)))

(defmethod preprocess-text ((engine nfa-engine) text)
  "NFA用テキスト前処理"
  (normalize-unicode text))

(defmethod compile-pattern-for-engine ((engine nfa-engine) pattern)
  "NFA用パターンコンパイル"
  (compile-to-nfa pattern))

(defmethod perform-match ((engine nfa-engine) nfa text)
  "NFA実行"
  (simulate-nfa nfa text))

(defmethod postprocess-result ((engine nfa-engine) result)
  "NFA結果後処理"
  (extract-capture-groups result))
```

### ストラテジーパターンの実装

```lisp
;; マッチング戦略の抽象化
(defclass matching-strategy ()
  ())

(defgeneric apply-strategy (strategy pattern text context)
  (:documentation "マッチング戦略を適用"))

;; 貪欲マッチング戦略
(defclass greedy-strategy (matching-strategy)
  ())

(defmethod apply-strategy ((strategy greedy-strategy) pattern text context)
  "貪欲マッチングの実行"
  (greedy-match pattern text context))

;; 非貪欲マッチング戦略
(defclass non-greedy-strategy (matching-strategy)
  ())

(defmethod apply-strategy ((strategy non-greedy-strategy) pattern text context)
  "非貪欲マッチングの実行"
  (non-greedy-match pattern text context))

;; 所有格マッチング戦略
(defclass possessive-strategy (matching-strategy)
  ())

(defmethod apply-strategy ((strategy possessive-strategy) pattern text context)
  "所有格マッチングの実行"
  (possessive-match pattern text context))

;; 戦略を使用するマッチャ
(defclass strategic-matcher (base-matcher)
  ((strategy :initarg :strategy :accessor matching-strategy)))

(defmethod match-text ((matcher strategic-matcher) text)
  "戦略パターンによるマッチング"
  (apply-strategy (matching-strategy matcher)
                  (pattern matcher)
                  text
                  matcher))
```

## 観察者パターンとイベント処理

### イベント駆動アーキテクチャ

```lisp
;; イベントシステム
(defclass event-emitter ()
  ((listeners :initform (make-hash-table) :accessor event-listeners)))

(defgeneric emit-event (emitter event-type &rest args)
  (:documentation "イベントを発行"))

(defgeneric add-listener (emitter event-type listener)
  (:documentation "イベントリスナーを追加"))

(defmethod emit-event ((emitter event-emitter) event-type &rest args)
  "イベントを全リスナーに通知"
  (let ((listeners (gethash event-type (event-listeners emitter))))
    (dolist (listener listeners)
      (apply listener args))))

(defmethod add-listener ((emitter event-emitter) event-type listener)
  "リスナーを登録"
  (push listener (gethash event-type (event-listeners emitter))))

;; イベント対応マッチャ
(defclass event-driven-matcher (base-matcher event-emitter)
  ())

(defmethod match-text :around ((matcher event-driven-matcher) text)
  "イベント発行付きマッチング"
  (emit-event matcher :match-start matcher text)
  (let ((result (call-next-method)))
    (emit-event matcher :match-complete matcher text result)
    result))

;; 使用例：ログ出力リスナー
(defun create-logging-listener ()
  "ログ出力用リスナーを作成"
  (lambda (matcher text &optional result)
    (format t "Match event: ~A on ~A -> ~A~%"
            matcher text result)))

(let ((matcher (make-instance 'event-driven-matcher :pattern "a+")))
  (add-listener matcher :match-start (create-logging-listener))
  (add-listener matcher :match-complete (create-logging-listener))
  (match-text matcher "aaabbb"))
```

## 高度なCLOS機能

### 条件システムとの統合

```lisp
;; 正規表現固有の条件
(define-condition regex-condition ()
  ((pattern :initarg :pattern :reader condition-pattern)
   (text :initarg :text :reader condition-text))
  (:documentation "正規表現処理の基底条件"))

(define-condition regex-compilation-error (regex-condition error)
  ((error-position :initarg :position :reader error-position))
  (:documentation "パターンコンパイルエラー"))

(define-condition regex-timeout (regex-condition)
  ((timeout-duration :initarg :timeout :reader timeout-duration))
  (:documentation "マッチングタイムアウト"))

;; 再起動の定義
(defmethod match-text :around ((matcher base-matcher) text)
  "エラー処理と再起動機能付きマッチング"
  (restart-case
      (handler-bind
          ((regex-timeout
            (lambda (condition)
              (when *debug-mode*
                (format t "Timeout occurred: ~A~%" condition))
              (invoke-restart 'use-approximate-match))))
        (call-next-method))
    (use-approximate-match ()
      :report "近似マッチングを使用"
      (approximate-match (pattern matcher) text))
    (ignore-error ()
      :report "エラーを無視して nil を返す"
      nil)))
```

### MOP による動的プログラミング

```lisp
;; 動的メソッド生成
(defclass dynamic-matcher-metaclass (standard-class)
  ())

(defmethod compute-applicable-methods-using-classes :around
           ((gf standard-generic-function)
            (classes list))
  "動的にメソッドを生成して適用"
  (let ((methods (call-next-method)))
    (when (and (null methods)
               (eq gf #'compile-pattern)
               (find 'dynamic-pattern-element classes :test #'subtypep))
      ;; パターン要素用の汎用メソッドを動的生成
      (add-method gf (generate-compile-method (first classes))))
    methods))

(defun generate-compile-method (element-class)
  "要素クラス用のコンパイルメソッドを生成"
  (make-instance 'standard-method
                 :lambda-list '(compiler element)
                 :specializers (list (find-class 'standard-compiler) element-class)
                 :function (compile-method-function element-class)))

;; スロット値に基づく動的振る舞い
(defclass configurable-matcher ()
  ((behavior-config :initarg :config :accessor behavior-config)))

(defmethod match-text ((matcher configurable-matcher) text)
  "設定に基づく動的マッチング"
  (let ((config (behavior-config matcher)))
    (case (getf config :mode)
      (:fast (fast-match matcher text))
      (:accurate (accurate-match matcher text))
      (:balanced (balanced-match matcher text))
      (t (standard-match matcher text)))))
```

## ベストプラクティス

### 1. 適切な抽象化レベル

```lisp
;; Good: 適切な抽象化
(defclass text-processor ()
  ())

(defgeneric process-text (processor text)
  (:documentation "テキスト処理の汎用インターフェース"))

(defclass regex-text-processor (text-processor)
  ((pattern :initarg :pattern :accessor processor-pattern)))

(defmethod process-text ((processor regex-text-processor) text)
  (match-text (processor-pattern processor) text))

;; Bad: 過度な抽象化
(defclass abstract-string-manipulator ()
  ())

(defgeneric manipulate-string-data (manipulator data context options)
  (:documentation "文字列データの汎用操作"))
```

### 2. 責務の明確化

```lisp
;; Good: 単一責任
(defclass pattern-compiler ()
  "パターンのコンパイルのみを担当")

(defclass pattern-optimizer ()
  "パターンの最適化のみを担当")

(defclass pattern-executor ()
  "パターンの実行のみを担当")

;; Bad: 複数責任
(defclass regex-everything ()
  "全てを担当する神クラス（避けるべき）")
```

### 3. 効率的な多重継承

```lisp
;; Mixin パターンの活用
(defclass cacheable-mixin ()
  ((cache :initform (make-hash-table) :accessor cache)))

(defclass loggable-mixin ()
  ((logger :initarg :logger :accessor logger)))

(defclass optimized-matcher (base-matcher cacheable-mixin loggable-mixin)
  "キャッシュとログ機能を持つマッチャ")
```

## まとめ

CLOSとの深い統合により、CL-Regexは以下を実現します：

- **柔軟性**: ジェネリック関数による拡張可能設計
- **再利用性**: 適切な継承階層とMixin
- **保守性**: 明確な責務分離
- **拡張性**: MOPによる動的機能追加

CLOSの力を活用することで、高品質の拡張性を持つ正規表現エンジンを構築できます。

次は、不足しているリファレンスドキュメントの作成に進みましょう。