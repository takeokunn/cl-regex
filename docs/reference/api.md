# CL-Regex API Reference

## 概要

CL-Regexは、高品質の性能を目指す純粋Common Lisp実装の正規表現エンジンです。本APIリファレンスでは、全ての公開関数、マクロ、クラス、および条件の詳細な仕様を提供します。

## パッケージ構造

```lisp
(defpackage :cl-regex
  (:use :cl)
  (:nicknames :clrx :regex)
  (:export
    ;; Core matching functions
    #:match #:match-all #:find-all #:scan
    ;; Pattern compilation
    #:compile-pattern #:compile-regex #:optimize-pattern
    ;; S-expression patterns
    #:match-sexpr #:compile-sexpr #:define-pattern
    ;; Replacement functions
    #:replace #:replace-all #:substitute-matches
    ;; Match object accessors
    #:match-string #:match-start #:match-end
    #:match-groups #:group-ref #:group-value
    #:named-group #:group-count
    ;; ... その他多数
    ))
```

## Core Functions

### Pattern Matching

#### `match`

```lisp
(match pattern text &key
       (start 0)
       (end nil)
       (case-sensitive t)
       (multiline nil)
       (dotall nil)
       (unicode t)
       (timeout nil)) => match-object or nil
```

**概要**: パターンとテキストをマッチングし、最初のマッチを返す

**引数**:
- `pattern`: 文字列、コンパイル済みパターン、またはS式パターン
- `text`: マッチング対象のテキスト（文字列）
- `start`: 検索開始位置（0-indexed）
- `end`: 検索終了位置（nilの場合はテキスト末尾）
- `case-sensitive`: 大文字小文字の区別（デフォルト: t）
- `multiline`: 複数行モード（^と$が行頭/行末にマッチ）
- `dotall`: ドットが改行にもマッチ
- `unicode`: Unicode文字のサポート
- `timeout`: タイムアウト秒数（nilで無制限）

**返り値**: `match-object`インスタンス、またはマッチしない場合はnil

**パフォーマンス**: O(n*m) worst case, O(n) average（nはテキスト長、mはパターン長）

**例**:
```lisp
;; 基本的なマッチング
(match "hello" "hello world")
; => #<MATCH "hello" :start 0 :end 5>

;; 大文字小文字を無視
(match "HELLO" "hello world" :case-sensitive nil)
; => #<MATCH "hello" :start 0 :end 5>

;; 部分文字列の検索
(match "world" "hello world" :start 6)
; => #<MATCH "world" :start 6 :end 11>

;; タイムアウト付き
(match "a*a*a*b" (make-string 100 :initial-element #\a) :timeout 1.0)
; => NIL (timeout)
```

#### `match-all`

```lisp
(match-all pattern text &key
           (start 0)
           (end nil)
           (limit nil)
           (overlap nil)
           case-sensitive
           multiline
           dotall) => list of match-objects
```

**概要**: 全ての非重複マッチを返す

**引数**:
- `limit`: 最大マッチ数（nilで無制限）
- `overlap`: 重複マッチを許可するか（デフォルト: nil）
- その他は`match`と同じ

**返り値**: `match-object`のリスト

**パフォーマンス**: O(n*m*k)（kはマッチ数）

**例**:
```lisp
(match-all "\\d+" "123 abc 456 def 789")
; => (#<MATCH "123"> #<MATCH "456"> #<MATCH "789">)

;; 制限付き
(match-all "\\w+" "one two three four" :limit 2)
; => (#<MATCH "one"> #<MATCH "two">)

;; 重複マッチ
(match-all "aa" "aaaa" :overlap t)
; => (#<MATCH "aa" :start 0> #<MATCH "aa" :start 1> #<MATCH "aa" :start 2>)
```

#### `scan`

```lisp
(scan pattern text &key
      (start 0)
      (end nil)
      (callback nil)) => positions or nil
```

**概要**: マッチの位置情報のみを高速に取得

**引数**:
- `callback`: 各マッチに対して呼ばれる関数（副作用のため）

**返り値**: (start . end)のコンスセル、またはnil

**パフォーマンス**: `match`より約20%高速（オブジェクト生成なし）

**例**:
```lisp
(scan "\\d+" "abc123def")
; => (3 . 6)

;; コールバック付き
(let ((matches '()))
  (scan "\\w+" "hello world"
        :callback (lambda (start end)
                   (push (cons start end) matches)))
  (reverse matches))
; => ((0 . 5) (6 . 11))
```

### Pattern Compilation

#### `compile-pattern`

```lisp
(compile-pattern pattern &key
                (optimize :balanced)
                (engine :auto)
                (jit :auto)
                (cache-size 256)
                (flags '())
                (analyze t)) => compiled-pattern
```

**概要**: パターンを事前コンパイルして最適化

**引数**:
- `pattern`: 正規表現文字列またはS式
- `optimize`: 最適化戦略
  - `:speed` - 実行速度優先（メモリ使用量増）
  - `:space` - メモリ効率優先（実行速度低下）
  - `:balanced` - バランス型（デフォルト）
- `engine`: 実行エンジンの選択
  - `:nfa` - 非決定性有限オートマトン
  - `:dfa` - 決定性有限オートマトン
  - `:hybrid` - ハイブリッド型
  - `:auto` - 自動選択（デフォルト）
- `jit`: JITコンパイル
  - `t` - 即座にJITコンパイル
  - `nil` - JITコンパイルなし
  - `:auto` - 使用頻度に応じて自動（デフォルト）
- `cache-size`: 内部キャッシュサイズ（バイト数）
- `flags`: コンパイルフラグのリスト
- `analyze`: パターン分析を実行するか

**返り値**: `compiled-pattern`オブジェクト

**パフォーマンス**: コンパイル時間O(m^2)、実行時20-100倍高速化

**例**:
```lisp
;; 速度優先でコンパイル
(defparameter *email-pattern*
  (compile-pattern "[\\w._%+-]+@[\\w.-]+\\.[A-Za-z]{2,}"
                  :optimize :speed
                  :jit t))

;; 使用
(match *email-pattern* "user@example.com")
; => #<MATCH "user@example.com">

;; パターン分析付き
(compile-pattern "a*a*a*b" :analyze t)
; WARNING: Potential catastrophic backtracking detected
; => #<COMPILED-PATTERN optimized>
```

#### `compile-sexpr`

```lisp
(compile-sexpr sexpr-pattern &rest options) => compiled-pattern
```

**概要**: S式パターンをコンパイル

**例**:
```lisp
(compile-sexpr
  '(:sequence
    (:group user (:+ (:class :alnum #\. #\_ #\-)))
    #\@
    (:group domain (:+ (:class :alnum #\.)))
    #\.
    (:group tld (:between 2 4 :alpha))))
```

### Replacement Functions

#### `replace`

```lisp
(replace pattern replacement text &key
         (count 1)
         (start 0)
         (end nil)
         case-sensitive) => string
```

**概要**: パターンにマッチした部分を置換

**引数**:
- `pattern`: マッチングパターン
- `replacement`: 置換文字列または関数
  - 文字列: バックリファレンス（$1, $2等）をサポート
  - 関数: (lambda (match) ...) 形式
- `text`: 対象テキスト
- `count`: 置換回数（nilで全て）

**返り値**: 置換後の文字列

**例**:
```lisp
;; 単純な置換
(replace "\\d+" "[NUM]" "I have 42 apples and 7 oranges")
; => "I have [NUM] apples and 7 oranges"

;; バックリファレンス
(replace "(\\w+)@(\\w+\\.\\w+)" "$2 user: $1" "john@example.com")
; => "example.com user: john"

;; 関数による動的置換
(replace "\\d+"
         (lambda (m)
           (format nil "~R" (parse-integer (match-string m))))
         "I have 42 apples")
; => "I have forty-two apples"

;; 名前付きグループ
(replace "(?<num>\\d+)" "Number ${num}" "Test 123")
; => "Test Number 123"
```

#### `replace-all`

```lisp
(replace-all pattern replacement text &rest options) => string
```

**概要**: 全てのマッチを置換（`replace`の`:count nil`と等価）

### Match Object API

#### `match-object` クラス

```lisp
(defclass match-object ()
  ((text :initarg :text :reader match-text
         :documentation "マッチした全体のテキスト")
   (start :initarg :start :reader match-start
          :documentation "マッチ開始位置")
   (end :initarg :end :reader match-end
        :documentation "マッチ終了位置")
   (groups :initarg :groups :reader match-groups
           :documentation "キャプチャグループのベクタ")
   (named-groups :initarg :named-groups :reader match-named-groups
                 :documentation "名前付きグループのハッシュテーブル")))
```

#### アクセサ関数

```lisp
;; 基本アクセサ
(match-string match &optional (group 0)) => string
(match-start match &optional (group 0)) => integer
(match-end match &optional (group 0)) => integer
(match-length match &optional (group 0)) => integer

;; グループアクセサ
(group-ref match group-id) => string or nil
(group-value match group-id) => string or nil
(named-group match name) => string or nil
(group-count match) => integer
(group-exists-p match group-id) => boolean

;; 便利関数
(match-before match) => string  ; マッチ前のテキスト
(match-after match) => string   ; マッチ後のテキスト
(match-groups-alist match) => alist  ; グループの連想リスト
```

**例**:
```lisp
(let ((m (match "(\\d{4})-(\\d{2})-(\\d{2})" "2024-03-15")))
  (list :full (match-string m)
        :year (group-ref m 1)
        :month (group-ref m 2)
        :day (group-ref m 3)))
; => (:full "2024-03-15" :year "2024" :month "03" :day "15")

;; 名前付きグループ
(let ((m (match "(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})"
                "2024-03-15")))
  (list :year (named-group m 'year)
        :month (named-group m 'month)
        :day (named-group m 'day)))
; => (:year "2024" :month "03" :day "15")
```

## S-Expression DSL

### パターン構築マクロ

#### `define-pattern`

```lisp
(define-pattern name pattern-spec &rest options)
```

**概要**: 名前付きパターンを定義

**例**:
```lisp
(define-pattern email-address
  (:sequence
    (:named local-part
      (:+ (:class :alnum #\. #\_ #\% #\+ #\-)))
    #\@
    (:named domain
      (:sequence
        (:+ (:class :alnum #\-))
        (:* (:sequence #\. (:+ (:class :alnum #\-))))))
    #\.
    (:named tld
      (:between 2 6 :alpha)))
  :documentation "RFC-compliant email address pattern"
  :optimize :speed
  :examples ("user@example.com" "admin+tag@company.org"))

;; 使用
(match email-address "john.doe@example.com")
```

### S式パターン要素

#### 基本要素

```lisp
;; リテラル
"literal string"
#\c  ; 文字リテラル

;; 文字クラス
:digit      ; [0-9]
:alpha      ; [a-zA-Z]
:alnum      ; [a-zA-Z0-9]
:word       ; \w
:space      ; \s
:any        ; .

;; カスタム文字クラス
(:class elements...)
(:not elements...)
(:range start end)
```

#### 量指定子

```lisp
(:* pattern)           ; 0回以上
(:+ pattern)           ; 1回以上
(:? pattern)           ; 0回または1回
(:repeat n pattern)    ; 正確にn回
(:between min max pattern)  ; min以上max以下
(:at-least n pattern)  ; n回以上
(:at-most n pattern)   ; n回以下
```

#### グループ化

```lisp
(:group pattern)                ; 番号付きグループ
(:named name pattern)            ; 名前付きグループ
(:non-capturing pattern)         ; 非キャプチャグループ
(:atomic pattern)                ; アトミックグループ
(:independent pattern)           ; 独立部分式
```

#### 選択と順序

```lisp
(:or patterns...)                ; 選択
(:sequence patterns...)          ; 連結
(:any-of patterns...)           ; 文字クラスの選択
```

#### アサーション

```lisp
(:look-ahead pattern)            ; 前方先読み
(:not-ahead pattern)             ; 否定前方先読み
(:look-behind pattern)           ; 後方先読み
(:not-behind pattern)            ; 否定後方先読み
(:boundary type)                 ; 境界（:word, :line-start等）
```

### 高度なS式パターン

```lisp
;; 条件付きマッチ
(:if test then-pattern else-pattern)

;; 再帰パターン
(:recurse name)
(:define-recurse name pattern)

;; バランスグループ
(:balanced open close)

;; Unicode プロパティ
(:unicode-property property-name)
(:unicode-category category)

;; Possessive量指定子
(:possessive (:+ pattern))
```

## CLOS Integration

### Pattern Protocol

```lisp
(defgeneric match-generic (pattern text &rest options)
  (:documentation "Generic function for pattern matching"))

;; 文字列パターン
(defmethod match-generic ((pattern string) (text string) &rest options)
  (apply #'match-string-pattern pattern text options))

;; コンパイル済みパターン
(defmethod match-generic ((pattern compiled-pattern) (text string) &rest options)
  (apply #'match-compiled pattern text options))

;; S式パターン
(defmethod match-generic ((pattern cons) (text string) &rest options)
  (apply #'match-sexpr pattern text options))

;; カスタムパターンクラス
(defclass custom-pattern (pattern)
  ((matcher :initarg :matcher)))

(defmethod match-generic ((pattern custom-pattern) (text string) &rest options)
  (funcall (slot-value pattern 'matcher) text options))
```

### Method Combinations

```lisp
(defgeneric process-match (match processor)
  (:method-combination progn))

(defmethod process-match :before ((match match-object) processor)
  (validate-match match))

(defmethod process-match ((match match-object) processor)
  (funcall processor match))

(defmethod process-match :after ((match match-object) processor)
  (log-match match))
```

## Prolog Integration

### Logic Programming Interface

#### Rule Definition

```lisp
(defrule pattern-matches (?pattern ?text ?result)
  "Define pattern matching as a Prolog predicate"
  (unify ?result (match ?pattern ?text)))

(defrule valid-email (?email)
  "Check if email is valid"
  (pattern-matches email-address ?email ?match)
  (not-null ?match))

;; 使用例
(query (valid-email "user@example.com"))
; => T

(query-all (pattern-matches ?p "hello@world.com" ?m)
           :limit 10)
; => (((?p . email-pattern) (?m . #<MATCH>)) ...)
```

#### DCG (Definite Clause Grammar) Support

```lisp
(defgrammar email-grammar
  (email --> local-part "@" domain "." tld)
  (local-part --> (+ (or alnum "._%-")))
  (domain --> (+ (or alnum ".-")))
  (tld --> (between 2 4 alpha)))

;; パースと生成
(parse-with-grammar email-grammar "user@example.com")
; => (:email (:local-part "user") (:domain "example") (:tld "com"))
```

## Performance Optimization

### Compiler Macros

```lisp
;; 定数パターンの最適化
(define-compiler-macro match (&whole form pattern text &rest args)
  (if (and (constantp pattern)
           (stringp (eval pattern)))
      `(match-optimized ,(optimize-pattern-at-compile-time (eval pattern))
                       ,text ,@args)
      form))

;; インライン展開
(declaim (inline fast-literal-match))
(defun fast-literal-match (text literal)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type simple-string text literal))
  (search literal text))
```

### Type Specialization

```lisp
;; 型特殊化メソッド
(defmethod match-generic ((pattern compiled-pattern)
                         (text simple-string)
                         &rest options)
  (declare (optimize (speed 3) (safety 0)))
  (fast-match-simple-string pattern text options))

(defmethod match-generic ((pattern compiled-pattern)
                         (text (vector character))
                         &rest options)
  (declare (optimize (speed 3) (safety 0)))
  (fast-match-char-vector pattern text options))
```

## Error Conditions

### Condition Hierarchy

```lisp
cl-regex-error
├── pattern-error
│   ├── syntax-error
│   ├── compilation-error
│   └── validation-error
├── match-error
│   ├── timeout-error
│   ├── memory-limit-error
│   └── backtrack-limit-error
└── configuration-error
```

### Error Handling

```lisp
(defcondition regex-syntax-error (pattern-error)
  ((pattern :initarg :pattern)
   (position :initarg :position)
   (message :initarg :message))
  (:report (lambda (c stream)
             (format stream "Syntax error in pattern at position ~D: ~A"
                     (slot-value c 'position)
                     (slot-value c 'message)))))

;; 使用例
(handler-bind
    ((regex-syntax-error
      (lambda (e)
        (format t "Pattern error: ~A~%" e)
        (invoke-restart 'use-fallback-pattern))))
  (compile-pattern "[invalid"))
```

## Configuration

### Special Variables

```lisp
(defparameter *default-engine* :hybrid
  "Default regex engine (:nfa :dfa :hybrid :vm)")

(defparameter *default-optimization* :balanced
  "Default optimization strategy")

(defparameter *regex-cache-size* 256
  "Number of compiled patterns to cache")

(defparameter *regex-timeout* nil
  "Default timeout in seconds (nil = no timeout)")

(defparameter *max-backtrack-limit* 10000
  "Maximum backtracking steps before giving up")

(defparameter *unicode-mode* t
  "Enable Unicode support")

(defparameter *debug-regex* nil
  "Enable debug output")

(defparameter *pattern-statistics* nil
  "Collect pattern matching statistics")
```

### Configuration Macros

```lisp
(defmacro with-regex-configuration ((&rest options) &body body)
  "Execute body with temporary regex configuration"
  `(let ((*default-engine* ,(getf options :engine *default-engine*))
         (*default-optimization* ,(getf options :optimization *default-optimization*))
         (*regex-timeout* ,(getf options :timeout *regex-timeout*))
         (*unicode-mode* ,(getf options :unicode *unicode-mode*)))
     ,@body))

;; 使用例
(with-regex-configuration (:engine :dfa
                          :optimization :speed
                          :timeout 5.0)
  (match-all complex-pattern large-text))
```

## Debugging and Profiling

### Debug Functions

```lisp
(trace-pattern pattern text &key (stream *standard-output*))
(explain-pattern pattern &key (detail-level :normal))
(validate-pattern pattern &key (strict t))
(pattern-complexity pattern) => complexity-score
(suggest-optimization pattern) => suggestions
```

### Profiling Tools

```lisp
(profile-pattern pattern text &key
                 (iterations 1000)
                 (warmup 100)) => profile-report

(benchmark-engines pattern text) => comparison-table

(analyze-backtracking pattern text) => backtrack-analysis
```

### Statistics Collection

```lisp
(with-pattern-statistics ()
  ;; ... perform matching operations ...
  (get-pattern-statistics))
; => #<STATISTICS :total-matches 1523
;                 :cache-hits 89%
;                 :avg-time 0.023ms>
```

## Thread Safety

CL-Regexは完全にスレッドセーフです：

- コンパイル済みパターンは不変でスレッド間で共有可能
- パターンキャッシュはロックフリーアルゴリズムを使用
- 各スレッドは独自のマッチングコンテキストを持つ

```lisp
;; マルチスレッド使用例
(defparameter *shared-pattern*
  (compile-pattern "\\d+" :thread-safe t))

(bt:make-thread
  (lambda ()
    (match *shared-pattern* "text123")))
```

## 拡張API

### カスタムオペレータ

```lisp
(define-regex-operator :japanese (&rest options)
  "Match Japanese characters"
  `(:or (:unicode-category "Hiragana")
        (:unicode-category "Katakana")
        (:unicode-category "Han")))

;; 使用
(match '(:+ :japanese) "こんにちは世界")
```

### プラグインシステム

```lisp
(define-regex-plugin :custom-assertions
  :version "1.0"
  :operators ((:balanced-parens . balanced-parens-matcher))
  :optimizers ((optimize-balanced . optimize-balanced-patterns)))

(load-regex-plugin :custom-assertions)
```

## パフォーマンスガイドライン

1. **パターンは事前コンパイル**: 繰り返し使用するパターンは必ずコンパイル
2. **適切なエンジン選択**: シンプルなパターンはDFA、複雑なパターンはNFA
3. **アトミックグループ使用**: バックトラッキングを制限
4. **possessive量指定子**: 不要なバックトラッキングを防ぐ
5. **アンカー使用**: ^や$で検索範囲を限定

## バージョン互換性

- CL-Regex 1.x: 基本API安定版
- CL-Regex 2.x: Prolog統合追加（後方互換）
- CL-Regex 3.x: 並列処理サポート（計画中）

## 参照

- [Pattern Syntax Reference](./pattern-syntax.md)
- [S-Expression DSL Reference](./s-expression-dsl.md)
- [CLOS Class Reference](./clos-classes.md)
- [Performance Metrics](./performance-metrics.md)