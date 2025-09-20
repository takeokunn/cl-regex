# CLOSクラス階層

CL-RegexのCommon Lisp Object System (CLOS) クラス階層の完全なリファレンスです。

## 基底クラス

### `regex-component`

すべてのCL-Regex要素の基底クラス。

```lisp
(defclass regex-component ()
  ((metadata :initform (make-hash-table) :accessor component-metadata)
   (created-at :initform (get-universal-time) :accessor created-at)
   (version :initarg :version :initform "1.0" :accessor component-version))
  (:documentation "すべての正規表現コンポーネントの基底クラス"))
```

#### スロット

| スロット | 型 | 説明 |
|----------|-----|------|
| `metadata` | `hash-table` | メタデータ情報 |
| `created-at` | `integer` | 作成時刻（Universal Time） |
| `version` | `string` | バージョン情報 |

#### メソッド

```lisp
(defgeneric component-info (component)
  (:documentation "コンポーネントの情報を取得"))

(defgeneric copy-component (component)
  (:documentation "コンポーネントの複製を作成"))
```

## パターン要素クラス

### `pattern-element`

パターン構成要素の基底クラス。

```lisp
(defclass pattern-element (regex-component)
  ((element-type :initarg :type :accessor element-type)
   (precedence :initarg :precedence :initform 0 :accessor element-precedence)
   (nullable :initarg :nullable :initform nil :accessor nullable-p))
  (:documentation "パターン要素の基底クラス"))
```

#### スロット

| スロット | 型 | 説明 |
|----------|-----|------|
| `element-type` | `keyword` | 要素の種類 |
| `precedence` | `integer` | 演算子優先順位 |
| `nullable` | `boolean` | 空文字列にマッチ可能か |

### `literal-element`

リテラル文字要素。

```lisp
(defclass literal-element (pattern-element)
  ((character :initarg :char :accessor literal-character)
   (case-sensitive :initarg :case-sensitive :initform t :accessor case-sensitive-p))
  (:default-initargs :type :literal :nullable nil))
```

#### スロット

| スロット | 型 | 説明 |
|----------|-----|------|
| `character` | `character` | リテラル文字 |
| `case-sensitive` | `boolean` | 大文字小文字を区別するか |

### `character-class`

文字クラス要素。

```lisp
(defclass character-class (pattern-element)
  ((character-set :initarg :characters :accessor character-set)
   (negated :initarg :negated :initform nil :accessor negated-p)
   (case-insensitive :initarg :case-insensitive :initform nil :accessor case-insensitive-p)
   (unicode-aware :initarg :unicode :initform t :accessor unicode-aware-p))
  (:default-initargs :type :character-class :nullable nil))
```

#### スロット

| スロット | 型 | 説明 |
|----------|-----|------|
| `character-set` | `string` または `list` | 文字セット |
| `negated` | `boolean` | 否定文字クラスか |
| `case-insensitive` | `boolean` | 大文字小文字を区別しないか |
| `unicode-aware` | `boolean` | Unicode対応か |

### `quantified-element`

量詞付き要素。

```lisp
(defclass quantified-element (pattern-element)
  ((inner-element :initarg :element :accessor inner-element)
   (min-count :initarg :min :initform 0 :accessor min-count)
   (max-count :initarg :max :initform nil :accessor max-count)
   (greedy :initarg :greedy :initform t :accessor greedy-p)
   (possessive :initarg :possessive :initform nil :accessor possessive-p))
  (:default-initargs :type :quantified))
```

#### スロット

| スロット | 型 | 説明 |
|----------|-----|------|
| `inner-element` | `pattern-element` | 量詞対象の要素 |
| `min-count` | `integer` | 最小繰り返し回数 |
| `max-count` | `integer` または `nil` | 最大繰り返し回数 |
| `greedy` | `boolean` | 貪欲マッチングか |
| `possessive` | `boolean` | 所有格マッチングか |

## グループクラス

### `group-element`

グループ要素の基底クラス。

```lisp
(defclass group-element (pattern-element)
  ((inner-pattern :initarg :pattern :accessor inner-pattern)
   (group-index :initarg :index :initform nil :accessor group-index))
  (:default-initargs :type :group))
```

### `capture-group`

キャプチャグループ。

```lisp
(defclass capture-group (group-element)
  ((group-name :initarg :name :initform nil :accessor group-name)
   (capturing :initform t :accessor capturing-p))
  (:default-initargs :type :capture-group))
```

#### スロット

| スロット | 型 | 説明 |
|----------|-----|------|
| `group-name` | `string` または `nil` | グループ名（名前付きキャプチャ） |
| `capturing` | `boolean` | キャプチャするか |

### `non-capture-group`

非キャプチャグループ。

```lisp
(defclass non-capture-group (group-element)
  ((capturing :initform nil :accessor capturing-p))
  (:default-initargs :type :non-capture-group))
```

### `atomic-group`

アトミックグループ。

```lisp
(defclass atomic-group (group-element)
  ((capturing :initform nil :accessor capturing-p)
   (atomic :initform t :accessor atomic-p))
  (:default-initargs :type :atomic-group))
```

## アンカークラス

### `anchor-element`

アンカー要素の基底クラス。

```lisp
(defclass anchor-element (pattern-element)
  ((anchor-type :initarg :anchor-type :accessor anchor-type)
   (multiline-aware :initarg :multiline :initform nil :accessor multiline-aware-p))
  (:default-initargs :type :anchor :nullable t))
```

### `start-anchor`

行/文字列開始アンカー。

```lisp
(defclass start-anchor (anchor-element)
  ()
  (:default-initargs :anchor-type :start))
```

### `end-anchor`

行/文字列終了アンカー。

```lisp
(defclass end-anchor (anchor-element)
  ()
  (:default-initargs :anchor-type :end))
```

### `word-boundary`

単語境界アンカー。

```lisp
(defclass word-boundary (anchor-element)
  ((boundary-type :initarg :boundary-type :initform :word :accessor boundary-type))
  (:default-initargs :anchor-type :word-boundary))
```

## 先読み・後読みクラス

### `lookaround-element`

先読み・後読みの基底クラス。

```lisp
(defclass lookaround-element (pattern-element)
  ((lookaround-pattern :initarg :pattern :accessor lookaround-pattern)
   (direction :initarg :direction :accessor lookaround-direction)
   (positive :initarg :positive :initform t :accessor positive-p)
   (variable-length :initarg :variable-length :initform nil :accessor variable-length-p))
  (:default-initargs :type :lookaround :nullable t))
```

#### スロット

| スロット | 型 | 説明 |
|----------|-----|------|
| `direction` | `:ahead` または `:behind` | 先読みか後読みか |
| `positive` | `boolean` | 正の条件か負の条件か |
| `variable-length` | `boolean` | 可変長後読みか |

### `lookahead`

先読み。

```lisp
(defclass lookahead (lookaround-element)
  ()
  (:default-initargs :direction :ahead))
```

### `lookbehind`

後読み。

```lisp
(defclass lookbehind (lookaround-element)
  ((max-length :initarg :max-length :initform nil :accessor max-length))
  (:default-initargs :direction :behind))
```

## 選択クラス

### `alternation-element`

選択要素。

```lisp
(defclass alternation-element (pattern-element)
  ((alternatives :initarg :alternatives :accessor alternatives)
   (optimized :initform nil :accessor optimized-p))
  (:default-initargs :type :alternation))
```

#### スロット

| スロット | 型 | 説明 |
|----------|-----|------|
| `alternatives` | `list` | 選択肢のリスト |
| `optimized` | `boolean` | 最適化済みか |

## コンパイラクラス

### `regex-compiler`

正規表現コンパイラの基底クラス。

```lisp
(defclass regex-compiler ()
  ((optimization-level :initarg :optimization :initform 2 :accessor optimization-level)
   (target-backend :initarg :backend :initform :nfa :accessor target-backend)
   (flags :initarg :flags :initform '() :accessor compiler-flags)
   (error-handler :initarg :error-handler :accessor error-handler))
  (:documentation "正規表現コンパイラ"))
```

#### スロット

| スロット | 型 | 説明 |
|----------|-----|------|
| `optimization-level` | `integer` | 最適化レベル（0-3） |
| `target-backend` | `keyword` | ターゲットバックエンド |
| `flags` | `list` | コンパイルフラグ |
| `error-handler` | `function` | エラーハンドラ |

### `nfa-compiler`

NFA用コンパイラ。

```lisp
(defclass nfa-compiler (regex-compiler)
  ((epsilon-elimination :initarg :epsilon-elim :initform t :accessor epsilon-elimination)
   (state-minimization :initarg :minimize :initform t :accessor state-minimization))
  (:default-initargs :backend :nfa))
```

### `dfa-compiler`

DFA用コンパイラ。

```lisp
(defclass dfa-compiler (regex-compiler)
  ((determinization :initform t :accessor determinization)
   (state-table-size :initarg :table-size :initform 1024 :accessor state-table-size))
  (:default-initargs :backend :dfa))
```

## マッチャクラス

### `regex-matcher`

マッチャの基底クラス。

```lisp
(defclass regex-matcher ()
  ((compiled-pattern :initarg :pattern :accessor compiled-pattern)
   (flags :initarg :flags :initform '() :accessor matcher-flags)
   (statistics :initform (make-hash-table) :accessor matcher-statistics))
  (:documentation "正規表現マッチャ"))
```

### `nfa-matcher`

NFAマッチャ。

```lisp
(defclass nfa-matcher (regex-matcher)
  ((current-states :initform '() :accessor current-states)
   (state-cache :initform (make-hash-table) :accessor state-cache)
   (backtrack-limit :initarg :backtrack-limit :initform 100000 :accessor backtrack-limit))
  (:documentation "NFA実行エンジン"))
```

### `dfa-matcher`

DFAマッチャ。

```lisp
(defclass dfa-matcher (regex-matcher)
  ((state-table :initarg :state-table :accessor state-table)
   (current-state :initform 0 :accessor current-state)
   (fast-path :initarg :fast-path :initform t :accessor fast-path-p))
  (:documentation "DFA実行エンジン"))
```

### `hybrid-matcher`

ハイブリッドマッチャ。

```lisp
(defclass hybrid-matcher (regex-matcher)
  ((nfa-engine :initarg :nfa :accessor nfa-engine)
   (dfa-engine :initarg :dfa :accessor dfa-engine)
   (switch-threshold :initarg :threshold :initform 1000 :accessor switch-threshold)
   (adaptive :initarg :adaptive :initform t :accessor adaptive-p))
  (:documentation "NFA/DFA ハイブリッドエンジン"))
```

## 結果クラス

### `match-result`

マッチ結果の基底クラス。

```lisp
(defclass match-result ()
  ((matched :initarg :matched :accessor matched-p)
   (start-position :initarg :start :accessor start-position)
   (end-position :initarg :end :accessor end-position)
   (input-text :initarg :text :accessor input-text))
  (:documentation "マッチ結果"))
```

### `capture-result`

キャプチャ付きマッチ結果。

```lisp
(defclass capture-result (match-result)
  ((captures :initarg :captures :initform '() :accessor captures)
   (named-captures :initarg :named :initform (make-hash-table :test 'equal)
                   :accessor named-captures))
  (:documentation "キャプチャグループ付きマッチ結果"))
```

### `capture-group-result`

個別キャプチャグループの結果。

```lisp
(defclass capture-group-result ()
  ((group-number :initarg :number :accessor group-number)
   (group-name :initarg :name :initform nil :accessor group-name)
   (start-position :initarg :start :accessor start-position)
   (end-position :initarg :end :accessor end-position)
   (captured-text :initarg :text :accessor captured-text))
  (:documentation "キャプチャグループの結果"))
```

## 最適化クラス

### `optimizer`

最適化器の基底クラス。

```lisp
(defclass optimizer ()
  ((optimization-passes :initform '() :accessor optimization-passes)
   (statistics :initform (make-hash-table) :accessor optimizer-statistics))
  (:documentation "正規表現最適化器"))
```

### `pattern-optimizer`

パターンレベル最適化器。

```lisp
(defclass pattern-optimizer (optimizer)
  ((constant-folding :initarg :constant-folding :initform t :accessor constant-folding)
   (dead-code-elimination :initarg :dead-code :initform t :accessor dead-code-elimination)
   (common-subexpression :initarg :cse :initform t :accessor common-subexpression))
  (:documentation "パターンレベルの最適化"))
```

### `execution-optimizer`

実行時最適化器。

```lisp
(defclass execution-optimizer (optimizer)
  ((jit-compilation :initarg :jit :initform nil :accessor jit-compilation)
   (caching :initarg :caching :initform t :accessor caching-enabled)
   (parallel-execution :initarg :parallel :initform nil :accessor parallel-execution))
  (:documentation "実行時最適化"))
```

## エラーと例外クラス

### `regex-error`

正規表現エラーの基底クラス。

```lisp
(define-condition regex-error (error)
  ((pattern :initarg :pattern :reader error-pattern)
   (position :initarg :position :initform nil :reader error-position))
  (:documentation "正規表現エラーの基底クラス"))
```

### `syntax-error`

構文エラー。

```lisp
(define-condition regex-syntax-error (regex-error)
  ((expected :initarg :expected :reader expected-token)
   (actual :initarg :actual :reader actual-token))
  (:documentation "正規表現構文エラー"))
```

### `compilation-error`

コンパイルエラー。

```lisp
(define-condition regex-compilation-error (regex-error)
  ((phase :initarg :phase :reader compilation-phase)
   (details :initarg :details :reader error-details))
  (:documentation "正規表現コンパイルエラー"))
```

### `runtime-error`

実行時エラー。

```lisp
(define-condition regex-runtime-error (regex-error)
  ((matcher :initarg :matcher :reader error-matcher)
   (text :initarg :text :reader error-text))
  (:documentation "正規表現実行時エラー"))
```

## Unicode対応クラス

### `unicode-processor`

Unicode処理の基底クラス。

```lisp
(defclass unicode-processor ()
  ((normalization-form :initarg :normalization :initform :nfc
                       :accessor normalization-form)
   (case-folding :initarg :case-folding :initform nil :accessor case-folding)
   (grapheme-aware :initarg :grapheme :initform nil :accessor grapheme-aware-p))
  (:documentation "Unicode処理器"))
```

### `unicode-character-class`

Unicode文字クラス。

```lisp
(defclass unicode-character-class (character-class)
  ((unicode-property :initarg :property :accessor unicode-property)
   (script :initarg :script :initform nil :accessor character-script)
   (block :initarg :block :initform nil :accessor character-block))
  (:documentation "Unicode文字プロパティベースの文字クラス"))
```

## プラグインクラス

### `regex-plugin`

プラグインの基底クラス。

```lisp
(defclass regex-plugin ()
  ((plugin-name :initarg :name :accessor plugin-name)
   (version :initarg :version :accessor plugin-version)
   (dependencies :initarg :dependencies :initform '() :accessor plugin-dependencies)
   (loaded :initform nil :accessor plugin-loaded-p))
  (:documentation "正規表現プラグイン"))
```

### `operator-plugin`

演算子プラグイン。

```lisp
(defclass operator-plugin (regex-plugin)
  ((operators :initform '() :accessor plugin-operators)
   (precedence-table :initform (make-hash-table) :accessor precedence-table))
  (:documentation "カスタム演算子プラグイン"))
```

## ユーティリティクラス

### `pattern-builder`

パターン構築支援。

```lisp
(defclass pattern-builder ()
  ((current-pattern :initform '() :accessor current-pattern)
   (flags :initform '() :accessor builder-flags)
   (validation :initarg :validation :initform t :accessor validation-enabled))
  (:documentation "パターン構築支援クラス"))
```

### `regex-debugger`

デバッグ支援。

```lisp
(defclass regex-debugger ()
  ((trace-enabled :initarg :trace :initform nil :accessor trace-enabled)
   (breakpoints :initform '() :accessor debugger-breakpoints)
   (step-mode :initform nil :accessor step-mode)
   (log-level :initarg :log-level :initform :info :accessor log-level))
  (:documentation "正規表現デバッガ"))
```

## メタクラス

### `regex-metaclass`

正規表現要素用メタクラス。

```lisp
(defclass regex-metaclass (standard-class)
  ((element-registry :allocation :class :initform (make-hash-table)))
  (:documentation "正規表現要素のメタクラス"))
```

## 継承関係図

```
regex-component
├── pattern-element
│   ├── literal-element
│   ├── character-class
│   │   └── unicode-character-class
│   ├── quantified-element
│   ├── group-element
│   │   ├── capture-group
│   │   ├── non-capture-group
│   │   └── atomic-group
│   ├── anchor-element
│   │   ├── start-anchor
│   │   ├── end-anchor
│   │   └── word-boundary
│   ├── lookaround-element
│   │   ├── lookahead
│   │   └── lookbehind
│   └── alternation-element
├── regex-compiler
│   ├── nfa-compiler
│   └── dfa-compiler
├── regex-matcher
│   ├── nfa-matcher
│   ├── dfa-matcher
│   └── hybrid-matcher
├── optimizer
│   ├── pattern-optimizer
│   └── execution-optimizer
└── regex-plugin
    └── operator-plugin

match-result
└── capture-result

unicode-processor

pattern-builder

regex-debugger
```

## 使用例

### 基本的なクラス使用

```lisp
;; リテラル要素の作成
(make-instance 'literal-element :char #\a)

;; 量詞付き要素
(make-instance 'quantified-element
               :element (make-instance 'literal-element :char #\a)
               :min 1 :max nil :greedy t)

;; キャプチャグループ
(make-instance 'capture-group
               :name "word"
               :pattern (make-instance 'character-class :characters "\\w+"))
```

### カスタムクラスの定義

```lisp
;; カスタム文字クラス
(defclass email-character-class (character-class)
  ((allow-plus :initarg :allow-plus :initform t :accessor allow-plus-p))
  (:documentation "メールアドレス用文字クラス"))

(defmethod compile-element ((element email-character-class))
  (if (allow-plus-p element)
      "[a-zA-Z0-9._%+-]"
      "[a-zA-Z0-9._%+-]"))
```

このクラス階層により、CL-Regexは高度に拡張可能で保守しやすい設計を実現しています。