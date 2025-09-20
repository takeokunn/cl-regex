# マクロリファレンス

CL-Regexで提供されるマクロの完全なリファレンスです。

## 基本マクロ

### `regex`

正規表現パターンを定義するメインマクロ。

```lisp
(defmacro regex (pattern &rest options)
  "正規表現パターンを定義"
  ...)

;; 使用例
(regex "a+b*")
(regex "\\d{3}-\\d{4}" :flags (:case-insensitive))
```

#### 引数

| 引数 | 型 | 説明 |
|------|-----|------|
| `pattern` | `string` または S式 | 正規表現パターン |
| `options` | キーワード引数 | コンパイルオプション |

#### オプション

| オプション | 型 | デフォルト | 説明 |
|------------|-----|------------|------|
| `:flags` | `list` | `()` | コンパイルフラグ |
| `:optimization` | `integer` | `2` | 最適化レベル |
| `:backend` | `keyword` | `:auto` | 実行バックエンド |
| `:cache` | `boolean` | `t` | コンパイル結果キャッシュ |

### `defpattern`

名前付きパターンを定義するマクロ。

```lisp
(defmacro defpattern (name pattern &rest options)
  "名前付きパターンを定義"
  ...)

;; 使用例
(defpattern email
  (regex "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"))

(defpattern phone-number
  (regex "\\(?\\d{3}\\)?[-. ]?\\d{3}[-. ]?\\d{4}")
  :documentation "北米電話番号形式")
```

#### 引数

| 引数 | 型 | 説明 |
|------|-----|------|
| `name` | `symbol` | パターン名 |
| `pattern` | 正規表現 | パターン定義 |
| `options` | キーワード引数 | 追加オプション |

#### オプション

| オプション | 型 | 説明 |
|------------|-----|------|
| `:documentation` | `string` | ドキュメント文字列 |
| `:export` | `boolean` | パッケージからエクスポートするか |
| `:test-cases` | `list` | テストケース |

## マッチングマクロ

### `match`

パターンマッチングを実行するマクロ。

```lisp
(defmacro match (pattern text &rest options)
  "パターンマッチングを実行"
  ...)

;; 使用例
(match email "user@example.com")
(match "\\d+" "123abc" :start 0 :end 3)
```

#### 引数

| 引数 | 型 | 説明 |
|------|-----|------|
| `pattern` | 正規表現 | マッチングパターン |
| `text` | `string` | 対象テキスト |

#### オプション

| オプション | 型 | デフォルト | 説明 |
|------------|-----|------------|------|
| `:start` | `integer` | `0` | 開始位置 |
| `:end` | `integer` | テキスト長 | 終了位置 |
| `:global` | `boolean` | `nil` | 全体マッチ |
| `:capture` | `boolean` | `t` | キャプチャグループ取得 |

### `match-case`

パターンマッチングによる条件分岐マクロ。

```lisp
(defmacro match-case (text &body clauses)
  "パターンマッチングによる条件分岐"
  ...)

;; 使用例
(match-case input-text
  (email (format t "Email: ~A" (capture 0)))
  (phone-number (format t "Phone: ~A" (capture 0)))
  (t (format t "Unknown format")))
```

#### 構文

```lisp
(match-case TEXT
  (PATTERN BODY...)
  (PATTERN BODY...)
  ...
  (t DEFAULT-BODY...))
```

### `with-match`

マッチ結果を使った処理のためのマクロ。

```lisp
(defmacro with-match ((pattern text &rest options) &body body)
  "マッチ結果を使った処理"
  ...)

;; 使用例
(with-match (email "user@example.com")
  (when (matched-p)
    (let ((username (capture-named "user"))
          (domain (capture-named "domain")))
      (format t "User: ~A, Domain: ~A" username domain))))
```

## 置換マクロ

### `replace`

文字列置換マクロ。

```lisp
(defmacro replace (pattern replacement text &rest options)
  "パターンマッチング置換"
  ...)

;; 使用例
(replace "\\d+" "X" "abc123def456")  ; "abcXdefX"
(replace email "<EMAIL>" text :global t)
```

#### 引数

| 引数 | 型 | 説明 |
|------|-----|------|
| `pattern` | 正規表現 | マッチングパターン |
| `replacement` | `string` または関数 | 置換文字列 |
| `text` | `string` | 対象テキスト |

#### オプション

| オプション | 型 | デフォルト | 説明 |
|------------|-----|------------|------|
| `:global` | `boolean` | `nil` | 全体置換 |
| `:case-insensitive` | `boolean` | `nil` | 大文字小文字無視 |

### `replace-all`

全体置換マクロ。

```lisp
(defmacro replace-all (pattern replacement text &rest options)
  "全体置換"
  ...)

;; 使用例
(replace-all "\\s+" " " "  multiple   spaces  ")  ; " multiple spaces "
```

## DSLマクロ

### `regex-dsl`

正規表現DSL構築マクロ。

```lisp
(defmacro regex-dsl (&body forms)
  "正規表現DSL"
  ...)

;; 使用例
(regex-dsl
  (sequence
    (one-or-more (char-class "a-z"))
    (literal "@")
    (one-or-more (char-class "a-z0-9.-"))
    (literal ".")
    (repeat (char-class "a-z") 2 4)))
```

#### DSL構文

| 構文 | 説明 | 例 |
|------|------|-----|
| `(literal STRING)` | リテラル文字列 | `(literal "abc")` |
| `(char-class SPEC)` | 文字クラス | `(char-class "a-z")` |
| `(sequence FORMS...)` | 連続 | `(sequence (literal "a") (literal "b"))` |
| `(alternation FORMS...)` | 選択 | `(alternation (literal "a") (literal "b"))` |
| `(optional FORM)` | オプション | `(optional (literal "s"))` |
| `(zero-or-more FORM)` | 0回以上 | `(zero-or-more (literal "a"))` |
| `(one-or-more FORM)` | 1回以上 | `(one-or-more (literal "a"))` |
| `(repeat FORM MIN MAX)` | 回数指定 | `(repeat (literal "a") 2 5)` |
| `(group FORM)` | グループ | `(group (literal "abc"))` |
| `(named-group NAME FORM)` | 名前付きグループ | `(named-group "word" (char-class "\\w+"))` |

### `defoperator`

カスタム演算子定義マクロ。

```lisp
(defmacro defoperator (name (&rest params) &body body)
  "カスタム演算子を定義"
  ...)

;; 使用例
(defoperator email-pattern ()
  "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")

(defoperator repeat-pattern (pattern min max)
  (format nil "(?:~A){~A,~A}" pattern min max))
```

## 条件マクロ

### `when-match`

マッチした場合の条件実行マクロ。

```lisp
(defmacro when-match ((pattern text &rest options) &body body)
  "マッチした場合のみ実行"
  ...)

;; 使用例
(when-match (email input-text)
  (send-email (capture 0)))
```

### `unless-match`

マッチしなかった場合の条件実行マクロ。

```lisp
(defmacro unless-match ((pattern text &rest options) &body body)
  "マッチしなかった場合のみ実行"
  ...)

;; 使用例
(unless-match (email input-text)
  (signal-invalid-email-error))
```

### `cond-match`

複数パターンの条件分岐マクロ。

```lisp
(defmacro cond-match (text &body clauses)
  "複数パターンの条件分岐"
  ...)

;; 使用例
(cond-match input-text
  (email (process-email (capture 0)))
  (phone-number (process-phone (capture 0)))
  (url (process-url (capture 0)))
  (t (process-unknown input-text)))
```

## 反復マクロ

### `scan`

テキストを走査してマッチを収集するマクロ。

```lisp
(defmacro scan (pattern text &rest options)
  "パターンでテキストを走査"
  ...)

;; 使用例
(scan "\\d+" "abc123def456ghi")  ; ("123" "456")
(scan email text :capture :named)
```

### `do-matches`

マッチ結果を順次処理するマクロ。

```lisp
(defmacro do-matches ((pattern text &rest options) &body body)
  "マッチ結果を順次処理"
  ...)

;; 使用例
(do-matches (email text)
  (format t "Found email: ~A~%" (capture 0)))
```

### `collect-matches`

マッチ結果を収集するマクロ。

```lisp
(defmacro collect-matches (pattern text &rest options)
  "マッチ結果を収集"
  ...)

;; 使用例
(collect-matches "\\w+" "hello world test")  ; ("hello" "world" "test")
```

## 検証マクロ

### `valid-p`

パターンの有効性を検証するマクロ。

```lisp
(defmacro valid-p (pattern text &rest options)
  "パターンがテキストにマッチするか"
  ...)

;; 使用例
(valid-p email "user@example.com")  ; T
(valid-p phone-number "invalid")    ; NIL
```

### `validate`

バリデーション付きマッチングマクロ。

```lisp
(defmacro validate (pattern text &key error-message error-type)
  "バリデーション付きマッチング"
  ...)

;; 使用例
(validate email user-input
  :error-message "Invalid email format"
  :error-type 'invalid-email-error)
```

## デバッグマクロ

### `debug-match`

デバッグ情報付きマッチングマクロ。

```lisp
(defmacro debug-match (pattern text &rest options)
  "デバッグ情報付きマッチング"
  ...)

;; 使用例
(debug-match "a+b*" "aaabbb")
;; マッチングステップの詳細表示
```

### `trace-match`

マッチング過程をトレースするマクロ。

```lisp
(defmacro trace-match (pattern text &rest options)
  "マッチング過程をトレース"
  ...)

;; 使用例
(trace-match complex-pattern input-text)
;; 実行ステップのトレース出力
```

## 最適化マクロ

### `compile-time-regex`

コンパイル時正規表現マクロ。

```lisp
(defmacro compile-time-regex (pattern &rest options)
  "コンパイル時に正規表現を最適化"
  ...)

;; 使用例
(compile-time-regex "\\d{3}-\\d{4}")
;; コンパイル時に最適化されたコードを生成
```

### `inline-match`

インライン展開マッチングマクロ。

```lisp
(defmacro inline-match (pattern text &rest options)
  "マッチング処理をインライン展開"
  ...)

;; 使用例
(inline-match "a+" input-text)
;; 高速化のためのインライン展開
```

## Unicode対応マクロ

### `unicode-regex`

Unicode対応正規表現マクロ。

```lisp
(defmacro unicode-regex (pattern &rest options)
  "Unicode対応正規表現"
  ...)

;; 使用例
(unicode-regex "\\p{L}+")  ; Unicode文字
(unicode-regex "\\p{Script=Hiragana}+" :normalization :nfc)
```

### `with-unicode-mode`

Unicode処理モード設定マクロ。

```lisp
(defmacro with-unicode-mode (options &body body)
  "Unicode処理モードで実行"
  ...)

;; 使用例
(with-unicode-mode (:normalization :nfc :case-folding t)
  (match unicode-pattern text))
```

## エラーハンドリングマクロ

### `with-regex-error-handling`

正規表現エラーハンドリングマクロ。

```lisp
(defmacro with-regex-error-handling (&body body)
  "正規表現エラーをハンドリング"
  ...)

;; 使用例
(with-regex-error-handling
  (match complex-pattern text)
  (handle-timeout-error (e) (use-fallback-pattern))
  (handle-syntax-error (e) (fix-pattern-and-retry)))
```

### `ignore-regex-errors`

正規表現エラーを無視するマクロ。

```lisp
(defmacro ignore-regex-errors (&body body)
  "正規表現エラーを無視して実行"
  ...)

;; 使用例
(ignore-regex-errors
  (match questionable-pattern text))
```

## テストマクロ

### `deftest-pattern`

パターンテスト定義マクロ。

```lisp
(defmacro deftest-pattern (name pattern test-cases &rest options)
  "パターンのテストケースを定義"
  ...)

;; 使用例
(deftest-pattern email-test email
  (("user@example.com" :should-match t)
   ("invalid-email" :should-match nil)
   ("test@domain.co.jp" :should-match t))
  :description "Email pattern test")
```

### `benchmark-pattern`

パターンベンチマークマクロ。

```lisp
(defmacro benchmark-pattern (pattern text &rest options)
  "パターンのベンチマーク実行"
  ...)

;; 使用例
(benchmark-pattern complex-pattern large-text
  :iterations 1000
  :report-format :detailed)
```

## 高度なマクロ

### `with-pattern-cache`

パターンキャッシュ管理マクロ。

```lisp
(defmacro with-pattern-cache (options &body body)
  "パターンキャッシュを管理"
  ...)

;; 使用例
(with-pattern-cache (:size 1000 :ttl 3600)
  (match pattern1 text1)
  (match pattern2 text2))
```

### `regex-lambda`

正規表現ラムダマクロ。

```lisp
(defmacro regex-lambda (pattern &body body)
  "正規表現マッチング付きラムダ"
  ...)

;; 使用例
(mapcar (regex-lambda email
          (when (matched-p)
            (capture-named "domain")))
        email-list)
```

## 設定マクロ

### `with-regex-options`

正規表現オプション設定マクロ。

```lisp
(defmacro with-regex-options (options &body body)
  "正規表現オプションを設定して実行"
  ...)

;; 使用例
(with-regex-options (:case-insensitive t :multiline t)
  (match pattern text))
```

### `configure-regex`

正規表現エンジン設定マクロ。

```lisp
(defmacro configure-regex (&key backend optimization cache)
  "正規表現エンジンを設定"
  ...)

;; 使用例
(configure-regex :backend :hybrid
                 :optimization 3
                 :cache :enabled)
```

## 使用例とベストプラクティス

### 基本的な使用パターン

```lisp
;; パターン定義
(defpattern email
  (regex "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"))

;; マッチング
(when-match (email input-text)
  (process-email (capture 0)))

;; 置換
(replace-all "\\s+" " " messy-text)
```

### 高度な使用パターン

```lisp
;; DSLを使った複雑なパターン
(defpattern url
  (regex-dsl
    (sequence
      (alternation (literal "http") (literal "https"))
      (literal "://")
      (one-or-more (char-class "a-zA-Z0-9.-"))
      (optional
        (sequence
          (literal ":")
          (repeat (char-class "0-9") 1 5)))
      (optional
        (sequence
          (literal "/")
          (zero-or-more (char-class "^\\s")))))))

;; エラーハンドリング付きマッチング
(with-regex-error-handling
  (match-case complex-input
    (url (process-url (capture 0)))
    (email (process-email (capture 0)))
    (t (handle-unknown-format complex-input))))
```

これらのマクロにより、CL-Regexは非常に表現力豊かで使いやすいAPIを提供します。