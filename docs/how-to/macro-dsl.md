# Macro DSL Creation Guide

## 目的

CL-Regexの強力なマクロDSLを使用して、読みやすく保守しやすい正規表現を構築する方法を学びます。

## 基本マクロ構文

### regexマクロ

```lisp
;; 伝統的な正規表現
"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"

;; regexマクロを使用
(regex
  (group 'user
    (+ (or alphanumeric "._%+-")))
  "@"
  (group 'domain
    (+ (or alphanumeric ".-")))
  "."
  (group 'tld
    (repeat 2 nil alpha)))  ; 2文字以上の英字
```

### defpatternマクロ

```lisp
;; パターンの定義
(defpattern email-address
  (regex
    (group 'local-part
      (+ (or word-char "." "-" "+")))
    "@"
    (group 'domain-name
      (+ (or word-char "." "-")))
    "."
    (group 'top-level-domain
      (repeat 2 4 alpha))))

;; 使用
(match email-address "user@example.com")
```

## 文字クラスマクロ

### 基本文字クラス

```lisp
;; 事前定義クラス
(regex
  digit           ; [0-9]
  alpha           ; [a-zA-Z]
  alphanumeric    ; [a-zA-Z0-9]
  word-char       ; [a-zA-Z0-9_]
  whitespace      ; [\s]
  any             ; .
  newline         ; \n
  tab)            ; \t
```

### カスタム文字クラス

```lisp
;; char-classマクロ
(defpattern hex-digit
  (char-class "0-9" "a-f" "A-F"))

;; not-charマクロ
(defpattern non-whitespace
  (not-char whitespace))

;; 複合文字クラス
(defpattern identifier-char
  (or alphanumeric "_" "-"))
```

## 量指定子マクロ

### 基本量指定子

```lisp
(regex
  ;; * - 0回以上
  (* alpha)
  
  ;; + - 1回以上
  (+ digit)
  
  ;; ? - 0または1回
  (optional "prefix-")
  
  ;; {n,m} - n回以上m回以下
  (repeat 3 5 digit)
  
  ;; {n} - ちょうどn回
  (exactly 4 digit))
```

### 非貪欲量指定子

```lisp
(regex
  ;; *? - 非貪欲ゼロ以上
  (*? any)
  
  ;; +? - 非貪欲一以上
  (+? any)
  
  ;; ?? - 非貪欲オプショナル
  (optional? "prefix"))
```

### 所有量指定子

```lisp
(regex
  ;; ++ - 所有的一以上
  (possessive (+ alpha))
  
  ;; *+ - 所有的ゼロ以上
  (possessive (* digit)))
```

## グルーピングマクロ

### 名前付きグループ

```lisp
(defpattern date-pattern
  (regex
    (group 'year (exactly 4 digit))
    "-"
    (group 'month (exactly 2 digit))
    "-"
    (group 'day (exactly 2 digit))))

;; グループ参照
(when-match (m date-pattern "2024-03-15")
  (list :year (group-ref m 'year)
        :month (group-ref m 'month)
        :day (group-ref m 'day)))
```

### 非キャプチャグループ

```lisp
(regex
  ;; 非キャプチャグループ
  (non-capturing
    (or "Mr" "Ms" "Mrs" "Dr"))
  ".?"
  whitespace
  (group 'name (+ alpha)))
```

### アトミックグループ

```lisp
(regex
  ;; バックトラッキングを防ぐ
  (atomic (+ digit))
  ".")
```

## アサーションマクロ

### ルックアラウンド

```lisp
(regex
  ;; 先読み
  (look-ahead "=")     ; (?=...)
  (not-ahead "!=")     ; (?!...)
  
  ;; 後読み
  (look-behind "$")    ; (?<=...)
  (not-behind "\\$"))  ; (?<!...)

;; 実用例：ドル記号付き数値
(defpattern dollar-amount
  (regex
    "$"
    (group 'amount 
      (+ digit)
      (optional
        "."
        (exactly 2 digit)))))
```

### 単語境界

```lisp
(regex
  (word-boundary)       ; \b
  (group 'word (+ word-char))
  (word-boundary))

;; 非単語境界
(regex
  (not-word-boundary)   ; \B
  (+ alpha))
```

## 高度なマクロ機能

### 条件分岐

```lisp
(defpattern conditional-match
  (regex
    (group 'prefix (optional "pre-"))
    (if-group 'prefix
      ;; prefixがマッチした場合
      (group 'content (+ upper))
      ;; マッチしなかった場合
      (group 'content (+ lower)))))
```

### 再帰パターン

```lisp
;; ネストされた括弧のマッチ
(defpattern nested-parens
  (regex
    "("
    (recursive
      (* (or 
         (not-char "(" ")")
         (recurse))))
    ")"))

;; 使用例
(match nested-parens "(a(b(c)d)e)")  ; マッチ
```

### バックリファレンス

```lisp
(defpattern repeated-word
  (regex
    (word-boundary)
    (group 'word (+ word-char))
    (+ whitespace)
    (backref 'word)       ; \1 または \k<word>
    (word-boundary)))

;; 数値バックリファレンス
(regex
  (group (+ alpha))
  "-"
  (backref 1))           ; \1
```

## カスタムDSLの作成

### ドメイン固有マクロ

```lisp
;; URLパーサー用DSL
(defmacro url-parts (&body parts)
  `(regex
     ,@(loop for part in parts
             collect (case part
                      (:protocol '(group 'protocol 
                                   (or "http" "https" "ftp")))
                      (:host '(group 'host
                               (+ (or alphanumeric "." "-"))))
                      (:port '(optional ":" 
                               (group 'port (+ digit))))
                      (:path '(optional "/"
                               (group 'path (* (not-char #\Space)))))
                      (otherwise part)))))

;; 使用例
(defpattern full-url
  (url-parts
    :protocol "://" :host :port :path))
```

### フルエントDSL

```lisp
;; 自然言語風DSL
(defmacro match-where (&body clauses)
  (let ((pattern-parts '()))
    (loop for (keyword . args) in clauses
          do (case keyword
               (:starts-with
                (push `(sequence-start ,@args) pattern-parts))
               (:ends-with
                (push `(sequence-end ,@args) pattern-parts))
               (:contains
                (push `(somewhere ,@args) pattern-parts))
               (:followed-by
                (push `(look-ahead ,@args) pattern-parts))))
    `(regex ,@(nreverse pattern-parts))))

;; 使用例
(defpattern secure-url
  (match-where
    (:starts-with "https://")
    (:contains (+ (not-char #\Space)))
    (:ends-with (optional "/"))))
```

## コンパイル時最適化

### マクロ展開最適化

```lisp
(defmacro optimized-regex (&body forms)
  `(compile-regex
    (regex ,@forms)
    :optimize :speed
    :jit t))

;; コンパイル時に最適化
(defpattern fast-email
  (optimized-regex
    (+ (or alphanumeric "._%+-"))
    "@"
    (+ (or alphanumeric ".-"))
    "."
    (repeat 2 4 alpha)))
```

### マクロの組み合わせ

```lisp
;; 複数のパターンを組み合わせ
(defmacro combine-patterns (&rest patterns)
  `(regex
     (or ,@(loop for p in patterns
                 collect `(inline-pattern ,p)))))

(defpattern contact-info
  (combine-patterns
    email-address
    phone-number
    postal-address))
```

## デバッグ用マクロ

### トレースマクロ

```lisp
(defmacro traced-regex (&body forms)
  `(let ((pattern (regex ,@forms)))
     (format t "Pattern expanded to: ~S~%" pattern)
     pattern))

;; デバッグ情報付き
(defmacro debug-pattern (name &body forms)
  `(progn
     (format t "Defining pattern: ~A~%" ',name)
     (defpattern ,name
       (regex
         ,@(loop for form in forms
                 collect `(progn
                           (format t "  Processing: ~S~%" ',form)
                           ,form))))))
```

## 実用例

### ログパーサーDSL

```lisp
(defmacro log-pattern (&key timestamp level message)
  `(regex
     ,@(when timestamp
         '((group 'timestamp
             (+ (or digit "-" " " ":")))))
     ,@(when level
         '((optional whitespace)
           "["
           (group 'level
             (or "DEBUG" "INFO" "WARN" "ERROR" "FATAL"))
           "]"))
     ,@(when message
         '((optional whitespace)
           (group 'message (* any))))))

;; 使用
(defpattern apache-log
  (log-pattern
    :timestamp t
    :level t
    :message t))
```

### バリデーションDSL

```lisp
(defmacro validation-pattern (type &rest constraints)
  (case type
    (:email
     '(regex
        (+ (or alphanumeric "._%+-"))
        "@"
        (+ (or alphanumeric ".-"))
        "."
        (repeat 2 4 alpha)))
    (:phone
     '(regex
        (optional (or "+" "00"))
        (repeat 1 3 digit)  ; country code
        (optional (or "-" " "))
        (repeat 3 12 digit)))
    (:credit-card
     '(regex
        (group 'card-number
          (repeat 13 19 digit))))))

;; 使用
(defpattern validate-email
  (validation-pattern :email))
```

### HTMLパーサーDSL

```lisp
(defmacro html-tag (tag-name &key attributes content self-closing)
  `(regex
     "<"
     ,(if (symbolp tag-name)
          `(group 'tag ,(string-downcase (symbol-name tag-name)))
          tag-name)
     ,@(when attributes
         '((* (sequence
                whitespace
                (group 'attr-name (+ (or alpha "-")))
                "="
                "\""
                (group 'attr-value (* (not-char #\"))) 
                "\""))))))
     ,(if self-closing
          '(optional whitespace "/>")
          `(sequence
             ">"
             ,@(when content
                 `((group 'content ,content)))
             "</" 
             ,(if (symbolp tag-name)
                  (string-downcase (symbol-name tag-name))
                  '(backref 'tag))
             ">"))))))

;; 使用
(defpattern div-tag
  (html-tag div
    :attributes t
    :content (* (not-char #\<))))
```

## ベストプラクティス

### マクロDSL設計指針

```mermaid
graph TD
    Design[マクロDSL設計] --> Readability[可読性]
    Design --> Performance[性能]
    Design --> Maintainability[保守性]

    Readability --> Naming[意味のある名前]
    Readability --> Structure[階層構造]
    Readability --> Comments[適切なコメント]

    Performance --> CompileTime[コンパイル時最適化]
    Performance --> RuntimeOpt[実行時最適化]
    Performance --> Caching[キャッシング]

    Maintainability --> Modularity[モジュール化]
    Maintainability --> Testing[テスト]
    Maintainability --> Documentation[ドキュメント]

    Naming --> Examples1[group 'email-user<br/>group 'domain-name]
    Structure --> Examples2[defpattern email<br/>  (local-part)<br/>  @<br/>  (domain-part)]
    Comments --> Examples3[;; IPv4の0-255範囲<br/>(?:25[0-5]|...)]

    CompileTime --> Examples4[optimized-regex<br/>コンパイル時展開]
    RuntimeOpt --> Examples5[pattern-cache<br/>JIT最適化]
    Caching --> Examples6[*compiled-patterns*<br/>ハッシュテーブル]

    Modularity --> Examples7[基本パターン<br/>複合パターン<br/>ドメイン固有DSL]
    Testing --> Examples8[property-based<br/>unit testing<br/>regression testing]
    Documentation --> Examples9[API docs<br/>examples<br/>tutorials]

```

### 実践ガイドライン

1. **読みやすさ優先**: 複雑なパターンを小さな部分に分割
2. **名前付きグループ**: 意味のある名前を使用
3. **コメント**: 複雑なロジックにはコメントを追加
4. **テスト**: パターンごとにテストを作成
5. **再利用**: 共通パターンを別マクロに抽出

### マクロ設計のアンチパターン

```lisp
;; ❌ 悪い例：可読性が低い
(defpattern bad-email
  (regex (+ (or alphanumeric "._%+-")) "@" (+ (or alphanumeric ".-")) "." (repeat 2 4 alpha)))

;; ✅ 良い例：段階的構築
(defpattern good-email
  (regex
    (group 'local-part
      (+ (local-part-chars)))
    "@"
    (group 'domain
      (+ (domain-chars)))
    "."
    (group 'tld
      (repeat 2 4 alpha))))

(defpattern local-part-chars
  (or alphanumeric "._%+-"))

(defpattern domain-chars
  (or alphanumeric ".-"))
```