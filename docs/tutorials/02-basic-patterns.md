# Basic Pattern Matching

## 目的

このチュートリアルでは、CL-Regexのパターンマッチング機能を詳細に学習します。各種メタ文字、文字クラス、アンカーなどを習得します。

## メタ文字の詳細

### 基本メタ文字

```lisp
;; . (ドット) - 改行以外の任意の1文字
(match ".at" "cat")  ;; => #<MATCH "cat">
(match ".at" "bat")  ;; => #<MATCH "bat">
(match ".at" "at")   ;; => NIL

;; ^ (キャレット) - 行の先頭
(match "^hello" "hello world")     ;; => #<MATCH "hello">
(match "^hello" "say hello")       ;; => NIL

;; $ (ドル) - 行の末尾
(match "world$" "hello world")     ;; => #<MATCH "world">
(match "world$" "world peace")     ;; => NIL

;; \\ (バックスラッシュ) - エスケープ
(match "\\." ".")                  ;; => #<MATCH ".">
(match "\\$" "price: $100")        ;; => #<MATCH "$">
```

### 量指定子

```lisp
;; * - 0回以上（貪欲）
(match "a*" "")       ;; => #<MATCH "">
(match "a*" "aaa")    ;; => #<MATCH "aaa">

;; + - 1回以上（貪欲）
(match "a+" "")       ;; => NIL
(match "a+" "aaa")    ;; => #<MATCH "aaa">

;; ? - 0回または1回
(match "colou?r" "color")   ;; => #<MATCH "color">
(match "colou?r" "colour")  ;; => #<MATCH "colour">

;; {n} - ちょうどn回
(match "a{3}" "aaa")    ;; => #<MATCH "aaa">
(match "a{3}" "aa")     ;; => NIL

;; {n,} - n回以上
(match "a{2,}" "aa")    ;; => #<MATCH "aa">
(match "a{2,}" "aaaa")  ;; => #<MATCH "aaaa">

;; {n,m} - n回以上m回以下
(match "a{2,4}" "aaa")  ;; => #<MATCH "aaa">
```

### 非貪欲量指定子

```lisp
;; *? - 0回以上（非貪欲）
(match "<.*>" "<tag>content</tag>")
;; => #<MATCH "<tag>content</tag>">

(match "<.*?>" "<tag>content</tag>")
;; => #<MATCH "<tag>">

;; +? - 1回以上（非貪欲）
(match "a+?b" "aaab")
;; => #<MATCH "aaab"> （最短マッチでも全体が必要）

;; ?? - 0回または1回（非貪欲）
(match "a??b" "ab")
;; => #<MATCH "ab">
```

## 文字クラス

### 基本文字クラス

```lisp
;; [abc] - 指定文字のいずれか
(match "[aeiou]" "e")     ;; => #<MATCH "e">

;; [^abc] - 指定文字以外
(match "[^aeiou]+" "xyz") ;; => #<MATCH "xyz">

;; [a-z] - 文字範囲
(match "[a-z]+" "hello")  ;; => #<MATCH "hello">
(match "[A-Z]+" "HELLO")  ;; => #<MATCH "HELLO">
(match "[0-9]+" "12345")  ;; => #<MATCH "12345">

;; 複数範囲の組み合わせ
(match "[a-zA-Z0-9]+" "Hello123")
;; => #<MATCH "Hello123">
```

### 事前定義文字クラス

```lisp
;; \\d - 数字 [0-9]
(match "\\d+" "12345")    ;; => #<MATCH "12345">

;; \\D - 数字以外 [^0-9]
(match "\\D+" "hello")    ;; => #<MATCH "hello">

;; \\w - 単語文字 [a-zA-Z0-9_]
(match "\\w+" "hello_world_123")
;; => #<MATCH "hello_world_123">

;; \\W - 単語文字以外
(match "\\W+" " !@# ")
;; => #<MATCH " !@# ">

;; \\s - 空白文字
(match "\\s+" "   \t\n")
;; => #<MATCH "   \t\n">

;; \\S - 空白文字以外
(match "\\S+" "hello")
;; => #<MATCH "hello">
```

### POSIX文字クラス

```lisp
;; [:alnum:] - 英数字
(match "[[:alnum:]]+" "Hello123")
;; => #<MATCH "Hello123">

;; [:alpha:] - 英字
(match "[[:alpha:]]+" "Hello")
;; => #<MATCH "Hello">

;; [:digit:] - 数字
(match "[[:digit:]]+" "12345")
;; => #<MATCH "12345">

;; [:lower:] - 小文字
(match "[[:lower:]]+" "hello")
;; => #<MATCH "hello">

;; [:upper:] - 大文字
(match "[[:upper:]]+" "HELLO")
;; => #<MATCH "HELLO">

;; [:punct:] - 句読点
(match "[[:punct:]]+" "!@#$%")
;; => #<MATCH "!@#$%">

;; [:space:] - 空白文字
(match "[[:space:]]+" " \t\n")
;; => #<MATCH " \t\n">
```

## アンカーと境界

### 位置アンカー

```lisp
;; \\A - 文字列の先頭（複数行モードでも）
(match "\\Ahello" "hello\nworld")
;; => #<MATCH "hello">

;; \\Z - 文字列の末尾（最後の改行前）
(match "world\\Z" "hello\nworld\n")
;; => #<MATCH "world">

;; \\z - 文字列の絶対的末尾
(match "world\\z" "hello\nworld")
;; => #<MATCH "world">
```

### 単語境界

```lisp
;; \\b - 単語境界
(match "\\bhello\\b" "hello world")
;; => #<MATCH "hello">

(match "\\bhello\\b" "helloworld")
;; => NIL

;; \\B - 単語境界以外
(match "\\Bello\\B" "helloworld")
;; => #<MATCH "ello">
```

## グルーピングとキャプチャ

### 基本グループ

```lisp
;; () - キャプチャグループ
(when-match (m "(\\d+)-(\\d+)-(\\d+)" "2024-03-15")
  (list (group-ref m 0)  ;; 全体マッチ
        (group-ref m 1)  ;; 第1グループ
        (group-ref m 2)  ;; 第2グループ
        (group-ref m 3))) ;; 第3グループ
;; => ("2024-03-15" "2024" "03" "15")
```

### 名前付きグループ

```lisp
;; (?<name>...) - 名前付きグループ
(when-match (m "(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})" 
             "2024-03-15")
  (list :year (group-ref m 'year)
        :month (group-ref m 'month)
        :day (group-ref m 'day)))
;; => (:year "2024" :month "03" :day "15")
```

### 非キャプチャグループ

```lisp
;; (?:...) - 非キャプチャグループ
(when-match (m "(?:Mr|Ms|Mrs)\\.?\\s+(\\w+)" "Mr. Smith")
  (group-ref m 1))
;; => "Smith"
```

## 選択とOR

```lisp
;; | - OR演算子
(match "cat|dog" "cat")  ;; => #<MATCH "cat">
(match "cat|dog" "dog")  ;; => #<MATCH "dog">
(match "cat|dog" "bird") ;; => NIL

;; グループ内でのOR
(match "(mon|tues|wednes|thurs|fri|satur|sun)day" "monday")
;; => #<MATCH "monday">

;; 複数パターンの組み合わせ
(match "\\b(https?|ftp)://[^\\s]+" "https://example.com")
;; => #<MATCH "https://example.com">
```

## ルックアラウンド

### 先読みアサーション

```lisp
;; (?=...) - 肯定先読み
(match "\\d+(?=\\$)" "100$ and 200 yen")
;; => #<MATCH "100"> (ドル記号の前の数字のみ)

;; (?!...) - 否定先読み
(match "\\d+(?!\\$)" "100$ and 200 yen")
;; => #<MATCH "200"> (ドル記号が続かない数字)
```

### 後読みアサーション

```lisp
;; (?<=...) - 肯定後読み
(match "(?<=\\$)\\d+" "$100 and 200 yen")
;; => #<MATCH "100"> (ドル記号の後の数字のみ)

;; (?<!...) - 否定後読み
(match "(?<!\\$)\\d+" "$100 and 200 yen")
;; => #<MATCH "200"> (ドル記号が前にない数字)
```

## バックリファレンス

```lisp
;; \\1, \\2, ... - 番号バックリファレンス
(match "(\\w+)\\s+\\1" "hello hello")
;; => #<MATCH "hello hello">

(match "(\\w+)\\s+\\1" "hello world")
;; => NIL

;; HTMLタグのマッチング
(match "<(\\w+)>.*?</\\1>" "<div>content</div>")
;; => #<MATCH "<div>content</div>">

;; \\k<name> - 名前付きバックリファレンス
(match "(?<tag>\\w+).*?\\k<tag>" "div content div")
;; => #<MATCH "div content div">
```

## フラグとモード

### ケース非依存

```lisp
;; (?i) - 大文字小文字を無視
(match "(?i)hello" "HELLO")
;; => #<MATCH "HELLO">

;; インラインフラグ
(match "(?i:hello) world" "HELLO world")
;; => #<MATCH "HELLO world">

;; APIでの指定
(match "hello" "HELLO" :case-insensitive t)
;; => #<MATCH "HELLO">
```

### 複数行モード

```lisp
;; (?m) - 複数行モード
(match "(?m)^hello" "world\nhello")
;; => #<MATCH "hello">

;; APIでの指定
(match "^hello" "world\nhello" :multiline t)
;; => #<MATCH "hello">
```

### ドットオールモード

```lisp
;; (?s) - ドットが改行にもマッチ
(match "(?s).*" "line1\nline2")
;; => #<MATCH "line1\nline2">

;; APIでの指定
(match ".*" "line1\nline2" :dotall t)
;; => #<MATCH "line1\nline2">
```

## 実用例

### メールアドレスの検証

```lisp
(defparameter *email-pattern*
  "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")

(defun valid-email-p (email)
  (not (null (match *email-pattern* email))))

(valid-email-p "user@example.com")     ;; => T
(valid-email-p "invalid.email")        ;; => NIL
```

### URLの解析

```lisp
(defparameter *url-pattern*
  "(?<protocol>https?)://(?<domain>[^/]+)(?<path>/.*)?")

(defun parse-url (url)
  (when-match (m *url-pattern* url)
    (list :protocol (group-ref m 'protocol)
          :domain (group-ref m 'domain)
          :path (or (group-ref m 'path) "/"))))

(parse-url "https://example.com/path/to/page")
;; => (:protocol "https" :domain "example.com" :path "/path/to/page")
```

### 電話番号の正規化

```lisp
(defun normalize-phone (phone)
  (let ((digits (replace-all "[^0-9]" "" phone)))
    (when (= (length digits) 10)
      (match "(\\d{3})(\\d{3})(\\d{4})" digits)
      (format nil "(~A) ~A-~A" 
              (group-ref m 1)
              (group-ref m 2)
              (group-ref m 3)))))

(normalize-phone "123-456-7890")
;; => "(123) 456-7890"
```

## 練習問題

1. IPアドレス（IPv4）をマッチするパターンを作成してください
2. HTMLタグから属性を抽出するパターンを作成してください
3. 日付フォーマット（YYYY/MM/DDまたはYYYY-MM-DD）を認識するパターンを作成してください

## 次のステップ

- [高度な機能](./03-advanced-features.md) - 再帰パターン、条件分岐など
- [カスタム拡張](./04-custom-extensions.md) - 独自パターンの実装