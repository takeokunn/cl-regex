# S-Expression Prolog Integration in CL-Regex

## 概要

CL-Regexは、S式Prologを深く統合することで、従来の正規表現エンジンを超えた論理プログラミングベースのパターンマッチングを実現します。このアプローチにより、宣言的なパターン定義、強力なバックトラッキング最適化、論理推論ベースのマッチングが可能になります。

## S式Prologの基礎

### ファクトとルール

```lisp
;; ファクトの定義
(fact (char-class digit #\0 #\9))
(fact (char-class alpha #\a #\z))
(fact (char-class alpha #\A #\Z))

;; ルールの定義
(rule (alphanumeric ?c)
  (or (char-class digit ?c ?c)
      (char-class alpha ?c ?c)))

;; クエリ
(query (alphanumeric #\5))  ; => T
(query (alphanumeric #\a))  ; => T
(query (alphanumeric #\!))  ; => NIL
```

### DCG (Definite Clause Grammar) との統合

```lisp
;; DCGルールで正規表現を定義
(dcg-rule email-address
  (local-part)
  (atom '@)
  (domain-name)
  (atom '.)
  (tld))

(dcg-rule local-part
  (+ (or alphanumeric (atom '.) (atom '-) (atom '+))))

(dcg-rule domain-name
  (+ (or alphanumeric (atom '.) (atom '-))))

(dcg-rule tld
  (repeat 2 4 alpha))
```

## パターンマッチングの論理化

### 統一化ベースマッチング

```lisp
;; 統一化を使用したパターンマッチング
(defclass prolog-pattern ()
  ((clauses :initform '())
   (variables :initform (make-hash-table))))

(defmethod unify-pattern ((pattern prolog-pattern) text)
  (let ((bindings (make-hash-table)))
    (prolog-match (slot-value pattern 'clauses)
                  text
                  bindings)))

;; 例: 変数を含むパターン
(rule (match-date ?year ?month ?day ?text)
  (and (match-digits 4 ?year ?text ?rest1)
       (match-literal "-" ?rest1 ?rest2)
       (match-digits 2 ?month ?rest2 ?rest3)
       (match-literal "-" ?rest3 ?rest4)
       (match-digits 2 ?day ?rest4 ?rest5)))
```

### 制約伝播

```lisp
;; 制約を使用したパターン定義
(rule (valid-email ?email)
  (and (match email-pattern ?email)
       (constraint (not (contains ?email "..")))  ; 連続ドット禁止
       (constraint (not (starts-with ?email "."))) ; ドット開始禁止
       (constraint (< (length ?email) 255))))      ; 長さ制限

;; 制約解決エンジン
(defclass constraint-solver ()
  ((constraints :initform '())
   (domains :initform (make-hash-table))))

(defmethod solve-constraints ((solver constraint-solver) variables)
  (arc-consistency solver)
  (backtrack-search solver variables))
```

## バックトラッキング最適化

### Prologスタイルのバックトラッキング

```lisp
;; カット演算子によるバックトラッキング制御
(rule (atomic-match ?pattern ?text ?result)
  (once  ; Prologの!に相当
    (match ?pattern ?text ?result)))

;; グリーンカットによる最適化
(rule (possessive-match ?pattern ?text)
  (match ?pattern ?text ?matched)
  (!)  ; バックトラッキングを防ぐ
  ?matched)

;; メモ化 (tabling) による最適化
(tabled-rule (fibonacci ?n ?result)
  (or (and (= ?n 0) (= ?result 0))
      (and (= ?n 1) (= ?result 1))
      (and (> ?n 1)
           (- ?n 1 ?n1)
           (- ?n 2 ?n2)
           (fibonacci ?n1 ?r1)
           (fibonacci ?n2 ?r2)
           (+ ?r1 ?r2 ?result))))
```

### インテリジェントバックトラッキング

```lisp
;; ヒューリスティックを使用した探索
(defclass intelligent-backtracker ()
  ((choice-points :initform '())
   (heuristics :initform (make-hash-table))
   (learned-clauses :initform '())))

(defmethod backtrack-with-learning ((bt intelligent-backtracker) goal)
  (handler-case
      (prove goal)
    (backtrack-needed ()
      (let ((learned (analyze-failure bt)))
        (push learned (slot-value bt 'learned-clauses))
        (intelligent-backtrack bt)))))

;; 失敗からの学習
(defmethod analyze-failure ((bt intelligent-backtracker))
  (let ((conflict-set (compute-conflict-set bt)))
    (generate-nogood conflict-set)))
```

## 論理推論ベースパターン

### 推論ルール

```lisp
;; 推移的マッチング
(rule (matches-pattern ?p1 ?text)
  (and (similar-to ?p1 ?p2)
       (matches-pattern ?p2 ?text)))

;; パターンの同値関係
(rule (equivalent-patterns ?p1 ?p2)
  (and (subsumes ?p1 ?p2)
       (subsumes ?p2 ?p1)))

;; パターンの包含関係
(rule (subsumes ?general ?specific)
  (forall (?text)
    (implies (matches ?specific ?text)
             (matches ?general ?text))))
```

### ファジー論理との統合

```lisp
;; 曖昧マッチング
(rule (fuzzy-match ?pattern ?text ?confidence)
  (and (partial-match ?pattern ?text ?matched)
       (calculate-similarity ?matched ?pattern ?similarity)
       (>= ?similarity 0.8)
       (= ?confidence ?similarity)))

;; 確率的マッチング
(probabilistic-rule (likely-email ?text 0.95)
  (and (contains ?text "@")
       (contains ?text ".")))

(probabilistic-rule (likely-url ?text 0.90)
  (or (starts-with ?text "http://")
      (starts-with ?text "https://")))
```

## 高度な機能

### メタプログラミング

```lisp
;; パターンの自動生成
(meta-rule (generate-pattern ?examples ?pattern)
  (and (analyze-examples ?examples ?features)
       (synthesize-pattern ?features ?pattern)
       (validate-pattern ?pattern ?examples)))

;; パターンの最適化
(meta-rule (optimize-pattern ?original ?optimized)
  (and (analyze-complexity ?original ?complexity)
       (identify-bottlenecks ?original ?bottlenecks)
       (apply-optimizations ?bottlenecks ?original ?optimized)
       (equivalent-patterns ?original ?optimized)))
```

### 分散マッチング

```lisp
;; 並列Prologエンジン
(defclass parallel-prolog-engine ()
  ((workers :initform (make-array 8))
   (work-queue :initform (make-queue))
   (result-queue :initform (make-queue))))

(defmethod parallel-query ((engine parallel-prolog-engine) goals)
  (distribute-goals engine goals)
  (collect-results engine))

;; Or-並列化
(parallel-rule (match-any ?patterns ?text ?result)
  (or-parallel
    (dolist (pattern ?patterns)
      (match pattern ?text ?result))))

;; And-並列化
(parallel-rule (match-all ?patterns ?text)
  (and-parallel
    (dolist (pattern ?patterns)
      (match pattern ?text))))
```

## 実装例

### 言語認識パターン

```lisp
;; 自然言語パターン
(dcg-rule sentence
  (noun-phrase)
  (verb-phrase))

(dcg-rule noun-phrase
  (optional determiner)
  (* adjective)
  (noun))

(dcg-rule verb-phrase
  (verb)
  (optional noun-phrase))

;; レキシカルルール
(rule (determiner ?word)
  (member ?word '("the" "a" "an")))

(rule (noun ?word)
  (or (dictionary-lookup ?word :noun)
      (ends-with ?word "tion")
      (ends-with ?word "ness")))
```

### セキュリティパターン

```lisp
;; SQLインジェクション検出
(rule (sql-injection-pattern ?input)
  (or (contains-any ?input '("'" "--" "/*" "*/" "xp_" "sp_"))
      (matches-pattern ?input "(union|select|insert|update|delete|from|where)")
      (contains-encoded ?input)))

(rule (contains-encoded ?input)
  (or (contains ?input "%27")  ; URL encoded '
      (contains ?input "%22")  ; URL encoded "
      (contains ?input "%3C")  ; URL encoded <
      (contains ?input "%3E"))) ; URL encoded >

;; XSS検出
(rule (xss-pattern ?input)
  (or (contains-script-tag ?input)
      (contains-javascript-protocol ?input)
      (contains-event-handler ?input)))
```

### データ抽出パターン

```lisp
;; 構造化データの抽出
(rule (extract-json-value ?json ?path ?value)
  (and (parse-json ?json ?parsed)
       (json-path ?parsed ?path ?value)))

(rule (extract-xml-content ?xml ?xpath ?content)
  (and (parse-xml ?xml ?dom)
       (xpath-query ?dom ?xpath ?nodes)
       (extract-text ?nodes ?content)))

;; テーブルデータの抽出
(rule (extract-table-data ?text ?headers ?rows)
  (and (find-table-start ?text ?start)
       (extract-headers ?text ?start ?headers ?body-start)
       (extract-rows ?text ?body-start ?headers ?rows)))
```

## パフォーマンス最適化

### インデックス付きルール

```lisp
;; インデックスを使用した高速化
(indexed-rule (pattern-lookup ?name ?pattern)
  :index ?name
  (member (?name . ?pattern) *pattern-database*))

;; ハッシュテーブルインデックス
(defclass hash-indexed-kb ()
  ((facts :initform (make-hash-table :test 'equal))
   (rules :initform (make-hash-table :test 'eq))))
```

### コンパイル時最適化

```lisp
;; WAM (Warren Abstract Machine) スタイルコンパイル
(defmethod compile-to-wam ((rule prolog-rule))
  (let ((instructions '()))
    (compile-head (rule-head rule) instructions)
    (compile-body (rule-body rule) instructions)
    (optimize-instructions instructions)))

;; インライン展開
(defmacro inline-rule (name &body body)
  `(defmethod ,name ((text string) (pos integer))
     (declare (optimize (speed 3) (safety 0)))
     ,@body))
```

## デバッグとプロファイリング

### トレース機能

```lisp
;; Prologスタイルトレース
(trace-rule match-email)
;; CALL: (match-email "user@example.com")
;; CALL: (local-part "user" ...)
;; EXIT: (local-part "user" ...)
;; CALL: (domain-name "example" ...)
;; EXIT: (domain-name "example" ...)
;; EXIT: (match-email "user@example.com")

;; プロファイリング
(profile-query
  (match complex-pattern large-text))
;; => Choice points: 1523
;;    Unifications: 8921
;;    Time: 0.023s
```

## ベストプラクティス

1. **宣言的記述**: パターンを「何を」ではなく「何であるか」で記述
2. **カットの適切な使用**: 不要なバックトラッキングを防ぐ
3. **インデックスの活用**: 大量のファクトにはインデックスを使用
4. **メモ化の活用**: 再帰的ルールにはtablingを使用
5. **テスト駆動開発**: ルールごとにテストケースを作成