# 可読性の高いマクロ設計ガイド

## 目的

Common Lispのマクロを駆使して、高品質の正規表現エンジンにおける可読性と拡張性を実現するための包括的なガイド。

## マクロ設計の哲学

### なぜマクロが重要か

1. **コンパイル時最適化**: パターンを事前解析し、最適なマッチャーコードを生成
2. **DSL構築**: 正規表現を自然に表現できるLisp風構文の提供
3. **抽象化の層**: 複雑な実装を隠蔽し、直感的なAPIを提供
4. **拡張可能性**: ユーザー定義のパターン演算子を容易に追加

## 基本原則

### 1. 意図を明確にする命名

```lisp
;; 悪い例：曖昧な名前
(defmacro m (p t)
  `(match ,p ,t))

;; 良い例：意図が明確
(defmacro match-pattern (pattern text)
  `(match ,pattern ,text))

;; さらに良い例：DSL風の自然な構文
(defmacro when-matches (pattern in text &body actions)
  (declare (ignore in))  ; 構文糖として'in'を使用
  `(when (match ,pattern ,text)
     ,@actions))

;; 最良の例：文脈を考慮した命名
(defmacro with-pattern-match ((var pattern text) &body body)
  "Bind VAR to the match result of PATTERN against TEXT, executing BODY if successful."
  (let ((gmatch (gensym "MATCH-")))
    `(let ((,gmatch (match ,pattern ,text)))
       (when ,gmatch
         (let ((,var ,gmatch))
           ,@body)))))
```

### 2. キーワード引数による柔軟性

```lisp
;; 悪い例：位置引数の羅列
(defmacro define-pattern (name pattern flags cache-size jit)
  ...)

;; 良い例：キーワード引数で意図を明確化
(defmacro define-pattern (name pattern
                          &key
                          (case-sensitive t)
                          (multiline nil)
                          (optimize :balanced)
                          cache-size
                          (jit :auto))
  `(register-pattern ',name
     (compile-regex ,pattern
       :case-sensitive ,case-sensitive
       :multiline ,multiline
       :optimize ,optimize
       :cache-size ,cache-size
       :jit ,jit)))

;; 最良の例：構造化されたオプション
(defmacro define-pattern (name pattern &rest options &key &allow-other-keys)
  (let ((compile-opts (extract-compile-options options))
        (runtime-opts (extract-runtime-options options))
        (metadata (extract-metadata options)))
    `(progn
       (defparameter ,name
         (compile-pattern ,pattern ,@compile-opts))
       (register-pattern-metadata ',name ',metadata)
       (configure-runtime ',name ,@runtime-opts))))
```

### 3. デストラクチャリングによる構造化

```lisp
;; パターンマッチの結果を構造化して取得
(defmacro destructuring-match (((&rest groups) pattern text) &body body)
  "Match PATTERN against TEXT, binding captured groups to GROUPS."
  (let ((gmatch (gensym "MATCH-")))
    `(let ((,gmatch (match-groups ,pattern ,text)))
       (when ,gmatch
         (destructuring-bind ,groups
             (match-group-values ,gmatch)
           ,@body)))))

;; 使用例
(destructuring-match ((username domain tld)
                      email-pattern
                      "user@example.com")
  (format t "User: ~A, Domain: ~A.~A" username domain tld))

;; ネストした構造のサポート
(defmacro with-nested-groups (((&rest outer) (&rest inner))
                              pattern text &body body)
  (let ((gmatch (gensym "MATCH-")))
    `(let ((,gmatch (match-nested ,pattern ,text)))
       (when ,gmatch
         (let ((,@outer (outer-groups ,gmatch))
               (,@inner (inner-groups ,gmatch)))
           ,@body)))))
```

## 衛生的マクロの作成

### 変数捕獲の完全な防止

```lisp
;; アナフォリックマクロ（意図的な変数捕獲）
(defmacro aif (test then &optional else)
  "Anaphoric IF - binds IT to the test result"
  `(let ((it ,test))
     (if it ,then ,else)))

;; 衛生的バージョン
(defmacro safe-if-match (pattern text then-form &optional else-form)
  (let ((gpattern (gensym "PATTERN-"))
        (gtext (gensym "TEXT-"))
        (gmatch (gensym "MATCH-")))
    `(let* ((,gpattern ,pattern)
            (,gtext ,text)
            (,gmatch (match ,gpattern ,gtext)))
       (if ,gmatch
           ,then-form
           ,else-form))))

;; ONCE-ONLYパターンの実装
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym (string n)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names
                       collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms
                         collect `(,n ,g)))
             ,@body)))))

;; 使用例
(defmacro safe-match-all (pattern text)
  (once-only (pattern text)
    `(when (and ,pattern ,text)
       (match-all ,pattern ,text))))
```

## 高度なDSL構築

### SQL風パターンマッチング

```lisp
(defmacro select-where (&key fields from where group-by having order-by limit)
  "SQL-like pattern matching interface"
  (let ((gmatches (gensym "MATCHES-"))
        (gresults (gensym "RESULTS-")))
    `(let* ((,gmatches (match-all ,from ,where))
            (,gresults
             ,(when fields
                `(mapcar (lambda (m)
                           (extract-fields m ',fields))
                         ,gmatches))))
       ,@(when group-by
           `((setf ,gresults (group-matches ,gresults ,group-by))))
       ,@(when having
           `((setf ,gresults (filter-matches ,gresults ,having))))
       ,@(when order-by
           `((setf ,gresults (sort-matches ,gresults ,order-by))))
       ,@(when limit
           `((setf ,gresults (limit-matches ,gresults ,limit))))
       ,gresults)))

;; 使用例
(select-where
  :fields (date time level message)
  :from log-pattern
  :where log-text
  :having (lambda (m) (equal (field m 'level) "ERROR"))
  :order-by 'time
  :limit 10)
```

### フルエントインターフェース

```lisp
(defmacro pattern-chain (initial &body operations)
  "Chainable pattern building interface"
  (let ((pattern-var (gensym "PATTERN-")))
    `(let ((,pattern-var ,initial))
       ,@(loop for op in operations
               collect (build-operation pattern-var op))
       ,pattern-var)))

(defun build-operation (var op)
  (destructuring-bind (method &rest args) op
    (ecase method
      (:then `(setf ,var (concat-patterns ,var ,@args)))
      (:or `(setf ,var (alternate-patterns ,var ,@args)))
      (:repeat `(setf ,var (repeat-pattern ,var ,@args)))
      (:capture `(setf ,var (capture-pattern ,var ,@args)))
      (:optional `(setf ,var (optional-pattern ,var ,@args))))))

;; 使用例
(pattern-chain (literal "http")
  (:optional (literal "s"))
  (:then (literal "://"))
  (:capture (one-or-more (not-char #\/)))
  (:optional (literal "/")
             (:capture (zero-or-more any-char))))
```

### BDD風テスト記述

```lisp
(defmacro describe-pattern (name &body specs)
  "BDD-style pattern specification"
  `(progn
     (defparameter ,name nil)
     ,@(loop for spec in specs
             collect (process-spec name spec))))

(defun process-spec (pattern-name spec)
  (destructuring-bind (keyword &rest args) spec
    (ecase keyword
      (:pattern
       `(setf ,pattern-name (compile-pattern ,@args)))
      (:should-match
       `(assert (every (lambda (text) (match ,pattern-name text))
                       ',args)))
      (:should-not-match
       `(assert (notany (lambda (text) (match ,pattern-name text))
                        ',args)))
      (:should-extract
       (destructuring-bind (text &rest expected) args
         `(assert (equal (match-groups ,pattern-name ,text)
                        ',expected)))))))

;; 使用例
(describe-pattern email-validator
  (:pattern "[\\w._%+-]+@[\\w.-]+\\.[A-Za-z]{2,}")
  (:should-match
   "user@example.com"
   "admin+tag@company.org"
   "first.last@subdomain.example.co.uk")
  (:should-not-match
   "invalid@"
   "@example.com"
   "no-at-sign.com")
  (:should-extract "user@example.com"
                   :user "user"
                   :domain "example.com"))
```

## コンパイル時最適化

### パターンの事前コンパイル

```lisp
(defmacro define-optimized-pattern (name pattern &rest options)
  "Define a pattern with compile-time optimization"
  (let ((optimized (optimize-pattern-at-compile-time pattern options)))
    `(progn
       ;; コンパイル時に生成された最適化コード
       (defparameter ,name ',optimized)

       ;; 特殊化されたマッチャー関数
       (defun ,(symbolicate name '-match) (text)
         ,(generate-specialized-matcher optimized))

       ;; インライン展開ヒント
       (declaim (inline ,(symbolicate name '-match)))

       ;; メタデータの保存
       (setf (get ',name 'pattern-metadata)
             ',(extract-metadata pattern options)))))

(defun optimize-pattern-at-compile-time (pattern options)
  ;; コンパイル時にパターンを解析・最適化
  (let ((ast (parse-pattern pattern)))
    (-> ast
        (apply-constant-folding)
        (eliminate-redundancy)
        (optimize-character-classes)
        (generate-minimal-dfa)
        (apply-boyer-moore-preprocessing))))
```

### 条件付きコード生成

```lisp
(defmacro adaptive-matcher (pattern)
  "Generate different code based on pattern complexity"
  (let ((complexity (analyze-pattern-complexity pattern)))
    (cond
      ;; シンプルなリテラルマッチ
      ((eq complexity :trivial)
       `(lambda (text) (search ,pattern text)))

      ;; 中程度の複雑さ：DFAを使用
      ((eq complexity :moderate)
       `(lambda (text)
          (execute-dfa ,(compile-to-dfa pattern) text)))

      ;; 複雑なパターン：NFAとバックトラッキング
      ((eq complexity :complex)
       `(lambda (text)
          (execute-nfa-with-backtracking
           ,(compile-to-nfa pattern) text)))

      ;; 超複雑：VMベースのエンジン
      (t
       `(lambda (text)
          (execute-vm ,(compile-to-bytecode pattern) text))))))
```

## エラーハンドリングとデバッグ

### 詳細なエラーメッセージ

```lisp
(defmacro with-pattern-error-handling ((&key pattern text) &body body)
  "Comprehensive error handling for pattern matching"
  `(handler-case
       (progn
         (validate-pattern ,pattern)
         (validate-input ,text)
         ,@body)
     (invalid-pattern-error (e)
       (error "Invalid pattern ~S: ~A~%Hint: ~A"
              ,pattern
              (error-message e)
              (suggest-correction e)))
     (match-timeout-error (e)
       (error "Pattern matching timed out after ~A seconds.~%Pattern: ~S~%Text length: ~D~%Consider optimizing the pattern or increasing timeout."
              (timeout-duration e)
              ,pattern
              (length ,text)))
     (catastrophic-backtracking-error (e)
       (error "Catastrophic backtracking detected!~%Pattern: ~S~%Problematic segment: ~S~%Suggestion: ~A"
              ,pattern
              (problematic-segment e)
              (suggest-alternative e)))))
```

### デバッグ支援マクロ

```lisp
(defmacro trace-pattern-matching (pattern text &key
                                          (show-states t)
                                          (show-backtracking t)
                                          (show-captures t))
  "Detailed tracing of pattern matching process"
  (let ((gresult (gensym "RESULT-")))
    `(let ((*trace-states* ,show-states)
           (*trace-backtracking* ,show-backtracking)
           (*trace-captures* ,show-captures))
       (format t "~&=== Pattern Matching Trace ===~%")
       (format t "Pattern: ~S~%" ,pattern)
       (format t "Text: ~S~%" ,text)
       (format t "~&--- Execution ---~%")
       (let ((,gresult (with-tracing
                         (match ,pattern ,text))))
         (format t "~&--- Result ---~%")
         (format t "Match: ~A~%" (if ,gresult "SUCCESS" "FAILURE"))
         (when ,gresult
           (format t "Matched text: ~S~%" (match-string ,gresult))
           (when ,show-captures
             (format t "Captures: ~S~%" (match-groups ,gresult))))
         ,gresult))))

;; 使用例
(trace-pattern-matching
  "(\\w+)@(\\w+)\\.(\\w+)"
  "user@example.com"
  :show-states t
  :show-backtracking t)
```

## パフォーマンス考慮のマクロ

### インライン展開と特殊化

```lisp
(defmacro define-specialized-matcher (name pattern &key (types '(string)))
  "Generate type-specialized matchers for performance"
  `(progn
     ,@(loop for type in types
             collect
             `(defun ,(symbolicate name '- type '-match) (text)
                (declare (type ,type text)
                         (optimize (speed 3) (safety 0)))
                ,(generate-optimized-code-for-type pattern type)))

     ;; ディスパッチ関数
     (defun ,name (text)
       (etypecase text
         ,@(loop for type in types
                 collect `(,type
                           (,(symbolicate name '- type '-match) text)))))))

;; 型特殊化の例
(define-specialized-matcher fast-email-matcher
  email-pattern
  :types (simple-string string (vector character)))
```

### メモ化とキャッシング

```lisp
(defmacro with-pattern-cache ((&key (size 1000) (strategy :lru)) &body body)
  "Cache pattern matching results"
  (let ((cache-var (gensym "CACHE-")))
    `(let ((,cache-var (make-pattern-cache :size ,size
                                           :strategy ,strategy)))
       (macrolet ((cached-match (pattern text)
                    `(or (cache-lookup ,',cache-var
                                      (cons ,pattern ,text))
                         (cache-store ,',cache-var
                                     (cons ,pattern ,text)
                                     (match ,pattern ,text)))))
         ,@body))))
```

## ベストプラクティス・テンプレート

### 完全なマクロテンプレート

```lisp
(defmacro production-ready-macro (required &rest args
                                  &key
                                  (option1 default1)
                                  (option2 default2)
                                  debug
                                  &body body
                                  &allow-other-keys)
  "Production-ready macro template with all best practices.

   Arguments:
     REQUIRED -- Required argument description
     OPTION1  -- Optional argument 1 (default: DEFAULT1)
     OPTION2  -- Optional argument 2 (default: DEFAULT2)
     DEBUG    -- Enable debug output
     BODY     -- Forms to execute

   Returns:
     The result of evaluating BODY

   Examples:
     (production-ready-macro foo :option1 bar
       (do-something))

   See Also:
     RELATED-MACRO, ANOTHER-MACRO"

  ;; 引数検証
  (check-type required symbol "a symbol")
  (validate-options args)

  ;; gensymによる衛生的変数
  (let ((greq (gensym "REQUIRED-"))
        (gopt1 (gensym "OPTION1-"))
        (gopt2 (gensym "OPTION2-")))

    ;; ONCE-ONLYパターン
    (once-only (required option1 option2)
      `(let ((,greq ,required)
             (,gopt1 ,option1)
             (,gopt2 ,option2))

         ;; デバッグ出力（条件付き）
         ,@(when debug
             `((format *trace-output*
                      "~&Macro expanding with: ~S, ~S, ~S~%"
                      ,greq ,gopt1 ,gopt2)))

         ;; エラーハンドリング
         (handler-case
             (progn ,@body)
           (error (e)
             (error "Error in macro execution: ~A" e)))))))
```

## チェックリスト

### マクロ設計の品質チェック

- [ ] **命名規約**
  - [ ] マクロ名は動作を明確に表現している
  - [ ] パラメータ名は役割を説明している
  - [ ] 内部変数はgensymを使用している

- [ ] **構造**
  - [ ] キーワード引数を適切に活用している
  - [ ] デストラクチャリングで可読性を向上させている
  - [ ] 適切な抽象化レベルを保っている

- [ ] **安全性**
  - [ ] 変数捕獲を防いでいる
  - [ ] 多重評価を避けている
  - [ ] 型チェックを行っている

- [ ] **ドキュメント**
  - [ ] 包括的なdocstringがある
  - [ ] 使用例を提供している
  - [ ] エラーメッセージが分かりやすい

- [ ] **パフォーマンス**
  - [ ] コンパイル時最適化を活用している
  - [ ] 不要な実行時オーバーヘッドがない
  - [ ] 適切な宣言（declare）を使用している

- [ ] **保守性**
  - [ ] マクロ展開結果が理解しやすい
  - [ ] デバッグが容易
  - [ ] テストが書きやすい

## まとめ

可読性の高いマクロ設計は、CL-Regexを高品質の正規表現エンジンにする重要な要素です。マクロを適切に活用することで、高性能と使いやすさを両立させることができます。