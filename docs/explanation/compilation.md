# コンパイルパイプライン

CL-Regexのコンパイルパイプラインの設計と実装について、字句解析から最適化まで詳しく解説します。

## コンパイルパイプライン概要

CL-Regexのコンパイルは以下の段階で構成されます：

```
正規表現文字列
    ↓
字句解析 (Lexical Analysis)
    ↓
構文解析 (Syntax Analysis)
    ↓
意味解析 (Semantic Analysis)
    ↓
AST最適化 (AST Optimization)
    ↓
コード生成 (Code Generation)
    ↓
実行時最適化 (Runtime Optimization)
    ↓
実行可能オートマトン
```

## 字句解析（Lexical Analysis）

### トークン化

正規表現文字列を意味のある要素（トークン）に分割します。

```lisp
;; トークンの定義
(defclass regex-token ()
  ((type :initarg :type :accessor token-type)
   (value :initarg :value :accessor token-value)
   (position :initarg :position :accessor token-position)
   (length :initarg :length :accessor token-length)))

;; トークンタイプ
(deftype token-type ()
  '(member :literal :dot :caret :dollar :question :asterisk :plus
           :left-paren :right-paren :left-bracket :right-bracket
           :left-brace :right-brace :pipe :backslash :escape
           :quantifier :unicode-property :posix-class))

;; 字句解析器
(defclass regex-lexer ()
  ((input :initarg :input :accessor lexer-input)
   (position :initform 0 :accessor lexer-position)
   (tokens :initform '() :accessor lexer-tokens)))

(defmethod next-token ((lexer regex-lexer))
  "次のトークンを取得"
  (with-slots (input position) lexer
    (when (< position (length input))
      (let ((current-char (char input position)))
        (case current-char
          (#\. (make-token :dot "." position 1))
          (#\^ (make-token :caret "^" position 1))
          (#\$ (make-token :dollar "$" position 1))
          (#\? (make-token :question "?" position 1))
          (#\* (make-token :asterisk "*" position 1))
          (#\+ (make-token :plus "+" position 1))
          (#\| (make-token :pipe "|" position 1))
          (#\( (make-token :left-paren "(" position 1))
          (#\) (make-token :right-paren ")" position 1))
          (#\[ (make-token :left-bracket "[" position 1))
          (#\] (make-token :right-bracket "]" position 1))
          (#\{ (handle-quantifier-or-literal lexer))
          (#\\ (handle-escape-sequence lexer))
          (t (make-token :literal (string current-char) position 1)))))))

(defmethod handle-escape-sequence ((lexer regex-lexer))
  "エスケープシーケンスの処理"
  (with-slots (input position) lexer
    (when (< (1+ position) (length input))
      (let ((next-char (char input (1+ position))))
        (case next-char
          (#\d (make-token :posix-class "\\d" position 2))
          (#\w (make-token :posix-class "\\w" position 2))
          (#\s (make-token :posix-class "\\s" position 2))
          (#\n (make-token :escape "\n" position 2))
          (#\t (make-token :escape "\t" position 2))
          (#\r (make-token :escape "\r" position 2))
          (#\u (handle-unicode-escape lexer))
          (#\p (handle-unicode-property lexer))
          (t (make-token :literal (string next-char) position 2)))))))

(defmethod handle-unicode-property ((lexer regex-lexer))
  "Unicode プロパティの処理 \\p{...}"
  (with-slots (input position) lexer
    (let ((start position))
      (incf position 2)  ; \p をスキップ
      (when (and (< position (length input))
                 (char= (char input position) #\{))
        (incf position)
        (let ((property-start position))
          (loop while (and (< position (length input))
                          (char/= (char input position) #\}))
                do (incf position))
          (when (< position (length input))
            (let ((property-name (subseq input property-start position)))
              (incf position)  ; } をスキップ
              (make-token :unicode-property
                          (format nil "\\p{~A}" property-name)
                          start
                          (- position start)))))))))
```

### エラーハンドリング

```lisp
(define-condition lexical-error (regex-error)
  ((invalid-character :initarg :char :reader invalid-character))
  (:documentation "字句解析エラー"))

(defmethod handle-lexical-error ((lexer regex-lexer) condition)
  "字句解析エラーの処理"
  (let ((position (lexer-position lexer))
        (input (lexer-input lexer)))
    (error 'lexical-error
           :pattern input
           :position position
           :char (char input position))))
```

## 構文解析（Syntax Analysis）

### 再帰下降パーサー

正規表現の文法に基づいて構文解析を行います。

```lisp
;; 正規表現の文法（EBNF）
;; regex ::= alternation
;; alternation ::= concatenation ('|' concatenation)*
;; concatenation ::= factor*
;; factor ::= atom quantifier?
;; atom ::= char | '.' | group | class
;; quantifier ::= '?' | '*' | '+' | '{' number (',' number?)? '}'

(defclass regex-parser ()
  ((lexer :initarg :lexer :accessor parser-lexer)
   (current-token :initform nil :accessor current-token)
   (lookahead :initform nil :accessor lookahead-token)))

(defmethod parse-regex ((parser regex-parser))
  "正規表現全体を解析"
  (advance-token parser)
  (let ((ast (parse-alternation parser)))
    (when (current-token parser)
      (error 'syntax-error :expected 'end-of-input
             :actual (token-type (current-token parser))))
    ast))

(defmethod parse-alternation ((parser regex-parser))
  "選択演算子を解析"
  (let ((left (parse-concatenation parser))
        (alternatives '()))
    (loop while (and (current-token parser)
                     (eq (token-type (current-token parser)) :pipe)) do
      (advance-token parser)  ; | をスキップ
      (push (parse-concatenation parser) alternatives))

    (if alternatives
        (make-ast-node :alternation
                       :alternatives (cons left (nreverse alternatives)))
        left)))

(defmethod parse-concatenation ((parser regex-parser))
  "連続を解析"
  (let ((elements '()))
    (loop while (and (current-token parser)
                     (not (member (token-type (current-token parser))
                                  '(:pipe :right-paren :end-of-input))))
          do (push (parse-factor parser) elements))

    (case (length elements)
      (0 (make-ast-node :empty))
      (1 (first elements))
      (t (make-ast-node :concatenation :elements (nreverse elements))))))

(defmethod parse-factor ((parser regex-parser))
  "因子（原子 + 量詞）を解析"
  (let ((atom (parse-atom parser)))
    (if (and (current-token parser)
             (member (token-type (current-token parser))
                     '(:question :asterisk :plus :left-brace)))
        (let ((quantifier (parse-quantifier parser)))
          (make-ast-node :quantified-element
                         :element atom
                         :quantifier quantifier))
        atom)))

(defmethod parse-atom ((parser regex-parser))
  "原子要素を解析"
  (let ((token (current-token parser)))
    (case (token-type token)
      (:literal
       (advance-token parser)
       (make-ast-node :literal :character (char (token-value token) 0)))

      (:dot
       (advance-token parser)
       (make-ast-node :dot))

      (:left-paren
       (parse-group parser))

      (:left-bracket
       (parse-character-class parser))

      (:posix-class
       (advance-token parser)
       (make-ast-node :posix-class :class (token-value token)))

      (:unicode-property
       (advance-token parser)
       (make-ast-node :unicode-property :property (token-value token)))

      (t (error 'syntax-error :expected 'atom :actual (token-type token))))))
```

### AST（抽象構文木）ノード

```lisp
;; AST ノードの定義
(defclass ast-node ()
  ((node-type :initarg :type :accessor ast-node-type)
   (attributes :initarg :attributes :initform '() :accessor ast-attributes)
   (children :initarg :children :initform '() :accessor ast-children)
   (source-location :initarg :location :accessor source-location)))

(defmethod make-ast-node (type &rest attributes)
  "AST ノードを作成"
  (make-instance 'ast-node :type type :attributes attributes))

;; 特化AST ノード
(defclass literal-node (ast-node)
  ((character :initarg :character :accessor literal-character))
  (:default-initargs :type :literal))

(defclass quantifier-node (ast-node)
  ((min-count :initarg :min :accessor min-count)
   (max-count :initarg :max :accessor max-count)
   (greedy :initarg :greedy :initform t :accessor greedy-p))
  (:default-initargs :type :quantifier))

(defclass group-node (ast-node)
  ((group-type :initarg :group-type :accessor group-type)
   (group-name :initarg :name :initform nil :accessor group-name)
   (inner-pattern :initarg :pattern :accessor inner-pattern))
  (:default-initargs :type :group))
```

### エラー回復

```lisp
(defmethod recover-from-syntax-error ((parser regex-parser) error)
  "構文エラーからの回復"
  (case (error-type error)
    (:missing-closing-paren
     ;; 不足している ) を補完
     (insert-synthetic-token parser :right-paren))

    (:invalid-quantifier
     ;; 無効な量詞をスキップ
     (skip-until-valid-token parser))

    (:unexpected-token
     ;; 予期しないトークンをスキップ
     (advance-token parser))))
```

## 意味解析（Semantic Analysis）

### 意味チェック

構文的に正しいASTに対して意味的な妥当性をチェックします。

```lisp
(defclass semantic-analyzer ()
  ((ast :initarg :ast :accessor analyzer-ast)
   (symbol-table :initform (make-hash-table) :accessor symbol-table)
   (errors :initform '() :accessor semantic-errors)))

(defmethod analyze-semantics ((analyzer semantic-analyzer))
  "意味解析を実行"
  (analyze-node analyzer (analyzer-ast analyzer))
  (when (semantic-errors analyzer)
    (error 'semantic-error :errors (semantic-errors analyzer))))

(defmethod analyze-node ((analyzer semantic-analyzer) node)
  "ノードの意味解析"
  (case (ast-node-type node)
    (:group (analyze-group analyzer node))
    (:backreference (analyze-backreference analyzer node))
    (:quantified-element (analyze-quantifier analyzer node))
    (:unicode-property (analyze-unicode-property analyzer node))
    (t (mapc (lambda (child) (analyze-node analyzer child))
             (ast-children node)))))

(defmethod analyze-group ((analyzer semantic-analyzer) group-node)
  "グループの意味解析"
  (let ((group-name (group-name group-node))
        (group-index (compute-group-index group-node)))

    ;; 名前の重複チェック
    (when (and group-name
               (gethash group-name (symbol-table analyzer)))
      (push (make-semantic-error :duplicate-group-name group-name)
            (semantic-errors analyzer)))

    ;; シンボルテーブルに登録
    (when group-name
      (setf (gethash group-name (symbol-table analyzer)) group-index))

    ;; 子ノードの解析
    (analyze-node analyzer (inner-pattern group-node))))

(defmethod analyze-backreference ((analyzer semantic-analyzer) backref-node)
  "後方参照の意味解析"
  (let ((reference (backreference-target backref-node)))
    (etypecase reference
      (string  ; 名前付き参照
       (unless (gethash reference (symbol-table analyzer))
         (push (make-semantic-error :undefined-group reference)
               (semantic-errors analyzer))))
      (integer  ; 番号付き参照
       (unless (valid-group-number-p reference)
         (push (make-semantic-error :invalid-group-number reference)
               (semantic-errors analyzer)))))))

(defmethod analyze-quantifier ((analyzer semantic-analyzer) quant-node)
  "量詞の意味解析"
  (let ((min (min-count quant-node))
        (max (max-count quant-node)))
    (when (and max (> min max))
      (push (make-semantic-error :invalid-quantifier-range min max)
            (semantic-errors analyzer)))

    ;; 子要素の解析
    (analyze-node analyzer (quantified-element quant-node))))
```

### 型推論

```lisp
(defmethod infer-types ((analyzer semantic-analyzer) node)
  "型推論を実行"
  (case (ast-node-type node)
    (:literal
     (setf (node-type node) :character))

    (:character-class
     (setf (node-type node) :character-set))

    (:quantified-element
     (let ((element-type (infer-types analyzer (quantified-element node))))
       (setf (node-type node)
             (if (nullable-p (quantified-element node))
                 :optional-sequence
                 :required-sequence))))

    (:alternation
     (let ((alternative-types
             (mapcar (lambda (alt) (infer-types analyzer alt))
                     (alternatives node))))
       (setf (node-type node) (unify-types alternative-types))))))
```

## AST最適化

### 定数畳み込み

```lisp
(defmethod constant-folding ((optimizer ast-optimizer) node)
  "定数畳み込み最適化"
  (case (ast-node-type node)
    (:concatenation
     (let ((optimized-elements '()))
       (dolist (element (concatenation-elements node))
         (let ((opt-element (constant-folding optimizer element)))
           (if (and (eq (ast-node-type opt-element) :literal)
                    (eq (ast-node-type (car optimized-elements)) :literal))
               ;; 隣接するリテラルをマージ
               (setf (car optimized-elements)
                     (merge-literals (car optimized-elements) opt-element))
               (push opt-element optimized-elements))))
       (make-ast-node :concatenation :elements (nreverse optimized-elements))))

    (:quantified-element
     (let ((element (constant-folding optimizer (quantified-element node)))
           (min (min-count node))
           (max (max-count node)))
       (cond
         ;; {0,0} → 空
         ((and (= min 0) (= max 0))
          (make-ast-node :empty))

         ;; {1,1} → 要素そのもの
         ((and (= min 1) (= max 1))
          element)

         ;; その他
         (t (make-ast-node :quantified-element
                           :element element :min min :max max)))))

    (t node)))

(defmethod merge-literals (literal1 literal2)
  "リテラル文字をマージ"
  (make-ast-node :literal-string
                 :string (concatenate 'string
                                    (string (literal-character literal1))
                                    (string (literal-character literal2)))))
```

### デッドコード除去

```lisp
(defmethod dead-code-elimination ((optimizer ast-optimizer) node)
  "デッドコード除去"
  (case (ast-node-type node)
    (:alternation
     (let ((live-alternatives
             (remove-if #'always-fails-p (alternatives node))))
       (case (length live-alternatives)
         (0 (make-ast-node :fail))
         (1 (dead-code-elimination optimizer (first live-alternatives)))
         (t (make-ast-node :alternation :alternatives live-alternatives)))))

    (:concatenation
     (let ((elements (concatenation-elements node)))
       (if (some #'always-fails-p elements)
           (make-ast-node :fail)
           (make-ast-node :concatenation
                          :elements (mapcar
                                      (lambda (e)
                                        (dead-code-elimination optimizer e))
                                      elements)))))

    (t (generic-optimization optimizer node))))

(defun always-fails-p (node)
  "ノードが常に失敗するかチェック"
  (case (ast-node-type node)
    (:fail t)
    (:character-class (empty-character-class-p node))
    (:quantified-element (always-fails-p (quantified-element node)))
    (t nil)))
```

### 共通部分式除去

```lisp
(defmethod common-subexpression-elimination ((optimizer ast-optimizer) node)
  "共通部分式除去"
  (let ((subexpression-table (make-hash-table :test 'equal)))

    ;; 部分式を収集
    (collect-subexpressions node subexpression-table)

    ;; 共通部分式を特定
    (let ((common-expressions
            (loop for expr being the hash-keys of subexpression-table
                  when (> (gethash expr subexpression-table) 1)
                  collect expr)))

      ;; 共通部分式を置換
      (reduce (lambda (node expr)
                (substitute-subexpression node expr (gensym "CSE")))
              common-expressions
              :initial-value node))))
```

## コード生成（Code Generation）

### NFAコード生成

```lisp
(defclass nfa-code-generator ()
  ((state-counter :initform 0 :accessor state-counter)
   (generated-states :initform '() :accessor generated-states)))

(defmethod generate-nfa-code ((generator nfa-code-generator) ast)
  "ASTからNFAコードを生成"
  (case (ast-node-type ast)
    (:literal (generate-literal-nfa generator ast))
    (:concatenation (generate-concatenation-nfa generator ast))
    (:alternation (generate-alternation-nfa generator ast))
    (:quantified-element (generate-quantifier-nfa generator ast))
    (t (error "Unsupported AST node type: ~A" (ast-node-type ast)))))

(defmethod generate-literal-nfa ((generator nfa-code-generator) literal-node)
  "リテラル文字のNFAを生成"
  (let ((start-state (allocate-state generator))
        (end-state (allocate-state generator))
        (character (literal-character literal-node)))

    (emit-nfa-code generator
      `(transition ,start-state ,end-state ,character))

    (make-nfa-fragment :start start-state :end end-state)))

(defmethod generate-concatenation-nfa ((generator nfa-code-generator) concat-node)
  "連続のNFAを生成"
  (let ((fragments (mapcar (lambda (element)
                            (generate-nfa-code generator element))
                          (concatenation-elements concat-node))))

    ;; フラグメントを連結
    (reduce #'connect-nfa-fragments fragments)))

(defmethod generate-alternation-nfa ((generator nfa-code-generator) alt-node)
  "選択のNFAを生成"
  (let ((start-state (allocate-state generator))
        (end-state (allocate-state generator))
        (alternative-fragments
          (mapcar (lambda (alt) (generate-nfa-code generator alt))
                  (alternatives alt-node))))

    ;; 開始状態から各選択肢への ε遷移
    (dolist (fragment alternative-fragments)
      (emit-nfa-code generator
        `(epsilon-transition ,start-state ,(fragment-start fragment))))

    ;; 各選択肢の終了状態から統合終了状態への ε遷移
    (dolist (fragment alternative-fragments)
      (emit-nfa-code generator
        `(epsilon-transition ,(fragment-end fragment) ,end-state)))

    (make-nfa-fragment :start start-state :end end-state)))
```

### DFAコード生成

```lisp
(defclass dfa-code-generator ()
  ((state-table :initform (make-hash-table) :accessor state-table)
   (transition-table :initform (make-hash-table) :accessor transition-table)))

(defmethod generate-dfa-code ((generator dfa-code-generator) nfa)
  "NFAからDFAコードを生成"
  (let ((dfa (convert-nfa-to-dfa nfa)))
    (generate-dfa-dispatch-table generator dfa)))

(defmethod generate-dfa-dispatch-table ((generator dfa-code-generator) dfa)
  "DFA状態遷移テーブルを生成"
  (dolist (state (dfa-states dfa))
    (let ((transitions (compute-transitions state)))
      (setf (gethash state (transition-table generator)) transitions)))

  ;; 実行可能コードを生成
  (emit-dfa-executor generator))

(defmethod emit-dfa-executor ((generator dfa-code-generator))
  "DFA実行器を生成"
  `(lambda (text)
     (let ((current-state ,(dfa-start-state))
           (position 0))
       (loop for char across text do
         (let ((next-state (gethash (cons current-state char)
                                   ,(transition-table generator))))
           (if next-state
               (setf current-state next-state)
               (return nil)))
         (incf position))
       (when (final-state-p current-state)
         (make-match-result :start 0 :end position)))))
```

## 実行時最適化

### JITコンパイル

```lisp
(defclass jit-compiler ()
  ((native-code-cache :initform (make-hash-table) :accessor native-code-cache)
   (optimization-level :initarg :level :initform 2 :accessor optimization-level)))

(defmethod jit-compile ((compiler jit-compiler) pattern)
  "パターンをネイティブコードにJITコンパイル"
  (let ((cached-code (gethash pattern (native-code-cache compiler))))
    (unless cached-code
      (setf cached-code (compile-to-native compiler pattern))
      (setf (gethash pattern (native-code-cache compiler)) cached-code))
    cached-code))

(defmethod compile-to-native ((compiler jit-compiler) pattern)
  "ネイティブコード生成"
  (let ((ast (parse-pattern pattern))
        (optimized-ast (optimize-ast ast (optimization-level compiler))))

    ;; プラットフォーム固有の最適化
    #+sbcl (compile-sbcl-native optimized-ast)
    #+ccl (compile-ccl-native optimized-ast)
    #+lispworks (compile-lispworks-native optimized-ast)
    #-(or sbcl ccl lispworks) (compile-generic-native optimized-ast)))
```

### 適応的最適化

```lisp
(defclass adaptive-optimizer ()
  ((execution-statistics :initform (make-hash-table) :accessor execution-statistics)
   (optimization-decisions :initform (make-hash-table) :accessor optimization-decisions)))

(defmethod collect-runtime-statistics ((optimizer adaptive-optimizer) pattern execution-time)
  "実行時統計を収集"
  (let ((stats (gethash pattern (execution-statistics optimizer))))
    (unless stats
      (setf stats (make-execution-stats))
      (setf (gethash pattern (execution-statistics optimizer)) stats))

    (update-stats stats execution-time)))

(defmethod make-optimization-decision ((optimizer adaptive-optimizer) pattern)
  "統計に基づく最適化決定"
  (let ((stats (gethash pattern (execution-statistics optimizer))))
    (when stats
      (cond
        ((high-frequency-p stats) :jit-compile)
        ((large-input-p stats) :dfa-conversion)
        ((complex-pattern-p pattern) :nfa-optimization)
        (t :no-optimization)))))
```

## デバッグサポート

### ソースマップ生成

```lisp
(defclass source-map ()
  ((ast-to-source :initform (make-hash-table) :accessor ast-to-source)
   (source-to-ast :initform (make-hash-table) :accessor source-to-ast)))

(defmethod generate-source-map ((compiler regex-compiler) ast source)
  "ソースマップを生成"
  (let ((source-map (make-instance 'source-map)))
    (walk-ast ast
              (lambda (node)
                (when (source-location node)
                  (setf (gethash node (ast-to-source source-map))
                        (source-location node))
                  (setf (gethash (source-location node) (source-to-ast source-map))
                        node))))
    source-map))
```

### コンパイル時間メトリクス

```lisp
(defmethod compile-with-metrics ((compiler regex-compiler) pattern)
  "メトリクス付きコンパイル"
  (let ((start-time (get-internal-real-time))
        (metrics (make-compilation-metrics)))

    (macrolet ((time-phase (phase-name &body body)
                 `(let ((phase-start (get-internal-real-time)))
                    (prog1 (progn ,@body)
                      (record-phase-time metrics ,phase-name
                                       (- (get-internal-real-time) phase-start))))))

      (let* ((tokens (time-phase :lexical-analysis
                       (tokenize pattern)))
             (ast (time-phase :syntax-analysis
                   (parse tokens)))
             (analyzed-ast (time-phase :semantic-analysis
                            (analyze-semantics ast)))
             (optimized-ast (time-phase :optimization
                             (optimize-ast analyzed-ast)))
             (code (time-phase :code-generation
                    (generate-code optimized-ast))))

        (setf (total-time metrics)
              (- (get-internal-real-time) start-time))

        (values code metrics)))))
```

## エラー報告

### 詳細エラーメッセージ

```lisp
(defmethod format-compilation-error ((error compilation-error))
  "コンパイルエラーの詳細フォーマット"
  (let ((pattern (error-pattern error))
        (position (error-position error))
        (phase (compilation-phase error)))

    (format nil "~A error at position ~D in pattern '~A':~%~A~%~V@{~C~:*~}^~%~A"
            phase position pattern pattern position #\Space
            (error-description error))))

;; エラーの例:
;; Syntax error at position 5 in pattern 'ab[cd':
;; ab[cd
;;      ^
;; Unclosed character class
```

## まとめ

CL-Regexのコンパイルパイプラインは以下の特徴を持ちます：

### 多段階最適化
- **字句・構文レベル**: 基本的な構造最適化
- **意味レベル**: 型推論と意味チェック
- **ASTレベル**: 高水準最適化
- **実行時レベル**: 適応的最適化

### 拡張性
- **プラガブル最適化**: 独立した最適化パス
- **複数バックエンド**: NFA/DFA/ハイブリッド対応
- **JITサポート**: 実行時コンパイル

### 開発者支援
- **詳細エラー報告**: ソース位置付きエラー
- **デバッグ情報**: ソースマップとメトリクス
- **プロファイリング**: 実行時統計収集

この包括的なコンパイルシステムにより、CL-Regexは高性能と開発者体験の両立を実現しています。