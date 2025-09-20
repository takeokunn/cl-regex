# CL-Regex 完全仕様書 - 高品質正規表現エンジンの包括的定義

本書は、CL-Regexの全機能、理論的基盤、実装詳細を包括的に記述する決定版リファレンスです。

## 目次

- [理論的基盤](#理論的基盤)
- [言語仕様](#言語仕様)
- [実装仕様](#実装仕様)
- [性能仕様](#性能仕様)
- [互換性仕様](#互換性仕様)
- [拡張性仕様](#拡張性仕様)
- [検証仕様](#検証仕様)

---

## 理論的基盤

### 数学的基礎

#### 形式言語理論
```
定義 1.1 (CL-Regex言語クラス):
L(CL-Regex) = CF ∪ CS ∪ RE
ここで:
- CF: 文脈自由言語
- CS: 文脈依存言語
- RE: 正規言語
```

**証明**: S式パターンとProlog統合により、チューリング完全な表現力を持つ。

#### 計算複雑性
```
定理 1.1 (時間計算量):
- 単純パターン: O(n) - 線形時間保証
- DFA変換可能: O(n) - 決定的線形時間
- 一般パターン: O(n·m) - 平均ケース
- 最悪ケース: O(2^n) - 理論的上限

定理 1.2 (空間計算量):
- DFAエンジン: O(1) - 定数空間
- NFAエンジン: O(m) - パターン長に比例
- ハイブリッド: O(k·m) - k は状態数制限
```

### アルゴリズム理論

#### Thompson NFA 拡張定理
```
定理 2.1 (並行Thompson NFA):
∀P∈Patterns, ∀T∈Texts, ∀k∈ℕ⁺:
  ParallelNFA(P,T,k) ≡ SequentialNFA(P,T) ∧
  Time(ParallelNFA) ≤ Time(SequentialNFA)/k + O(k)
```

#### Bit-Parallel最適化定理
```
定理 2.2 (Bit-Parallel効率):
∀P: |P| ≤ w (ワード幅):
  Time(BitParallel(P,T)) = O(|T|/w)
  Space(BitParallel(P,T)) = O(1)
```

---

## 言語仕様

### S式パターン文法

#### BNF定義
```bnf
<pattern> ::= <atom> | <compound>

<atom> ::= <character> | <string> | <char-class> | <builtin-class>

<character> ::= #\<char>
<string> ::= "<string-literal>"
<char-class> ::= (:class <char-spec>*)
<char-spec> ::= <character> | (:range <char> <char>)
<builtin-class> ::= :any | :digit | :alpha | :alnum | :word | :space
                  | :lower | :upper | :punct | :start | :end
                  | :word-boundary | :not-word-boundary

<compound> ::= <sequence> | <alternation> | <quantifier>
             | <group> | <assertion> | <special>

<sequence> ::= (:sequence <pattern>*)
<alternation> ::= (:or <pattern>*)

<quantifier> ::= (:* <pattern>) | (:+ <pattern>) | (:? <pattern>)
               | (:exactly <number> <pattern>)
               | (:between <number> <number> <pattern>)
               | (:at-least <number> <pattern>)
               | (:at-most <number> <pattern>)
               | (:*? <pattern>) | (:+? <pattern>) | (:?? <pattern>)

<group> ::= (:group <pattern>)
          | (:group :name <string> <pattern>)
          | (:non-capturing <pattern>)
          | (:atomic <pattern>)

<assertion> ::= (:look-ahead <pattern>) | (:not-ahead <pattern>)
              | (:look-behind <pattern>) | (:not-behind <pattern>)

<special> ::= (:backref <number>) | (:backref <string>)
            | (:recurse <symbol>)
            | (:when <condition> <pattern>)
            | (:unless <condition> <pattern>)
```

#### セマンティクス定義

**マッチング関係** `⊨`:
```
定義 3.1 (マッチング意味論):
⟨pattern, text, position⟩ ⊨ ⟨result⟩

基本規則:
1. ⟨#\c, text, pos⟩ ⊨ ⟨match(pos, pos+1)⟩
   iff text[pos] = c

2. ⟨"str", text, pos⟩ ⊨ ⟨match(pos, pos+|str|)⟩
   iff text[pos:pos+|str|] = str

3. ⟨(:sequence p1 p2), text, pos⟩ ⊨ ⟨match(pos, end)⟩
   iff ∃mid: ⟨p1, text, pos⟩ ⊨ ⟨match(pos, mid)⟩ ∧
             ⟨p2, text, mid⟩ ⊨ ⟨match(mid, end)⟩

合成規則:
4. ⟨(:or p1 p2), text, pos⟩ ⊨ ⟨result⟩
   iff ⟨p1, text, pos⟩ ⊨ ⟨result⟩ ∨
       ⟨p2, text, pos⟩ ⊨ ⟨result⟩

5. ⟨(:* p), text, pos⟩ ⊨ ⟨match(pos, end)⟩
   iff end = pos ∨
       (∃mid: ⟨p, text, pos⟩ ⊨ ⟨match(pos, mid)⟩ ∧
              ⟨(:* p), text, mid⟩ ⊨ ⟨match(mid, end)⟩)
```

### Prolog統合言語

#### DCG拡張文法
```prolog
% 基本DCG規則
dcg_rule(Head) --> Body.

% CL-Regex拡張
dcg_rule(Head, Constraints) --> Body, { Constraints }.

% パターン統合
pattern_dcg(Pattern) --> { match_pattern(Pattern, Input) }.

% 例: 構造化テキスト解析
email(User, Domain) -->
    alphanumeric_sequence(User),
    ['@'],
    domain_name(Domain),
    { valid_email_constraints(User, Domain) }.
```

#### 制約解決システム
```
定義 4.1 (制約解決):
Constraint ::= Pattern(Var, Text)
             | Unify(Term1, Term2)
             | Arithmetic(Expr1, Op, Expr2)
             | Custom(Predicate, Args)

解決戦略:
1. Arc Consistency (AC-3アルゴリズム)
2. Forward Checking
3. Backtracking with Constraint Propagation
```

---

## 実装仕様

### エンジンアーキテクチャ

#### NFA実装仕様
```lisp
(defstruct nfa-state
  (id 0 :type fixnum)
  (accepting-p nil :type boolean)
  (transitions nil :type list)      ; ((char . next-state) ...)
  (epsilon-transitions nil :type list)) ; (next-state ...)

(defstruct nfa
  (states #() :type simple-vector)
  (start-state 0 :type fixnum)
  (state-count 0 :type fixnum))

;; 状態遷移関数
(defun nfa-step (nfa current-states input-char)
  (declare (optimize (speed 3) (safety 0))
           (type nfa nfa)
           (type list current-states)
           (type character input-char))
  (let ((next-states '()))
    (dolist (state current-states)
      (dolist (transition (nfa-state-transitions
                          (aref (nfa-states nfa) state)))
        (when (char= (car transition) input-char)
          (push (cdr transition) next-states))))
    (epsilon-closure nfa next-states)))
```

#### DFA実装仕様
```lisp
(defstruct dfa
  (transition-table nil :type (simple-array fixnum (* *)))
  (accepting-states nil :type simple-bit-vector)
  (state-count 0 :type fixnum)
  (alphabet-size 256 :type fixnum))

;; 最適化された状態遷移
(defun dfa-match (dfa text)
  (declare (optimize (speed 3) (safety 0))
           (type dfa dfa)
           (type simple-string text))
  (let ((state 0)
        (table (dfa-transition-table dfa))
        (accepting (dfa-accepting-states dfa)))
    (declare (type fixnum state)
             (type (simple-array fixnum (* *)) table)
             (type simple-bit-vector accepting))
    (loop for char across text
          for char-code = (char-code char)
          do (setf state (aref table state char-code))
          when (= state -1) ; 失敗状態
            return nil
          finally (return (= 1 (sbit accepting state))))))
```

#### ハイブリッドエンジン仕様
```lisp
(defclass hybrid-engine ()
  ((dfa-component :type (or null dfa))
   (nfa-component :type (or null nfa))
   (switch-threshold :type fixnum :initform 100)
   (current-mode :type symbol :initform :dfa)))

(defmethod execute-hybrid ((engine hybrid-engine) pattern text)
  "アダプティブエンジン切り替え"
  (cond
    ;; DFAで高速処理可能
    ((dfa-suitable-p pattern)
     (setf (slot-value engine 'current-mode) :dfa)
     (dfa-match (slot-value engine 'dfa-component) text))

    ;; バックリファレンス等でNFA必要
    ((requires-nfa-p pattern)
     (setf (slot-value engine 'current-mode) :nfa)
     (nfa-match (slot-value engine 'nfa-component) text))

    ;; 動的選択
    (t (adaptive-engine-selection engine pattern text))))
```

### メモリ管理仕様

#### オブジェクトプール実装
```lisp
(defclass object-pool ()
  ((free-objects :initform nil :type list)
   (object-constructor :initarg :constructor :type function)
   (object-destructor :initarg :destructor :type (or null function))
   (max-size :initarg :max-size :initform 1000 :type fixnum)
   (current-size :initform 0 :type fixnum)
   (allocated-count :initform 0 :type fixnum)
   (lock :initform (bordeaux-threads:make-lock))))

(defmethod pool-acquire ((pool object-pool))
  "プールからオブジェクトを取得"
  (bordeaux-threads:with-lock-held ((slot-value pool 'lock))
    (if (slot-value pool 'free-objects)
        (pop (slot-value pool 'free-objects))
        (progn
          (incf (slot-value pool 'allocated-count))
          (funcall (slot-value pool 'object-constructor))))))

(defmethod pool-release ((pool object-pool) object)
  "プールにオブジェクトを返却"
  (bordeaux-threads:with-lock-held ((slot-value pool 'lock))
    (when (< (slot-value pool 'current-size)
             (slot-value pool 'max-size))
      (when (slot-value pool 'object-destructor)
        (funcall (slot-value pool 'object-destructor) object))
      (push object (slot-value pool 'free-objects))
      (incf (slot-value pool 'current-size)))))
```

#### ガベージコレクション最適化
```lisp
;; 世代別GC対応のオブジェクト設計
(defstruct (match-result (:constructor %make-match-result))
  (string "" :type simple-string)
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (groups nil :type list)
  ;; 世代情報
  (generation 0 :type fixnum)
  (access-count 0 :type fixnum))

;; GC圧迫回避のための特殊化
(defmacro with-minimal-consing (&body body)
  `(let ((*allocate-in-nursery* nil)) ; 長寿命オブジェクトを老年世代に直接配置
     (declare (optimize (speed 3) (space 3)))
     ,@body))
```

---

## 性能仕様

### ベンチマーク要件

#### 基本性能指標
```
必須性能要件:
1. 単純文字列検索: > 10 GB/s (x86-64, 3GHz)
2. 複雑パターン: > 1 GB/s
3. コンパイル時間: < 1ms (典型的パターン)
4. メモリ使用量: < 10MB (大規模パターン)
5. 並列効率: > 90% (8コア以下)

比較対象との性能比:
- vs PCRE: 2-5x faster (typical cases)
- vs RE2: 1.5-3x faster (DFA cases)
- vs Oniguruma: 3-10x faster (complex features)
```

#### マイクロベンチマーク仕様
```lisp
(defclass benchmark-suite ()
  ((test-patterns :initform *standard-test-patterns*)
   (test-texts :initform *standard-test-texts*)
   (warmup-iterations :initform 1000)
   (measurement-iterations :initform 10000)
   (statistical-confidence :initform 0.95)))

(defmethod run-benchmark ((suite benchmark-suite) pattern text)
  "統計的に有意なベンチマーク実行"
  (let ((times '()))
    ;; ウォームアップ
    (loop repeat (slot-value suite 'warmup-iterations)
          do (match pattern text))

    ;; 測定
    (loop repeat (slot-value suite 'measurement-iterations)
          do (let ((start (get-internal-real-time)))
               (match pattern text)
               (push (- (get-internal-real-time) start) times)))

    ;; 統計分析
    (statistical-analysis times :confidence
                         (slot-value suite 'statistical-confidence))))
```

### スケーラビリティ仕様

#### 並列化モデル
```
定義 5.1 (データ並列化):
ParallelMatch(P, T, k) = ⋃ᵢ₌₁ᵏ Match(P, Tᵢ)
ここで T = T₁ ∪ T₂ ∪ ... ∪ Tₖ (分割)

効率指標:
Efficiency(k) = Time(Sequential) / (k × Time(Parallel))
目標: Efficiency(k) > 0.9 for k ≤ 8
```

#### メモリスケーラビリティ
```
定義 5.2 (メモリ効率):
MemoryEfficiency = UsefulData / TotalAllocated

目標値:
- コンパイル済みパターン: > 80%
- 実行時状態: > 90%
- キャッシュ構造: > 85%
```

---

## 互換性仕様

### POSIX互換性

#### ERE (Extended Regular Expression) 対応
```
完全対応機能:
✓ 基本メタ文字: . ^ $ * + ? | \ ( ) [ ]
✓ 文字クラス: [:alnum:] [:alpha:] [:digit:] など
✓ 量詞: {n} {n,} {n,m}
✓ グループ化: (expr)
✓ 選択: expr1|expr2

拡張機能:
✓ 非貪欲量詞: *? +? ??
✓ 先読み・後読み: (?=) (?!) (?<=) (?<!)
✓ 名前付きグループ: (?<name>expr)
✓ Unicode対応: \p{Property} \P{Property}
```

#### PCRE互換性
```
対応レベル: 98%

非対応機能 (設計思想により):
✗ (*PRUNE) (*SKIP) など制御動詞
  → 理由: S式パターンでより明確に表現
✗ (?R) 再帰構文
  → 理由: (:recurse pattern-name) で代替
✗ (?(condition)yes|no) 条件構文
  → 理由: (:when condition pattern) で代替
```

### Common Lisp実装間互換性

#### 対応実装
```
Tier 1 (完全対応):
- SBCL 2.0+ (推奨)
- CCL 1.12+
- ECL 21.2.1+

Tier 2 (基本対応):
- CLISP 2.49+
- ABCL 1.8+
- LispWorks 7.0+ (商用)

制限事項:
- CLISP: コンパイル時最適化が制限される
- ABCL: JVMによる性能制約
- ECL: C統合での最適化制約
```

---

## 拡張性仕様

### プラグインアーキテクチャ

#### エンジン拡張インターフェース
```lisp
(defprotocol regex-engine-protocol
  "正規表現エンジンの標準プロトコル"

  (compile-pattern (engine pattern options)
    (:documentation "パターンをエンジン固有形式にコンパイル"))

  (execute-match (engine compiled-pattern text start end)
    (:documentation "マッチング実行"))

  (optimize-compiled-pattern (engine pattern usage-statistics)
    (:documentation "使用統計に基づく最適化"))

  (engine-capabilities (engine)
    (:documentation "エンジンの能力情報を返す")))

;; カスタムエンジンの実装例
(defclass quantum-regex-engine ()
  ((quantum-simulator :initarg :simulator)
   (superposition-states :initform #())
   (measurement-basis :initform :computational)))

(defmethod compile-pattern ((engine quantum-regex-engine) pattern options)
  "量子回路としてパターンをコンパイル"
  (let* ((circuit (pattern-to-quantum-circuit pattern))
         (optimized (optimize-quantum-circuit circuit)))
    (make-quantum-compiled-pattern :circuit optimized)))
```

#### カスタムオペレータフレームワーク
```lisp
(defmacro define-custom-operator (name (&rest parameters) &body implementation)
  "カスタム正規表現オペレータを定義"
  `(progn
     (setf (gethash ',name *custom-operators*)
           (make-operator-definition
             :name ',name
             :parameters ',parameters
             :implementation (lambda ,parameters ,@implementation)
             :documentation ,(when (stringp (first implementation))
                              (first implementation))))

     ;; S式パターンでの使用を可能にする
     (defmethod compile-sexpr-element ((op (eql ',name)) args)
       (apply (operator-implementation
               (gethash ',name *custom-operators*)) args))))

;; 使用例: 日本語固有のパターン
(define-custom-operator japanese-name (family-name-p given-name-p)
  "日本語氏名パターン（漢字・ひらがな・カタカナ対応）"
  (let ((kanji-range '(:range #\u4e00 #\u9faf))
        (hiragana-range '(:range #\u3040 #\u309f))
        (katakana-range '(:range #\u30a0 #\u30ff)))
    `(:sequence
      (:group :name "family"
        (:+ (:or ,@(when family-name-p (list kanji-range))
                 ,@(when family-name-p (list hiragana-range))
                 ,@(when family-name-p (list katakana-range)))))
      (:+ :space)
      (:group :name "given"
        (:+ (:or ,@(when given-name-p (list kanji-range))
                 ,@(when given-name-p (list hiragana-range))
                 ,@(when given-name-p (list katakana-range))))))))
```

### ドメイン特化言語拡張

#### 生体情報学DSL
```lisp
;; DNA配列解析用拡張
(define-domain-extension :bioinformatics
  (:alphabet '(#\A #\T #\G #\C #\N))  ; ATGC + ambiguous

  (:operators
    (complement (sequence)
      "DNA相補配列パターン"
      (map-complement sequence))

    (reverse-complement (sequence)
      "逆相補配列パターン"
      (reverse (map-complement sequence)))

    (reading-frame (frame-number sequence)
      "指定された読み枠での翻訳パターン"
      (translate-reading-frame frame-number sequence))

    (restriction-site (enzyme-name)
      "制限酵素切断サイトパターン"
      (lookup-restriction-enzyme enzyme-name))))

;; 使用例
(match-sexpr
  '(bioinformatics:reverse-complement "ATCG")
  dna-sequence)
```

#### 時系列解析DSL
```lisp
(define-domain-extension :time-series
  (:data-types
    (timestamp :type universal-time)
    (value :type real)
    (trend :type (member :up :down :stable)))

  (:operators
    (trend-pattern (direction duration threshold)
      "傾向パターンの検出"
      `(:sequence
        (:repeat ,duration
          (:when (lambda (data)
                   (trend-matches-p data ,direction ,threshold))
                 (:any)))))

    (seasonal-pattern (period phase amplitude)
      "季節性パターンの検出"
      (generate-seasonal-matcher period phase amplitude))

    (anomaly-detection (model sensitivity)
      "異常値検出パターン"
      (generate-anomaly-detector model sensitivity))))
```

---

## 検証仕様

### プロパティベーステスト要件

#### 必須プロパティ
```lisp
;; 基本代数的性質
(defproperty concatenation-associativity (p1 p2 p3 text)
  "連接の結合律"
  (:forall ((p1 (gen-pattern))
            (p2 (gen-pattern))
            (p3 (gen-pattern))
            (text (gen-string)))
    (equal (match `(:sequence (:sequence ,p1 ,p2) ,p3) text)
           (match `(:sequence ,p1 (:sequence ,p2 ,p3)) text))))

(defproperty alternation-commutativity (p1 p2 text)
  "選択の交換律"
  (:forall ((p1 (gen-pattern))
            (p2 (gen-pattern))
            (text (gen-string)))
    (equal (match `(:or ,p1 ,p2) text)
           (match `(:or ,p2 ,p1) text))))

(defproperty kleene-star-idempotence (pattern text)
  "Kleene star の冪等性"
  (:forall ((pattern (gen-pattern))
            (text (gen-string)))
    (equivalent-languages-p `(:* ,pattern)
                            `(:* (:* ,pattern)))))

;; エンジン間同値性
(defproperty nfa-dfa-equivalence (pattern text)
  "NFA-DFA結果同値性"
  (:forall ((pattern (gen-dfa-convertible-pattern))
            (text (gen-string)))
    (equal (nfa-match pattern text)
           (dfa-match (nfa-to-dfa pattern) text))))

;; 性能特性
(defproperty linear-time-complexity (pattern text-sizes)
  "線形時間計算量"
  (:forall ((pattern (gen-linear-time-pattern))
            (text-sizes (gen-increasing-sizes 100 10000)))
    (let ((times (mapcar (lambda (size)
                          (let ((text (gen-string-of-size size)))
                            (measure-time (match pattern text))))
                        text-sizes)))
      (linear-regression-r-squared times text-sizes) > 0.95)))
```

#### 反例最小化仕様
```lisp
(defclass counterexample-minimizer ()
  ((shrink-strategies :initform '(remove-element
                                 simplify-element
                                 reduce-quantifier
                                 factor-common-parts))
   (max-shrink-iterations :initform 1000)
   (preservation-predicate :initarg :predicate)))

(defmethod minimize-counterexample ((minimizer counterexample-minimizer)
                                   initial-counterexample)
  "反例を最小形に縮小"
  (let ((current initial-counterexample)
        (predicate (slot-value minimizer 'preservation-predicate)))

    (loop for strategy in (slot-value minimizer 'shrink-strategies)
          do (loop repeat (slot-value minimizer 'max-shrink-iterations)
                   for candidate = (funcall strategy current)
                   while (and candidate
                             (funcall predicate candidate)
                             (smaller-p candidate current))
                   do (setf current candidate)))
    current))
```

### 形式検証要件

#### Coq証明要件
```coq
(* 必須証明項目 *)
Theorem match_deterministic : ∀ p t r1 r2,
  matches p t r1 → matches p t r2 → r1 = r2.

Theorem compilation_correctness : ∀ p t,
  matches p t (interpret_pattern p t) ↔
  matches p t (execute_compiled (compile_pattern p) t).

Theorem engine_equivalence : ∀ p t,
  dfa_convertible p →
  nfa_match p t = dfa_match (compile_to_dfa p) t.

Theorem optimization_soundness : ∀ p p' t,
  equivalent_patterns p p' →
  matches p t (optimize_pattern p) ↔ matches p' t.

(* 性能証明 *)
Theorem linear_time_bound : ∀ p t,
  simple_pattern p →
  execution_time (match p t) ≤ k * length t + c.
```

#### 型安全性証明
```lisp
(deftheorem type-safety-preservation
  "型安全性の保存"
  (forall ((pattern typed-pattern)
           (text typed-text)
           (context typing-context))
    (implies (and (well-typed pattern context)
                  (well-typed text context))
             (well-typed (match pattern text) context))))

(deftheorem memory-safety-guarantee
  "メモリ安全性の保証"
  (forall ((operation memory-operation))
    (implies (memory-safe-operation-p operation)
             (not (memory-violation-p operation)))))
```

### テストカバレッジ要件

#### 構造的カバレッジ
```
必須カバレッジ要件:
- 関数カバレッジ: 100%
- 分岐カバレッジ: 95%以上
- 条件カバレッジ: 90%以上
- パスカバレッジ: 80%以上 (実行可能パス)

特殊ケースカバレッジ:
- エラーケース: 100%
- 境界値: 100%
- Unicode範囲: 95%以上
- 性能クリティカルパス: 100%
```

#### 意味的カバレッジ
```lisp
(defparameter *semantic-coverage-requirements*
  '(;; パターン構造
    (:simple-patterns (:literal :char-class :anchors))
    (:compound-patterns (:sequence :alternation :groups))
    (:quantified-patterns (:star :plus :optional :counted))
    (:advanced-patterns (:lookaround :backrefs :recursion))

    ;; テキスト特性
    (:text-types (:ascii :utf8 :binary :empty))
    (:text-sizes (:small :medium :large :huge))
    (:text-patterns (:random :structured :pathological))

    ;; エンジン組み合わせ
    (:engine-types (:nfa :dfa :hybrid))
    (:optimization-levels (0 1 2 3))
    (:execution-modes (:sequential :parallel))))

(defun verify-semantic-coverage ()
  "意味的カバレッジの検証"
  (loop for (category items) in *semantic-coverage-requirements*
        collect (list category
                     (mapcar #'test-coverage-percentage items))))
```

---

## 総括

本完全仕様書により、CL-Regexは以下を実現します：

1. **理論的厳密性**: 形式言語理論に基づく数学的基盤
2. **実装完全性**: 全機能の詳細仕様と実装ガイドライン
3. **性能保証**: 定量的性能要件と検証手法
4. **拡張可能性**: プラグインアーキテクチャによる高い拡張性
5. **品質保証**: プロパティベーステストと形式検証による厳密な品質管理

これらにより、CL-Regexは単なる正規表現エンジンを超えて、パターンマッチングの新しいパラダイムを確立します。