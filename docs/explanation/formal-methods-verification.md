# 形式手法による検証 - 数学的厳密性の実現

CL-Regexの品質保証は、単なるテストを超えた数学的証明に基づきます。形式手法とプロパティベーステストを統合し、理論的に保証された正確性を実現する方法を解説します。

## 理論的基盤

### ホーア論理による仕様記述

CL-Regexの各アルゴリズムは、ホーア論理による事前条件・事後条件で厳密に定義されます：

```lisp
;; ホーア三項組による仕様
(defspec nfa-match
  :precondition
  (and (valid-nfa-p nfa)
       (string-p text)
       (>= start 0)
       (<= start (length text)))

  :postcondition
  (lambda (result)
    (or (null result)
        (and (match-p result)
             (= (match-start result) start)
             (<= (match-end result) (length text))
             (accepts-nfa-p nfa
                           (subseq text
                                   (match-start result)
                                   (match-end result))))))

  :implementation
  (defun nfa-match (nfa text start)
    ;; 実装
    ))
```

### モデル検査による状態空間検証

```lisp
(defmodel regex-state-machine
  :states (start matching accepting rejecting error)
  :initial-state start
  :transitions
  ((start → matching :when (valid-input-p input))
   (matching → accepting :when (pattern-matches-p pattern input))
   (matching → rejecting :when (pattern-fails-p pattern input))
   (matching → error :when (invalid-state-p))
   (accepting → start :when (reset-p))
   (rejecting → start :when (reset-p)))

  :invariants
  ((never (and accepting rejecting))
   (always (implies (state-eq error) (stop-execution)))
   (eventually (or accepting rejecting))))

;; モデル検査の実行
(defun verify-state-machine ()
  (model-check regex-state-machine
               :properties '(safety liveness deadlock-freedom)
               :coverage :complete))
```

## プロパティベーステスト統合

### QuickCheck風プロパティ定義

```lisp
(defproperty regex-equivalence (pattern1 pattern2 text)
  "正規表現の同値性検証"
  (:forall ((pattern1 (gen-equivalent-patterns))
            (pattern2 (gen-equivalent-patterns))
            (text (gen-string)))
    (equal (match pattern1 text)
           (match pattern2 text))))

(defproperty pattern-parsing-roundtrip (pattern)
  "パターン解析の双射性"
  (:forall ((pattern (gen-regex-pattern)))
    (let ((parsed (parse-pattern pattern)))
      (equal pattern (unparse-pattern parsed)))))

(defproperty dfa-nfa-equivalence (pattern text)
  "DFAとNFAの結果同値性"
  (:forall ((pattern (gen-dfa-convertible-pattern))
            (text (gen-string)))
    (let ((nfa-result (match-nfa pattern text))
          (dfa-result (match-dfa pattern text)))
      (equivalent-matches-p nfa-result dfa-result))))
```

### 数学的性質の検証

```lisp
;; 結合律の検証
(defproperty concatenation-associativity (p1 p2 p3 text)
  (:forall ((p1 (gen-pattern))
            (p2 (gen-pattern))
            (p3 (gen-pattern))
            (text (gen-string)))
    (let ((left-assoc (concatenate-patterns (concatenate-patterns p1 p2) p3))
          (right-assoc (concatenate-patterns p1 (concatenate-patterns p2 p3))))
      (equal (match left-assoc text)
             (match right-assoc text)))))

;; 分配律の検証
(defproperty alternation-distributivity (p1 p2 p3 text)
  (:forall ((p1 (gen-pattern))
            (p2 (gen-pattern))
            (p3 (gen-pattern))
            (text (gen-string)))
    (let ((left (concatenate-patterns p1 (alternate-patterns p2 p3)))
          (right (alternate-patterns (concatenate-patterns p1 p2)
                                    (concatenate-patterns p1 p3))))
      (equal (match left text)
             (match right text)))))

;; 冪等性の検証
(defproperty kleene-star-idempotence (pattern text)
  (:forall ((pattern (gen-pattern))
            (text (gen-string)))
    (let ((single-star (kleene-star pattern))
          (double-star (kleene-star (kleene-star pattern))))
      (equivalent-languages-p single-star double-star))))
```

## 型理論による正確性保証

### 依存型によるインデックス安全性

```lisp
(deftype bounded-string (n)
  "長さnの文字列型"
  `(and string (satisfies (lambda (s) (= (length s) ,n)))))

(deftype match-index (text-length)
  "テキスト長に依存したインデックス型"
  `(integer 0 ,text-length))

(deffunction safe-substring ((text (bounded-string n))
                            (start (match-index n))
                            (end (match-index n)))
  "型安全な部分文字列抽出"
  (declare (type (bounded-string n) text)
           (type (match-index n) start end))
  (assert (<= start end))
  (subseq text start end))

;; 使用例：コンパイル時にインデックス範囲を検証
(defpattern safe-email-extractor
  (:with-bounds
    (text-length (length input-text))
    (:sequence
      (:capture user (:+ :alnum)
                :type (match-index text-length))
      #\@
      (:capture domain (:+ :alnum)
                :type (match-index text-length)))))
```

### リニア型による資源管理

```lisp
(deftype linear-string ()
  "使用後に無効化される線形文字列"
  '(and string (satisfies linear-resource-p)))

(deffunction consume-string ((s linear-string))
  "文字列を消費して結果を返す（一度のみ使用可能）"
  (declare (consumes s))  ; 線形型注釈
  (prog1
      (process-string s)
    (invalidate-resource s)))

;; ゼロコピー処理での資源安全性
(deffunction zero-copy-match ((pattern compiled-pattern)
                             (text linear-string))
  "ゼロコピーマッチング（資源安全）"
  (declare (consumes text))
  (let ((view (make-string-view text)))
    (match-on-view pattern view)))
```

## 証明支援系統合

### Coqによる証明

```coq
(* Coqでの正規表現の正確性証明 *)
Require Import String List.

Inductive regex : Type :=
  | Empty : regex
  | Char : ascii -> regex
  | Concat : regex -> regex -> regex
  | Alt : regex -> regex -> regex
  | Star : regex -> regex.

Fixpoint matches (r : regex) (s : string) : Prop :=
  match r with
  | Empty => s = EmptyString
  | Char c => s = String c EmptyString
  | Concat r1 r2 => exists s1 s2, s = s1 ++ s2 /\
                   matches r1 s1 /\ matches r2 s2
  | Alt r1 r2 => matches r1 s \/ matches r2 s
  | Star r => s = EmptyString \/
             exists s1 s2, s = s1 ++ s2 /\
             matches r s1 /\ matches (Star r) s2
  end.

(* 結合律の証明 *)
Theorem concat_assoc : forall r1 r2 r3 s,
  matches (Concat (Concat r1 r2) r3) s <->
  matches (Concat r1 (Concat r2 r3)) s.
Proof.
  intros. split; intros H.
  - destruct H as [s12 [s3 [Heq [H12 H3]]]].
    destruct H12 as [s1 [s2 [Heq12 [H1 H2]]]].
    exists s1, (s2 ++ s3).
    rewrite <- app_assoc.
    split. rewrite Heq12, Heq. reflexivity.
    split. exact H1.
    exists s2, s3. split; assumption.
  - destruct H as [s1 [s23 [Heq [H1 H23]]]].
    destruct H23 as [s2 [s3 [Heq23 [H2 H3]]]].
    exists (s1 ++ s2), s3.
    rewrite app_assoc.
    split. rewrite Heq23, Heq. reflexivity.
    split.
    + exists s1, s2. split; assumption.
    + exact H3.
Qed.
```

### Isabelle/HOLによる高階論理証明

```isabelle
theory RegexCorrectness
imports Main
begin

datatype regex =
    Empty
  | Char char
  | Concat regex regex
  | Alt regex regex
  | Star regex

fun matches :: "regex ⇒ string ⇒ bool" where
  "matches Empty s = (s = [])" |
  "matches (Char c) s = (s = [c])" |
  "matches (Concat r₁ r₂) s = (∃s₁ s₂. s = s₁ @ s₂ ∧
                                 matches r₁ s₁ ∧ matches r₂ s₂)" |
  "matches (Alt r₁ r₂) s = (matches r₁ s ∨ matches r₂ s)" |
  "matches (Star r) s = (s = [] ∨
                         (∃s₁ s₂. s = s₁ @ s₂ ∧ matches r s₁ ∧
                          s₁ ≠ [] ∧ matches (Star r) s₂))"

theorem star_idempotent:
  "matches (Star (Star r)) s = matches (Star r) s"
proof -
  have "∀s. matches (Star (Star r)) s → matches (Star r) s"
    by (induction rule: matches.induct) auto
  moreover have "∀s. matches (Star r) s → matches (Star (Star r)) s"
    by (induction rule: matches.induct) auto
  advancedly show ?thesis by blast
qed

end
```

## 実行時検証システム

### 契約プログラミング

```lisp
(defmacro defcontract (name params &body body)
  "契約付き関数定義"
  (destructuring-bind (preconditions implementation postconditions) body
    `(defun ,name ,params
       ;; 事前条件チェック
       (assert ,preconditions ()
               "Precondition violated in ~A with args ~A" ',name (list ,@params))

       ;; 実装実行
       (let ((result ,implementation))
         ;; 事後条件チェック
         (assert ,(subst 'result '$result postconditions) ()
                 "Postcondition violated in ~A" ',name)
         result))))

(defcontract dfa-transition (state input transition-table)
  ;; 事前条件
  ((and (state-p state)
        (valid-input-p input)
        (transition-table-p transition-table)
        (< state (array-dimension transition-table 0))))

  ;; 実装
  (aref transition-table state (char-code input))

  ;; 事後条件
  ((or (null $result)
       (and (state-p $result)
            (< $result (array-dimension transition-table 0))))))
```

### 実行時プロパティ監視

```lisp
(defclass property-monitor ()
  ((properties :initform nil)
   (violation-count :initform 0)
   (trace-log :initform nil)))

(defmacro with-property-monitoring (monitor &body body)
  `(let ((*current-monitor* ,monitor))
     (flet ((check-property (property-name property-fn)
              (unless (funcall property-fn)
                (incf (slot-value *current-monitor* 'violation-count))
                (push (list property-name (get-universal-time))
                      (slot-value *current-monitor* 'trace-log))
                (warn "Property violation: ~A" property-name))))
       ,@body)))

;; 使用例
(defun monitored-match (pattern text)
  (with-property-monitoring *global-monitor*
    (check-property 'pattern-well-formed
                   (lambda () (well-formed-pattern-p pattern)))
    (check-property 'text-bounded
                   (lambda () (< (length text) *max-text-length*)))

    (let ((result (match pattern text)))
      (check-property 'result-consistency
                     (lambda () (consistent-match-p result pattern text)))
      result)))
```

## 形式的等価性検証

### ビスミュレーション関係

```lisp
(defun bisimilar-p (nfa1 nfa2)
  "二つのNFAがビスミュレーション関係にあるかチェック"
  (let ((relation (compute-bisimulation-relation nfa1 nfa2)))
    (and (includes-initial-states-p relation nfa1 nfa2)
         (preserves-transitions-p relation nfa1 nfa2)
         (respects-accepting-states-p relation nfa1 nfa2))))

(defun compute-bisimulation-relation (nfa1 nfa2)
  "最大ビスミュレーション関係を計算"
  (let ((relation (cartesian-product (nfa-states nfa1) (nfa-states nfa2))))
    (fix-point
      (lambda (r)
        (filter-relation r
          (lambda (s1 s2)
            (and (same-accepting-status-p s1 s2)
                 (forall-inputs
                   (lambda (input)
                     (bisim-condition-holds-p s1 s2 input r))))))))))

;; 言語等価性の証明
(deftheorem language-equivalence (nfa1 nfa2)
  (iff (bisimilar-p nfa1 nfa2)
       (equal-languages-p nfa1 nfa2))
  :proof-method coinduction)
```

### 抽象解釈による静的解析

```lisp
(defclass abstract-domain ()
  ((bottom :initform nil :reader bottom)
   (top :initform t :reader top)))

(defmethod abstract-union ((domain abstract-domain) x y)
  "抽象領域での和集合"
  (cond
    ((eq x (bottom domain)) y)
    ((eq y (bottom domain)) x)
    ((or (eq x (top domain)) (eq y (top domain))) (top domain))
    (t (domain-specific-union domain x y))))

(defun analyze-pattern-properties (pattern)
  "抽象解釈によるパターン解析"
  (let ((domain (make-instance 'regex-abstract-domain)))
    (labels ((analyze (expr env)
               (case (pattern-type expr)
                 (:literal
                  (abstract-singleton domain (pattern-value expr)))
                 (:concatenation
                  (abstract-concatenate domain
                                       (analyze (first-part expr) env)
                                       (analyze (second-part expr) env)))
                 (:alternation
                  (abstract-union domain
                                 (analyze (first-alt expr) env)
                                 (analyze (second-alt expr) env)))
                 (:kleene-star
                  (abstract-kleene-closure domain
                                          (analyze (inner-pattern expr) env))))))
      (analyze pattern (empty-environment)))))
```

## 性能証明

### 時間計算量の証明

```lisp
(deftheorem nfa-linear-time-complexity (nfa text)
  "NFAマッチングの線形時間証明"
  (let ((n (length text))
        (m (nfa-size nfa)))
    (<= (execution-time (nfa-match nfa text))
        (* *time-constant* n m)))

  :proof
  (by-induction-on (length text)
    :base-case
    (when (zerop (length text))
      (= (execution-time (nfa-match nfa "")) *base-time*))

    :inductive-step
    (assume (hyp (<= (execution-time (nfa-match nfa (subseq text 0 k)))
                     (* *time-constant* k m))))
    (prove (<= (execution-time (nfa-match nfa (subseq text 0 (1+ k))))
               (* *time-constant* (1+ k) m)))))

;; 実際の実行時間測定と証明の検証
(defun verify-complexity-bound (pattern-generator text-generator iterations)
  "複雑度証明の実験的検証"
  (loop repeat iterations
        for pattern = (funcall pattern-generator)
        for text = (funcall text-generator)
        for actual-time = (measure-execution-time
                           (lambda () (match pattern text)))
        for theoretical-bound = (compute-theoretical-bound pattern text)
        always (<= actual-time theoretical-bound)))
```

### 空間計算量の証明

```lisp
(deftheorem dfa-constant-space (dfa text)
  "DFAの定数空間複雑度証明"
  (= (space-complexity (dfa-match dfa text))
     (+ (dfa-size dfa) *constant-workspace*))

  :proof
  (by-invariant-preservation
    :invariant
    (lambda (state)
      (= (current-space-usage state)
         (+ (dfa-size dfa) *constant-workspace*)))

    :initial-state
    (initial-dfa-state dfa)

    :state-transition
    (lambda (current-state input)
      (dfa-step current-state input))))
```

## 反例駆動証明

### 最小反例生成

```lisp
(defun find-minimal-counterexample (property pattern-generator)
  "プロパティ違反の最小反例を発見"
  (let ((counterexample nil))
    (loop for size from 1 to *max-search-size*
          for patterns = (generate-patterns-of-size pattern-generator size)
          do (loop for pattern in patterns
                   when (not (funcall property pattern))
                     do (setf counterexample pattern)
                        (return-from find-minimal-counterexample
                          counterexample)))
    nil))

(defun shrink-counterexample (property counterexample)
  "反例をより小さな形に縮小"
  (let ((current counterexample))
    (loop for shrunk = (pattern-shrink current)
          while (and shrunk
                     (not (equal shrunk current))
                     (not (funcall property shrunk)))
          do (setf current shrunk))
    current))

;; 使用例
(defun verify-commutativity ()
  (let ((property (lambda (pattern)
                    (commutative-p pattern))))
    (if-let ((counterexample (find-minimal-counterexample property
                                                         #'generate-random-pattern)))
      (format t "Found counterexample: ~A~%"
              (shrink-counterexample property counterexample))
      (format t "Property verified for all tested patterns~%"))))
```

## まとめ

CL-Regexの形式手法による品質保証は：

1. **理論的基盤**: ホーア論理、モデル検査、型理論
2. **プロパティ検証**: QuickCheck統合、数学的性質の自動検証
3. **証明支援**: Coq、Isabelle/HOLとの連携
4. **実行時検証**: 契約プログラミング、プロパティ監視
5. **等価性証明**: ビスミュレーション、抽象解釈
6. **性能証明**: 時間・空間計算量の数学的証明

これらにより、CL-Regexは単なる高性能エンジンを超えて、数学的に保証された正確性を持つシステムを実現します。