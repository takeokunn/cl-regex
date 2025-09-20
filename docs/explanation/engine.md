# NFA/DFA Engine - 正規表現エンジンの仕組み

CL-Regexの核心となるNFA（非決定性有限オートマトン）とDFA（決定性有限オートマトン）エンジンの設計と実装について詳しく解説します。

## 基本概念

### 有限オートマトンとは

有限オートマトンは、入力文字列を処理して受理/拒否を決定する抽象機械です。正規表現エンジンは、この数学的モデルを実装して文字列マッチングを実現します。

```
状態遷移図の例（パターン "ab*c"）:
   a      b      c
→ ① → ② ⇄ ② → ③ (受理状態)
  開始  中間  中間  終了
```

### NFAとDFAの違い

| 特徴 | NFA | DFA |
|------|-----|-----|
| **状態数** | 少ない | 多い（指数的増加の可能性） |
| **実行速度** | 遅い（バックトラッキング） | 高速（線形時間） |
| **メモリ使用量** | 少ない | 多い |
| **実装複雑度** | 単純 | 複雑 |
| **機能性** | 高機能（後方参照等） | 基本機能のみ |

## NFA（非決定性有限オートマトン）エンジン

### NFA の設計原理

NFAエンジンは、パターンマッチング中に複数の可能な状態を同時に追跡します。

```lisp
;; NFA状態の表現
(defclass nfa-state ()
  ((state-id :initarg :id :accessor state-id)
   (transitions :initform '() :accessor transitions)
   (epsilon-transitions :initform '() :accessor epsilon-transitions)
   (final-state :initform nil :accessor final-state-p)))

;; 遷移の表現
(defclass nfa-transition ()
  ((from-state :initarg :from :accessor from-state)
   (to-state :initarg :to :accessor to-state)
   (character :initarg :char :accessor transition-character)
   (character-class :initarg :class :accessor character-class)))
```

### ε遷移の処理

ε遷移（空文字での遷移）は、NFAの重要な機能です。

```lisp
(defun epsilon-closure (states nfa)
  "ε遷移で到達可能なすべての状態を計算"
  (let ((closure (copy-list states))
        (stack (copy-list states)))
    (loop while stack do
      (let ((current (pop stack)))
        (dolist (epsilon-target (epsilon-transitions current))
          (unless (member epsilon-target closure)
            (push epsilon-target closure)
            (push epsilon-target stack)))))
    closure))

;; 使用例: 量詞の実装
(defun compile-quantifier-to-nfa (element min max)
  "量詞付き要素をNFAに変換"
  (let ((start-state (make-nfa-state))
        (end-state (make-nfa-state :final t)))

    ;; min回の必須繰り返し
    (loop for i from 0 below min do
      (connect-nfa-fragment start-state element))

    ;; max回までのオプション繰り返し
    (when max
      (loop for i from min below max do
        (add-epsilon-transition current-state end-state)
        (connect-nfa-fragment current-state element)))

    ;; ε遷移によるスキップパス
    (when (zerop min)
      (add-epsilon-transition start-state end-state))

    (make-nfa-fragment :start start-state :end end-state)))
```

### Thompson構築法

NFAの構築には、Ken Thompsonが開発した手法を使用します。

```lisp
(defun compile-pattern-to-nfa (pattern)
  "パターンをNFAに変換（Thompson構築法）"
  (case (pattern-type pattern)
    (:literal
     (make-literal-nfa (pattern-character pattern)))

    (:concatenation
     (reduce #'concatenate-nfa-fragments
             (mapcar #'compile-pattern-to-nfa (pattern-elements pattern))))

    (:alternation
     (let ((start (make-nfa-state))
           (end (make-nfa-state :final t)))
       (dolist (alternative (pattern-alternatives pattern))
         (let ((fragment (compile-pattern-to-nfa alternative)))
           (add-epsilon-transition start (fragment-start fragment))
           (add-epsilon-transition (fragment-end fragment) end)))
       (make-nfa-fragment :start start :end end)))

    (:repetition
     (compile-quantifier-to-nfa
       (compile-pattern-to-nfa (pattern-element pattern))
       (pattern-min-count pattern)
       (pattern-max-count pattern)))))

(defun make-literal-nfa (character)
  "単一文字のNFAを作成"
  (let ((start (make-nfa-state))
        (end (make-nfa-state :final t)))
    (add-transition start end character)
    (make-nfa-fragment :start start :end end)))
```

### NFA実行エンジン

```lisp
(defclass nfa-executor ()
  ((nfa :initarg :nfa :accessor executor-nfa)
   (current-states :initform '() :accessor current-states)
   (capture-groups :initform (make-hash-table) :accessor capture-groups)))

(defmethod execute-nfa ((executor nfa-executor) text)
  "NFAでテキストをマッチング"
  (setf (current-states executor)
        (epsilon-closure (list (nfa-start-state (executor-nfa executor)))
                         (executor-nfa executor)))

  (loop for char across text
        for position from 0 do
    (setf (current-states executor)
          (step-nfa-states (current-states executor) char))

    ;; 受理状態に到達した場合
    (when (some #'final-state-p (current-states executor))
      (return (make-match-result :start 0 :end (1+ position) :matched t))))

  ;; マッチしなかった場合
  (make-match-result :matched nil))

(defun step-nfa-states (states character)
  "一文字の入力で状態遷移を実行"
  (let ((next-states '()))
    (dolist (state states)
      (dolist (transition (transitions state))
        (when (character-matches-p character transition)
          (push (to-state transition) next-states))))
    (remove-duplicates (epsilon-closure next-states))))
```

## DFA（決定性有限オートマトン）エンジン

### DFAの設計原理

DFAエンジンは、各状態から各文字に対して最大一つの遷移先しか持ちません。これにより線形時間での実行が保証されます。

```lisp
;; DFA状態の表現
(defclass dfa-state ()
  ((state-id :initarg :id :accessor state-id)
   (transition-table :initform (make-hash-table) :accessor transition-table)
   (final-state :initform nil :accessor final-state-p)
   (nfa-states :initarg :nfa-states :accessor corresponding-nfa-states)))

;; DFA遷移テーブル
(defun make-transition-table (alphabet)
  "文字セットに対する遷移テーブルを作成"
  (let ((table (make-hash-table :test 'equal)))
    (dolist (char alphabet)
      (setf (gethash char table) nil))  ; 初期値はnil（遷移なし）
    table))
```

### サブセット構築法

NFAからDFAへの変換には、サブセット構築法（Powerset Construction）を使用します。

```lisp
(defun nfa-to-dfa (nfa)
  "NFAをDFAに変換（サブセット構築法）"
  (let ((dfa-states (make-hash-table :test 'equal))
        (worklist '())
        (alphabet (compute-alphabet nfa)))

    ;; 初期状態を作成
    (let* ((initial-nfa-states (epsilon-closure (list (nfa-start-state nfa))))
           (initial-dfa-state (make-dfa-state-from-nfa-states initial-nfa-states)))
      (setf (gethash initial-nfa-states dfa-states) initial-dfa-state)
      (push initial-nfa-states worklist))

    ;; 新しいDFA状態を構築
    (loop while worklist do
      (let ((current-nfa-states (pop worklist)))
        (dolist (char alphabet)
          (let ((target-nfa-states (epsilon-closure
                                     (move current-nfa-states char))))
            (when target-nfa-states
              (unless (gethash target-nfa-states dfa-states)
                (let ((new-dfa-state (make-dfa-state-from-nfa-states target-nfa-states)))
                  (setf (gethash target-nfa-states dfa-states) new-dfa-state)
                  (push target-nfa-states worklist)))

              ;; 遷移を追加
              (let ((from-dfa-state (gethash current-nfa-states dfa-states))
                    (to-dfa-state (gethash target-nfa-states dfa-states)))
                (setf (gethash char (transition-table from-dfa-state))
                      to-dfa-state)))))))

    dfa-states))

(defun move (nfa-states character)
  "指定文字での遷移先NFA状態集合を計算"
  (let ((result '()))
    (dolist (state nfa-states)
      (dolist (transition (transitions state))
        (when (character-matches-p character transition)
          (push (to-state transition) result))))
    (remove-duplicates result)))
```

### DFA最適化

#### 状態最小化

```lisp
(defun minimize-dfa (dfa)
  "DFAの状態数を最小化"
  (let ((equivalence-classes (compute-initial-partition dfa)))

    ;; 不変点に到達するまで分割を細分化
    (loop
      (let ((new-partition (refine-partition equivalence-classes dfa)))
        (if (equal new-partition equivalence-classes)
            (return equivalence-classes)
            (setf equivalence-classes new-partition))))

    ;; 最小化されたDFAを構築
    (construct-minimized-dfa equivalence-classes dfa)))

(defun compute-initial-partition (dfa)
  "初期分割（受理状態と非受理状態）を計算"
  (let ((accepting-states '())
        (non-accepting-states '()))
    (dolist (state (dfa-states dfa))
      (if (final-state-p state)
          (push state accepting-states)
          (push state non-accepting-states)))
    (remove-if #'null (list accepting-states non-accepting-states))))

(defun refine-partition (partition dfa)
  "分割を細分化"
  (let ((new-partition '()))
    (dolist (class partition)
      (let ((subclasses (split-equivalence-class class dfa partition)))
        (setf new-partition (append subclasses new-partition))))
    new-partition))
```

### DFA実行エンジン

```lisp
(defclass dfa-executor ()
  ((dfa :initarg :dfa :accessor executor-dfa)
   (current-state :initform nil :accessor current-state)))

(defmethod execute-dfa ((executor dfa-executor) text)
  "DFAでテキストをマッチング"
  (setf (current-state executor) (dfa-start-state (executor-dfa executor)))

  (loop for char across text
        for position from 0 do
    (let ((next-state (gethash char
                               (transition-table (current-state executor)))))
      (if next-state
          (setf (current-state executor) next-state)
          (return (make-match-result :matched nil))))

    ;; 受理状態チェック
    (when (final-state-p (current-state executor))
      (return (make-match-result :start 0 :end (1+ position) :matched t))))

  ;; 最終状態で受理状態かチェック
  (if (final-state-p (current-state executor))
      (make-match-result :start 0 :end (length text) :matched t)
      (make-match-result :matched nil)))
```

## ハイブリッドエンジン

### 適応的実行戦略

CL-Regexは、パターンとテキストの特性に応じてNFAとDFAを切り替えるハイブリッドエンジンを提供します。

```lisp
(defclass hybrid-engine ()
  ((nfa-engine :initarg :nfa :accessor nfa-engine)
   (dfa-engine :initarg :dfa :accessor dfa-engine)
   (strategy :initarg :strategy :initform :adaptive :accessor execution-strategy)
   (dfa-cache :initform (make-hash-table :test 'equal) :accessor dfa-cache)))

(defmethod choose-execution-engine ((engine hybrid-engine) pattern text)
  "実行エンジンを選択"
  (case (execution-strategy engine)
    (:adaptive (adaptive-engine-selection pattern text))
    (:nfa-first (nfa-with-dfa-fallback pattern text))
    (:dfa-first (dfa-with-nfa-fallback pattern text))
    (t (error "Unknown execution strategy"))))

(defun adaptive-engine-selection (pattern text)
  "適応的エンジン選択"
  (let ((pattern-complexity (analyze-pattern-complexity pattern))
        (text-size (length text)))

    (cond
      ;; 単純なパターン + 大きなテキスト → DFA
      ((and (< pattern-complexity 5) (> text-size 10000))
       :dfa)

      ;; 複雑なパターン（後方参照等） → NFA
      ((has-backreferences-p pattern)
       :nfa)

      ;; 中程度の複雑さ → DFA試行後NFA
      ((< pattern-complexity 10)
       :dfa-with-nfa-fallback)

      ;; デフォルト → NFA
      (t :nfa))))

(defmethod execute-hybrid ((engine hybrid-engine) pattern text)
  "ハイブリッド実行"
  (let ((chosen-engine (choose-execution-engine engine pattern text)))
    (case chosen-engine
      (:nfa (execute-nfa (nfa-engine engine) text))
      (:dfa (execute-dfa (dfa-engine engine) text))
      (:dfa-with-nfa-fallback
       (handler-case
           (execute-dfa (dfa-engine engine) text)
         (dfa-limitation-error ()
           (execute-nfa (nfa-engine engine) text)))))))
```

### 動的DFAキャッシュ

```lisp
(defun cached-dfa-execution (engine pattern text)
  "キャッシュ付きDFA実行"
  (let ((cached-dfa (gethash pattern (dfa-cache engine))))
    (unless cached-dfa
      (setf cached-dfa (compile-pattern-to-dfa pattern))
      (setf (gethash pattern (dfa-cache engine)) cached-dfa))

    (execute-dfa cached-dfa text)))
```

## 高度な機能

### 後方参照のサポート

```lisp
(defclass nfa-with-backreferences (nfa-executor)
  ((capture-stack :initform '() :accessor capture-stack)
   (backref-constraints :initform '() :accessor backref-constraints)))

(defmethod handle-backreference ((executor nfa-with-backreferences) group-number)
  "後方参照の処理"
  (let ((captured-text (gethash group-number (capture-groups executor))))
    (when captured-text
      (create-literal-constraint captured-text))))
```

### 先読み・後読みの実装

```lisp
(defun compile-lookahead-to-nfa (pattern positive-p)
  "先読みをNFAに変換"
  (let ((lookahead-nfa (compile-pattern-to-nfa pattern))
        (continuation-state (make-nfa-state)))

    ;; 先読み成功時の処理
    (if positive-p
        (add-conditional-epsilon-transition
          current-state continuation-state
          (lambda (context) (matches-lookahead-p lookahead-nfa context)))
        (add-conditional-epsilon-transition
          current-state continuation-state
          (lambda (context) (not (matches-lookahead-p lookahead-nfa context)))))

    continuation-state))
```

### JITコンパイル

```lisp
(defclass jit-compiler ()
  ((compilation-threshold :initarg :threshold :initform 100 :accessor threshold)
   (compiled-patterns :initform (make-hash-table) :accessor compiled-patterns)
   (execution-count :initform (make-hash-table) :accessor execution-count)))

(defmethod maybe-jit-compile ((compiler jit-compiler) pattern)
  "実行回数に基づくJITコンパイル"
  (let ((count (gethash pattern (execution-count compiler) 0)))
    (incf (gethash pattern (execution-count compiler) 0))

    (when (and (>= count (threshold compiler))
               (not (gethash pattern (compiled-patterns compiler))))
      (setf (gethash pattern (compiled-patterns compiler))
            (compile-to-native-code pattern)))))
```

## パフォーマンス分析

### 計算量の比較

| エンジン | 時間計算量 | 空間計算量 | 特徴 |
|----------|------------|------------|------|
| **NFA** | O(nm) | O(n) | nはパターン長、mはテキスト長 |
| **DFA** | O(m) | O(2^n) | 線形時間、指数的空間 |
| **ハイブリッド** | O(m) - O(nm) | O(n) - O(2^n) | 適応的 |

### ベンチマーク例

```lisp
(defun benchmark-engines (pattern text iterations)
  "エンジン性能比較"
  (let ((nfa-time (time-execution
                    (lambda () (execute-nfa pattern text))
                    iterations))
        (dfa-time (time-execution
                    (lambda () (execute-dfa pattern text))
                    iterations))
        (hybrid-time (time-execution
                       (lambda () (execute-hybrid pattern text))
                       iterations)))

    (format t "NFA: ~A ms~%" nfa-time)
    (format t "DFA: ~A ms~%" dfa-time)
    (format t "Hybrid: ~A ms~%" hybrid-time)))
```

## まとめ

CL-Regexのエンジン設計は以下の特徴を持ちます：

### NFAエンジンの利点
- **高機能性**: 後方参照、先読み・後読みの完全サポート
- **メモリ効率**: 状態数がパターンサイズに比例
- **実装の単純さ**: Thompson構築法による直接的な変換

### DFAエンジンの利点
- **高速実行**: 線形時間での実行保証
- **予測可能性**: 実行時間がテキスト長に比例
- **キャッシュ効率**: 単純な状態遷移

### ハイブリッドアプローチ
- **適応性**: パターンとデータに応じた最適なエンジン選択
- **後方互換性**: 全機能のサポートを維持
- **性能最適化**: 実行時特性に基づく動的最適化

この設計により、CL-Regexは高品質の性能と機能性を両立した正規表現エンジンを実現しています。