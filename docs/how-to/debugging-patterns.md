# パターンのデバッグ

複雑な正規表現パターンのデバッグ手法と、CL-Regexが提供するデバッグ支援機能について説明します。

## 概要

正規表現のデバッグで重要な要素：

- **パターンの可視化**: AST表示とフロー分析
- **実行トレース**: マッチング過程の追跡
- **性能分析**: ボトルネックの特定
- **テストケース**: 体系的な検証

## 基本的なデバッグ手法

### パターン構造の可視化

```lisp
;; AST（抽象構文木）の表示
(defclass pattern-debugger ()
  ((debug-mode :initarg :debug :initform nil :accessor debug-mode)
   (trace-enabled :initarg :trace :initform nil :accessor trace-enabled)
   (ast-display :initarg :ast :initform nil :accessor ast-display)))

(defmethod compile-with-debug ((debugger pattern-debugger) pattern)
  "デバッグ情報付きでパターンをコンパイル"
  (let ((ast (parse-regex pattern)))
    (when (ast-display debugger)
      (display-ast ast))
    (when (debug-mode debugger)
      (add-debug-annotations ast))
    (compile-ast ast)))

(defun display-ast (ast &optional (depth 0))
  "AST構造を階層表示"
  (let ((indent (make-string (* depth 2) :initial-element #\Space)))
    (format t "~A~A~%" indent (ast-node-type ast))
    (dolist (child (ast-children ast))
      (display-ast child (1+ depth)))))

;; 使用例
(defparameter *debugger* (make-instance 'pattern-debugger
                                       :debug t
                                       :trace t
                                       :ast t))

(compile-with-debug *debugger* "a(b|c)*d")
;; 出力例：
;; SEQUENCE
;;   LITERAL a
;;   REPETITION
;;     ALTERNATION
;;       LITERAL b
;;       LITERAL c
;;   LITERAL d
```

### 実行トレースの取得

```lisp
;; 実行トレース機能
(defclass trace-matcher (base-matcher)
  ((trace-log :initform '() :accessor trace-log)
   (step-counter :initform 0 :accessor step-counter)
   (max-steps :initarg :max-steps :initform 10000 :accessor max-steps)))

(defmethod match-with-trace ((matcher trace-matcher) text)
  "トレース付きマッチング実行"
  (setf (trace-log matcher) '())
  (setf (step-counter matcher) 0)
  (let ((*trace-enabled* t))
    (match-text matcher text)))

(defmacro trace-step (operation &body body)
  "実行ステップをトレース"
  `(when *trace-enabled*
     (push (list (incf (step-counter *current-matcher*))
                 ',operation
                 (current-position)
                 (current-state))
           (trace-log *current-matcher*))
     ,@body))

;; 使用例
(defmethod match-literal ((matcher trace-matcher) literal text position)
  (trace-step literal-match
    (when (and (< position (length text))
               (char= (char text position) literal))
      (1+ position))))

;; トレースログの表示
(defun display-trace (matcher)
  "トレースログを表示"
  (format t "=== マッチングトレース ===~%")
  (dolist (entry (reverse (trace-log matcher)))
    (destructuring-bind (step operation position state) entry
      (format t "Step ~3D: ~A at pos ~D, state: ~A~%"
              step operation position state))))
```

## 高度なデバッグ機能

### インタラクティブデバッガ

```lisp
;; インタラクティブデバッガクラス
(defclass interactive-debugger (trace-matcher)
  ((breakpoints :initform '() :accessor breakpoints)
   (interactive-mode :initform nil :accessor interactive-mode)
   (step-mode :initform nil :accessor step-mode)))

(defmethod add-breakpoint ((debugger interactive-debugger) condition)
  "ブレークポイントを追加"
  (push condition (breakpoints debugger)))

(defmethod check-breakpoint ((debugger interactive-debugger) state)
  "ブレークポイント条件をチェック"
  (some (lambda (condition) (funcall condition state))
        (breakpoints debugger)))

(defmacro debugger-break (&optional (debugger '*current-debugger*))
  "デバッガブレーク"
  `(when (and ,debugger (interactive-mode ,debugger))
     (debugger-prompt ,debugger)))

(defun debugger-prompt (debugger)
  "デバッガプロンプト表示"
  (format t "~%=== CL-Regex Debugger ===~%")
  (format t "Commands: (c)ontinue, (s)tep, (i)nspect, (b)reakpoint, (q)uit~%")
  (loop
    (format t "debug> ")
    (let ((command (read-line)))
      (case (char command 0)
        (#\c (return))  ; continue
        (#\s (setf (step-mode debugger) t) (return))  ; step
        (#\i (inspect-current-state debugger))  ; inspect
        (#\b (add-interactive-breakpoint debugger))  ; breakpoint
        (#\q (error "デバッグ中断"))))))  ; quit

(defmethod inspect-current-state ((debugger interactive-debugger))
  "現在の状態を表示"
  (format t "Position: ~A~%" (current-position))
  (format t "Pattern: ~A~%" (pattern debugger))
  (format t "State: ~A~%" (current-state))
  (format t "Stack: ~A~%" (state-stack)))
```

### パターン最適化の可視化

```lisp
;; 最適化プロセスの可視化
(defclass optimization-visualizer ()
  ((optimization-log :initform '() :accessor optimization-log)
   (show-intermediate :initarg :show-intermediate
                      :initform t
                      :accessor show-intermediate)))

(defmethod visualize-optimization ((visualizer optimization-visualizer) pattern)
  "最適化プロセスを可視化"
  (let ((current-ast (parse-regex pattern)))
    (format t "=== 最適化プロセス ===~%")
    (format t "Original:~%")
    (display-ast current-ast)

    (dolist (optimizer *optimization-passes*)
      (let ((old-ast current-ast))
        (setf current-ast (apply-optimization optimizer current-ast))
        (unless (ast-equal old-ast current-ast)
          (format t "~%After ~A:~%" (optimizer-name optimizer))
          (when (show-intermediate visualizer)
            (display-ast current-ast))
          (push (list (optimizer-name optimizer) old-ast current-ast)
                (optimization-log visualizer)))))
    current-ast))

;; 最適化効果の分析
(defmethod analyze-optimization-impact ((visualizer optimization-visualizer))
  "最適化の効果を分析"
  (loop for (optimizer old-ast new-ast) in (optimization-log visualizer) do
    (let ((complexity-reduction (- (ast-complexity old-ast)
                                   (ast-complexity new-ast))))
      (format t "~A: complexity reduced by ~D~%"
              optimizer complexity-reduction))))
```

### 性能分析とプロファイリング

```lisp
;; 性能プロファイラ
(defclass performance-profiler ()
  ((timing-data :initform (make-hash-table) :accessor timing-data)
   (memory-usage :initform '() :accessor memory-usage)
   (operation-counts :initform (make-hash-table) :accessor operation-counts)))

(defmacro profile-operation (operation &body body)
  "操作の性能をプロファイル"
  (let ((start-time (gensym))
        (start-memory (gensym))
        (result (gensym)))
    `(let ((,start-time (get-internal-real-time))
           (,start-memory (sb-kernel:dynamic-usage))
           (,result (progn ,@body)))
       (record-timing ',operation ,start-time)
       (record-memory-usage ',operation ,start-memory)
       (increment-operation-count ',operation)
       ,result)))

(defmethod record-timing ((profiler performance-profiler) operation start-time)
  "実行時間を記録"
  (let ((duration (- (get-internal-real-time) start-time)))
    (push duration (gethash operation (timing-data profiler)))))

(defmethod generate-performance-report ((profiler performance-profiler))
  "性能レポートを生成"
  (format t "=== 性能分析レポート ===~%")
  (loop for operation being the hash-keys of (timing-data profiler) do
    (let ((times (gethash operation (timing-data profiler))))
      (format t "~A:~%" operation)
      (format t "  平均時間: ~,3F ms~%"
              (/ (reduce #'+ times) (length times) internal-time-units-per-second 1000))
      (format t "  実行回数: ~D~%"
              (gethash operation (operation-counts profiler) 0)))))

;; 使用例
(defmethod match-text :around ((matcher base-matcher) text)
  (profile-operation match-text
    (call-next-method)))
```

## 視覚的デバッグツール

### 状態遷移図の生成

```lisp
;; 状態遷移図ジェネレータ
(defclass state-diagram-generator ()
  ((states :initform '() :accessor states)
   (transitions :initform '() :accessor transitions)
   (output-format :initarg :format :initform :graphviz :accessor output-format)))

(defmethod generate-state-diagram ((generator state-diagram-generator) automaton)
  "NFAまたはDFAの状態遷移図を生成"
  (collect-states generator automaton)
  (collect-transitions generator automaton)
  (output-diagram generator))

(defmethod output-diagram ((generator state-diagram-generator))
  "Graphviz形式で図を出力"
  (case (output-format generator)
    (:graphviz (output-graphviz generator))
    (:ascii (output-ascii generator))
    (:json (output-json generator))))

(defmethod output-graphviz ((generator state-diagram-generator))
  "Graphviz DOT形式で出力"
  (format t "digraph RegexAutomaton {~%")
  (format t "  rankdir=LR;~%")

  ;; 状態の定義
  (dolist (state (states generator))
    (format t "  ~A [label=\"~A\"~A];~%"
            (state-id state)
            (state-label state)
            (if (final-state-p state) ", shape=doublecircle" "")))

  ;; 遷移の定義
  (dolist (transition (transitions generator))
    (format t "  ~A -> ~A [label=\"~A\"];~%"
            (transition-from transition)
            (transition-to transition)
            (transition-label transition)))

  (format t "}~%"))
```

### マッチング過程の可視化

```lisp
;; マッチング可視化
(defclass match-visualizer ()
  ((text :initarg :text :accessor match-text)
   (pattern :initarg :pattern :accessor match-pattern)
   (steps :initform '() :accessor match-steps)
   (current-position :initform 0 :accessor current-position)))

(defmethod visualize-match ((visualizer match-visualizer))
  "マッチング過程を段階的に表示"
  (dolist (step (match-steps visualizer))
    (display-match-step step)))

(defmethod display-match-step (step)
  "個別のマッチングステップを表示"
  (destructuring-bind (position operation result text) step
    (format t "Position ~D: ~A~%" position operation)
    (format t "  ~A~%" text)
    (format t "  ~V@{~C~:*~}^~%" position #\Space)
    (format t "  Result: ~A~%~%" result)))

;; 使用例
(defun debug-match (pattern text)
  "デバッグモードでマッチングを実行"
  (let ((visualizer (make-instance 'match-visualizer
                                   :pattern pattern
                                   :text text))
        (debugger (make-instance 'interactive-debugger
                                :debug t
                                :trace t)))
    (setf *current-visualizer* visualizer)
    (match-with-trace debugger text)
    (visualize-match visualizer)))
```

## テスト駆動デバッグ

### 段階的テストケース

```lisp
;; テスト駆動デバッグフレームワーク
(defclass debug-test-suite ()
  ((test-cases :initform '() :accessor test-cases)
   (current-pattern :initarg :pattern :accessor current-pattern)
   (debug-mode :initform t :accessor debug-mode)))

(defmacro add-debug-test (suite description pattern text expected)
  "デバッグテストケースを追加"
  `(push (list ,description ,pattern ,text ,expected)
         (test-cases ,suite)))

(defmethod run-debug-tests ((suite debug-test-suite))
  "デバッグテストを実行"
  (dolist (test-case (test-cases suite))
    (destructuring-bind (description pattern text expected) test-case
      (format t "Testing: ~A~%" description)
      (let ((result (if (debug-mode suite)
                        (debug-match pattern text)
                        (regex-match pattern text))))
        (if (equal result expected)
            (format t "  PASS~%")
            (format t "  FAIL: expected ~A, got ~A~%" expected result))))))

;; 複雑パターンの段階的構築
(defun build-pattern-incrementally (components)
  "パターンを段階的に構築してテスト"
  (let ((current-pattern "")
        (test-texts '("abc" "123" "a1b2c3" "")))
    (dolist (component components)
      (setf current-pattern (concatenate 'string current-pattern component))
      (format t "~%Pattern: ~A~%" current-pattern)
      (dolist (text test-texts)
        (let ((result (regex-match current-pattern text)))
          (format t "  '~A' -> ~A~%" text result))))))

;; 使用例
(build-pattern-incrementally '("a" "+" "b" "*" "c" "?"))
```

## エラー診断とトラブルシューティング

### 一般的な問題の診断

```lisp
;; 問題診断システム
(defclass pattern-analyzer ()
  ((common-issues :initform (load-common-issues) :accessor common-issues)
   (suggestions :initform '() :accessor suggestions)))

(defmethod analyze-pattern ((analyzer pattern-analyzer) pattern)
  "パターンの潜在的問題を分析"
  (setf (suggestions analyzer) '())

  ;; 一般的な問題をチェック
  (check-catastrophic-backtracking analyzer pattern)
  (check-ambiguous-alternation analyzer pattern)
  (check-inefficient-quantifiers analyzer pattern)
  (check-unicode-issues analyzer pattern)

  (suggestions analyzer))

(defmethod check-catastrophic-backtracking ((analyzer pattern-analyzer) pattern)
  "破滅的バックトラッキングをチェック"
  (when (regex-match "\\([^)]*\\*[^)]*\\)\\+" pattern)
    (push "破滅的バックトラッキングの可能性があります。量詞の組み合わせを見直してください。"
          (suggestions analyzer))))

(defmethod check-ambiguous-alternation ((analyzer pattern-analyzer) pattern)
  "曖昧な選択をチェック"
  (when (regex-match "\\([^|]*\\|[^|]*\\*\\)" pattern)
    (push "選択肢が曖昧です。より具体的なパターンを使用してください。"
          (suggestions analyzer))))

;; 自動修正提案
(defmethod suggest-fixes ((analyzer pattern-analyzer) pattern)
  "パターンの修正案を提案"
  (let ((fixes '()))
    (when (regex-match ".*\\*.*\\*.*" pattern)
      (push (cons "連続する * を + に変更することを検討"
                  (regex-replace "\\*\\*" pattern "+"))
            fixes))

    (when (regex-match "\\[a-zA-Z\\]" pattern)
      (push (cons "文字クラスを \\p{L} に変更（Unicode対応）"
                  (regex-replace "\\[a-zA-Z\\]" pattern "\\p{L}"))
            fixes))

    fixes))
```

## デバッグユーティリティ

### 便利なヘルパー関数

```lisp
;; デバッグヘルパー関数
(defun quick-debug (pattern text)
  "クイックデバッグ実行"
  (let ((debugger (make-instance 'trace-matcher :trace t)))
    (match-with-trace debugger text)
    (display-trace debugger)))

(defun pattern-complexity (pattern)
  "パターンの複雑度を計算"
  (let ((ast (parse-regex pattern)))
    (calculate-complexity ast)))

(defun estimate-performance (pattern text-length)
  "性能を見積もり"
  (let ((complexity (pattern-complexity pattern)))
    (format t "推定実行時間: ~A~%"
            (case complexity
              ((1 2 3) "非常に高速")
              ((4 5 6) "高速")
              ((7 8 9) "中程度")
              (t "低速の可能性")))))

(defun validate-pattern (pattern)
  "パターンの妥当性をチェック"
  (handler-case
      (progn
        (parse-regex pattern)
        (format t "パターンは有効です~%")
        t)
    (error (condition)
      (format t "パターンエラー: ~A~%" condition)
      nil)))

;; バッチデバッグ
(defun debug-multiple-patterns (patterns test-text)
  "複数パターンをまとめてデバッグ"
  (dolist (pattern patterns)
    (format t "~%=== Pattern: ~A ===~%" pattern)
    (quick-debug pattern test-text)))
```

## まとめ

CL-Regexのデバッグ機能により：

- **可視化**: パターン構造と実行過程の理解
- **トレース**: 詳細な実行追跡
- **分析**: 性能とエラーの診断
- **対話**: インタラクティブなデバッグ

これらのツールを活用することで、複雑な正規表現パターンでも効率的にデバッグできます。

次は [CLOSとの統合](./clos-integration.md) について学習しましょう。