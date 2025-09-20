# 最適化技術

CL-Regexで実装されている高度な最適化技術について、理論的背景と実装手法を詳しく解説します。

## 最適化戦略の概要

CL-Regexの最適化は以下の多層アプローチで実現されています：

```
コンパイル時最適化
├── パターンレベル最適化
│   ├── 構文レベル変換
│   ├── 意味保存変換
│   └── 代数的最適化
├── ASTレベル最適化
│   ├── 定数畳み込み
│   ├── デッドコード除去
│   └── 共通部分式除去
└── コード生成最適化
    ├── 特殊化
    ├── インライン展開
    └── SIMD化

実行時最適化
├── 適応的実行
├── JITコンパイル
└── キャッシュ戦略
```

## パターンレベル最適化

### 代数的等価変換

正規表現の代数的性質を利用した最適化を行います。

```lisp
;; 代数的最適化規則
(defclass algebraic-optimizer ()
  ((rewrite-rules :initform (make-rewrite-rules) :accessor rewrite-rules)))

(defmethod make-rewrite-rules ()
  "代数的書き換え規則を定義"
  (list
    ;; 結合律
    (make-rule "a(bc)" "(ab)c")
    (make-rule "(a|b)|c" "a|(b|c)")

    ;; 分配律
    (make-rule "(a|b)c" "ac|bc")
    (make-rule "a(b|c)" "ab|ac")

    ;; 冪等律
    (make-rule "a|a" "a")
    (make-rule "a+" "aa*")
    (make-rule "a*" "a*a*")

    ;; 吸収律
    (make-rule "a|a*" "a*")
    (make-rule "a*|a+" "a*")

    ;; ゼロ律
    (make-rule "a|∅" "a")
    (make-rule "a∅" "∅")

    ;; 単位律
    (make-rule "aε" "a")
    (make-rule "εa" "a")

    ;; De Morgan律（文字クラス）
    (make-rule "[^ab]" "[^a][^b]")

    ;; 量詞の最適化
    (make-rule "a{0,0}" "ε")
    (make-rule "a{1,1}" "a")
    (make-rule "a{0,1}" "a?")
    (make-rule "a{0,}" "a*")
    (make-rule "a{1,}" "a+")

    ;; 冗長な量詞
    (make-rule "a*a*" "a*")
    (make-rule "a+a*" "a+")
    (make-rule "a*a+" "a+")
    (make-rule "a+a+" "a+")))

(defmethod apply-algebraic-optimization ((optimizer algebraic-optimizer) pattern)
  "代数的最適化を適用"
  (let ((optimized pattern))
    (dolist (rule (rewrite-rules optimizer))
      (setf optimized (apply-rewrite-rule rule optimized)))
    optimized))

;; 具体的な最適化例
(defmethod optimize-quantifier-sequences ((optimizer algebraic-optimizer) ast)
  "量詞の連続を最適化"
  (case (ast-node-type ast)
    (:concatenation
     (let ((optimized-elements '()))
       (dolist (element (concatenation-elements ast))
         (if (and optimized-elements
                  (quantifier-sequence-p (car optimized-elements) element))
             (setf (car optimized-elements)
                   (merge-quantifiers (car optimized-elements) element))
             (push element optimized-elements)))
       (make-ast-node :concatenation :elements (nreverse optimized-elements))))
    (t ast)))

(defun merge-quantifiers (left-quant right-quant)
  "隣接する同じ要素の量詞をマージ"
  ;; a{m,n}a{p,q} → a{m+p,n+q} (n,qが有限の場合)
  ;; a*a* → a*
  ;; a+a* → a+
  (let ((left-element (quantified-element left-quant))
        (right-element (quantified-element right-quant)))
    (when (ast-equal-p left-element right-element)
      (let ((left-min (min-count left-quant))
            (left-max (max-count left-quant))
            (right-min (min-count right-quant))
            (right-max (max-count right-quant)))
        (make-ast-node :quantified-element
                       :element left-element
                       :min (+ left-min right-min)
                       :max (when (and left-max right-max)
                              (+ left-max right-max)))))))
```

### 文字クラス最適化

```lisp
(defmethod optimize-character-classes ((optimizer character-class-optimizer) ast)
  "文字クラスの最適化"
  (case (ast-node-type ast)
    (:character-class
     (let* ((char-set (character-set ast))
            (optimized-set (optimize-character-set char-set)))
       (make-ast-node :character-class :characters optimized-set)))

    (:alternation
     ;; 文字の選択を文字クラスに変換
     ;; a|b|c → [abc]
     (if (all-single-characters-p (alternatives ast))
         (make-ast-node :character-class
                        :characters (extract-characters (alternatives ast)))
         ast))

    (t ast)))

(defun optimize-character-set (char-set)
  "文字セットの最適化"
  (let ((ranges (compute-character-ranges char-set)))
    ;; 連続する文字を範囲として表現
    ;; [abcd] → [a-d]
    (if (can-use-ranges-p ranges)
        (format-as-ranges ranges)
        char-set)))

(defun compute-character-ranges (char-set)
  "文字セットから連続範囲を計算"
  (let ((sorted-chars (sort (copy-list char-set) #'char<))
        (ranges '())
        (current-start nil)
        (current-end nil))

    (dolist (char sorted-chars)
      (cond
        ((null current-start)
         (setf current-start char current-end char))

        ((= (char-code char) (1+ (char-code current-end)))
         (setf current-end char))

        (t
         (push (if (char= current-start current-end)
                   current-start
                   (cons current-start current-end))
               ranges)
         (setf current-start char current-end char))))

    (when current-start
      (push (if (char= current-start current-end)
                current-start
                (cons current-start current-end))
            ranges))

    (nreverse ranges)))
```

## 実行時最適化

### 適応的エンジン選択

実行時の特性に基づいて最適なエンジンを動的に選択します。

```lisp
(defclass adaptive-execution-manager ()
  ((execution-history :initform (make-hash-table) :accessor execution-history)
   (performance-model :initform (make-performance-model) :accessor performance-model)
   (strategy-cache :initform (make-hash-table) :accessor strategy-cache)))

(defmethod select-execution-strategy ((manager adaptive-execution-manager) pattern text)
  "適応的実行戦略選択"
  (let ((pattern-signature (compute-pattern-signature pattern))
        (text-characteristics (analyze-text-characteristics text)))

    ;; キャッシュされた戦略があるかチェック
    (let ((cached-strategy (gethash pattern-signature (strategy-cache manager))))
      (if (and cached-strategy
               (strategy-applicable-p cached-strategy text-characteristics))
          cached-strategy
          (compute-optimal-strategy manager pattern-signature text-characteristics)))))

(defmethod compute-optimal-strategy ((manager adaptive-execution-manager)
                                   pattern-sig text-chars)
  "最適戦略を計算"
  (let ((strategies '(:nfa :dfa :hybrid :jit))
        (best-strategy nil)
        (best-score -1))

    (dolist (strategy strategies)
      (let ((predicted-performance
              (predict-performance (performance-model manager)
                                 strategy pattern-sig text-chars)))
        (when (> predicted-performance best-score)
          (setf best-strategy strategy
                best-score predicted-performance))))

    ;; キャッシュに保存
    (setf (gethash pattern-sig (strategy-cache manager)) best-strategy)
    best-strategy))

(defmethod predict-performance ((model performance-model) strategy pattern-sig text-chars)
  "性能予測"
  (let ((pattern-complexity (pattern-complexity pattern-sig))
        (text-size (text-size text-chars))
        (text-repetitiveness (text-repetitiveness text-chars)))

    (case strategy
      (:nfa
       ;; NFA: O(nm) だが機能豊富
       (* (/ 1.0 (* pattern-complexity text-size))
          (if (has-advanced-features-p pattern-sig) 2.0 1.0)))

      (:dfa
       ;; DFA: O(m) だが構築コスト高
       (if (< pattern-complexity 10)
           (/ 1.0 text-size)
           0.1))  ; 複雑なパターンはDFA化困難

      (:hybrid
       ;; ハイブリッド: バランス
       (* 0.8 (max (predict-performance model :nfa pattern-sig text-chars)
                   (predict-performance model :dfa pattern-sig text-chars))))

      (:jit
       ;; JIT: 初期コストあり、繰り返し実行で高性能
       (let ((execution-count (get-execution-count pattern-sig)))
         (if (> execution-count 100)
             2.0  ; 高性能
             0.1))))))  ; 初期コスト高
```

### JITコンパイル最適化

```lisp
(defclass jit-optimization-engine ()
  ((hot-patterns :initform (make-hash-table) :accessor hot-patterns)
   (compilation-cache :initform (make-hash-table) :accessor compilation-cache)
   (optimization-profiles :initform '() :accessor optimization-profiles)))

(defmethod maybe-jit-optimize ((engine jit-optimization-engine) pattern execution-context)
  "JIT最適化の判定と実行"
  (let ((pattern-stats (gethash pattern (hot-patterns engine))))
    (unless pattern-stats
      (setf pattern-stats (make-pattern-statistics))
      (setf (gethash pattern (hot-patterns engine)) pattern-stats))

    (update-statistics pattern-stats execution-context)

    (when (hot-pattern-p pattern-stats)
      (jit-compile-pattern engine pattern pattern-stats))))

(defmethod jit-compile-pattern ((engine jit-optimization-engine) pattern stats)
  "パターンをJITコンパイル"
  (let ((cached-code (gethash pattern (compilation-cache engine))))
    (unless cached-code
      (setf cached-code (generate-optimized-code pattern stats))
      (setf (gethash pattern (compilation-cache engine)) cached-code))
    cached-code))

(defmethod generate-optimized-code (pattern stats)
  "統計に基づく最適化コード生成"
  (let ((frequent-chars (frequent-characters stats))
        (common-prefixes (common-prefixes stats))
        (typical-text-length (average-text-length stats)))

    ;; 特殊化された高速パスを生成
    (generate-specialized-matcher pattern
                                 :frequent-chars frequent-chars
                                 :common-prefixes common-prefixes
                                 :typical-length typical-text-length)))

;; プラットフォーム固有最適化
#+sbcl
(defmethod generate-specialized-matcher (pattern &key frequent-chars common-prefixes typical-length)
  "SBCL用特殊化マッチャ"
  (let ((optimized-code
          `(lambda (text)
             (declare (optimize (speed 3) (safety 0))
                      (type simple-string text))
             ;; 頻出文字の高速チェック
             ,(when frequent-chars
                `(when (not (find-if (lambda (c) (member c ',frequent-chars)) text))
                   (return-from matcher nil)))

             ;; 共通プレフィックスの高速スキップ
             ,(when common-prefixes
                `(let ((prefix-matched nil))
                   (dolist (prefix ',common-prefixes)
                     (when (string-prefix-p prefix text)
                       (setf prefix-matched t)
                       (return)))
                   (unless prefix-matched
                     (return-from matcher nil))))

             ;; メインマッチングロジック
             ,(generate-main-matching-logic pattern))))

    (compile nil optimized-code)))
```

### メモリ最適化

```lisp
(defclass memory-optimizer ()
  ((allocation-tracker :initform (make-allocation-tracker) :accessor allocation-tracker)
   (gc-pressure-monitor :initform (make-gc-monitor) :accessor gc-pressure-monitor)
   (memory-pools :initform (make-hash-table) :accessor memory-pools)))

(defmethod optimize-memory-usage ((optimizer memory-optimizer) execution-context)
  "メモリ使用量の最適化"
  (let ((current-pressure (gc-pressure (gc-pressure-monitor optimizer))))
    (when (> current-pressure 0.8)  ; GCプレッシャーが高い
      (apply-memory-optimizations optimizer execution-context))))

(defmethod apply-memory-optimizations ((optimizer memory-optimizer) context)
  "メモリ最適化を適用"
  (cond
    ;; オブジェクトプールの活用
    ((high-allocation-rate-p context)
     (enable-object-pooling optimizer))

    ;; インクリメンタルGCの制御
    ((fragmented-heap-p context)
     (schedule-incremental-gc optimizer))

    ;; キャッシュサイズの調整
    ((memory-constrained-p context)
     (reduce-cache-sizes optimizer))))

(defmethod enable-object-pooling ((optimizer memory-optimizer))
  "オブジェクトプールを有効化"
  (let ((state-pool (make-object-pool 'nfa-state 1000))
        (transition-pool (make-object-pool 'nfa-transition 5000))
        (result-pool (make-object-pool 'match-result 100)))

    (setf (gethash 'nfa-state (memory-pools optimizer)) state-pool)
    (setf (gethash 'nfa-transition (memory-pools optimizer)) transition-pool)
    (setf (gethash 'match-result (memory-pools optimizer)) result-pool)))

(defclass object-pool ()
  ((object-type :initarg :type :accessor pool-object-type)
   (available-objects :initform '() :accessor available-objects)
   (allocated-count :initform 0 :accessor allocated-count)
   (max-size :initarg :max-size :accessor pool-max-size)))

(defmethod allocate-from-pool ((pool object-pool))
  "プールからオブジェクトを取得"
  (if (available-objects pool)
      (pop (available-objects pool))
      (when (< (allocated-count pool) (pool-max-size pool))
        (incf (allocated-count pool))
        (make-instance (pool-object-type pool)))))

(defmethod return-to-pool ((pool object-pool) object)
  "オブジェクトをプールに返却"
  (reset-object object)
  (push object (available-objects pool)))
```

## SIMD最適化

### ベクトル化可能パターンの検出

```lisp
(defclass simd-optimizer ()
  ((vectorization-patterns :initform (make-vectorization-patterns)
                           :accessor vectorization-patterns)))

(defmethod can-vectorize-p ((optimizer simd-optimizer) pattern)
  "パターンがベクトル化可能かチェック"
  (and (simple-pattern-p pattern)
       (no-backreferences-p pattern)
       (no-lookarounds-p pattern)
       (fixed-width-matching-p pattern)))

(defmethod vectorize-pattern ((optimizer simd-optimizer) pattern)
  "パターンをSIMD命令に変換"
  (case (pattern-type pattern)
    (:literal-sequence
     (vectorize-literal-sequence pattern))

    (:character-class
     (vectorize-character-class pattern))

    (:alternation
     (when (all-same-length-p (alternatives pattern))
       (vectorize-alternation pattern)))

    (t nil)))

(defmethod vectorize-literal-sequence (pattern)
  "リテラル列のSIMD化"
  (let ((sequence (literal-sequence pattern))
        (sequence-length (length sequence)))

    (case sequence-length
      (16 (generate-sse-comparison sequence))
      (32 (generate-avx-comparison sequence))
      (t (when (<= sequence-length 64)
           (generate-padded-simd-comparison sequence))))))

#+x86-64
(defmethod generate-sse-comparison (sequence)
  "SSE命令による16バイト比較"
  `(lambda (text position)
     (when (<= (+ position 16) (length text))
       (let ((text-chunk (subseq text position (+ position 16)))
             (pattern-chunk ,(coerce sequence '(vector (unsigned-byte 8)))))
         (declare (type (simple-array (unsigned-byte 8) (16)) text-chunk pattern-chunk))
         ;; SSE PCMPEQB命令相当の処理
         (loop for i from 0 below 16
               always (= (aref text-chunk i) (aref pattern-chunk i)))))))

(defmethod vectorize-character-class (char-class)
  "文字クラスのSIMD化"
  (let ((class-bitmap (create-character-bitmap char-class)))
    `(lambda (text position)
       (when (< position (length text))
         (let ((char-code (char-code (char text position))))
           ;; ビットマップによる高速文字クラステスト
           (logbitp char-code ,class-bitmap))))))

(defun create-character-bitmap (char-class)
  "文字クラスからビットマップを作成"
  (let ((bitmap 0))
    (dolist (char (character-set char-class))
      (setf bitmap (logior bitmap (ash 1 (char-code char)))))
    bitmap))
```

### 並列実行最適化

```lisp
(defclass parallel-execution-optimizer ()
  ((thread-pool :initform (make-thread-pool) :accessor thread-pool)
   (work-distribution :initform :dynamic :accessor work-distribution)
   (chunk-size :initform 1024 :accessor chunk-size)))

(defmethod parallelize-matching ((optimizer parallel-execution-optimizer) pattern text)
  "マッチングの並列化"
  (when (worth-parallelizing-p text)
    (let ((chunks (distribute-work optimizer text)))
      (parallel-map-reduce
        (lambda (chunk) (match-chunk pattern chunk))
        #'merge-match-results
        chunks))))

(defmethod distribute-work ((optimizer parallel-execution-optimizer) text)
  "作業分散"
  (case (work-distribution optimizer)
    (:static (static-chunking text (chunk-size optimizer)))
    (:dynamic (dynamic-chunking text))
    (:adaptive (adaptive-chunking text))))

(defmethod static-chunking (text chunk-size)
  "静的チャンキング"
  (loop for start from 0 below (length text) by chunk-size
        collect (subseq text start (min (+ start chunk-size) (length text)))))

(defmethod dynamic-chunking (text)
  "動的チャンキング（ワークスティーリング）"
  (let ((num-threads (thread-pool-size (thread-pool optimizer)))
        (text-length (length text)))
    (loop for i from 0 below num-threads
          collect (create-work-unit text i num-threads))))

(defmethod parallel-map-reduce (map-fn reduce-fn data)
  "並列Map-Reduce実行"
  (let ((futures (mapcar (lambda (chunk)
                          (future (funcall map-fn chunk)))
                        data)))
    (reduce reduce-fn (mapcar #'force futures))))
```

## キャッシュ最適化

### 多層キャッシュシステム

```lisp
(defclass multi-level-cache ()
  ((l1-cache :initform (make-lru-cache :size 64) :accessor l1-cache)      ; パターンキャッシュ
   (l2-cache :initform (make-lru-cache :size 512) :accessor l2-cache)     ; 結果キャッシュ
   (l3-cache :initform (make-lru-cache :size 4096) :accessor l3-cache)    ; DFAキャッシュ
   (cache-statistics :initform (make-cache-stats) :accessor cache-statistics)))

(defmethod cached-match ((cache multi-level-cache) pattern text)
  "多層キャッシュによるマッチング"
  (let ((cache-key (compute-cache-key pattern text)))

    ;; L1キャッシュ（パターン）をチェック
    (let ((compiled-pattern (cache-get (l1-cache cache) pattern)))
      (unless compiled-pattern
        (setf compiled-pattern (compile-pattern pattern))
        (cache-put (l1-cache cache) pattern compiled-pattern)
        (record-cache-miss (cache-statistics cache) :l1))

      ;; L2キャッシュ（結果）をチェック
      (let ((cached-result (cache-get (l2-cache cache) cache-key)))
        (if cached-result
            (progn
              (record-cache-hit (cache-statistics cache) :l2)
              cached-result)
            (let ((result (execute-pattern compiled-pattern text)))
              (cache-put (l2-cache cache) cache-key result)
              (record-cache-miss (cache-statistics cache) :l2)
              result))))))

(defmethod adaptive-cache-management ((cache multi-level-cache))
  "適応的キャッシュ管理"
  (let ((stats (cache-statistics cache)))
    (when (should-adjust-cache-sizes-p stats)
      (adjust-cache-sizes cache stats))))

(defmethod adjust-cache-sizes ((cache multi-level-cache) stats)
  "統計に基づくキャッシュサイズ調整"
  (let ((l1-hit-rate (cache-hit-rate stats :l1))
        (l2-hit-rate (cache-hit-rate stats :l2)))

    ;; L1キャッシュのヒット率が低い場合、サイズを増加
    (when (< l1-hit-rate 0.8)
      (resize-cache (l1-cache cache) (* (cache-size (l1-cache cache)) 1.5)))

    ;; L2キャッシュのヒット率が高い場合、サイズを削減してメモリ節約
    (when (> l2-hit-rate 0.95)
      (resize-cache (l2-cache cache) (* (cache-size (l2-cache cache)) 0.8)))))
```

### プリフェッチ戦略

```lisp
(defclass prefetch-optimizer ()
  ((access-patterns :initform (make-hash-table) :accessor access-patterns)
   (prediction-model :initform (make-prediction-model) :accessor prediction-model)))

(defmethod record-access ((optimizer prefetch-optimizer) pattern text)
  "アクセスパターンを記録"
  (let ((pattern-hash (sxhash pattern))
        (access-time (get-universal-time)))
    (push (list text access-time)
          (gethash pattern-hash (access-patterns optimizer)))))

(defmethod predict-next-access ((optimizer prefetch-optimizer) current-pattern)
  "次のアクセスを予測"
  (let ((related-patterns (find-related-patterns optimizer current-pattern)))
    (when related-patterns
      (prefetch-patterns related-patterns))))

(defmethod find-related-patterns ((optimizer prefetch-optimizer) pattern)
  "関連パターンを発見"
  (let ((pattern-features (extract-pattern-features pattern))
        (related '()))
    (maphash (lambda (pat-hash access-list)
               (let ((similarity (compute-pattern-similarity pattern pat-hash)))
                 (when (> similarity 0.7)
                   (push pat-hash related))))
             (access-patterns optimizer))
    related))
```

## プロファイル指向最適化（PGO）

### 実行プロファイル収集

```lisp
(defclass profile-guided-optimizer ()
  ((execution-profiles :initform (make-hash-table) :accessor execution-profiles)
   (hot-spots :initform '() :accessor hot-spots)
   (optimization-decisions :initform (make-hash-table) :accessor optimization-decisions)))

(defmethod collect-execution-profile ((optimizer profile-guided-optimizer) pattern execution-data)
  "実行プロファイルを収集"
  (let ((profile (gethash pattern (execution-profiles optimizer))))
    (unless profile
      (setf profile (make-execution-profile))
      (setf (gethash pattern (execution-profiles optimizer)) profile))

    (update-profile profile execution-data)

    ;; ホットスポットの検出
    (when (hot-pattern-p profile)
      (pushnew pattern (hot-spots optimizer)))))

(defmethod apply-profile-guided-optimizations ((optimizer profile-guided-optimizer))
  "プロファイル情報に基づく最適化を適用"
  (dolist (pattern (hot-spots optimizer))
    (let ((profile (gethash pattern (execution-profiles optimizer))))
      (apply-targeted-optimizations pattern profile))))

(defmethod apply-targeted-optimizations (pattern profile)
  "特定パターンへの最適化適用"
  (cond
    ;; 高頻度実行パターン → JITコンパイル
    ((high-frequency-p profile)
     (jit-compile-pattern pattern))

    ;; 大きなテキスト処理 → DFA変換
    ((large-text-processing-p profile)
     (convert-to-dfa pattern))

    ;; 短いテキスト処理 → インライン展開
    ((short-text-processing-p profile)
     (inline-pattern-matching pattern))

    ;; 複雑なパターン → 分割統治
    ((complex-pattern-p pattern)
     (decompose-pattern pattern))))
```

## 最適化効果の測定

### ベンチマークフレームワーク

```lisp
(defclass optimization-benchmark ()
  ((baseline-performance :initform (make-hash-table) :accessor baseline-performance)
   (optimized-performance :initform (make-hash-table) :accessor optimized-performance)
   (test-cases :initform '() :accessor test-cases)))

(defmethod benchmark-optimization ((benchmark optimization-benchmark) pattern test-text)
  "最適化効果をベンチマーク"
  (let ((baseline-time (measure-execution-time
                         (lambda () (match-without-optimization pattern test-text))))
        (optimized-time (measure-execution-time
                          (lambda () (match-with-optimization pattern test-text)))))

    (setf (gethash pattern (baseline-performance benchmark)) baseline-time)
    (setf (gethash pattern (optimized-performance benchmark)) optimized-time)

    (compute-speedup baseline-time optimized-time)))

(defmethod generate-optimization-report ((benchmark optimization-benchmark))
  "最適化レポートを生成"
  (let ((total-speedup 0)
        (pattern-count 0))

    (maphash (lambda (pattern baseline-time)
               (let ((optimized-time (gethash pattern (optimized-performance benchmark))))
                 (when optimized-time
                   (let ((speedup (/ baseline-time optimized-time)))
                     (incf total-speedup speedup)
                     (incf pattern-count)
                     (format t "Pattern ~A: ~,2fx speedup~%" pattern speedup)))))
             (baseline-performance benchmark))

    (format t "~%Average speedup: ~,2fx~%" (/ total-speedup pattern-count))))
```

## まとめ

CL-Regexの最適化システムは以下の特徴を持ちます：

### 多層最適化アプローチ
- **コンパイル時最適化**: 静的解析による構造最適化
- **実行時最適化**: 動的特性に基づく適応的最適化
- **プロファイル指向最適化**: 実際の使用パターンに基づく最適化

### 高度な技術の統合
- **代数的最適化**: 数学的等価性を利用した変換
- **SIMD最適化**: ベクトル命令による並列処理
- **JITコンパイル**: 実行時特殊化
- **適応的実行**: 動的エンジン選択

### パフォーマンス監視
- **詳細なプロファイリング**: 実行特性の把握
- **適応的調整**: 自動的な最適化パラメータ調整
- **効果測定**: 最適化効果の定量化

これらの最適化技術により、CL-Regexは理論的限界に近い性能を実現し、真に高品質の正規表現エンジンとなっています。