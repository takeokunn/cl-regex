# パフォーマンス最適化 How-to ガイド

CL-Regexで高品質の性能を引き出すための実践的なテクニック集です。

## 目次

- [基本的な最適化戦略](#基本的な最適化戦略)
- [コンパイル時最適化](#コンパイル時最適化)
- [実行時最適化](#実行時最適化)
- [メモリ最適化](#メモリ最適化)
- [並列化とスケーリング](#並列化とスケーリング)
- [プロファイリングとボトルネック特定](#プロファイリングとボトルネック特定)
- [実世界の最適化例](#実世界の最適化例)

---

## 基本的な最適化戦略

### 1. 適切なエンジンの選択

```mermaid
flowchart TD
    A[Pattern Analysis] --> B{Pattern Complexity?}

    B -->|Simple Literal| C[Boyer-Moore String Search<br/>O(n) guaranteed]
    B -->|Short & Simple| D[Bit-Parallel Algorithm<br/>SIMD-friendly]
    B -->|No Backrefs| E[DFA Engine<br/>Linear time]
    B -->|Complex Features| F[Hybrid NFA/DFA<br/>Adaptive switching]
    B -->|Very Complex| G[Optimized NFA<br/>Backtrack control]

    C --> H[🚀 Fastest for literals]
    D --> I[⚡ CPU cache optimal]
    E --> J[📈 Predictable performance]
    F --> K[🔧 Best of both worlds]
    G --> L[🎯 Feature complete]

```

#### 自動エンジン選択の活用

```lisp
;; CL-Regexが自動的に最適なエンジンを選択
(match "hello" text)  ; 自動的にBoyer-Moore選択
(match "a.*b" text)   ; 自動的にDFA選択
(match "(?<=\\w)test(?=\\w)" text)  ; 自動的にNFA選択

;; 手動でエンジンを指定
(with-engine :boyer-moore
  (match "literal-string" large-text))

(with-engine :dfa
  (match "[a-z]+" text))
```

#### エンジン特性の理解

```lisp
;; 各エンジンの性能特性
(defun engine-characteristics ()
  '((:boyer-moore
     :best-for "Long literal strings"
     :time-complexity "O(n)"
     :space-complexity "O(m)"
     :features "None, literals only")

    (:bit-parallel
     :best-for "Short patterns, character classes"
     :time-complexity "O(nm/w)"  ; w = word width
     :space-complexity "O(1)"
     :features "Limited but very fast")

    (:dfa
     :best-for "No backreferences, predictable patterns"
     :time-complexity "O(n)"
     :space-complexity "O(2^m) compile, O(1) runtime"
     :features "Most regex features except backrefs")

    (:nfa
     :best-for "Complex features, backreferences"
     :time-complexity "O(nm) average, O(2^n) worst"
     :space-complexity "O(m)"
     :features "All regex features")))
```

### 2. パターンの事前分析

```lisp
(defun optimize-pattern-usage (pattern text)
  "パターンの特性を分析して最適な戦略を選択"
  (let ((analysis (analyze-pattern pattern)))
    (cond
      ;; リテラル文字列 → 文字列検索アルゴリズム
      ((literal-pattern-p analysis)
       (boyer-moore-search pattern text))

      ;; 短いパターン → bit-parallel
      ((and (simple-pattern-p analysis)
            (< (pattern-length pattern) 64))
       (bit-parallel-match pattern text))

      ;; バックリファレンスなし → DFA
      ((not (has-backreferences-p analysis))
       (dfa-match pattern text))

      ;; 複雑なパターン → 最適化されたNFA
      (t (optimized-nfa-match pattern text)))))
```

---

## コンパイル時最適化

### 1. 静的パターンの最大活用

```lisp
;; ❌ 動的パターン（毎回解釈）
(defun slow-email-validation (email)
  (match "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}" email))

;; ✅ 静的パターン（コンパイル時最適化）
(defpattern fast-email-pattern
  (:sequence
    (:+ (:class :alnum #\. #\_ #\% #\+ #\-))
    #\@
    (:+ (:class :alnum #\. #\-))
    #\.
    (:between 2 6 :alpha)))

(defun fast-email-validation (email)
  (match fast-email-pattern email))
```

### 2. マクロによる特殊化

```lisp
;; コンパイル時にパターンを完全に展開・最適化
(defmacro compile-time-optimized-match (pattern text)
  (let ((optimized-code (compile-pattern-to-code pattern)))
    `(progn
       (declare (optimize (speed 3) (safety 0)))
       ,optimized-code)))

;; 使用例：コンパイル時に最適化されたコードを生成
(compile-time-optimized-match "hello.*world" input-string)
;; => 展開されて高度に最適化されたネイティブコードになる
```

### 3. パターン結合最適化

```lisp
;; ❌ 複数回のマッチング
(defun inefficient-validation (text)
  (and (match "^[a-zA-Z]" text)        ; 先頭チェック
       (match "[a-zA-Z0-9]*$" text)    ; 全体チェック
       (match ".*[0-9].*" text)))      ; 数字含有チェック

;; ✅ 単一の最適化されたパターン
(defpattern combined-validation-pattern
  (:sequence
    :start
    (:look-ahead (:sequence (:* :any) :digit (:* :any)))  ; 数字含有チェック
    :alpha                                                ; 先頭は英字
    (:* :alnum)                                          ; 英数字のみ
    :end))

(defun efficient-validation (text)
  (match combined-validation-pattern text))
```

### 4. 部分式の共通化

```lisp
;; 共通部分式を事前定義
(defpattern common-word-pattern (:+ (:class :alpha #\-)))
(defpattern common-number-pattern (:+ :digit))

;; 複合パターンで再利用
(defpattern product-code-pattern
  (:sequence
    common-word-pattern
    #\-
    common-number-pattern
    #\-
    (:repeat 3 :alpha)))

(defpattern invoice-number-pattern
  (:sequence
    "INV"
    common-number-pattern
    #\-
    (:repeat 4 :digit)))
```

---

## 実行時最適化

### 1. JIT コンパイル活用

```lisp
;; JIT コンパイルを有効化
(enable-jit :threshold 100)  ; 100回使用されたらJITコンパイル

;; 手動JITコンパイル
(defparameter *jit-compiled-pattern*
  (jit-compile-pattern complex-pattern :optimization-level 3))

;; 使用例
(loop for line in large-dataset
      count (match *jit-compiled-pattern* line))
```

### 2. 適応的エンジン切り替え

```lisp
(defclass adaptive-matcher ()
  ((pattern :initarg :pattern)
   (nfa-engine :initform nil)
   (dfa-engine :initform nil)
   (current-engine :initform :auto)
   (performance-stats :initform (make-hash-table))))

(defmethod match-adaptive ((matcher adaptive-matcher) text)
  "性能統計に基づいて最適なエンジンを動的選択"
  (let* ((stats (slot-value matcher 'performance-stats))
         (best-engine (select-best-engine stats text)))

    (unless (eq best-engine (slot-value matcher 'current-engine))
      (switch-engine matcher best-engine))

    (time-and-record-performance
      (execute-match matcher text)
      stats best-engine)))
```

### 3. プリフェッチとキャッシュ最適化

```lisp
;; メモリアクセスパターンの最適化
(defun cache-friendly-scan (pattern texts)
  "キャッシュ効率を考慮したバッチ処理"
  (let ((compiled-pattern (compile-pattern pattern))
        (results (make-array (length texts))))

    ;; データをブロック単位で処理（キャッシュ局所性向上）
    (loop for block-start from 0 below (length texts) by *cache-block-size*
          for block-end = (min (+ block-start *cache-block-size*) (length texts))
          do (loop for i from block-start below block-end
                   do (setf (aref results i)
                           (match compiled-pattern (elt texts i)))))
    results))
```

### 4. SIMD エミュレーション

```lisp
;; 文字クラスマッチングのSIMD風最適化
(defun simd-emulated-char-class-match (char-bitmap text)
  "複数文字を並列でチェック"
  (declare (optimize (speed 3) (safety 0))
           (type (simple-bit-vector 256) char-bitmap)
           (type simple-string text))

  (let ((results (make-array (length text) :element-type 'bit)))
    ;; 8文字ずつ並列処理（8-way unroll）
    (loop for i from 0 below (length text) by 8
          do (setf (aref results i)       (bit char-bitmap (char-code (char text i)))
                   (aref results (+ i 1)) (bit char-bitmap (char-code (char text (+ i 1))))
                   (aref results (+ i 2)) (bit char-bitmap (char-code (char text (+ i 2))))
                   (aref results (+ i 3)) (bit char-bitmap (char-code (char text (+ i 3))))
                   (aref results (+ i 4)) (bit char-bitmap (char-code (char text (+ i 4))))
                   (aref results (+ i 5)) (bit char-bitmap (char-code (char text (+ i 5))))
                   (aref results (+ i 6)) (bit char-bitmap (char-code (char text (+ i 6))))
                   (aref results (+ i 7)) (bit char-bitmap (char-code (char text (+ i 7))))))
    results))
```

---

## メモリ最適化

### 1. ゼロコピー文字列処理

```lisp
;; String-viewを使った非破壊的処理
(defstruct string-view
  (base-string "" :type simple-string :read-only t)
  (start 0 :type fixnum)
  (end 0 :type fixnum))

(defmethod match-on-view ((pattern compiled-pattern) (view string-view))
  "文字列をコピーせずにサブシーケンスをマッチング"
  (with-accessors ((str string-view-base-string)
                   (start string-view-start)
                   (end string-view-end)) view
    (match-substring pattern str start end)))

;; 使用例：大きなファイルの行単位処理
(defun process-large-file-efficiently (filename pattern)
  (with-open-file (stream filename :direction :input)
    (let ((buffer (make-string 65536)))
      (loop for bytes-read = (read-sequence buffer stream)
            while (> bytes-read 0)
            do (process-buffer-with-views buffer bytes-read pattern)))))
```

### 2. オブジェクトプールの活用

```lisp
(defclass match-result-pool ()
  ((free-results :initform nil)
   (allocated-count :initform 0)
   (pool-size-limit :initform 1000)))

(defmethod acquire-match-result ((pool match-result-pool))
  "プールからMatchオブジェクトを取得（高速）"
  (or (pop (slot-value pool 'free-results))
      (make-instance 'match-result)))

(defmethod release-match-result ((pool match-result-pool) match-result)
  "プールにMatchオブジェクトを返却"
  (reset-match-result match-result)
  (push match-result (slot-value pool 'free-results)))

;; 使用例
(let ((pool (make-instance 'match-result-pool)))
  (loop for text in large-text-list
        for result = (acquire-match-result pool)
        do (progn
             (execute-match-into-result pattern text result)
             (process-result result)
             (release-match-result pool result))))
```

### 3. ガベージコレクション圧迫回避

```lisp
;; GC圧迫を避ける実装パターン
(defmacro with-minimal-allocation (&body body)
  "最小限のアロケーションで実行"
  `(let ((*allocate-in-static-space* t))
     (declare (optimize (speed 3) (space 3)))
     ,@body))

;; バッファ再利用パターン
(defclass reusable-buffer ()
  ((data :type (vector character) :initform (make-array 1024 :adjustable t :fill-pointer 0))
   (capacity :type fixnum :initform 1024)))

(defmethod ensure-buffer-capacity ((buffer reusable-buffer) required-size)
  (when (> required-size (slot-value buffer 'capacity))
    (adjust-array (slot-value buffer 'data)
                  (* required-size 2) :fill-pointer 0)
    (setf (slot-value buffer 'capacity) (* required-size 2))))
```

---

## 並列化とスケーリング

### 1. データ並列処理

```lisp
(defun parallel-pattern-search (pattern texts &key (thread-count 4))
  "大量テキストの並列パターンマッチング"
  (let* ((chunk-size (ceiling (length texts) thread-count))
         (results (make-array (length texts)))
         (threads (make-array thread-count)))

    ;; 各スレッドに作業を分散
    (dotimes (i thread-count)
      (let* ((start (* i chunk-size))
             (end (min (* (1+ i) chunk-size) (length texts)))
             (local-pattern (compile-pattern pattern))) ; スレッドローカル

        (setf (aref threads i)
              (bt:make-thread
                (lambda ()
                  (loop for j from start below end
                        do (setf (aref results j)
                                (match local-pattern (elt texts j)))))))))

    ;; 全スレッド完了を待機
    (map 'nil #'bt:join-thread threads)
    results))
```

### 2. パイプライン並列処理

```lisp
(defclass pipeline-matcher ()
  ((input-queue :initform (make-queue))
   (output-queue :initform (make-queue))
   (worker-threads :initform nil)
   (pattern :initarg :pattern)))

(defmethod start-pipeline ((matcher pipeline-matcher) worker-count)
  "パイプライン処理開始"
  (loop repeat worker-count
        collect (bt:make-thread
                  (lambda ()
                    (loop for text = (dequeue (slot-value matcher 'input-queue))
                          while text
                          do (let ((result (match (slot-value matcher 'pattern) text)))
                               (enqueue (slot-value matcher 'output-queue)
                                       (cons text result))))))))

;; ストリーミング処理での使用
(defun stream-process-with-pipeline (stream pattern)
  (let ((matcher (make-instance 'pipeline-matcher :pattern pattern)))
    (start-pipeline matcher 4)

    ;; 入力ストリーム
    (bt:make-thread
      (lambda ()
        (loop for line = (read-line stream nil)
              while line
              do (enqueue (slot-value matcher 'input-queue) line))))

    ;; 結果収集
    (loop for result = (dequeue (slot-value matcher 'output-queue))
          while result
          collect result)))
```

### 3. Lock-Free データ構造

```lisp
(defstruct atomic-counter
  (value 0 :type fixnum))

(defun atomic-increment (counter)
  "アトミックなカウンタインクリメント"
  (loop
    (let* ((old-value (atomic-counter-value counter))
           (new-value (1+ old-value)))
      (when (eq old-value
                (sb-ext:compare-and-swap
                 (atomic-counter-value counter)
                 old-value
                 new-value))
        (return new-value)))))

;; 並列マッチング統計収集
(defun parallel-match-with-stats (pattern texts thread-count)
  (let ((match-counter (make-atomic-counter))
        (total-counter (make-atomic-counter)))

    (parallel-process texts thread-count
      (lambda (text)
        (atomic-increment total-counter)
        (when (match pattern text)
          (atomic-increment match-counter))))

    (list :total (atomic-counter-value total-counter)
          :matches (atomic-counter-value match-counter))))
```

---

## プロファイリングとボトルネック特定

### 1. 詳細パフォーマンス測定

```lisp
(defclass performance-profiler ()
  ((samples :initform nil)
   (start-time :initform 0)
   (memory-usage-samples :initform nil)))

(defmethod start-profiling ((profiler performance-profiler))
  (setf (slot-value profiler 'start-time) (get-internal-real-time))
  (gc :full t))  ; クリーンな状態でスタート

(defmethod record-sample ((profiler performance-profiler) operation-name)
  (push (list operation-name
              (- (get-internal-real-time) (slot-value profiler 'start-time))
              (get-bytes-consed))
        (slot-value profiler 'samples)))

;; 使用例
(defun profile-pattern-matching (pattern text iterations)
  (let ((profiler (make-instance 'performance-profiler)))
    (start-profiling profiler)

    (record-sample profiler :pattern-compilation-start)
    (let ((compiled-pattern (compile-pattern pattern)))
      (record-sample profiler :pattern-compilation-end)

      (loop repeat iterations do
        (record-sample profiler :match-start)
        (match compiled-pattern text)
        (record-sample profiler :match-end)))

    (analyze-profiling-results profiler)))
```

### 2. ボトルネック自動検出

```lisp
(defun detect-performance-bottlenecks (pattern text)
  "性能ボトルネックを自動検出"
  (let ((analysis (make-hash-table)))

    ;; パターン解析時間
    (setf (gethash :pattern-analysis analysis)
          (measure-time (analyze-pattern pattern)))

    ;; コンパイル時間
    (setf (gethash :compilation analysis)
          (measure-time (compile-pattern pattern)))

    ;; 実行時間
    (setf (gethash :execution analysis)
          (measure-time (match pattern text)))

    ;; メモリ使用量
    (setf (gethash :memory analysis)
          (measure-memory-usage (lambda () (match pattern text))))

    ;; ボトルネック特定
    (identify-bottlenecks analysis)))

(defun identify-bottlenecks (analysis)
  "分析結果からボトルネックを特定"
  (let ((bottlenecks nil))
    (when (> (gethash :compilation analysis) 0.001) ; 1ms以上
      (push :slow-compilation bottlenecks))
    (when (> (gethash :execution analysis) 0.01)   ; 10ms以上
      (push :slow-execution bottlenecks))
    (when (> (gethash :memory analysis) 1048576)   ; 1MB以上
      (push :high-memory-usage bottlenecks))
    bottlenecks))
```

### 3. アルゴリズム複雑度測定

```lisp
(defun measure-algorithmic-complexity (pattern text-sizes)
  "実際の入力サイズと実行時間から複雑度を測定"
  (let ((measurements nil))
    (loop for size in text-sizes
          for test-text = (generate-test-text size)
          do (let ((time (measure-execution-time
                           (lambda () (match pattern test-text)))))
               (push (list size time) measurements)))

    ;; 複雑度を推定
    (estimate-complexity (reverse measurements))))

(defun estimate-complexity (measurements)
  "測定値から複雑度クラスを推定"
  (let* ((ratios (loop for ((size1 time1) (size2 time2)) on measurements
                       while (and size2 time2)
                       collect (/ time2 time1 (/ size2 size1))))
         (avg-ratio (/ (reduce #'+ ratios) (length ratios))))
    (cond
      ((< avg-ratio 1.2) :constant)
      ((< avg-ratio 2.0) :linear)
      ((< avg-ratio 4.0) :quadratic)
      (t :exponential))))
```

---

## 実世界の最適化例

### 1. ログファイル解析の最適化

```lisp
;; ❌ 非効率な実装
(defun slow-log-analysis (log-file)
  (with-open-file (stream log-file)
    (loop for line = (read-line stream nil)
          while line
          when (match "ERROR.*database" line)  ; 毎回パターン解釈
            collect (extract-timestamp line))))

;; ✅ 最適化された実装
(defpattern error-log-pattern
  (:sequence
    (:named timestamp
            (:sequence (:repeat 4 :digit) #\-
                      (:repeat 2 :digit) #\-
                      (:repeat 2 :digit) #\Space
                      (:repeat 2 :digit) #\:
                      (:repeat 2 :digit) #\:
                      (:repeat 2 :digit)))
    " [ERROR] "
    (:* :any)
    "database"
    (:* :any)))

(defun fast-log-analysis (log-file)
  (let ((buffer (make-array 65536 :element-type 'character))
        (results nil))
    (with-open-file (stream log-file :direction :input)
      (loop for bytes-read = (read-sequence buffer stream)
            while (> bytes-read 0)
            do (let ((text (make-array bytes-read
                                     :element-type 'character
                                     :displaced-to buffer)))
                 (with-all-matches (error-log-pattern text match)
                   (push (named-group match 'timestamp) results)))))
    (nreverse results)))
```

### 2. 大規模テキスト検索の最適化

```lisp
;; メモリマップファイル + 並列処理
(defun optimized-large-file-search (filename pattern)
  "大容量ファイルの高速検索"
  (let* ((mmap-file (memory-map-file filename))
         (file-size (file-size filename))
         (chunk-size (* 1024 1024))  ; 1MB chunks
         (thread-count 8)
         (results (make-array thread-count :initial-element nil)))

    ;; ファイルをチャンクに分割して並列処理
    (let ((threads
            (loop for i from 0 below thread-count
                  for start = (* i (floor file-size thread-count))
                  for end = (if (= i (1- thread-count))
                              file-size
                              (* (1+ i) (floor file-size thread-count)))
                  collect
                  (bt:make-thread
                    (lambda ()
                      (setf (aref results i)
                            (search-memory-region mmap-file pattern start end)))))))

      ;; 結果をマージ
      (map 'nil #'bt:join-thread threads)
      (apply #'append (coerce results 'list)))))

(defun search-memory-region (mmap-file pattern start end)
  "メモリ領域内のパターン検索"
  (let ((region-text (memory-region-to-string mmap-file start end))
        (matches nil))
    (with-all-matches (pattern region-text match)
      (push (+ start (match-start match)) matches))
    (nreverse matches)))
```

### 3. リアルタイムストリーム処理

```lisp
(defclass real-time-pattern-matcher ()
  ((patterns :initarg :patterns)
   (buffer :initform (make-adjustable-string))
   (buffer-size-limit :initform 65536)
   (match-callback :initarg :match-callback)))

(defmethod process-stream-chunk ((matcher real-time-pattern-matcher) chunk)
  "ストリームチャンクをリアルタイム処理"
  (with-slots (buffer patterns match-callback buffer-size-limit) matcher

    ;; バッファに追加
    (extend-buffer buffer chunk)

    ;; パターンマッチング実行
    (loop for pattern in patterns do
      (with-all-matches (pattern buffer match)
        (funcall match-callback pattern match)))

    ;; バッファサイズ管理
    (when (> (length buffer) buffer-size-limit)
      (trim-buffer buffer))))

;; 高頻度ストリーム処理の使用例
(defun high-frequency-stream-monitor (stream patterns callback)
  (let ((matcher (make-instance 'real-time-pattern-matcher
                                :patterns patterns
                                :match-callback callback))
        (buffer (make-array 4096 :element-type 'character)))

    (loop while (listen stream)
          do (let ((bytes-read (read-sequence buffer stream)))
               (when (> bytes-read 0)
                 (process-stream-chunk
                   matcher
                   (subseq buffer 0 bytes-read)))))))
```

### 4. 動的パターン最適化

```lisp
(defclass adaptive-pattern-optimizer ()
  ((pattern-cache :initform (make-hash-table :test 'equal))
   (usage-statistics :initform (make-hash-table :test 'equal))
   (optimization-threshold :initform 100)))

(defmethod optimized-match ((optimizer adaptive-pattern-optimizer) pattern text)
  "使用統計に基づいて動的にパターンを最適化"
  (with-slots (pattern-cache usage-statistics optimization-threshold) optimizer

    ;; 使用統計更新
    (incf (gethash pattern usage-statistics 0))

    ;; キャッシュ確認
    (let ((cached-pattern (gethash pattern pattern-cache)))
      (if cached-pattern
          (match cached-pattern text)
          (progn
            ;; 使用頻度が閾値を超えたら最適化
            (when (> (gethash pattern usage-statistics) optimization-threshold)
              (let ((optimized (optimize-pattern-for-usage pattern text)))
                (setf (gethash pattern pattern-cache) optimized)))

            ;; 通常のマッチング
            (match pattern text))))))

(defun optimize-pattern-for-usage (pattern sample-text)
  "使用パターンに基づいてパターンを最適化"
  (let ((analysis (analyze-pattern-with-sample pattern sample-text)))
    (cond
      ((frequent-literal-matches-p analysis)
       (optimize-for-literal-search pattern))
      ((short-patterns-p analysis)
       (optimize-for-bit-parallel pattern))
      ((no-complex-features-p analysis)
       (optimize-for-dfa pattern))
      (t (optimize-for-nfa pattern)))))
```

## まとめ

CL-Regexの性能最適化は以下の階層で行います：

1. **アルゴリズム選択**: 問題に最適なエンジンの選択
2. **コンパイル時最適化**: マクロとコンパイル時計算の活用
3. **実行時最適化**: JITコンパイルと動的適応
4. **メモリ最適化**: ゼロコピーとオブジェクトプール
5. **並列化**: データ並列とパイプライン並列
6. **プロファイリング**: 継続的なボトルネック監視

これらのテクニックを適切に組み合わせることで、CL-Regexは高品質の性能を実現できます。重要なのは、実際の使用パターンに基づいて最適化を選択することです。