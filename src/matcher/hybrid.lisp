;;;; hybrid.lisp - ハイブリッド正規表現エンジン（NFA/DFA自動切り替え）

(in-package :cl-regex)

;;; ハイブリッドエンジン：パターンの特性に応じてNFA/DFAを自動選択

(defclass hybrid-engine ()
  ((pattern :initarg :pattern :reader engine-pattern)
   (analysis :initarg :analysis :reader engine-analysis)
   (nfa :initarg :nfa :reader engine-nfa)
   (dfa :initarg :dfa :reader engine-dfa)
   (strategy :initarg :strategy :reader engine-strategy))
  (:documentation "NFA/DFAを動的に切り替えるハイブリッドエンジン"))

(defmethod initialize-instance :after ((engine hybrid-engine) &key pattern)
  "エンジン初期化時に最適な戦略を選択"
  (let ((analysis (analyze-pattern pattern)))
    (setf (slot-value engine 'analysis) analysis)
    (setf (slot-value engine 'strategy)
          (select-optimal-strategy analysis))

    ;; 必要に応じてNFA/DFAを構築
    (when (member (engine-strategy engine) '(:nfa :hybrid))
      (setf (slot-value engine 'nfa) (build-nfa pattern)))
    (when (member (engine-strategy engine) '(:dfa :hybrid))
      (setf (slot-value engine 'dfa) (build-dfa pattern)))))

(defun select-optimal-strategy (analysis)
  "パターン分析結果から最適な実行戦略を選択"
  (cond
    ;; バックリファレンスがある場合はNFA必須
    ((pattern-analysis-has-backreferences analysis) :nfa)
    ;; ルックアラウンドがある場合もNFA
    ((pattern-analysis-has-lookaround analysis) :nfa)
    ;; 単純なパターンはDFA
    ((< (pattern-analysis-complexity-score analysis) 10) :dfa)
    ;; それ以外はハイブリッド
    (t :hybrid)))

;;; パターン分析

(defun analyze-pattern (pattern)
  "パターンを分析して最適化のヒントを収集"
  (make-pattern-analysis
   :has-backreferences (contains-backreferences-p pattern)
   :has-lookaround (contains-lookaround-p pattern)
   :is-anchored (is-anchored-p pattern)
   :literal-prefix (extract-literal-prefix pattern)
   :minimum-length (compute-minimum-length pattern)
   :maximum-length (compute-maximum-length pattern)
   :complexity-score (compute-complexity-score pattern)))

(defun compute-complexity-score (pattern)
  "パターンの複雑さスコアを計算"
  (let ((score 0))
    ;; 量指定子の数
    (incf score (* 2 (count-quantifiers pattern)))
    ;; 分岐の数
    (incf score (* 3 (count-alternations pattern)))
    ;; グループの数
    (incf score (count-groups pattern))
    ;; 文字クラスの数
    (incf score (count-char-classes pattern))
    score))

;;; マッチング実行

(defmethod execute-match ((engine hybrid-engine) text &key (start 0) (end nil))
  "ハイブリッドエンジンでマッチング実行"
  (let ((end (or end (length text))))
    (ecase (engine-strategy engine)
      (:nfa
       (execute-nfa-match (engine-nfa engine) text start end))
      (:dfa
       (execute-dfa-match (engine-dfa engine) text start end))
      (:hybrid
       (execute-hybrid-match engine text start end)))))

(defmethod execute-hybrid-match ((engine hybrid-engine) text start end)
  "DFAで高速処理し、必要に応じてNFAにフォールバック"
  (let ((dfa-result (execute-dfa-match (engine-dfa engine) text start end)))
    (if dfa-result
        ;; DFAでマッチした場合、NFAで詳細情報を取得
        (refine-with-nfa engine dfa-result text)
        nil)))

(defmethod refine-with-nfa ((engine hybrid-engine) dfa-result text)
  "DFA結果をNFAで詳細化（グループキャプチャなど）"
  (let* ((start (match-start dfa-result))
         (end (match-end dfa-result))
         (nfa-result (execute-nfa-match (engine-nfa engine) text start end)))
    ;; NFAの詳細情報でDFA結果を補完
    (when nfa-result
      (setf (slot-value nfa-result 'groups)
            (extract-groups (engine-nfa engine) text start end))
      nfa-result)))

;;; 最適化テクニック

(defmethod optimize-for-literal-prefix ((engine hybrid-engine))
  "リテラルプレフィックスがある場合の最適化"
  (let ((prefix (pattern-analysis-literal-prefix (engine-analysis engine))))
    (when prefix
      ;; Boyer-Moore風のスキップテーブルを構築
      (build-skip-table prefix))))

(defun build-skip-table (pattern)
  "Boyer-Mooreアルゴリズムのスキップテーブル構築"
  (let ((table (make-hash-table :test #'eql))
        (pattern-length (length pattern)))
    (loop for i from 0 below (1- pattern-length)
          for char = (char pattern i)
          do (setf (gethash char table) (- pattern-length i 1)))
    table))

;;; 並列処理対応

(defmethod parallel-match ((engine hybrid-engine) texts &key (thread-count 4))
  "複数テキストに対する並列マッチング"
  (let ((channel (lparallel:make-channel))
        (kernel (lparallel:make-kernel thread-count)))
    (unwind-protect
         (progn
           ;; 各テキストに対して並列でマッチング
           (dolist (text texts)
             (lparallel:submit-task channel
                                   (lambda ()
                                     (execute-match engine text))))
           ;; 結果収集
           (loop repeat (length texts)
                 collect (lparallel:receive-result channel)))
      (lparallel:end-kernel kernel))))

;;; キャッシング

(defvar *pattern-cache* (make-hash-table :test #'equal :synchronized t))
(defparameter *max-cache-size* 1000)

(defun get-cached-pattern (pattern-source)
  "キャッシュからパターンを取得"
  (gethash pattern-source *pattern-cache*))

(defun cache-pattern (pattern-source compiled)
  "パターンをキャッシュに保存"
  (when (< (hash-table-count *pattern-cache*) *max-cache-size*)
    (setf (gethash pattern-source *pattern-cache*) compiled)))

;;; インライン最適化

(declaim (inline quick-literal-match))
(defun quick-literal-match (literal text start end)
  "リテラル文字列の高速マッチング"
  (declare (type simple-string literal text)
           (type fixnum start end)
           (optimize (speed 3) (safety 0)))
  (search literal text :start2 start :end2 end))

;;; デバッグ・プロファイリング

(defvar *trace-matching* nil)

(defmacro with-tracing (&body body)
  "マッチングプロセスのトレース"
  `(let ((*trace-matching* t))
     (progn
       (when *trace-matching*
         (format t "~&Starting pattern matching...~%"))
       (prog1
           (progn ,@body)
         (when *trace-matching*
           (format t "~&Pattern matching completed.~%"))))))

(defmethod profile-match ((engine hybrid-engine) text &key (iterations 1000))
  "マッチング性能のプロファイリング"
  (format t "~&Profiling ~D iterations...~%" iterations)
  (let ((start-time (get-internal-real-time)))
    (dotimes (i iterations)
      (execute-match engine text))
    (let ((elapsed (/ (- (get-internal-real-time) start-time)
                     internal-time-units-per-second)))
      (format t "Total time: ~,3F seconds~%" elapsed)
      (format t "Average time: ~,6F seconds per match~%" (/ elapsed iterations))
      (format t "Throughput: ~,0F matches/second~%" (/ iterations elapsed)))))