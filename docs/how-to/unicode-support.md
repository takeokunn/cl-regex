# Unicode対応

CL-RegexでUnicode文字を正しく処理し、国際化対応の正規表現を実装する方法を説明します。

## 概要

CL-RegexのUnicode対応機能：

- **完全Unicode対応**: Unicode 15.0準拠
- **文字カテゴリ**: General Category対応
- **文字ブロック**: Unicode Block対応
- **正規化**: NFD/NFC/NFKD/NFKC対応
- **結合文字**: Combining Characters対応

## 基本的なUnicode対応

### Unicode文字クラスの使用

```lisp
;; 基本的なUnicode文字クラス
(defpattern unicode-letter
  (regex "\\p{L}"))  ; 任意の文字

(defpattern unicode-digit
  (regex "\\p{N}"))  ; 任意の数字

(defpattern unicode-symbol
  (regex "\\p{S}"))  ; 任意の記号

;; より詳細なカテゴリ
(defpattern uppercase-letter
  (regex "\\p{Lu}"))  ; 大文字

(defpattern lowercase-letter
  (regex "\\p{Ll}"))  ; 小文字

(defpattern decimal-number
  (regex "\\p{Nd}"))  ; 10進数字
```

### Unicode文字ブロックの使用

```lisp
;; 特定の文字ブロック
(defpattern japanese-hiragana
  (regex "\\p{InHiragana}"))

(defpattern japanese-katakana
  (regex "\\p{InKatakana}"))

(defpattern cjk-unified-ideographs
  (regex "\\p{InCJK_Unified_Ideographs}"))

(defpattern arabic-script
  (regex "\\p{InArabic}"))

(defpattern cyrillic-script
  (regex "\\p{InCyrillic}"))
```

## 高度なUnicode処理

### カスタムUnicode演算子の実装

```lisp
;; Unicode General Category演算子
(defclass unicode-category-operator (custom-operator)
  ((category :initarg :category :accessor unicode-category))
  (:default-initargs
   :symbol :unicode-cat
   :arity 1
   :precedence 10))

(defmethod compile-operator ((op unicode-category-operator) category)
  "Unicode General Categoryパターンを生成"
  (format nil "\\p{~A}" category))

;; Unicode Block演算子
(defclass unicode-block-operator (custom-operator)
  ((block :initarg :block :accessor unicode-block))
  (:default-initargs
   :symbol :unicode-block
   :arity 1
   :precedence 10))

(defmethod compile-operator ((op unicode-block-operator) block)
  "Unicode Blockパターンを生成"
  (format nil "\\p{In~A}" block))

;; 演算子の登録
(register-operator (make-instance 'unicode-category-operator))
(register-operator (make-instance 'unicode-block-operator))

;; 使用例
(defpattern any-letter
  (regex (:unicode-cat "L")))

(defpattern japanese-text
  (regex (:unicode-block "Hiragana")
         (:unicode-block "Katakana")
         (:unicode-block "CJK_Unified_Ideographs")))
```

### 言語別文字クラス

```lisp
;; 日本語文字処理
(defclass japanese-text-processor ()
  ((hiragana-pattern :initform "\\p{InHiragana}")
   (katakana-pattern :initform "\\p{InKatakana}")
   (kanji-pattern :initform "\\p{InCJK_Unified_Ideographs}")
   (fullwidth-pattern :initform "\\p{InHalfwidth_and_Fullwidth_Forms}")))

(defmethod japanese-word-pattern ((processor japanese-text-processor))
  "日本語単語のパターン"
  (format nil "[~A~A~A]+"
          (slot-value processor 'hiragana-pattern)
          (slot-value processor 'katakana-pattern)
          (slot-value processor 'kanji-pattern)))

;; 中国語文字処理
(defclass chinese-text-processor ()
  ((simplified-pattern :initform "\\p{InCJK_Unified_Ideographs}")
   (traditional-pattern :initform "\\p{InCJK_Unified_Ideographs_Extension_A}")
   (punctuation-pattern :initform "\\p{InCJK_Symbols_and_Punctuation}")))

;; 韓国語文字処理
(defclass korean-text-processor ()
  ((hangul-syllables :initform "\\p{InHangul_Syllables}")
   (hangul-jamo :initform "\\p{InHangul_Jamo}")
   (hangul-compatibility :initform "\\p{InHangul_Compatibility_Jamo}")))

(defmethod korean-word-pattern ((processor korean-text-processor))
  "韓国語単語のパターン"
  (format nil "[~A~A~A]+"
          (slot-value processor 'hangul-syllables)
          (slot-value processor 'hangul-jamo)
          (slot-value processor 'hangul-compatibility)))
```

## Unicode正規化

### 正規化エンジンの実装

```lisp
;; Unicode正規化クラス
(defclass unicode-normalizer ()
  ((normalization-form :initarg :form
                       :initform :nfc
                       :accessor normalization-form)
   (composition-table :initform (load-composition-table)
                      :accessor composition-table)
   (decomposition-table :initform (load-decomposition-table)
                        :accessor decomposition-table)))

(defgeneric normalize-text (normalizer text)
  (:documentation "テキストをUnicode正規化"))

(defmethod normalize-text ((normalizer unicode-normalizer) text)
  "指定された正規化形式でテキストを正規化"
  (case (normalization-form normalizer)
    (:nfd (normalize-nfd text))
    (:nfc (normalize-nfc text))
    (:nfkd (normalize-nfkd text))
    (:nfkc (normalize-nfkc text))
    (t text)))

;; 正規化を考慮したマッチング
(defclass normalized-matcher (base-matcher)
  ((normalizer :initarg :normalizer :accessor normalizer)))

(defmethod match-text ((matcher normalized-matcher) text)
  "正規化を適用してマッチング"
  (let ((normalized-text (normalize-text (normalizer matcher) text))
        (normalized-pattern (normalize-text (normalizer matcher)
                                           (pattern matcher))))
    (cl-regex:match normalized-pattern normalized-text)))
```

### 結合文字の処理

```lisp
;; 結合文字対応マッチャ
(defclass combining-character-matcher (base-matcher)
  ((ignore-combining :initarg :ignore-combining
                     :initform nil
                     :accessor ignore-combining)))

(defmethod preprocess-pattern ((matcher combining-character-matcher) pattern)
  "結合文字を考慮したパターン前処理"
  (if (ignore-combining matcher)
      ;; 結合文字を無視するパターンに変換
      (regex-replace-all "(.)" pattern "\\1\\p{M}*")
      pattern))

;; 使用例
(defparameter *accent-insensitive-matcher*
  (make-instance 'combining-character-matcher
                 :pattern "café"
                 :ignore-combining t))

;; "café", "cafe", "café" すべてにマッチ
```

## 言語特化パターン

### 東アジア言語対応

```lisp
;; CJK統合処理クラス
(defclass cjk-processor ()
  ((script-detection :initform t :accessor script-detection)
   (word-segmentation :initform t :accessor word-segmentation)
   (variant-matching :initform nil :accessor variant-matching)))

(defmethod detect-script ((processor cjk-processor) text)
  "テキストの文字体系を検出"
  (cond
    ((regex-match "\\p{InHiragana}" text) :japanese)
    ((regex-match "\\p{InHangul_Syllables}" text) :korean)
    ((regex-match "\\p{InCJK_Unified_Ideographs}" text)
     (if (regex-match "\\p{InHiragana}|\\p{InKatakana}" text)
         :japanese
         :chinese))
    (t :unknown)))

;; 日本語固有の処理
(defclass japanese-matcher (cjk-processor)
  ((kanji-variants :initform (load-kanji-variants) :accessor kanji-variants)
   (okurigana-flexible :initform t :accessor okurigana-flexible)))

(defmethod match-japanese-word ((matcher japanese-matcher) word text)
  "日本語単語の柔軟マッチング"
  (let ((patterns (generate-japanese-patterns word matcher)))
    (some (lambda (pattern) (regex-match pattern text)) patterns)))

(defmethod generate-japanese-patterns ((matcher japanese-matcher) word)
  "日本語単語のバリエーションパターンを生成"
  (let ((base-pattern (convert-to-regex word))
        (variants '()))
    ;; ひらがな・カタカナ変換
    (push (hiragana-to-katakana-pattern base-pattern) variants)
    (push (katakana-to-hiragana-pattern base-pattern) variants)

    ;; 漢字異体字対応
    (when (kanji-variants matcher)
      (push (expand-kanji-variants base-pattern matcher) variants))

    ;; 送り仮名の柔軟性
    (when (okurigana-flexible matcher)
      (push (flexible-okurigana-pattern base-pattern) variants))

    (remove-duplicates variants :test #'string=)))
```

### アラビア語・ペルシア語対応

```lisp
;; アラビア語処理クラス
(defclass arabic-processor ()
  ((text-direction :initform :rtl :accessor text-direction)
   (diacritics-sensitive :initarg :diacritics
                         :initform nil
                         :accessor diacritics-sensitive)
   (ligature-aware :initarg :ligature
                   :initform t
                   :accessor ligature-aware)))

(defmethod preprocess-arabic-text ((processor arabic-processor) text)
  "アラビア語テキストの前処理"
  (let ((processed-text text))
    ;; ダイアクリティカルマーク除去
    (unless (diacritics-sensitive processor)
      (setf processed-text
            (regex-replace-all "\\p{Mn}" processed-text "")))

    ;; 合字の正規化
    (when (ligature-aware processor)
      (setf processed-text
            (normalize-arabic-ligatures processed-text)))

    processed-text))

(defmethod normalize-arabic-ligatures (text)
  "アラビア語合字の正規化"
  ;; لا → ل + ا
  (regex-replace-all "\\uFEFB|\\uFEFC" text "\\u0644\\u0627"))
```

## パフォーマンス最適化

### Unicode処理の最適化

```lisp
;; Unicode対応高速マッチャ
(defclass optimized-unicode-matcher (base-matcher)
  ((unicode-cache :initform (make-hash-table :test 'equal)
                  :accessor unicode-cache)
   (precompiled-categories :initform (make-hash-table)
                          :accessor precompiled-categories)))

(defmethod match-text :around ((matcher optimized-unicode-matcher) text)
  "Unicodeマッチングのキャッシュ最適化"
  (let ((cache-key (cons (pattern matcher) text)))
    (or (gethash cache-key (unicode-cache matcher))
        (setf (gethash cache-key (unicode-cache matcher))
              (call-next-method)))))

;; SIMD風最適化（概念実装）
(defclass simd-unicode-matcher (optimized-unicode-matcher)
  ((vectorized-operations :initform t :accessor vectorized-operations)
   (batch-size :initform 64 :accessor batch-size)))

(defmethod match-text ((matcher simd-unicode-matcher) text)
  "バッチ処理によるUnicodeマッチング最適化"
  (when (vectorized-operations matcher)
    (batch-unicode-match matcher text)))
```

### メモリ効率の改善

```lisp
;; 軽量Unicode処理
(defclass lightweight-unicode-matcher (base-matcher)
  ((lazy-loading :initform t :accessor lazy-loading)
   (minimal-tables :initform t :accessor minimal-tables)))

(defmethod load-unicode-data ((matcher lightweight-unicode-matcher))
  "必要最小限のUnicodeデータのみロード"
  (when (lazy-loading matcher)
    (load-required-categories-only matcher)))
```

## テストとバリデーション

### Unicode対応テストスイート

```lisp
;; Unicodeテストケース
(defparameter *unicode-test-cases*
  '((:basic-latin "Hello World" "\\p{InBasic_Latin}+")
    (:japanese "こんにちは世界" "\\p{InHiragana}+\\p{InCJK_Unified_Ideographs}+")
    (:arabic "مرحبا بالعالم" "\\p{InArabic}+")
    (:emoji "🌍🚀✨" "\\p{InEmoticons}|\\p{InMiscellaneous_Symbols}")
    (:mixed "Hello世界🌍" "\\p{L}|\\p{S}+")))

(defun run-unicode-tests ()
  "Unicode対応テストの実行"
  (loop for (name text pattern) in *unicode-test-cases* do
    (format t "Testing ~A: " name)
    (if (regex-match pattern text)
        (format t "PASS~%")
        (format t "FAIL~%"))))

;; 正規化テスト
(defparameter *normalization-test-cases*
  '(("é" "e\u0301")      ; NFC vs NFD
    ("ä" "a\u0308")      ; 合成文字 vs 結合文字
    ("ｶ" "カ")))         ; 全角 vs 半角

(defun test-normalization ()
  "Unicode正規化のテスト"
  (loop for (nfc nfd) in *normalization-test-cases* do
    (let ((normalizer (make-instance 'unicode-normalizer :form :nfc)))
      (assert (string= (normalize-text normalizer nfd) nfc)))))
```

## ベストプラクティス

### 1. 正規化の統一

```lisp
;; 推奨：入力時に正規化
(defun normalize-input (text)
  "入力テキストの正規化"
  (normalize-text (make-instance 'unicode-normalizer :form :nfc) text))

;; 非推奨：マッチング時に毎回正規化
(defun bad-practice (pattern text)
  (regex-match (normalize-input pattern) (normalize-input text)))
```

### 2. 文字エンコーディングの統一

```lisp
;; UTF-8での統一処理
(defparameter *default-encoding* :utf-8)

(defun ensure-utf8 (text)
  "テキストのUTF-8エンコーディング確保"
  (if (stringp text)
      text
      (babel:octets-to-string text :encoding :utf-8)))
```

### 3. エラーハンドリング

```lisp
(define-condition unicode-processing-error (error)
  ((text :initarg :text :reader error-text)
   (operation :initarg :operation :reader error-operation)))

(defmethod handle-unicode-error ((error unicode-processing-error))
  "Unicode処理エラーのハンドリング"
  (format t "Unicode処理エラー: ~A in ~A~%"
          (error-operation error)
          (error-text error)))
```

## まとめ

CL-RegexのUnicode対応により：

- **国際化**: 多言語テキストの正確な処理
- **正規化**: 文字の統一表現
- **最適化**: 高速なUnicode処理
- **柔軟性**: 言語特化のカスタマイズ

Unicode対応により、真にグローバルな正規表現エンジンを実現できます。

次は [デバッグパターン](./debugging-patterns.md) や [CLOSとの統合](./clos-integration.md) について学習しましょう。