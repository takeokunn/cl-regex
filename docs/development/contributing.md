# コントリビューションガイド

CL-Regexプロジェクトへの貢献を歓迎します！このガイドでは、効果的で高品質な貢献を行うための指針を提供します。

## 目次

- [プロジェクトの目標](#プロジェクトの目標)
- [開発環境のセットアップ](#開発環境のセットアップ)
- [コントリビューションの種類](#コントリビューションの種類)
- [開発プロセス](#開発プロセス)
- [コーディング規約](#コーディング規約)
- [テスト要件](#テスト要件)
- [ドキュメント要件](#ドキュメント要件)
- [パフォーマンス要件](#パフォーマンス要件)
- [レビュープロセス](#レビュープロセス)

---

## プロジェクトの目標

CL-Regexは以下の目標を追求しています：

### 1. 高品質の性能
- **ゼロオーバーヘッド**: コンパイル時最適化による実行時コストの最小化
- **アルゴリズム革新**: 既存手法を上回るする新しいアプローチの開発
- **スケーラビリティ**: 大規模データと並列処理への対応

### 2. 効率的な表現力
- **S式パターン**: 従来の文字列正規表現を上回るした表現力
- **Prolog統合**: 論理プログラミングとの融合
- **再帰的パターン**: 文脈自由言語・文脈依存言語への対応

### 3. 優れた開発体験
- **マクロDSL**: 極めて読みやすく保守しやすいコード
- **型安全性**: Common Lispの型システムを活用した安全性
- **デバッグ支援**: 包括的なデバッグ・プロファイリング機能

---

## 開発環境のセットアップ

### 必要な環境

```bash
# Common Lisp実装（推奨: SBCL 2.0+）
# Ubuntu/Debian
sudo apt-get install sbcl

# macOS
brew install sbcl

# Quicklisp（パッケージマネージャー）
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)"
```

### プロジェクトのクローンとセットアップ

```bash
# リポジトリクローン
git clone https://github.com/takeokunn/cl-regex.git
cd cl-regex

# 開発用依存関係のインストール
sbcl --eval "(ql:quickload :cl-regex-test)" --quit

# テスト実行で環境確認
sbcl --eval "(ql:quickload :cl-regex-test)" \
     --eval "(cl-regex-test:run-all-tests)" \
     --quit
```

### 推奨開発ツール

```lisp
;; SLIME（Emacs用）または SLY
;; Vim用: Slimv
;; VS Code用: ALIVE

;; パフォーマンス測定ツール
(ql:quickload :trivial-benchmark)

;; プロファイリングツール
(ql:quickload :sb-profile)  ; SBCL専用
```

---

## コントリビューションの種類

### 🚀 パフォーマンス改善
最も価値の高いコントリビューションです。

**求められる改善**:
- 新しいアルゴリズムの実装
- 既存アルゴリズムの最適化
- 並列化・SIMD活用
- メモリ効率改善

**提出要件**:
- ベンチマーク結果の添付
- 理論的根拠の説明
- 既存テストの通過

### 🎯 新機能開発
CL-Regexの表現力を拡張する機能です。

**例**:
- 新しいS式パターン要素
- Prolog規則の拡張
- カスタムオペレータ
- デバッグ・プロファイリング機能

### 🐛 バグ修正
品質向上に直結する重要なコントリビューションです。

**プロセス**:
1. Issue作成（再現手順を含む）
2. 根本原因の特定
3. 修正とテストケース追加
4. パフォーマンス影響の確認

### 📚 ドキュメント改善
Diátaxisフレームワークに基づく高品質なドキュメント。

**改善対象**:
- チュートリアルの拡充
- How-toガイドの追加
- API仕様の詳細化
- 実例とユースケース

### 🧪 テストケース追加
Property-Based TestingとTDDの実践。

**重点領域**:
- エッジケースのカバレッジ
- 性能リグレッションテスト
- 互換性テスト
- ストレステスト

---

## 開発プロセス

### 1. Issue駆動開発

```mermaid
flowchart LR
    A[Issue作成] --> B[議論・設計]
    B --> C[実装開始]
    C --> D[Draft PR作成]
    D --> E[レビュー・改善]
    E --> F[最終レビュー]
    F --> G[マージ]

    classDef planning fill:#e1f5fe,stroke:#01579b,stroke-width:2px
    classDef implementation fill:#e8f5e8,stroke:#1b5e20,stroke-width:2px
    classDef review fill:#fff3e0,stroke:#e65100,stroke-width:2px

    class A,B planning
    class C,D implementation
    class E,F,G review
```

### 2. ブランチ戦略

```bash
# 機能ブランチの作成
git checkout -b feature/good-optimization

# パフォーマンス改善の場合
git checkout -b perf/simd-acceleration

# バグ修正の場合
git checkout -b fix/memory-leak-in-nfa
```

### 3. コミットメッセージ規約

```bash
# 形式: <type>(<scope>): <description>

# 例:
feat(engine): implement SIMD-accelerated character class matching
perf(dfa): optimize state transition table lookup
fix(parser): handle escaped characters in S-expr patterns
docs(tutorial): add advanced pattern examples
test(nfa): add property-based tests for backtracking
```

---

## コーディング規約

### 1. Lisp スタイルガイド

```lisp
;; ✅ 推奨スタイル

;; 関数名: ケバブケース
(defun compile-pattern-optimized (pattern options)
  "パターンを最適化してコンパイル")

;; 変数名: 明確で説明的
(let ((compiled-dfa (build-dfa-from-nfa nfa))
      (optimization-level (getf options :optimization-level 2)))
  ...)

;; 定数: *で囲む
(defparameter *default-optimization-level* 2)
(defconstant +max-dfa-states+ 65536)

;; パッケージ内部関数: %プレフィックス
(defun %internal-state-transition (state char)
  ...)
```

### 2. パフォーマンス重視のコーディング

```lisp
;; 型宣言の積極的使用
(defun fast-character-match (char char-class)
  (declare (optimize (speed 3) (safety 1))
           (type character char)
           (type (simple-bit-vector 256) char-class))
  (= 1 (bit char-class (char-code char))))

;; インライン化対象の明示
(declaim (inline simple-operation))
(defun simple-operation (a b)
  (+ a b))

;; メモリ効率的な実装
(defun process-large-text (text pattern)
  (declare (type simple-string text))
  (let ((results (make-array 100 :adjustable t :fill-pointer 0)))
    ...))
```

### 3. エラーハンドリング

```lisp
;; 明確なエラー型の定義
(define-condition regex-compilation-error (regex-error)
  ((pattern :initarg :pattern :reader error-pattern)
   (phase :initarg :phase :reader error-phase)))

;; 適切なエラー情報の提供
(defun compile-pattern (pattern)
  (handler-case
      (compile-pattern-internal pattern)
    (error (e)
      (error 'regex-compilation-error
             :pattern pattern
             :phase :parsing
             :message (format nil "Failed to compile pattern: ~A" e)))))
```

---

## テスト要件

### 1. Property-Based Testing

```lisp
;; 必須プロパティのテスト
(deftest test-match-idempotency ()
  "マッチング結果の冪等性をテスト"
  (check-it
    (generator (gen-pattern) (gen-text))
    (lambda (pattern text)
      (let ((result1 (match pattern text))
            (result2 (match pattern text)))
        (equal result1 result2)))))

(deftest test-compilation-correctness ()
  "コンパイル済みパターンと動的パターンの結果一致性"
  (check-it
    (generator (gen-simple-pattern) (gen-text))
    (lambda (pattern text)
      (let ((dynamic-result (match pattern text))
            (compiled-result (match (compile-pattern pattern) text)))
        (equal dynamic-result compiled-result)))))
```

### 2. パフォーマンステスト

```lisp
(deftest test-performance-regression ()
  "性能リグレッションの検出"
  (let* ((pattern "complex.*pattern.*with.*groups")
         (text (generate-test-text 10000))
         (baseline-time 0.001))  ; 1ms基準

    (multiple-value-bind (result time)
        (time-execution (lambda () (match pattern text)))
      (is (< time (* baseline-time 1.1)))  ; 10%以内の劣化許容
      (is result))))  ; 正しい結果も確認
```

### 3. 境界値テスト

```lisp
(deftest test-edge-cases ()
  "エッジケースの包括的テスト"
  (testing "空文字列"
    (is (match "" "")))

  (testing "極端に長いパターン"
    (let ((long-pattern (make-string 10000 :initial-element #\a)))
      (is (match long-pattern long-pattern))))

  (testing "Unicode文字"
    (is (match "[α-ω]+" "αβγδε")))

  (testing "再帰的パターンの深いネスト"
    (let ((deep-nested "((((((test))))))"))
      (is (match balanced-parens deep-nested)))))
```

---

## ドキュメント要件

### 1. コード内ドキュメント

```lisp
(defun advanced-pattern-optimizer (pattern options)
  "高度なパターン最適化を実行

Parameters:
  PATTERN - 最適化対象のパターン (string or compiled-pattern)
  OPTIONS - 最適化オプション (plist)
    :level (0-3) - 最適化レベル
    :target (:speed :space :balance) - 最適化目標
    :engine (:auto :nfa :dfa :hybrid) - 強制エンジン選択

Returns:
  COMPILED-PATTERN - 最適化されたコンパイル済みパターン

Examples:
  (advanced-pattern-optimizer \"hello.*world\" '(:level 3 :target :speed))

Performance:
  - レベル3での最適化は10-100倍の高速化を実現
  - メモリ使用量は通常50-80%削減

See Also:
  COMPILE-PATTERN, OPTIMIZE-FOR-SPEED"
  ...)
```

### 2. Diátaxis準拠ドキュメント

新機能追加時は以下をすべて更新：

- **Tutorial**: 段階的学習ガイド
- **How-to**: 具体的な使用方法
- **Reference**: API仕様と詳細
- **Explanation**: 設計思想と理論

### 3. ベンチマーク結果の文書化

```markdown
## パフォーマンス改善結果

### 改善内容
SIMD命令を活用した文字クラスマッチング最適化

### ベンチマーク環境
- CPU: Intel Core i7-9750H (2.6GHz)
- Memory: 16GB DDR4-2666
- OS: Ubuntu 20.04 LTS
- SBCL: 2.2.9

### 結果
| Pattern Type | Before | After | Improvement |
|-------------|--------|-------|-------------|
| [a-z]+      | 1.2ms  | 0.3ms | 4.0x        |
| [0-9]{1,5}  | 0.8ms  | 0.2ms | 4.0x        |
| \\w+        | 1.5ms  | 0.4ms | 3.8x        |

### 理論的根拠
bit-parallel アルゴリズムにより、8文字を並列処理...
```

---

## パフォーマンス要件

### 1. パフォーマンス劣化の防止

```lisp
;; 自動パフォーマンステスト
(defun ensure-no-performance-regression (old-implementation new-implementation)
  "性能劣化がないことを確認"
  (let ((test-cases (generate-performance-test-cases)))
    (loop for (pattern text) in test-cases
          for old-time = (benchmark old-implementation pattern text)
          for new-time = (benchmark new-implementation pattern text)
          do (assert (<= new-time (* old-time 1.05))  ; 5%以内の劣化許容
                     () "Performance regression detected: ~A -> ~A"
                     old-time new-time))))
```

### 2. メモリ効率要件

```lisp
;; メモリリーク検出
(deftest test-memory-efficiency ()
  (gc :full t)
  (let ((initial-memory (get-bytes-consed)))

    ;; 大量処理実行
    (loop repeat 10000 do
      (match complex-pattern sample-text))

    (gc :full t)
    (let ((final-memory (get-bytes-consed))
          (acceptable-growth (* 1024 1024)))  ; 1MB許容

      (is (< (- final-memory initial-memory) acceptable-growth)))))
```

### 3. スケーラビリティ要件

```lisp
;; 並列スケーラビリティテスト
(deftest test-parallel-scalability ()
  (let ((single-thread-time (benchmark-single-thread))
        (quad-thread-time (benchmark-parallel-threads 4)))

    ;; 理想的には4倍高速、最低でも2倍は必要
    (is (>= (/ single-thread-time quad-thread-time) 2.0))))
```

---

## レビュープロセス

### 1. セルフレビューチェックリスト

**実装品質**:
- [ ] コーディング規約に準拠
- [ ] 適切な型宣言とoptimize宣言
- [ ] エラーハンドリングの実装
- [ ] メモリリークの回避

**テスト**:
- [ ] 新機能のテストケース追加
- [ ] 既存テストの通過確認
- [ ] パフォーマンステストの実行
- [ ] エッジケースのカバー

**ドキュメント**:
- [ ] 関数・マクロのdocstring
- [ ] ユーザー向けドキュメント更新
- [ ] 設計決定の記録
- [ ] 使用例の提供

**パフォーマンス**:
- [ ] ベンチマーク結果の測定
- [ ] 既存機能への影響確認
- [ ] メモリ使用量の測定
- [ ] 理論的根拠の説明

### 2. コードレビュー観点

**レビュアーが確認すべき点**:

1. **アルゴリズム設計**
   - 理論的正しさ
   - 時間・空間複雑度
   - エッジケース対応

2. **実装品質**
   - Lispらしい実装
   - パフォーマンス最適化
   - 保守性

3. **統合性**
   - 既存システムとの整合性
   - APIの一貫性
   - 将来の拡張性

### 3. マージ基準

**必須条件**:
- [ ] すべてのテストが通過
- [ ] パフォーマンス劣化なし
- [ ] ドキュメント更新完了
- [ ] 2名以上の承認

**パフォーマンス改善の場合**:
- [ ] 10%以上の改善または新機能
- [ ] ベンチマーク結果の添付
- [ ] 理論的根拠の説明

---

## まとめ

CL-Regexは高品質の正規表現エンジンを目指しています。すべてのコントリビューションがこの目標に向かって高い品質基準を満たすことを期待しています。

**重要な原則**:
1. **性能最優先**: すべての変更で性能への影響を考慮
2. **理論的根拠**: 実装には必ず理論的背景を持つ
3. **包括的テスト**: Property-Based Testingによる品質保証
4. **明確なドキュメント**: Diátaxisフレームワークによる体系的説明

質問や提案があれば、Issueやディスカッションでお気軽にお声がけください。高品質のエンジンを一緒に作り上げましょう！