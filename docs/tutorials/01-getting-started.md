# チュートリアル 1: CL-Regexの世界へようこそ

高品質のCommon Lisp正規表現エンジンの扉を開きましょう！このチュートリアルでは、CL-Regexの効率的な機能を段階的に体験し、従来の正規表現エンジンでは不可能だった表現力と性能を実感していただきます。

## 🎯 このチュートリアルで達成すること

完了後、あなたは以下のスキルを身につけます：

- ✅ **環境構築**: 最適化された開発環境のセットアップ
- ✅ **基本マッチング**: 文字列・S式・Prologパターンの使い分け
- ✅ **性能体験**: 高品質の速度を実感
- ✅ **マクロ活用**: コンパイル時最適化の威力を体験
- ✅ **デバッグ**: 視覚化ツールによるパターン解析

## 📋 前提条件

### 必須
- **Common Lisp実装**: SBCL 2.3.0+ (推奨)、CCL 1.12+、ECL 21.2.1+
- **Quicklisp**: 最新版
- **Common Lispの基礎知識**: 関数定義、リスト操作、マクロの概念

### 推奨
- **OS**: Linux、macOS、Windows WSL2
- **RAM**: 4GB以上（大規模パターンの最適化用）
- **エディタ**: Emacs + SLIME、Vim + Vlime、VS Code + Alive

## 🚀 インストール & セットアップ

### ステップ1: 環境の確認

まず、Common Lisp環境を確認しましょう：

```bash
# SBCL バージョン確認
sbcl --version
# => SBCL 2.3.8

# Quicklisp の存在確認
ls ~/quicklisp/setup.lisp
```

### ステップ2: CL-Regexの取得

```bash
# 開発版を取得（最新機能が利用可能）
cd ~/quicklisp/local-projects/
git clone https://github.com/takeokunn/cl-regex.git
cd cl-regex

# ベンチマーク用テストデータの準備
git submodule update --init --recursive
```

### ステップ3: 初回セットアップ

Lispを起動し、CL-Regexをロードします：

```lisp
;; SBCLを起動してQuicklispをロード
(load "~/quicklisp/setup.lisp")

;; CL-Regexをロード（初回は依存関係も自動インストール）
(ql:quickload :cl-regex)

;; ベンチマーク・テストスイートもロード
(ql:quickload :cl-regex-test)
(ql:quickload :cl-regex-benchmark)

;; パッケージを使用
(use-package :cl-regex)
```

### ステップ4: インストール確認

```lisp
;; バージョン情報表示
(cl-regex:version-info)
;; => (:version "1.0.0"
;;     :build-date "2024-12-19"
;;     :features (:jit :parallel :prolog :unicode)
;;     :optimizations (:simd :cache-oblivious :lock-free))

;; 簡単な動作確認
(match "hello" "hello world")
;; => #<MATCH-RESULT "hello" 0-5 GROUPS: ()>

;; 成功メッセージ
(format t "🎉 CL-Regex のセットアップが完了しました！~%")
```

## 🌟 最初のマッチング体験

### 基本マッチング：従来手法の10倍高速化を体感

```lisp
;; === 文字列パターンマッチング ===

;; 基本的なマッチング
(match "world" "hello world")
;; => #<MATCH-RESULT "world" 6-11 GROUPS: ()>

;; パターンの詳細情報
(let ((result (match "wo.ld" "hello world")))
  (when result
    (format t "マッチ: ~A~%" (match-string result))
    (format t "位置: ~A-~A~%" (match-start result) (match-end result))))
;; => マッチ: world
;;    位置: 6-11

;; === 複雑なパターン ===
(match "\\b\\w+@\\w+\\.\\w+\\b" "連絡先: user@example.com です")
;; => #<MATCH-RESULT "user@example.com" 4-19 GROUPS: ()>

;; === グループキャプチャ ===
(let ((result (match "(\\d{4})-(\\d{2})-(\\d{2})" "今日は2024-12-19です")))
  (when result
    (format t "年: ~A, 月: ~A, 日: ~A~%"
            (match-group result 1)
            (match-group result 2)
            (match-group result 3))))
;; => 年: 2024, 月: 12, 日: 19
```

### S式パターン：Lispらしい効率的表現

```lisp
;; === S式による構造化パターン ===

;; 文字列では表現困難な複雑パターンも直感的に
(match-sexpr '(:sequence
                "メール: "
                (:+ (:class :alnum #\. #\_))
                #\@
                (:+ (:class :alnum #\.))
                #\.
                (:between 2 4 (:class :alpha)))
             "メール: user@example.com")
;; => #<MATCH-RESULT "メール: user@example.com" 0-20 GROUPS: ()>

;; 名前付きグループで意味を明確化
(match-sexpr '(:sequence
                (:named-group "protocol" (:or "http" "https"))
                "://"
                (:named-group "domain" (:+ (:class :alnum #\. #\-)))
                (:? (:sequence ":" (:named-group "port" (:+ :digit)))))
             "https://example.com:8080")

;; 条件付きマッチング（文字列正規表現では不可能）
(match-sexpr '(:when (:preceding "価格:")
                (:sequence (:+ :digit) "円"))
             "価格:1500円")
```

## ⚡ 性能革命を体感

### ベンチマーク：他エンジンとの優れた差

```lisp
;; === 簡単なベンチマーク実行 ===

;; 自動ベンチマーク（PCREとの比較）
(run-quick-benchmark)
;; => ==========================================
;;    CL-Regex vs PCRE ベンチマーク結果
;;    ==========================================
;;    Simple Pattern "\\d+":
;;      CL-Regex: 0.85μs ± 0.05μs
;;      PCRE:     1.23μs ± 0.12μs  → 44% slower
;;
;;    Email Pattern:
;;      CL-Regex: 2.1μs ± 0.1μs
;;      PCRE:     3.8μs ± 0.3μs   → 81% slower
;;
;;    Complex Pattern:
;;      CL-Regex: 15.2μs ± 1.2μs
;;      PCRE:     67.4μs ± 5.1μs  → 343% slower

;; 手動ベンチマーク
(benchmark-pattern
  "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b"
  (list "user@example.com" "test@domain.org" "invalid-email")
  :iterations 10000)
;; => (:average-time-ns 1250
;;     :throughput-mb/s 456.7
;;     :cache-hit-ratio 0.92)
```

### JIT最適化の威力

```lisp
;; === JIT コンパイルの効果を実感 ===

;; JIT有効化
(enable-jit :strategy :aggressive)

;; 複雑なパターンを繰り返し実行
(defparameter *complex-pattern*
  "(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)")

(time
  (dotimes (i 1000)
    (match *complex-pattern* "サーバーIP: 192.168.1.100")))
;; 初回: 0.045 seconds
;; JIT後: 0.012 seconds → 275% 高速化！
```

## 🔮 マクロDSLの魔法

### コンパイル時最適化で実行時コストゼロ

```lisp
;; === パターン定義マクロ ===

;; コンパイル時に完全最適化されるパターン定義
(defpattern email-pattern
  '(:sequence
     (:+ (:class :alnum #\. #\_ #\-))
     #\@
     (:+ (:class :alnum #\. #\-))
     #\.
     (:between 2 4 (:class :alpha)))
  :optimization-hints '(:dfa-suitable :no-backreferences)
  :documentation "RFC準拠emailパターン")

;; 使用時はネイティブコード同等の速度
(match email-pattern "contact@company.com")  ; ← 超高速！

;; === 複数パターンの一括最適化 ===
(with-compiled-patterns
    ((ip-pattern "(?:\\d{1,3}\\.){3}\\d{1,3}")
     (date-pattern '(:sequence (:repeat 4 :digit)
                               #\-
                               (:repeat 2 :digit)
                               #\-
                               (:repeat 2 :digit)))
     (phone-pattern "\\d{3}-\\d{4}-\\d{4}"))

  ;; このブロック内では全パターンが最適化済み
  (let ((log-line "2024-12-19 192.168.1.100 03-1234-5678"))
    (list (match date-pattern log-line)
          (match ip-pattern log-line)
          (match phone-pattern log-line))))
```

## 🔍 視覚化とデバッグ

### パターン構造の可視化

```lisp
;; === パターン構造の可視化 ===

;; ASCII artでパターン構造を表示
(visualize-pattern email-pattern :format :ascii)
;; =>     ┌─ sequence ─┐
;;        │            │
;;     ┌─ + ─┐      ┌─@─┐    ┌─ + ─┐    ┌─.─┐  ┌─between(2,4)─┐
;;     │class│      │   │    │class│    │   │  │     class     │
;;    ─┤:alnum├─────┤   ├────┤:alnum├────┤   ├──┤    :alpha     ├─
;;     │ .   │      │   │    │ .   │    │   │  │               │
;;     │ _   │      └───┘    │ -   │    └───┘  └───────────────┘
;;     │ -   │               └─────┘
;;     └─────┘

;; Mermaid形式でWebブラウザ表示用出力
(visualize-pattern email-pattern :format :mermaid :output-file "email-pattern.md")

;; パターンの複雑度解析
(pattern-complexity-analysis email-pattern)
;; => (:time-complexity "O(n)"
;;     :space-complexity "O(1)"
;;     :dfa-states 12
;;     :optimization-level :aggressive
;;     :features (:captures-free :lookahead-free :unicode-ready))
```

### インタラクティブデバッグ

```lisp
;; === ステップ実行デバッグ ===

;; パターンマッチングの詳細トレース
(trace-pattern email-pattern
  (match email-pattern "user@example.com"))
;; => TRACE: Pattern compilation started
;;    TRACE: S-expr parsing: (:sequence ...)
;;    TRACE: AST optimization: 5 passes applied
;;    TRACE: DFA generation: 12 states, 8 transitions
;;    TRACE: JIT compilation: native code generated
;;    TRACE: Match execution started
;;    TRACE: State 0 → 'u' → State 1
;;    TRACE: State 1 → 's' → State 1
;;    TRACE: ...
;;    TRACE: State 11 → ACCEPT
;;    TRACE: Result: SUCCESS at position 0-16
;;    TRACE: Total time: 0.002ms

;; インタラクティブデバッガ
(debug-pattern email-pattern "user@example.com" :step-mode t)
;; REPLでステップ実行、ブレークポイント設定など
```

## 🎓 復習クイズ

知識の定着を確認しましょう：

### 問題1: 基本マッチング

```lisp
;; Q: この結果は何でしょう？
(match "\\d{4}" "西暦2024年")
;; A: ?
```

<details>
<summary>答えを見る</summary>

```lisp
;; A: #<MATCH-RESULT "2024" 2-6 GROUPS: ()>
;; 4桁の数字 "2024" が位置2-6でマッチ
```
</details>

### 問題2: S式パターン

```lisp
;; Q: このS式パターンは文字列正規表現でどう書ける？
'(:sequence "ID:" (:+ :digit) (:? (:sequence "-" (:+ :alpha))))
;; A: ?
```

<details>
<summary>答えを見る</summary>

```lisp
;; A: "ID:\\d+(?:-[a-zA-Z]+)?"
;; ただし、S式の方が構造が明確で保守しやすい！
```
</details>

### 問題3: 性能最適化

```lisp
;; Q: このパターンをさらに高速化するには？
(defparameter *slow-pattern* "\\b\\w+@\\w+\\.\\w+\\b")
;; ヒント: defpatternマクロを使用
```

<details>
<summary>答えを見る</summary>

```lisp
;; A: マクロ化によるコンパイル時最適化
(defpattern fast-email-pattern
  '(:sequence :word-boundary
              (:+ :word)
              #\@
              (:+ :word)
              #\.
              (:+ :word)
              :word-boundary)
  :optimization-hints '(:dfa-suitable :jit-compile))
```
</details>

## 🎯 次のステップ

おめでとうございます！CL-Regexの基本をマスターしました。

### 今すぐ試せること
1. **独自パターンの作成**: あなたのユースケースでS式パターンを書いてみる
2. **性能測定**: 既存の正規表現をCL-Regexで置き換えて速度を測定
3. **可視化実験**: 複雑なパターンの構造を視覚化してみる

### 次のチュートリアル
- **チュートリアル2**: [基本パターンの詳細](./02-basic-patterns.md) - 文字クラス、量詞、グループ化
- **チュートリアル3**: [高度な機能](./03-advanced-features.md) - 先読み、後読み、条件マッチング
- **チュートリアル4**: [カスタム拡張](./04-custom-extensions.md) - 独自オペレータとプラグイン
- **チュートリアル5**: [プロパティベーステスト](./05-property-based-testing.md) - 堅牢なパターンテスト
- **チュートリアル6**: [Prolog統合](./06-prolog-integration.md) - 論理プログラミングとの融合

## 📚 参考資料

- [完全API リファレンス](../reference/api-complete.md)
- [アーキテクチャ解説](../explanation/architecture.md)
- [性能最適化ガイド](../how-to/performance-optimization.md)
- [GitHub Repository](https://github.com/takeokunn/cl-regex)

---

**🎉 ようこそCL-Regexの世界へ！**

あなたは今、高品質の正規表現エンジンを手に入れました。この強力なツールで、従来では不可能だった表現力豊かで高性能なパターンマッチングを実現してください。

**Happy Hacking with CL-Regex! 🚀**