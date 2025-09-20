# 設定オプション

CL-Regexの動作を制御する設定オプションの完全なリファレンスです。

## グローバル設定

### `*default-optimization-level*`

デフォルトの最適化レベルを設定します。

```lisp
(defparameter *default-optimization-level* 2
  "デフォルトの最適化レベル (0-3)")
```

| 値 | 説明 | 特徴 |
|----|------|------|
| `0` | 最適化なし | 最速コンパイル、デバッグ向け |
| `1` | 基本最適化 | 軽量な最適化 |
| `2` | 標準最適化 | バランス重視（デフォルト） |
| `3` | 積極的最適化 | 最高性能、遅いコンパイル |

### `*default-backend*`

デフォルトの実行バックエンドを設定します。

```lisp
(defparameter *default-backend* :auto
  "デフォルトの実行バックエンド")
```

| 値 | 説明 | 用途 |
|----|------|------|
| `:auto` | 自動選択 | 汎用（推奨） |
| `:nfa` | NFA実行エンジン | 複雑なパターン |
| `:dfa` | DFA実行エンジン | 単純で高速 |
| `:hybrid` | ハイブリッド | 適応的実行 |
| `:jit` | JITコンパイル | 繰り返し実行 |

### `*enable-cache*`

コンパイル結果のキャッシュを有効にするかを設定します。

```lisp
(defparameter *enable-cache* t
  "コンパイル結果のキャッシュを有効にする")
```

### `*cache-size*`

キャッシュサイズの上限を設定します。

```lisp
(defparameter *cache-size* 1000
  "コンパイル結果キャッシュのサイズ")
```

### `*debug-mode*`

デバッグモードの有効/無効を設定します。

```lisp
(defparameter *debug-mode* nil
  "デバッグモードの有効/無効")
```

## コンパイルオプション

### `:optimization`

個別パターンの最適化レベルを指定します。

```lisp
(regex "a+b*" :optimization 3)
```

### `:backend`

個別パターンの実行バックエンドを指定します。

```lisp
(regex "simple" :backend :dfa)
(regex "complex" :backend :nfa)
```

### `:flags`

コンパイルフラグを指定します。

```lisp
(regex "pattern" :flags (:case-insensitive :multiline))
```

#### 利用可能なフラグ

| フラグ | 説明 | 例 |
|--------|------|-----|
| `:case-insensitive` | 大文字小文字を区別しない | `A` が `a` にマッチ |
| `:multiline` | 複数行モード | `^` `$` が各行に適用 |
| `:dotall` | `.` が改行にマッチ | `.` が `\n` にマッチ |
| `:unicode` | Unicode対応 | Unicode プロパティ有効 |
| `:extended` | 拡張モード | 空白とコメント無視 |
| `:anchored` | 文字列開始でのみマッチ | 暗黙の `^` 付加 |
| `:dollar-endonly` | `$` が文字列末尾のみ | 改行前では無効 |
| `:ungreedy` | デフォルトで非貪欲 | `*` が `*?` として動作 |

### `:cache`

個別パターンのキャッシュ設定を指定します。

```lisp
(regex "pattern" :cache nil)  ; キャッシュ無効
(regex "pattern" :cache t)    ; キャッシュ有効
```

### `:timeout`

コンパイル・実行のタイムアウトを指定します。

```lisp
(regex "pattern" :timeout 5000)  ; 5秒でタイムアウト
```

## 実行オプション

### `:start`

マッチング開始位置を指定します。

```lisp
(match pattern text :start 10)
```

### `:end`

マッチング終了位置を指定します。

```lisp
(match pattern text :start 0 :end 100)
```

### `:global`

グローバルマッチ（全体検索）を有効にします。

```lisp
(match pattern text :global t)
```

### `:capture`

キャプチャグループの取得を制御します。

```lisp
(match pattern text :capture nil)     ; キャプチャなし
(match pattern text :capture :named)  ; 名前付きのみ
(match pattern text :capture :all)    ; すべて（デフォルト）
```

### `:overlapped`

重複マッチを許可します。

```lisp
(match "aa" "aaaa" :global t :overlapped t)  ; 3つのマッチ
```

## Unicode設定

### `*unicode-version*`

サポートするUnicodeバージョンを設定します。

```lisp
(defparameter *unicode-version* "15.0"
  "サポートするUnicodeバージョン")
```

### `*default-normalization*`

デフォルトのUnicode正規化形式を設定します。

```lisp
(defparameter *default-normalization* :nfc
  "デフォルトのUnicode正規化形式")
```

| 値 | 説明 |
|----|------|
| `:nfd` | 正規分解 |
| `:nfc` | 正規合成（デフォルト） |
| `:nfkd` | 互換分解 |
| `:nfkc` | 互換合成 |
| `nil` | 正規化なし |

### `:normalization`

個別パターンの正規化設定を指定します。

```lisp
(regex "café" :normalization :nfd)
```

### `:case-folding`

Unicode大文字小文字変換を有効にします。

```lisp
(regex "pattern" :case-folding t)
```

### `:grapheme-aware`

書記素クラスタ対応を有効にします。

```lisp
(regex "." :grapheme-aware t)  ; 書記素クラスタ単位でマッチ
```

## パフォーマンス設定

### `*jit-threshold*`

JITコンパイルの閾値を設定します。

```lisp
(defparameter *jit-threshold* 100
  "JITコンパイルを開始する実行回数")
```

### `*parallel-threshold*`

並列実行の閾値を設定します。

```lisp
(defparameter *parallel-threshold* 10000
  "並列実行を開始するテキストサイズ")
```

### `:jit`

個別パターンのJITコンパイルを制御します。

```lisp
(regex "pattern" :jit t)     ; JIT有効
(regex "pattern" :jit nil)   ; JIT無効
(regex "pattern" :jit :auto) ; 自動判定
```

### `:parallel`

並列実行を制御します。

```lisp
(match pattern large-text :parallel t)
```

### `:simd`

SIMD最適化を制御します。

```lisp
(regex "pattern" :simd t)  ; SIMD最適化有効
```

## メモリ管理設定

### `*memory-limit*`

使用メモリの上限を設定します。

```lisp
(defparameter *memory-limit* (* 100 1024 1024)  ; 100MB
  "使用メモリの上限（バイト）")
```

### `*gc-threshold*`

ガベージコレクション実行の閾値を設定します。

```lisp
(defparameter *gc-threshold* (* 10 1024 1024)  ; 10MB
  "GC実行の閾値")
```

### `:memory-pool`

メモリプールの使用を制御します。

```lisp
(regex "pattern" :memory-pool :enabled)
```

## エラーハンドリング設定

### `*error-mode*`

エラー処理モードを設定します。

```lisp
(defparameter *error-mode* :strict
  "エラー処理モード")
```

| 値 | 説明 |
|----|------|
| `:strict` | 厳密（エラーで停止） |
| `:permissive` | 寛容（エラーを無視） |
| `:warning` | 警告（継続実行） |

### `:error-handler`

個別パターンのエラーハンドラを指定します。

```lisp
(regex "pattern" :error-handler #'my-error-handler)
```

### `:max-backtrack*`

バックトラッキングの上限を設定します。

```lisp
(regex "pattern" :max-backtrack 100000)
```

## ログ設定

### `*log-level*`

ログレベルを設定します。

```lisp
(defparameter *log-level* :info
  "ログレベル")
```

| 値 | 説明 |
|----|------|
| `:trace` | 詳細トレース |
| `:debug` | デバッグ情報 |
| `:info` | 一般情報（デフォルト） |
| `:warn` | 警告のみ |
| `:error` | エラーのみ |
| `:none` | ログなし |

### `*log-destination*`

ログ出力先を設定します。

```lisp
(defparameter *log-destination* *standard-output*
  "ログ出力先")
```

## プラグイン設定

### `*plugin-directories*`

プラグインディレクトリを設定します。

```lisp
(defparameter *plugin-directories*
  '("/usr/local/lib/cl-regex/plugins/"
    "~/.cl-regex/plugins/")
  "プラグインディレクトリのリスト")
```

### `*auto-load-plugins*`

プラグインの自動ロードを制御します。

```lisp
(defparameter *auto-load-plugins* t
  "プラグインの自動ロード")
```

## 統計・監視設定

### `*collect-statistics*`

統計情報の収集を制御します。

```lisp
(defparameter *collect-statistics* nil
  "統計情報の収集")
```

### `:profile`

個別パターンのプロファイリングを制御します。

```lisp
(regex "pattern" :profile t)
```

### `*benchmark-mode*`

ベンチマークモードを制御します。

```lisp
(defparameter *benchmark-mode* nil
  "ベンチマークモード")
```

## 環境固有設定

### Lispworks設定

```lisp
#+lispworks
(progn
  (setf *default-backend* :dfa)
  (setf *enable-cache* t))
```

### SBCL設定

```lisp
#+sbcl
(progn
  (setf *jit-threshold* 50)
  (setf *parallel-threshold* 5000))
```

### CCL設定

```lisp
#+ccl
(progn
  (setf *memory-limit* (* 50 1024 1024))
  (setf *gc-threshold* (* 5 1024 1024)))
```

## 設定ファイル

### 設定ファイルの読み込み

```lisp
(load-configuration "/path/to/cl-regex.conf")
```

### 設定ファイルの例

```lisp
;; cl-regex.conf
(:optimization-level 3
 :backend :hybrid
 :cache-size 2000
 :unicode-version "15.0"
 :log-level :info
 :jit-threshold 50
 :plugins (:unicode-extended :japanese-text))
```

### 環境変数による設定

| 環境変数 | 対応設定 | 例 |
|----------|----------|-----|
| `CL_REGEX_OPTIMIZATION` | `*default-optimization-level*` | `3` |
| `CL_REGEX_BACKEND` | `*default-backend*` | `hybrid` |
| `CL_REGEX_CACHE_SIZE` | `*cache-size*` | `2000` |
| `CL_REGEX_DEBUG` | `*debug-mode*` | `true` |

## プロファイル設定

### パフォーマンスプロファイル

```lisp
(configure-profile :performance
  :optimization 3
  :backend :hybrid
  :jit t
  :parallel t
  :simd t)
```

### メモリ節約プロファイル

```lisp
(configure-profile :memory-efficient
  :optimization 1
  :backend :dfa
  :cache-size 100
  :memory-pool :enabled)
```

### デバッグプロファイル

```lisp
(configure-profile :debug
  :optimization 0
  :debug-mode t
  :log-level :trace
  :collect-statistics t)
```

## 動的設定変更

### 実行時設定変更

```lisp
(configure-regex
  :optimization 3
  :backend :hybrid
  :cache-size 2000)
```

### 一時的設定変更

```lisp
(with-temporary-configuration (:optimization 0 :debug-mode t)
  (match complex-pattern text))
```

### 設定の保存・復元

```lisp
;; 現在の設定を保存
(defparameter *saved-config* (save-configuration))

;; 設定を復元
(restore-configuration *saved-config*)
```

## 設定の検証

### 設定値の検証

```lisp
(validate-configuration)  ; 現在の設定を検証
```

### 推奨設定の取得

```lisp
(get-recommended-configuration
  :usage :general-purpose
  :text-size :medium
  :pattern-complexity :high)
```

## ベストプラクティス

### 本番環境推奨設定

```lisp
(configure-regex
  :optimization 2
  :backend :auto
  :cache-size 1000
  :log-level :warn
  :error-mode :strict)
```

### 開発環境推奨設定

```lisp
(configure-regex
  :optimization 1
  :debug-mode t
  :log-level :debug
  :collect-statistics t
  :error-mode :strict)
```

### 高性能環境推奨設定

```lisp
(configure-regex
  :optimization 3
  :backend :hybrid
  :jit t
  :parallel t
  :cache-size 5000)
```

この設定システムにより、CL-Regexは様々な環境と要件に柔軟に対応できます。