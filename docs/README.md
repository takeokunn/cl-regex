# CL-Regex Documentation

高性能かつ拡張可能なCommon Lisp正規表現エンジンのドキュメント

## ドキュメント構成（Diátaxis Framework）

本ドキュメントは[Diátaxis](https://diataxis.fr/)フレームワークに基づいて構成されています。

### 📚 [Tutorials（チュートリアル）](./tutorials/)
学習指向 - CL-Regexの使い方を段階的に学ぶ
- [Getting Started](./tutorials/01-getting-started.md) - 最初の一歩
- [Basic Pattern Matching](./tutorials/02-basic-patterns.md) - 基本的なパターンマッチング
- [Advanced Features](./tutorials/03-advanced-features.md) - 高度な機能の活用
- [Building Custom Extensions](./tutorials/04-custom-extensions.md) - 拡張機能の開発

### 🛠 [How-To Guides（ハウツーガイド）](./how-to/)
タスク指向 - 特定の問題を解決する方法
- [Performance Optimization](./how-to/performance-optimization.md) - パフォーマンス最適化
- [Custom Operators](./how-to/custom-operators.md) - カスタム演算子の実装
- [Unicode Support](./how-to/unicode-support.md) - Unicode対応
- [Debugging Patterns](./how-to/debugging-patterns.md) - パターンのデバッグ
- [Integration with CLOS](./how-to/clos-integration.md) - CLOSとの統合
- [Macro DSL Creation](./how-to/macro-dsl.md) - マクロDSLの構築

### 📖 [Explanation（技術説明）](./explanation/)
理解指向 - CL-Regexの設計思想と内部構造
- [Architecture Overview](./explanation/architecture.md) - アーキテクチャ概要
- [NFA/DFA Engine](./explanation/engine.md) - 正規表現エンジンの仕組み
- [Compilation Pipeline](./explanation/compilation.md) - コンパイルパイプライン
- [Optimization Techniques](./explanation/optimizations.md) - 最適化技術
- [CLOS Design Patterns](./explanation/clos-patterns.md) - CLOSデザインパターン
- [Macro System Design](./explanation/macro-system.md) - マクロシステムの設計

### 📋 [Reference（リファレンス）](./reference/)
情報指向 - API仕様と詳細な技術情報
- [API Documentation](./reference/api.md) - API詳細仕様
- [Pattern Syntax](./reference/syntax.md) - パターン構文リファレンス
- [Operators Reference](./reference/operators.md) - 演算子リファレンス
- [CLOS Classes](./reference/classes.md) - CLOSクラス階層
- [Macro Reference](./reference/macros.md) - マクロリファレンス
- [Configuration Options](./reference/configuration.md) - 設定オプション

## クイックリンク

- 🚀 **初めての方**: [Getting Started Tutorial](./tutorials/01-getting-started.md)
- 💡 **実装者向け**: [Architecture Overview](./explanation/architecture.md)
- 🔧 **API利用者**: [API Documentation](./reference/api.md)
- 🧪 **テスト戦略**: [Testing Strategy](./explanation/testing-strategy.md)

## プロジェクト概要

CL-Regexは、Common Lispで実装された高性能な正規表現エンジンです。

### 主要な特徴

- **マクロベースDSL**: 直感的な正規表現記述
- **CLOS統合**: 拡張可能なオブジェクト指向設計
- **最適化コンパイラ**: JITコンパイルによる高速実行
- **プロパティベーステスト**: 堅牢性の保証
- **Unicode完全対応**: 国際化対応
- **拡張可能アーキテクチャ**: プラグインシステム

## ライセンス

MIT License