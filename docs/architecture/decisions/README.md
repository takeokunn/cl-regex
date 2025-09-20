# アーキテクチャ決定記録 (ADR) ディレクトリ

このディレクトリには、CL-Regexプロジェクトにおける重要なアーキテクチャ決定の記録が含まれています。

## ADR (Architecture Decision Records) について

ADRは、ソフトウェアアーキテクチャに関する重要な決定を文書化する手法です。各決定は、コンテキスト、決定内容、理由、結果、トレードオフを明確に記録します。

## 現在のADR一覧

### 実装済み

| ADR | タイトル | ステータス | 決定日 |
|-----|----------|-----------|--------|
| [ADR-001](../design-decisions.md#adr-001-マクロシステムの採用による優れた最適化) | マクロシステムの採用による優れた最適化 | ✅ 採用 | 2024-03 |
| [ADR-002](../design-decisions.md#adr-002-pure-lisp実装による完全制御) | Pure Lisp実装による完全制御 | ✅ 採用 | 2024-03 |
| [ADR-003](../design-decisions.md#adr-003-closによる多重ディスパッチ設計) | CLOSによる多重ディスパッチ設計 | ✅ 採用 | 2024-03 |
| [ADR-004](../design-decisions.md#adr-004-s式prolog統合による宣言的パターン) | S式Prolog統合による宣言的パターン | ✅ 採用 | 2024-03 |
| [ADR-005](../design-decisions.md#adr-005-property-based-testing-pbt--tdd戦略) | Property-Based Testing + TDD戦略 | ✅ 採用 | 2024-03 |
| [ADR-006](../design-decisions.md#adr-006-ハイブリッドエンジン設計) | ハイブリッドエンジン設計 | ✅ 採用 | 2024-03 |

### 検討中

| ADR | タイトル | ステータス | 検討開始 |
|-----|----------|-----------|----------|
| ADR-007 | GPU加速統合 | 🤔 検討中 | 2024-04 |
| ADR-008 | 機械学習ベース最適化 | 🤔 検討中 | 2024-04 |
| ADR-009 | 分散システム対応 | 🤔 検討中 | 2024-05 |

## ADRの構造

各ADRは以下の構造で記述されています：

```markdown
## ADR-XXX: タイトル

### ステータス
[提案中 | 採用 | 廃止 | 置換済み]

### コンテキスト
決定が必要になった背景と状況

### 決定内容
採用した解決策

### 理由
なぜこの決定を行ったか

### 結果
決定による影響と効果

### トレードオフ
利点と欠点
```

## 新しいADRの追加

1. `docs/explanation/design-decisions.md`に新しいADRセクションを追加
2. このREADMEの表を更新
3. 関連するドキュメントを更新

## 参考資料

- [Architecture Decision Records](https://adr.github.io/)
- [Documenting Architecture Decisions](https://cognitect.com/blog/2011/11/15/documenting-architecture-decisions)