# lookup.elのmacosの辞書.appをバックエンドにする何か

## 依存関係
辞書.appの検索にはosx-dictionaryに依存しています。
- https://github.com/aztack/osx-dictionary/tree/master
適宜cmake等してシステムにインストールしてください。
## 設定等への設定の外だし
はまだできてません。

## インストール
cloneしてロードパス通してrequireしてください。
### 例
わたしの手元の設定を下記します
```
(add-to-list 'load-path "~/.emacs.d/lookup")
(add-to-list 'load-path "~/.emacs.d/lookup-macos")
(autoload 'lookup "lookup" nil t)
(require 'nmacos) ;; これが必要
(autoload 'lookup-region "lookup" nil t)
(autoload 'lookup-pattern "lookup" nil t)
(setq lookup-enable-splash nil)
(setq lookup-search-agents
      '(
        (nmacos) ;; 辞書にmacosの辞書を追加
        (ndeb "~/dic/kojign04/") ;; 他のepwingとかの辞書
        (ndeb "~/dic/sinseiki/") ;; 他のepwingとかの辞書
        (ndeb "~/dic/Wikip_ja20230120/") ;; 他のepwingとかの辞書
        ))

```

