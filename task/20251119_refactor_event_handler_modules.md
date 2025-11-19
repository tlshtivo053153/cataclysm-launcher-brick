# イベントハンドラモジュールのリファクタリング計画

`Events`ディレクトリ内のUIイベント処理モジュールと、`Soundpack`ディレクトリ内のコアビジネスロジックモジュール間で命名の衝突が確認されました。これにより、コードの可読性と保守性が低下する可能性があります。

このリファクタリングでは、イベントハンドラモジュールに`Handler`という接尾辞を追加することで、責務を明確にし、命名の衝突を解消します。

## TODOリスト

- [ ] `src/Events/Soundpack/Common.hs`を`src/Events/Soundpack/CommonHandler.hs`に名前変更し、モジュール宣言とインポートを更新する
- [ ] `src/Events/Soundpack/Install.hs`を`src/Events/Soundpack/InstallHandler.hs`に名前変更し、モジュール宣言とインポートを更新する
- [ ] `src/Events/Soundpack/List.hs`を`src/Events/Soundpack/ListHandler.hs`に名前変更し、モジュール宣言とインポートを更新する
- [ ] `src/Events/Soundpack/Uninstall.hs`を`src/Events/Soundpack/UninstallHandler.hs`に名前変更し、モジュール宣言とインポートを更新する
- [ ] プロジェクトをコンパイルして、リファクタリングが成功したことを確認する
