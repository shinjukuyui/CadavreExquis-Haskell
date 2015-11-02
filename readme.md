# 優美なる死体 Haskell実装

Hypercardスタック「優美なる死体 v2.1」をHaskellで実装しました。辞書データはMySQLに登録します。

## 準備

### データベースの準備

データベースを作成後、ce.sqlを実行してください。

~~~~
mysql > source ce.sql
~~~~

### Twitterの準備

生成した文はTwitterにPostします。Twitterでのアプリケーションの認証が必要です。TwitterにPostしない場合は169行目以降をコメントアウトしてください。

### コードの準備

~~~~
$ cabal sandbox init
$ cabal install --only-dependencies
~~~~

ce.yml.sampleをコピーしてce.ymlファイルを作成し、データベースとTwitterの接続情報を記述してください。

## 実行

~~~~
$ cabal run ce
~~~~

## 既知の問題

実行時にFromJSONErrが起きますが現在調査中です。
