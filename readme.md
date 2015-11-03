# 優美なる死体 Haskell実装

Hypercardスタック「優美なる死体 v2.1」をHaskellで実装しました。辞書データはMySQLに登録します。

## 準備

### データベースの準備

データベースを作成後、ce.sqlを実行してください。

~~~~
mysql> source /path/to/dir/ce.sql
~~~~

### Twitterの準備

生成した文はTwitterにPostします。Twitterでのアプリケーションの認証が必要です。TwitterにPostしない場合はce.hsの169行目以降をコメントアウトしてください。

### コードの準備

[stack](http://www.stackage.org/)が必要です。また、GHC7.10が必要です。インストールしているGHCのバージョンが古い場合は、stack setup を実行してください。

~~~~
$ stack build
~~~~

ce.yml.sampleをコピーしてce.ymlファイルを作成し、データベースとTwitterの接続情報を記述してください。

## 実行

~~~~
$ stack exec ce
~~~~
