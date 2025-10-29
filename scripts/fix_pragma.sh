#!/bin/bash

# プラグマ修正スクリプト
# 引数: 修正する.hsファイルパス（複数可）
#
# Usage: ./fix_pragma.sh [options] file1.hs file2.hs ...
# Options:
#   --dry-run     : 実際の変更を実行せず、変更点を表示
#   -b suffix     : バックアップファイルの接尾辞を指定（例: -b .orig）
# 機能: 引数で指定した.hsファイルのLANGUAGEプラグマの終わり（#}）を修正（#-}）に置換
#       修正前ファイルを.bakとしてバックアップ

set -euo pipefail

# デフォルト値
DRY_RUN=false
BACKUP_SUFFIX=".bak"

# オプション解析
while [[ $# -gt 0 ]]; do
  case $1 in
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    -b)
      BACKUP_SUFFIX="$2"
      shift 2
      ;;
    -*)
      echo "不明なオプション: $1" >&2
      exit 1
      ;;
    *)
      break
      ;;
  esac
done

# 引数が存在するか確認
if [[ $# -eq 0 ]]; then
  echo "エラー: ファイルを指定してください。" >&2
  echo "Usage: $0 [options] file1.hs file2.hs ..." >&2
  exit 1
fi

# ファイル処理関数
process_file() {
  local file="$1"
  local dry_run="$2"
  local backup_suffix="$3"

  if [[ ! -f "$file" ]]; then
    echo "エラー: ファイルが存在しません: $file" >&2
    return 1
  fi

  if [[ ! -r "$file" ]]; then
    echo "エラー: ファイルが読み込めません: $file" >&2
    return 1
  fi

  if [[ "$file" != *.hs ]]; then
    echo "警告: .hsファイルではありません: $file" >&2
    return 0
  fi

  if grep -q '#}' "$file"; then
    if [[ "$dry_run" == "true" ]]; then
      echo "修正（dry-run）: $file"
      grep '#}' "$file" | while read -r line; do
        echo "  変更前: $line"
        echo " 変更後: ${line/\#\}/#-}}"
      done
    else
      echo "修正: $file"
      cp "$file" "${file}${backup_suffix}"
      sed -i "s/#}/#-}/g" "$file"
      echo " バックアップ作成: ${file}${backup_suffix}"
    fi
 else
    echo "修正なし: $file"
  fi
}

# メイン処理
for file in "$@"; do
  process_file "$file" "$DRY_RUN" "$BACKUP_SUFFIX"
done

echo "完了"