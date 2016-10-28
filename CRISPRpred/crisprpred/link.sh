#!/usr/bin/env bash
# vim: set noexpandtab tabstop=2:

target_dir=R
rm -rf "$target_dir"
mkdir -p "$target_dir"
cd "$target_dir"

while read -r -d '' p
do
  export_dirs+=("$p")
done < <(find ../R_src/ -mindepth 1 '(' -type d -name backup -prune ')' -o '(' -type f -name .export -printf '%h\0' ')')

while read -r -d '' f
do
  ln -fs "$f"
done < <(find "${export_dirs[@]}" -maxdepth 1 -name '*.R' -print0)

