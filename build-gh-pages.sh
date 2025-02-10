#!/usr/bin/env bash
set -e
current_branch=$(git symbolic-ref --short HEAD)
echo "currently on branch: $current_branch"
if ! git diff-index --quiet HEAD --; then
echo "Error: There are modified or uncommitted files. Please commit or stash changes first."
exit 1
fi
if git worktree list | grep -q ./gh-pages; then
git worktree remove ./gh-pages
git branch -D gh-pages
fi
git checkout --orphan gh-pages
git rm -rf .
echo "gh-pages branch" > README.md
git add README.md
git commit -m "initialize gh-pages branch"
git checkout "$current_branch"
git worktree add ./gh-pages gh-pages
cd gh-pages && mkdir -p ./target/esbuild
cp ../index.html ./
cp ../favicon.ico ./
cp ../edit.svg ./
cp ../ext.svg ./
cp ../target/esbuild/bundle.js ./target/esbuild
git add .
git commit -m "update gh-pages branch with new built site"
cd ..
echo "done building gh-pages branch"