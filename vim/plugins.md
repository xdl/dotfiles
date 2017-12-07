# Plugins

```
pushd ~/.vim/bundle
```

## Core

```
git clone https://github.com/scrooloose/nerdtree.git;
git clone https://github.com/scrooloose/nerdcommenter.git;
git clone https://github.com/ctrlpvim/ctrlp.vim.git;
git clone https://github.com/tpope/vim-surround;
git clone https://github.com/tpope/vim-repeat;
git clone https://github.com/tpope/vim-fugitive;
git clone https://github.com/tpope/vim-unimpaired;
git clone https://github.com/xdl/vim-slime.git;
pushd vim-slime && git checkout tmux_eval_result_in_buffer && popd;
git clone https://github.com/scrooloose/syntastic.git;
git clone https://github.com/SirVer/ultisnips.git
git clone https://github.com/xdl/vim-snippets.git;
pushd vim-snippets && git checkout personal_snippets && popd;
git clone https://github.com/bling/vim-airline;
git clone https://github.com/morhetz/gruvbox.git;
git clone https://github.com/joshdick/onedark.vim;
git clone https://github.com/sjl/badwolf;
```

(ultisnips requires Python)

## Nice to Have

```
# YCM requires cmake
git clone https://github.com/Valloric/YouCompleteMe;
pushd YouCompleteMe && git submodule update --init --recursive && ./install.py --tern-completer --clang-completer && popd;
git clone https://github.com/terryma/vim-expand-region;
git clone https://github.com/xdl/vim-replr.git;
git clone https://github.com/vim-scripts/SyntaxRange;
git clone https://github.com/godlygeek/tabular.git;
```

## Language Specific

```
git clone https://github.com/jdonaldson/vaxe;
git clone https://github.com/mxw/vim-jsx;
git clone https://github.com/fatih/vim-go.git;
git clone https://github.com/pangloss/vim-javascript;
git clone https://github.com/leafgarland/typescript-vim.git;
git clone https://github.com/mattn/emmet-vim.git;
git clone https://github.com/PProvost/vim-ps1.git;
git clone https://github.com/alunny/pegjs-vim;
```
