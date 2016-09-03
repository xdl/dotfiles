# initialisation
echo "initialising..."
rm -rf ~/.vim && \
mkdir ~/.vim && \
mkdir ~/.vim/autoload  && \
mkdir ~/.vim/bundle && \
mkdir ~/.vim/tmpfiles;

# installing pathogen
echo "installing pathogen..."
mkdir ~/.vim/tmp && \
pushd ~/.vim/tmp && \
git clone https://github.com/tpope/vim-pathogen.git && \
cp ~/.vim/tmp/vim-pathogen/autoload/pathogen.vim ~/.vim/autoload && \
rm -rf ~/.vim/tmp;
popd;

# everything else
echo "installing everything else..."
pushd ~/.vim/bundle;

# Barebones, low impact
git clone https://github.com/scrooloose/nerdtree.git;
git clone https://github.com/scrooloose/nerdcommenter.git;

git clone https://github.com/ctrlpvim/ctrlp.vim.git;

git clone https://github.com/tpope/vim-surround;
git clone https://github.com/tpope/vim-repeat;
git clone https://github.com/tpope/vim-fugitive;

git clone https://github.com/jpalardy/vim-slime.git;

git clone https://github.com/scrooloose/syntastic.git;

git clone https://github.com/SirVer/ultisnips.git
git clone https://github.com/xdl/vim-snippets.git;

# Useful, but may cause performance hit

git clone https://github.com/bling/vim-airline;

# Rarely used

git clone https://github.com/terryma/vim-expand-region;
git clone https://github.com/xdl/vim-replr.git;
git clone https://github.com/godlygeek/tabular.git;

# Syntax

#git clone https://github.com/fatih/vim-go.git;

#git clone https://github.com/pangloss/vim-javascript;
#git clone https://github.com/leafgarland/typescript-vim.git;

#git clone https://github.com/mattn/emmet-vim.git;

#git clone https://github.com/PProvost/vim-ps1.git;

# Colorschemes

git clone https://github.com/morhetz/gruvbox.git;
git clone https://github.com/sjl/badwolf;

popd;

echo "done!"
