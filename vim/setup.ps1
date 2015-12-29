# initialisation
Remove-Item -Recurse -Force $home\vimfiles;
New-Item $home\vimfiles -type directory;
New-Item $home\vimfiles\bundle -type directory;
New-Item $home\vimfiles\autoload -type directory;
New-Item $home\vimfiles\tmpfiles -type directory;

# installing pathogen
Write-Host "installing pathogen..."
pushd $home\vimfiles;
git clone https://github.com/tpope/vim-pathogen.git;
Copy-Item vim-pathogen\autoload\pathogen.vim autoload;
Remove-Item -Recurse -Force vim-pathogen;
popd;

# plugins
Write-Host "installing everything else..."
pushd $home\vimfiles\bundle;

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

Write-Host "Done!"
