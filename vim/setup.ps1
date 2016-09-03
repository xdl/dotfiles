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
Write-Host "install everything else from plugins.md"
pushd $home\vimfiles\bundle;
