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
echo "install everything else from plugins.md"
pushd ~/.vim/bundle;
