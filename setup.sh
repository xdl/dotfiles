#rm -f ~/.gitconfig;
#ln -s ~/dotfiles/.gitconfig ~/.gitconfig;

#rm -f ~/.bashrc;
#ln -s ~/dotfiles/.bashrc ~/.bashrc;

echo "creating symbolic link for tmux..."
rm -f ~/.tmux.conf;
ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf;

echo "creating symbolic link for vim..."
rm -f ~/.vimrc;
ln -s ~/dotfiles/vim/.vimrc ~/.vimrc;

echo "creating symbolic link for emacs..."
mkdir -p ~/.emacs.d;
rm -f ~/.emacs.d/init.el;
ln -s ~/dotfiles/emacs/init.el ~/.emacs.d/init.el;

echo "done!"
