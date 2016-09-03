#rm -f ~/.gitconfig;
#ln -s ~/dotfiles/.gitconfig ~/.gitconfig;

#rm -f ~/.bashrc;
#ln -s ~/dotfiles/.bashrc ~/.bashrc;

rm -f ~/.tmux.conf;
ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf;

rm -f ~/.vimrc;
ln -s ~/dotfiles/vim/.vimrc ~/.vimrc;

mkdir -p ~/.emacs.d;
rm -f ~/.emacs.d/init.el;
ln -s ~/dotfiles/emacs/init.el ~/.emacs.d/init.el;
