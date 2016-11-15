#rm -f ~/.gitconfig;
#ln -s ~/dotfiles/.gitconfig ~/.gitconfig;

#echo "creating symbolic link for bashrc..."
#rm -f ~/.bashrc;
#ln -s ~/dotfiles/.bashrc ~/.bashrc;

#rm -f ~/.bash_profile;
#ln -s ~/dotfiles/.bash_profile ~/.bash_profile;

echo "creating symbolic link for .tmux.conf..."
rm -f ~/.tmux.conf;
ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf;

echo "creating symbolic link for .vimrc..."
rm -f ~/.vimrc;
ln -s ~/dotfiles/vim/.vimrc ~/.vimrc;

echo "creating symbolic link for .tern-project..."
rm -f ~/.tern-project;
ln -s ~/dotfiles/.tern-project ~/.tern-project;

echo "creating symbolic link for emacs..."
mkdir -p ~/.emacs.d;
rm -f ~/.emacs.d/init.el;
ln -s ~/dotfiles/emacs/init.el ~/.emacs.d/init.el;

echo "done!"
