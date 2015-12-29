# deleting any existing configs
rm ~/.gitconfig;
rm ~/.bashrc;
rm ~/.tmux.conf;
rm ~/.vimrc;

# soft-linking over the ones from this repo
ln -s ~/dotfiles/.gitconfig ~/.gitconfig;
ln -s ~/dotfiles/.bashrc ~/.bashrc;
ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf;
ln -s ~/dotfiles/vim/.vimrc ~/.vimrc;
