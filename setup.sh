# deleting any existing configs
rm ~/.gitconfig;
rm ~/.bashrc;
rm ~/.tmux.conf;
rm ~/.vimrc;

# hard linking over those other ones
ln ~/dotfiles/.gitconfig ~/.gitconfig;
ln ~/dotfiles/.bashrc ~/.bashrc;
ln ~/dotfiles/.tmux.conf ~/.tmux.conf;
ln ~/dotfiles/vim/.vimrc ~/.vimrc;
