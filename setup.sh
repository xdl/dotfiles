if  [ ! -f ~/.bashrc ]; then
    echo "linking over .bashrc...";
    ln -s ~/dotfiles/.bashrc ~/.bashrc;
else
    echo ".bashrc already exists; skipping";
fi

if  [ ! -f ~/.bash_profile ]; then
    echo "linking over .bash_profile...";
    ln -s ~/dotfiles/.bash_profile ~/.bash_profile;
else
    echo ".bash_profile already exists; skipping";
fi

if  [ ! -f ~/.tmux.conf ]; then
    echo "linking over .tmux.conf...";
    ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf;
else
    echo ".tmux.conf already exists; skipping";
fi

if  [ ! -f ~/.vimrc ]; then
    echo "linking over minimal .vimrc...";
    ln -s ~/dotfiles/vim/.vimrc-minimal ~/.vimrc;
else
    echo ".vimrc already exists; skipping";
fi

if  [ ! -f ~/.tern-project ]; then
    echo "linking over .tern-project...";
    ln -s ~/dotfiles/.tern-project ~/.tern-project;
else
    echo ".tern-project already exists; skipping";
fi
  
mkdir -p ~/.emacs.d;

if  [ ! -f ~/.emacs.d/init.el ]; then
    echo "linking over init.el...";
    ln -s ~/dotfiles/emacs/init.el ~/.emacs.d/init.el;
else
    echo "init.el already exists; skipping";
fi

if  [ ! -f ~/.gitconfig ]; then
    echo "copying over .gitconfig... remember to set email address!";
    cp ~/dotfiles/.gitconfig ~/.gitconfig;
else
    echo ".gitconfig already exists; skipping";
fi

echo "Done!"
