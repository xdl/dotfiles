# Dotfiles

Configuration files/scripts for various tools and shells

## Windows

Current tmux/Emacs setup is through WSL, with graphics supplied by VcXsrv (https://parsethesource.com/articles/emacs-in-the-wsl/)

* Enable higher DPI text scaling with letting the application perform scaling in C:\Program Files\VcXsrv\vcxsrv.exe (https://superuser.com/questions/1370361/blurry-fonts-on-using-windows-default-scaling-with-wsl-gui-applications-hidpi)

## {Li,U}nix

Initial setup:

```
chmod u+x setup.sh
./setup.sh
```

For vim:

```
chmod u+x vim/setup.sh
vim/setup.sh
```

For fonts:

    # Chinese
    sudo apt install xfonts-wqy
    
    # DeJaVu
    sudo apt-get install fonts-dejavu

## Emacs dependencies

Requires dot (graphviz) for orgmode

    sudo apt install graphviz
