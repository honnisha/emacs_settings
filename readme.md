<p align="center">
  <a href="https://github.com/honnisha/emacs_settings">
    <img alt="Godot Jolt" src="https://github.com/honnisha/emacs_settings/blob/master/logo.png">
  </a>
</p>

## AUR Helper

[Install wiki](https://github.com/Jguer/yay)

```bash
pacman -S --needed git base-devel
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
```

## Arch packages

<h4>Other</h4>

```bash
yay -S ttf-hack ttf-hack-nerd ttf-all-the-icons tree-sitter dropbox outline-client-appimage emacs-nativecomp wezterm
```

## Configs

```bash
rm ~/.config/doom/*
ln -s ~/Projects/emacs_settings/doom_configs/packages.el ~/.config/doom/packages.el
ln -s ~/Projects/emacs_settings/doom_configs/init.el ~/.config/doom/init.el
ln -s ~/Projects/emacs_settings/doom_configs/config.el ~/.config/doom/config.el
ln -s ~/Projects/emacs_settings/configs/wezterm.lua ~/.wezterm.lua
ln -s ~/Projects/emacs_settings/configs/Outline-Client.AppImage.desktop ~/.config/autostart/Outline-Client.AppImage.desktop
sudo chmod a-w ~/.config/autostart/Outline-Client.AppImage.desktop
ln -s ~/Projects/emacs_settings/configs/isort.cfg ~/.isort.cfg
```

## Doom emacs

[Install wiki](https://github.com/doomemacs/doomemacs?tab=readme-ov-file#install)

```bash
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
~/.config/emacs/bin/doom sync
```

## Arch Dependencies

<h4>Python</h4>

```bash
yay -S python python-pip pyright python-lsp-server ruff python-virtualenv python-psycopg2 python-black ruff-lsp python-lsp-server hunspell
pip install epc orjson sexpdata six orjson pyright python-lsp-server[all] rope ruff ruff-lsp flake8 mypy pylint isort virtualenvwrapper virtualenv==20.0.23 "python-lsp-server[all]" setuptools pipenv  python-lsp-server --break-system-packages
```

<h4>Rust</h4>

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
yay -S rust-analyzer
```

<h4>Docker</h4>

```bash
yay -S docker-compose docker
sudo systemctl enable docker
sudo groupadd docker
sudo usermod -aG docker $USER
sudo chmod 666 /var/run/docker.sock
```

<h4>Bash</h4>

`sudo emacs ~/.bashrc`
```
source "$HOME/Dropbox/aliases"

LC_ALL="en_US.UTF-8"
LC_CTYPE="en_US.UTF-8"
```

<h4>Vterm</h4>

```bash
yay -S cmake-git
```

<h4>NTFS partitions</h4>

```bash
yay -S ntfs-3g os-prober
sudo grub-mkconfig -o /boot/grub/grub.cfg
```
