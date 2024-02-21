Emacs settings
=======
Autor: Maria Ivanova (honnisha) ganagsh@gmail.com

Installation
=======

init.el
```
(setq settings_path "/home/honnisha/Projects/emacs_settings/")
(setq dropbox_path "/home/honnisha/Dropbox/")
(setq doom-font (font-spec :family "Hack" :size 11 :weight 'light))
(load! (concat settings_path "init.el"))
```

config.el
```
(load! (concat settings_path "config.el"))
```

packages.el
```
(load-file (concat settings_path "packages.el"))
```

Arch packages
=======

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
VIRTUALENVWRAPPER_PYTHON=$(which python3)
source ~/.local/bin/virtualenvwrapper.sh
```

<h4>Vterm</h4>
```bash
yay -S cmake-git
```

<h4>AUR Helper</h4>
```bash
pacman -S --needed git base-devel
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
```

<h4>NTFS partitions</h4>
```bash
yay -S ntfs-3g os-prober
sudo grub-mkconfig -o /boot/grub/grub.cfg
```

<h4>Fonts</h4>
```bash
yay -S ttf-hack ttf-hack-nerd ttf-all-the-icons
```

<h4>Tree-sitter</h4>
```bash
yay -S tree-sitter
```

<h4>Python</h4>
```bash
yay -S python python-pip pyright python-lsp-server ruff python-virtualenv python-psycopg2 python-black ruff-lsp python-lsp-server hunspell
pip install epc orjson sexpdata six orjson pyright python-lsp-server[all] rope ruff ruff-lsp flake8 mypy pylint isort virtualenvwrapper virtualenv==20.0.23 "python-lsp-server[all]" setuptools pipenv python-lsp-server --break-system-packages
python -m ensurepip --default-pip
```

<h4>Isort config</h4>
https://github.com/timothycrosley/isort</br>
`sudo emacs ~/.isort.cfg`
```
[settings]
multi_line_output=4
```

<h4>Rust</h4>
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
yay -S rust-analyzer
```

Screenshots
=======
<div align="center"><img src="https://github.com/gangashman/emacs_settings/blob/master/screenshots/1.png"/></div>

<div align="center"><img src="https://github.com/gangashman/emacs_settings/blob/master/screenshots/2.png"/></div>
