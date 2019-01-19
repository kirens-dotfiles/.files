{ }:
''
# Prevent unintentional overwrites
abbr --add mv mv -i
abbr --add cp cp -i

# TMux
alias tmux-end-inactive="tmux ls | grep --invert-match attached | grep -oP '^[^:]+' | xargs -L 1 tmux kill-session -t"

# View files
abbr --add r vi -R

# Git shorthands
abbr --add gs git status
abbr --add gsu gs -uno
abbr --add ga git add
abbr --add gc git commit
abbr --add gcm git commit -m
abbr --add gcam git commit -am
abbr --add gf git fetch
abbr --add gm git merge
abbr --add gp git push
abbr --add gd git diff
abbr --add gl git log
abbr --add gl1 git log -n 1
abbr --add gprune git fetch --prune

# Move about and look around
abbr --add l ls -lAh
abbr --add la ls -A
abbr --add ld ls -ld

# Copy paste pipe
abbr --add cpy pbcopy
abbr --add pst pbpaste

# Nixos shortcut
alias nix-storefolders="find /nix/store -maxdepth 1 -type d -name '*' -a ! -name 'trash' -a ! -wholename '/nix/store' -a ! -name '.links'"
alias nix-storefind='nix-storefolders | grep'

abbr --add nix-system sudo nix-env -p /nix/var/nix/profiles/system
abbr --add nix-bootentry sudo nixos-rebuild switch -p
''
