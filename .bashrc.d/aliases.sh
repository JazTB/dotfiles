#ALIASES
alias ls='ls -a --color=auto'
alias ll='ls -alh --color=auto'
alias grep='grep --color=auto'

alias hx='helix'
alias nv='nvim'
alias nvqt='nvim-qt'

alias xi='doas xbps-install -yv'
alias xr='doas xbps-remove'
alias xiu='doas xbps-install -Syuv'
alias xix='doas xbps-install -Syuv xbps; doas xbps-install -Syuv'
alias xq='xbps-query -Rs'

# PL toolings
alias dr='dotnet run'
alias db='dotnet build'
alias dc='dotnet clean'

alias sr='swift run'
alias sb='swift build'
alias sc='swift package clean'

alias cr='cargo run'
alias cb='cargo build'

alias cl='rlwrap sbcl'
alias ri='roswell install'
alias fnl='rlwrap fennel --lua lua5.1'

alias gears='__GL_SYNC_TO_VBLANK=0 glxgears'
alias ruby='ruby --yjit'

alias nb='nix-build'
alias ns='nix-shell'
alias nsp='nix-shell -p '

alias dn='dub init ' # D-language package
