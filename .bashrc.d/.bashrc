# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

### PATH

append_path () {
    case ":$PATH:" in
        *:"$1":*)
            ;;
        *)
            PATH="${PATH:+$PATH:}$1"
    esac
}

export append_path

PATH="$HOME/BIN:$HOME/OPT/shbin/:$HOME/.ghcup/bin/:$HOME/.cabal/bin/:"$PATH
append_path "$HOME/.nimble/bin"
append_path "$HOME/common-lisp/lem"
append_path "$HOME/.ghcup/bin"
append_path "$HOME/.cabal/bin"
append_path "$HOME/.cargo/bin"
append_path "$HOME/.local/share/gem/ruby/3.3.0/bin"
append_path "$HOME/.local/bin"
append_path "$HOME/.dotnet/tools"
append_path "$HOME/.roswell/bin"
append_path "$HOME/.qlot/bin"

### SOURCES

for file in ~/.bashrc.d/*.sh; do
    if [ -f $file ]; then
        source $file
    fi
done

source "$HOME/.cargo/env"
source "$HOME/OPT/env"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
JAVA_HOME=/usr/lib/jvm/default/
export JAVA_HOME
KOTLIN_HOME="$HOME/.sdkman/candidates/kotlin/current/"
export KOTLIN_HOME

eval "$(rbenv init -)"
export MUSL_HOME=$HOME/MUSL

[ -f ~/.reminderlist.txt ] && echo -e "REMINDERLIST >>\n$(cat ~/.reminderlist.txt)\n---\n"
