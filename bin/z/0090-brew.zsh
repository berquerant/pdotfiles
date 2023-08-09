alias arm="exec arch -arch arm64e /bin/zsh --login"
alias x64="exec arch -arch x86_64 /bin/zsh --login"

__arch=$(uname -m)
if [ "$__arch" = "arm64" ]; then
    eval $(/opt/homebrew/bin/brew shellenv)
elif [ "$__arch" = "x86_64" ]; then
    eval $(/usr/local/bin/brew shellenv)
fi
