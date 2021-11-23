# load this file when you want to use GNU commands but macOS

# homebrew
PATH=/usr/local/bin:/usr/local/sbin:${PATH}
MANPATH=/usr/local/share/man:${MANPATH}

# coreutils
PATH=/usr/local/opt/coreutils/libexec/gnubin:${PATH}
MANPATH=/usr/local/opt/coreutils/libexec/gnuman:${MANPATH}
# findutils
PATH=/usr/local/opt/findutils/libexec/gnubin:${PATH}
MANPATH=/usr/local/opt/findutils/libexec/gnuman:${MANPATH}
# sed
PATH=/usr/local/opt/gnu-sed/libexec/gnubin:${PATH}
MANPATH=/usr/local/opt/gnu-sed/libexec/gnuman:${MANPATH}
# tar
PATH=/usr/local/opt/gnu-tar/libexec/gnubin:${PATH}
MANPATH=/usr/local/opt/gnu-tar/libexec/gnuman:${MANPATH}
# grep
PATH=/usr/local/opt/grep/libexec/gnubin:${PATH}
MANPATH=/usr/local/opt/grep/libexec/gnuman:${MANPATH}
# binutils
PATH=/usr/local/opt/binutils/bin:${PATH}
