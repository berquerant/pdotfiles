#!/bin/bash
set -euo pipefail

# emacs-extract-modes.sh:
# Extract modes configured via use-package and bind-key in Emacs configuration,
# and generate a shell script / command that loads each mode.

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Options:
  -d, --dir DIR      Path to Emacs configuration directory (default: \${EMACSD:-\$HOME/.emacs.d})
  -f, --format FMT   Output format: list, elisp, command, script, all (default: all)
  -t, --target TGT   Target mode group: all, use-package, bind-key (default: all)
  -h, --help         Show this help message
EOF
}

d=$(cd "$(dirname "$0")" || exit 1; pwd)
dotfiles_root=$(cd "${d}/.." || exit 1; pwd)

emacs_dir="${EMACSD:-${HOME}/.emacs.d}"
# Fallback to repository .emacs.d if directory does not exist
if [ ! -d "${emacs_dir}" ] && [ -d "${dotfiles_root}/.emacs.d" ]; then
    emacs_dir="${dotfiles_root}/.emacs.d"
fi

format="all"
target="all"

while [ $# -gt 0 ]; do
    case "$1" in
        -d|--dir)
            emacs_dir="$2"
            shift 2
            ;;
        -f|--format)
            format="$2"
            shift 2
            ;;
        -t|--target)
            target="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage >&2
            exit 1
            ;;
    esac
done

py_script="${d}/emacs-extract-modes.py"
if [ ! -f "${py_script}" ]; then
    echo "Error: Python helper not found: ${py_script}" >&2
    exit 1
fi

if [ "${format}" = "script" ]; then
    # Generate standalone runnable shell script
    cat <<EOF
#!/bin/bash
# Generated Emacs mode loader script
set -euo pipefail

EMACS_DIR="\${1:-\"${emacs_dir}\"}"
INIT_FILE="\${EMACS_DIR}/init.el"

echo "Loading modes in Emacs batch mode..."
emacs --batch -d "\${EMACS_DIR}" -l "\${INIT_FILE}" --eval \\
  '$(python3 "${py_script}" --emacs-dir "${emacs_dir}" --target "${target}" --format elisp)'
EOF
    exit 0
fi

python3 "${py_script}" --emacs-dir "${emacs_dir}" --target "${target}" --format "${format}"
