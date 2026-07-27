#!/usr/bin/env bash
set -euo pipefail

# Defaults
DEFAULT_NAME="Hieu Nguyen"
DEFAULT_EMAIL="zacahabim@gmail.com"
DEFAULT_EDITOR="emacs -nw"
DEFAULT_LFS="yes"
DEFAULT_GPG="no"

OUTPUT="$HOME/.gitconfig"
ALIAS_PATH=".gitalias.txt"

prompt() {
    local var="$1" prompt_text="$2" default="$3"
    read -rp "$prompt_text ($default): " input
    eval "$var=\"${input:-$default}\""
}

prompt_email() {
    local var="$1"
    local script_dir
    script_dir="$(cd "$(dirname "$0")" && pwd)"
    local repo_root
    repo_root="$(cd "$script_dir" && git rev-parse --show-toplevel 2>/dev/null || echo "$script_dir/..")"
    local work_email_file="$repo_root/.work_email"
    local work_email=""
    if [[ -f "$work_email_file" ]]; then
        work_email=$(cat "$work_email_file")
    fi

    echo ""
    echo "Select email:"
    echo "  1) $DEFAULT_EMAIL (default)"
    if [[ -n "$work_email" ]]; then
        echo "  2) $work_email (work)"
        echo "  3) Custom"
    else
        echo "  2) Custom"
    fi
    read -rp "Choice [1]: " choice
    choice="${choice:-1}"

    case "$choice" in
        1) eval "$var=\"$DEFAULT_EMAIL\"" ;;
        2)
            if [[ -n "$work_email" ]]; then
                eval "$var=\"$work_email\""
            else
                read -rp "Enter email: " custom_email
                eval "$var=\"$custom_email\""
            fi
            ;;
        3)
            read -rp "Enter email: " custom_email
            eval "$var=\"$custom_email\""
            ;;
        *) eval "$var=\"$DEFAULT_EMAIL\"" ;;
    esac
}

if [[ "${1:-}" == "-m" ]]; then
    prompt NAME "Name" "$DEFAULT_NAME"
    prompt_email EMAIL
    prompt EDITOR_CMD "Editor" "$DEFAULT_EDITOR"
    prompt USE_LFS "Configure git lfs? [yes/no]" "$DEFAULT_LFS"
    prompt USE_GPG "Configure gpg signing? [yes/no]" "$DEFAULT_GPG"
else
    NAME="$DEFAULT_NAME"
    EMAIL="$DEFAULT_EMAIL"
    EDITOR_CMD="$DEFAULT_EDITOR"
    USE_LFS="$DEFAULT_LFS"
    USE_GPG="$DEFAULT_GPG"
fi

CONTENT="[user]
	email = $EMAIL
	name = $NAME"

if [[ "$USE_GPG" == "yes" ]]; then
    if [[ "${1:-}" == "-m" ]]; then
        mapfile -t GPG_KEYS < <(gpg --list-secret-keys --keyid-format long 2>/dev/null | grep '^sec' | sed 's|.*/||;s| .*||')
        if [[ ${#GPG_KEYS[@]} -eq 0 ]]; then
            echo "No GPG keys found."
            prompt GPG_KEY "GPG key ID (enter manually)" ""
        else
            echo ""
            echo "Available GPG keys:"
            for i in "${!GPG_KEYS[@]}"; do
                echo "  $((i+1))) ${GPG_KEYS[$i]}"
            done
            read -rp "Select key [1-${#GPG_KEYS[@]}]: " selection
            GPG_KEY="${GPG_KEYS[$((selection-1))]}"
        fi
    else
        GPG_KEY=""
    fi
    CONTENT+="
	signingkey = $GPG_KEY"
fi

CONTENT+="
[include]
	path = $ALIAS_PATH
[core]
	editor = $EDITOR_CMD
	hooksPath = ~/.git-hooks"

if [[ "$USE_LFS" == "yes" ]]; then
    CONTENT+="
[filter \"lfs\"]
	clean = git-lfs clean -- %f
	smudge = git-lfs smudge -- %f
	process = git-lfs filter-process
	required = true"
fi

if [[ "$USE_GPG" == "yes" ]]; then
    CONTENT+="
[gpg]
	program = gpg
[commit]
	gpgsign = true
[tag]
	gpgsign = true
[push]
	gpgsign = if-asked"
fi

echo ""
echo "--- Preview of $OUTPUT ---"
echo "$CONTENT"
echo "---"
echo ""
read -rp "Write to $OUTPUT? [y/N]: " confirm
if [[ "$confirm" =~ ^[Yy]$ ]]; then
    echo "$CONTENT" > "$OUTPUT"
    cp "$(dirname "$0")/.gitalias.txt" "$HOME/.gitalias.txt"

    # Install global pre-commit hook
    mkdir -p "$HOME/.git-hooks"
    cat > "$HOME/.git-hooks/pre-commit" <<'HOOK'
#!/usr/bin/env bash
EMAIL=$(git config user.email)
echo "Committing as: $EMAIL"
read -rp "Continue? [Y/n]: " confirm </dev/tty
if [[ "$confirm" =~ ^[Nn]$ ]]; then
    echo "Aborted."
    exit 1
fi
HOOK
    chmod +x "$HOME/.git-hooks/pre-commit"

    echo "Written to $OUTPUT"
    echo "Copied .gitalias.txt to $HOME/.gitalias.txt"
    echo "Installed global pre-commit hook to $HOME/.git-hooks/pre-commit"
else
    echo "Aborted."
fi
