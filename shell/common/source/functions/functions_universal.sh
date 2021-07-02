#!/bin/sh

# Returns the block of sourced code for the function or alias given as an argument:
func() {
  local CODE="$(declare -f $*)"
  if [[ -z "$CODE" ]]; then
    alias | grep --color=never "alias $*="
  else
    echo "$CODE"
  fi
}

# Exit with 0 status if input name is a shell function.
# Otherwise, exit with nonzero status.
funcp() {
  local LC_ALL=C
  local funcName="${1}"

  if [ -z "${funcName}" ]; then
    err "No function name provided."
    return 1
  fi
  {
    type -t "${funcName}" | grep -q "function"
  } &>/dev/null
}

# Returns the index of an array element
# Syntax: get_index array_name (no $ in front of array name) $element
get_index() {
  local array_name=$1[@]
  declare -a arr=("${!array_name}")
  local element=$2

  for i in "${!arr[@]}"; do
    if [[ "${arr[$i]}" == "${element}" ]]; then
      echo "${i}"
      exit 0
    fi
  done
  echo -1
}

# Shows all symlinks and the files they point to for a given directory.
# If no direcotry is specified, it checks the current directory.
syml() {
  local directory="."
  if [[ "$#" -gt 0 ]]; then
    directory="$1"
  fi

  ls -la "$directory" | grep '\->' | awk {'print $9 " " $10 " " $11'}
}

# Traverse the file tree recursively and delete all backup files:
# Syntax: rmbaks [-a | --all]
# adding the -a or --all argument removes all tmp files as well.
rmbaks() {
  local all="$1"
  if [[ "$1" == "-a" || "$1" == "--all" ]]; then
    find . -type f \( -name "*.bak" -or -name "*.tmp" \) -print0 | xargs -0 rm
  else
    find . -type f -name "*.bak" -print0 | xargs -0 rm
  fi
}

# Make a new executable bash script.
mkbin() {
  local executableName="$1"
  local bashTemplate
  if [ -L "$DOTFILES_HOME" ]; then
    bashTemplate="$(find -L "$DOTFILES_HOME" -type f -iname "*mkbin.bash")"
  elif [ -d "$DOTFILES_HOME" ]; then
    bashTemplate="$(find "$DOTFILES_HOME" -type f -iname "*mkbin.bash")"
  else
    err "No \$DOTFILES_HOME directory found! Please ensure that this variable is set," \
      "that the directory exists, and try again."
    return 1
  fi
  while [ -z "$executableName" ]; do
    echoe "Please enter a name for the executable (with or without file extension):"
    read -er executableName
  done
  if [[ "$executableName" =~ \..*$ ]]; then
    executableName="${executableName/.*//}"
  fi
  cp "$bashTemplate" "$executableName"
  chmod 755 "$executableName"
}

# Return the user's current shell name, such as "bash" or "zsh".
currentshell() {
  basename "${SHELL}"
}

# Run a command over multiple lines of input from stdin or from a file
# in the fastest way possible.
do_multiple() {
  local cmd="$1"
  local inputSource="${2:-/dev/stdin}"
  local allInput="$(cat "$inputSource")"
  local sequenceCmd="xargs"

  if [ -z "$cmd" ]; then
    err "No command provided"
    return 1
  fi
  if [ -z "$allInput" ]; then
    err "Input source empty"
    return 2
  fi
  if [ "$(command -v parallel)" != "" ]; then
    # Use GNU Parallel if possible.
    sequenceCmd="parallel"
  fi
  eval "${sequenceCmd} ${cmd} <<< \"${allInput}\""
}

# Remove all swap files
rmswap() {
  rm -f "${VIM_CONFIG_HOME}/swaps/*"
}

# Run one or more initialization scripts based on input topics.
#
# Usage:
#   runinit test      -> ~/.dotfiles/init/init_test
#   runinit test java -> ~/.dotfiles/init/init_test; ~/.dotfiles/init/init_java
runinit() {
  local initTopics=("${@}")
  local funcName="${FUNCNAME[0]}"
  local dotfilesInit="${DOTFILES_INIT:-${DOTFILES_HOME}/init}"
  local initFile
  local fullPath
  local initTopic

  runinit_usage() {
    cat <<EOF
USAGE:
  ${funcName} TOPIC [...]

DESCRIPTION:
  Run one or more initialization scripts based on input topics.

EXAMPLES:
  # Run one init script and exit.
  ${funcName} java

  # Run two init scripts in sequence.
  ${funcName} java docker
EOF
  }

  if [ "${#initTopics[@]}" -eq 0 ]; then
    err "No initialization topic(s) provided."
    runinit_usage
    return 1
  elif echo "${initTopics[0]}" | grep -E -q "(-h)|(--help)"; then
    runinit_usage
    return 0
  fi

  for initTopic in "${initTopics[@]}"; do
    initFile="init_${initTopic}"
    fullPath="${dotfilesInit}/${initFile}"
    if [ -x "${fullPath}" ]; then
      "${fullPath}"
    else
      err "init file non-executable or does not exist: ${fullPath} "
      return 2
    fi
  done
}

################################################################################
##                             Functions taken from                           ##
##                            mathiasbynens/dotfiles                          ##
##                   https://github.com/mathiasbynens/dotfiles                ##
################################################################################

# Create a new directory and enter it
mkd() {
  mkdir -p "$@" && cd "$_"
}

# Change working directory to the top-most Finder window location
cdf() { # short for `cdfinder`
  cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')"
}

# Create a .tar.gz archive, using `zopfli`, `pigz` or `gzip` for compression
targz() {
  local tmpFile="${@%/}.tar"
  tar -cvf "${tmpFile}" --exclude=".DS_Store" "${@}" || return 1

  size=$(
    stat -f"%z" "${tmpFile}" 2>/dev/null # OS X `stat`
    stat -c"%s" "${tmpFile}" 2>/dev/null # GNU `stat`
  )

  local cmd=""
  if ((size < 52428800)) && hash zopfli 2>/dev/null; then
    # the .tar file is smaller than 50 MB and Zopfli is available; use it
    cmd="zopfli"
  else
    if hash pigz 2>/dev/null; then
      cmd="pigz"
    else
      cmd="gzip"
    fi
  fi

  echo "Compressing .tar using \`${cmd}\`…"
  "${cmd}" -v "${tmpFile}" || return 1
  [ -f "${tmpFile}" ] && rm "${tmpFile}"
  echo "${tmpFile}.gz created successfully."
}

# Determine size of a file or total size of a directory
fs() {
  if du -b /dev/null >/dev/null 2>&1; then
    local arg=-sbh
  else
    local arg=-sh
  fi
  if [[ -n "$*" ]]; then
    du $arg -- "$@"
  else
    du $arg .[^.]* *
  fi
}

# Use Git’s colored diff when available
if hash git >>/dev/null 2>&1; then
  diff() {
    if [ -x "$(command -v bat)" ]; then
      # Use `bat` to colorize output, if available.
      git diff --no-index --name-only --diff-filter=d -- "$@" | xargs bat --diff
    else
      # Default to using git's own colorization capabilities.
      git diff --no-index --color-words "$@"
    fi
  }
fi

# Create a data URL from a file
dataurl() {
  local mimeType=$(file -b --mime-type "$1")
  if echo "${mimeType}" | grep -q "^text/.*" ; then
    mimeType="${mimeType};charset=utf-8"
  fi
  echo "data:${mimeType};base64,$(openssl base64 -in "$1" | tr -d '\n')"
}

# Create a git.io short URL
gitio() {
  if [ -z "${1}" -o -z "${2}" ]; then
    echo "Usage: \`gitio slug url\`"
    return 1
  fi
  curl -i http://git.io/ -F "url=${2}" -F "code=${1}"
}

# Start an HTTP server from a directory, optionally specifying the port
pythonserver() {
  local port="${1:-8000}"
  sleep 1 && open "http://localhost:${port}/" &
  # Set the default Content-Type to `text/plain` instead of `application/octet-stream`
  # And serve everything as UTF-8 (although not technically correct, this doesn’t break anything for binary files)
  python -c $'import SimpleHTTPServer;\nmap = SimpleHTTPServer.SimpleHTTPRequestHandler.extensions_map;\nmap[""] = "text/plain";\nfor key, value in map.items():\n\tmap[key] = value + ";charset=UTF-8";\nSimpleHTTPServer.test();' "$port"
}

# Start a PHP server from a directory, optionally specifying the port
# (Requires PHP 5.4.0+.)
phpserver() {
  local port="${1:-4000}"
  local ip=$(ipconfig getifaddr en1)
  sleep 1 && open "http://${ip}:${port}/" &
  php -S "${ip}:${port}"
}

# Compare original and gzipped file size
gz() {
  local origsize=$(wc -c <"$1")
  local gzipsize=$(gzip -c "$1" | wc -c)
  local ratio=$(echo "$gzipsize * 100 / $origsize" | bc -l)
  printf "orig: %d bytes\n" "$origsize"
  printf "gzip: %d bytes (%2.2f%%)\n" "$gzipsize" "$ratio"
}

# Syntax-highlight JSON strings or files
# Usage: `json '{"foo":42}'` or `echo '{"foo":42}' | json`
json() {
  if [ -t 0 ]; then # argument
    python -mjson.tool <<<"$*" | pygmentize -l javascript
  else # pipe
    python -mjson.tool | pygmentize -l javascript
  fi
}

# Run `dig` and display the most useful info
digga() {
  dig +nocmd "$1" any +multiline +noall +answer
}

# UTF-8-encode a string of Unicode symbols
escape() {
  printf "\\\x%s" $(printf "$@" | xxd -p -c1 -u)
  # print a newline unless we’re piping the output to another program
  if [ -t 1 ]; then
    echo "" # newline
  fi
}

# Decode \x{ABCD}-style Unicode escape sequences
unidecode() {
  perl -e "binmode(STDOUT, ':utf8'); print \"$*\""
  # print a newline unless we’re piping the output to another program
  if [ -t 1 ]; then
    echo "" # newline
  fi
}

# Get a character’s Unicode code point
codepoint() {
  perl -e "use utf8; print sprintf('U+%04X', ord(\"$*\"))"
  # print a newline unless we’re piping the output to another program
  if [ -t 1 ]; then
    echo "" # newline
  fi
}

# Show all the names (CNs and SANs) listed in the SSL certificate
# for a given domain
getcertnames() {
  if [ -z "${1}" ]; then
    echo "ERROR: No domain specified."
    return 1
  fi

  local domain="${1}"
  echo "Testing ${domain}…"
  echo "" # newline

  local tmp=$(echo -e "GET / HTTP/1.0\nEOT" |
    openssl s_client -connect "${domain}:443" -servername "${domain}" 2>&1)

  if [[ "${tmp}" = *"-----BEGIN CERTIFICATE-----"* ]]; then
    local certText=$(echo "${tmp}" |
      openssl x509 -text -certopt "no_aux, no_header, no_issuer, no_pubkey, \
      no_serial, no_sigdump, no_signame, no_validity, no_version")
    echo "Common Name:"
    echo "" # newline
    echo "${certText}" | grep "Subject:" | sed -e "s/^.*CN=//" | sed -e "s/\/emailAddress=.*//"
    echo "" # newline
    echo "Subject Alternative Name(s):"
    echo "" # newline
    echo "${certText}" | grep -A 1 "Subject Alternative Name:" |
      sed -e "2s/DNS://g" -e "s/ //g" | tr "," "\n" | tail -n +2
    return 0
  else
    echo "ERROR: Certificate not found."
    return 1
  fi
}

# `tre` is a shorthand for `tree` with hidden files and color enabled.
tre() {
  local ignorePatterns=".git|node_modules|bower_components"
  local treeCmd

  if [ -x "$(command -v colorls)" ]; then
    # Use fancy colorls tree command to show file tree, if we have colorls
    # installed.
    treeCmd="colorls --tree --color=always ${*}"
  elif [ -x "$(command -v exa)" ]; then
    treeCmd="exa --tree -a --color=always --git-ignore -I ${ignorePatterns} ${*}"
  elif [ -x "$(command -v tree)" ]; then
    # Use standard tree command
    treeCmd="tree -aC -I ${ignorePatterns} --dirsfirst ${*}"
  else
    err "No tree command available."
    return 1
  fi

  eval "${treeCmd} | less -Fr"
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
