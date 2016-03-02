#!/usr/bin/env bash

#########################################
##                General              ##
#########################################

# Returns the block of sourced code for the function or alias given as an argument:
func () {
    local CODE="`declare -f $@`";
    if [[ -z "$CODE" ]]; then
        alias -m | grep "alias $@=";
    else
        echo "$CODE";
    fi
}

#########################################
##           File Manipulation         ##
#########################################


#########################################
##       Directory Manipulation        ##
#########################################

shortpath () {
    local input_path="$1";
    egrep "^\s*export" "$DIR_ALIAS_FILE" | sed 's/^ *export *//' | sed 's/=.*//' | {
        local var best_var to_replace best_to_replace;
        while read -s var; do
            local dir_alias="`env | egrep "^${var}\b"`";
            if [[ -n $dir_alias ]]; then
                to_replace="`echo "$dir_alias" | sed -e "s/${var}=//" -e 's/"//g'`";
                if [[ "$input_path" =~ ^"${to_replace%/}/".* ]]; then
                    if [[ ${#to_replace} -gt ${#best_to_replace} ]]; then
                        best_to_replace="$to_replace";
                        best_var="$var";
                    fi;
                fi;
            fi;
        done;
        if [[ -n $best_to_replace ]]; then
            input_path="`echo "$input_path" | sed "s:$best_to_replace:\\$$best_var:"`";
        fi;
        echo "$input_path"
    }
}

# Makes an alias for the current working directory:
diralias () {
    local short_path="`shortpath "$(pwd)"`";
    sed -i "" "/export $1=/d" "$DIR_ALIAS_FILE";
    echo "export $1=\"$short_path\"" >> "$DIR_ALIAS_FILE";
    source ~/.dirs
}


#########################################
##              Navigation             ##
#########################################

# Wrapper for the cd function that adds some memory to it for
# retracing directories.
cd () {
    local adir;
    local -i cnt;
    if [[ $1 == "--" ]]; then
        dirs -v;
        return 0;
    fi;
    local the_new_dir="${1:-$HOME}";
    [[ ${the_new_dir:0:1} == '-' ]] && the_new_dir="`translate_dir_hist "$the_new_dir"`";
    [[ ${the_new_dir:0:1} == '~' ]] && the_new_dir="${HOME}${the_new_dir:1}";
    [[ -e "$the_new_dir" ]] || {
        local temp="`env | grep "^${the_new_dir%%/*}=" | sed 's/.*=//'`";
        [[ -z $temp ]] && {
            echo "Error: $the_new_dir does not exist." 1>&2;
            return 1
        };
        [[ $the_new_dir == */* ]] && temp="$temp/${the_new_dir#*/}";
        the_new_dir="$temp"
    };
    pushd "$the_new_dir" >> /dev/null;
    [[ $? -ne 0 ]] && return 1;
    the_new_dir="$(pwd)";
    popd -n +11 2> /dev/null > /dev/null;
    for ((cnt=1; cnt <= 10; cnt++))
    do
        local x2="$(dirs +${cnt} 2>/dev/null)";
        [[ $? -ne 0 ]] && return 0;
        [[ ${x2:0:1} == '~' ]] && x2="${HOME}${x2:1}";
        if [[ "${x2}" == "${the_new_dir}" ]]; then
            popd -n +$cnt 2> /dev/null > /dev/null;
            ((cnt=cnt-1));
        fi;
    done;
    return 0
}

# Maps an input integer of the form -i with a directory on the directory stack:
translate_dir_hist () {
    [[ ! -z "$1" && "$1" =~ \-[0-9]+ ]] && local num_dirs_to_go_back=${1:1} || return 1
    dir_arr=($(dirs))
    [[ $num_dirs_to_go_back -gt ${#dir_arr[@]} ]] && return 2
    echo ${dir_arr[$num_dirs_to_go_back]}
}

#########################################
##            PATH/Variables           ##
#########################################

# Adds a directory to the PATH environment variable.
# By default adds the working directory, but can take
# an argument as well:
atp () {
    if [[ "$#" -ne 1 ]]; then
        local to_add=$(shortpath `pwd`);
    else
        local to_add=$(shortpath `$1`);
    fi

    echo $PATH | grep -ho "$to_add"

    if [ $? -ne 0 ]; then
        PATH=$PATH:$to_add
        sed -i "" "s:^export::g" ~/.path
        echo "export PATH=$PATH" >> ~/.path
        source ~/.path
        src
    else
        return 1
    fi
}


#########################################
##             Information             ##
#########################################

# Used for printing errors:
echoe () { echo "${RED}ERROR${reset_color}: $@" 1>&2 ; }

# Used for printing warnings:
echow () { echo "${YELLOW}WARNING${reset_color}: $@" 1>&2 ; }

# Prints useful network information regarding open connections.
netinfo () {
    if [[ "$1" == "-l" ]]; then
        lsof -i | grep -E "(LISTEN|ESTABLISHED)" | awk '{print $1, $8, $9}'
    elif [[ "$#" -gt 0 && "$1" != "-l" ]]; then
        echoe "Unknown operand $1"
        echo "Usage: netinfo [-l]" 1>&2
        return 1
    else
        lsof -i | grep -E "(LISTEN|ESTABLISHED)"
    fi
}

#################################################
##            Functions taken from             ##
##           mathiasbynens/dotfiles            ##
##  https://github.com/mathiasbynens/dotfiles  ##
#################################################

# Simple calculator
calc () {
    local result="";
    result="$(printf "scale=10;$*\n" | bc --mathlib | tr -d '\\\n')";
    #                       └─ default (when `--mathlib` is used) is 20
    #
    if [[ "$result" == *.* ]]; then
        # improve the output for decimal numbers
        printf "$result" |
        sed -e 's/^\./0./'        `# add "0" for cases like ".5"` \
            -e 's/^-\./-0./'      `# add "0" for cases like "-.5"`\
            -e 's/0*$//;s/\.$//';  # remove trailing zeros
    else
        printf "$result";
    fi;
    printf "\n";
}

# Create a new directory and enter it
mkd () {
    mkdir -p "$@" && cd "$_";
}

# Change working directory to the top-most Finder window location
cdf () { # short for `cdfinder`
    cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')";
}

# Create a .tar.gz archive, using `zopfli`, `pigz` or `gzip` for compression
targz () {
    local tmpFile="${@%/}.tar";
    tar -cvf "${tmpFile}" --exclude=".DS_Store" "${@}" || return 1;

    size=$(
        stat -f"%z" "${tmpFile}" 2> /dev/null; # OS X `stat`
        stat -c"%s" "${tmpFile}" 2> /dev/null # GNU `stat`
    );

    local cmd="";
    if (( size < 52428800 )) && hash zopfli 2> /dev/null; then
        # the .tar file is smaller than 50 MB and Zopfli is available; use it
        cmd="zopfli";
    else
        if hash pigz 2> /dev/null; then
            cmd="pigz";
        else
            cmd="gzip";
        fi;
    fi;

    echo "Compressing .tar using \`${cmd}\`…";
    "${cmd}" -v "${tmpFile}" || return 1;
    [ -f "${tmpFile}" ] && rm "${tmpFile}";
    echo "${tmpFile}.gz created successfully.";
}

# Determine size of a file or total size of a directory
fs () {
    if du -b /dev/null > /dev/null 2>&1; then
        local arg=-sbh;
    else
        local arg=-sh;
    fi
    if [[ -n "$@" ]]; then
        du $arg -- "$@";
    else
        du $arg .[^.]* *;
    fi;
}

# Use Git’s colored diff when available
hash git &>/dev/null;
if [ $? -eq 0 ]; then
    diff() {
        git diff --no-index --color-words "$@";
    }
fi;

# Create a data URL from a file
dataurl () {
    local mimeType=$(file -b --mime-type "$1");
    if [[ $mimeType == text/* ]]; then
        mimeType="${mimeType};charset=utf-8";
    fi
    echo "data:${mimeType};base64,$(openssl base64 -in "$1" | tr -d '\n')";
}

# Create a git.io short URL
gitio () {
    if [ -z "${1}" -o -z "${2}" ]; then
        echo "Usage: \`gitio slug url\`";
        return 1;
    fi;
    curl -i http://git.io/ -F "url=${2}" -F "code=${1}";
}

# Start an HTTP server from a directory, optionally specifying the port
server () {
    local port="${1:-8000}";
    sleep 1 && open "http://localhost:${port}/" &
    # Set the default Content-Type to `text/plain` instead of `application/octet-stream`
    # And serve everything as UTF-8 (although not technically correct, this doesn’t break anything for binary files)
    python -c $'import SimpleHTTPServer;\nmap = SimpleHTTPServer.SimpleHTTPRequestHandler.extensions_map;\nmap[""] = "text/plain";\nfor key, value in map.items():\n\tmap[key] = value + ";charset=UTF-8";\nSimpleHTTPServer.test();' "$port";
}

# Start a PHP server from a directory, optionally specifying the port
# (Requires PHP 5.4.0+.)
phpserver () {
    local port="${1:-4000}";
    local ip=$(ipconfig getifaddr en1);
    sleep 1 && open "http://${ip}:${port}/" &
    php -S "${ip}:${port}";
}

# Compare original and gzipped file size
gz () {
    local origsize=$(wc -c < "$1");
    local gzipsize=$(gzip -c "$1" | wc -c);
    local ratio=$(echo "$gzipsize * 100 / $origsize" | bc -l);
    printf "orig: %d bytes\n" "$origsize";
    printf "gzip: %d bytes (%2.2f%%)\n" "$gzipsize" "$ratio";
}

# Syntax-highlight JSON strings or files
# Usage: `json '{"foo":42}'` or `echo '{"foo":42}' | json`
json () {
    if [ -t 0 ]; then # argument
        python -mjson.tool <<< "$*" | pygmentize -l javascript;
    else # pipe
        python -mjson.tool | pygmentize -l javascript;
    fi;
}

# Run `dig` and display the most useful info
digga () {
    dig +nocmd "$1" any +multiline +noall +answer;
}

# UTF-8-encode a string of Unicode symbols
escape () {
    printf "\\\x%s" $(printf "$@" | xxd -p -c1 -u);
    # print a newline unless we’re piping the output to another program
    if [ -t 1 ]; then
        echo ""; # newline
    fi;
}

# Decode \x{ABCD}-style Unicode escape sequences
unidecode () {
    perl -e "binmode(STDOUT, ':utf8'); print \"$@\"";
    # print a newline unless we’re piping the output to another program
    if [ -t 1 ]; then
        echo ""; # newline
    fi;
}

# Get a character’s Unicode code point
codepoint () {
    perl -e "use utf8; print sprintf('U+%04X', ord(\"$@\"))";
    # print a newline unless we’re piping the output to another program
    if [ -t 1 ]; then
        echo ""; # newline
    fi;
}

# Show all the names (CNs and SANs) listed in the SSL certificate
# for a given domain
getcertnames () {
    if [ -z "${1}" ]; then
        echo "ERROR: No domain specified.";
        return 1;
    fi;

    local domain="${1}";
    echo "Testing ${domain}…";
    echo ""; # newline

    local tmp=$(echo -e "GET / HTTP/1.0\nEOT" \
        | openssl s_client -connect "${domain}:443" -servername "${domain}" 2>&1);

    if [[ "${tmp}" = *"-----BEGIN CERTIFICATE-----"* ]]; then
        local certText=$(echo "${tmp}" \
            | openssl x509 -text -certopt "no_aux, no_header, no_issuer, no_pubkey, \
            no_serial, no_sigdump, no_signame, no_validity, no_version");
        echo "Common Name:";
        echo ""; # newline
        echo "${certText}" | grep "Subject:" | sed -e "s/^.*CN=//" | sed -e "s/\/emailAddress=.*//";
        echo ""; # newline
        echo "Subject Alternative Name(s):";
        echo ""; # newline
        echo "${certText}" | grep -A 1 "Subject Alternative Name:" \
            | sed -e "2s/DNS://g" -e "s/ //g" | tr "," "\n" | tail -n +2;
        return 0;
    else
        echo "ERROR: Certificate not found.";
        return 1;
    fi;
}

# `s` with no arguments opens the current directory in Sublime Text, otherwise
# opens the given location
s () {
    if [ $# -eq 0 ]; then
        subl .;
    else
        subl "$@";
    fi;
}

# `a` with no arguments opens the current directory in Atom Editor, otherwise
# opens the given location
a () {
    if [ $# -eq 0 ]; then
        atom .;
    else
        atom "$@";
    fi;
}

# `v` with no arguments opens the current directory in Vim, otherwise opens the
# given location
v () {
    if [ $# -eq 0 ]; then
        vim .;
    else
        vim "$@";
    fi;
}

# `o` with no arguments opens the current directory, otherwise opens the given
# location
o () {
    if [ $# -eq 0 ]; then
        open .;
    else
        open "$@";
    fi;
}

# `tre` is a shorthand for `tree` with hidden files and color enabled, ignoring
# the `.git` directory, listing directories first. The output gets piped into
# `less` with options to preserve color and line numbers, unless the output is
# small enough for one screen.
tre () {
    tree -aC -I '.git|node_modules|bower_components' --dirsfirst "$@" | less -FRNX;
}
