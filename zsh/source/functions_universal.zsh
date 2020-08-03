#!/usr/bin/env zsh

# Informative {{{
# Returns the block of sourced code for the function or alias given as an argument:
func() {
    local CODE="`declare -f $@`";
    if [[ -z "$CODE" ]]; then
        alias -m | grep "${@}=";
    else
        echo "$CODE";
    fi
}

# }}}

# Message Printing {{{
echoe() {
    echo -e "$@" 1>&2;
}

echo_err() {
    echoe "[${RED}ERROR${NC}] $@";
}

echo_warn() {
    echoe "[${ORANGE}WARNING${NC}] $@";
}
# }}}

# Directory Manipulation/Traversal {{{
# Initialize a $DIR_ALIAS_FILE if none exists:
init_dir_alias_file() {
    local zshDirsFile="$DOTFILES_HOME/copy/dotfiles_to_copy/.zsh_dirs"
    if [[ ! -f "$DIR_ALIAS_FILE" ]]; then
        if [[ -n "$1" ]]; then
            DIR_ALIAS_FILE="$1";
            {
                touch "$DIR_ALIAS_FILE" && \
                [[ -f "$zshDirsFile" ]] && cat "$zshDirsFile" >> "$DIR_ALIAS_FILE";
            } || {
                echo_err "Could not create DIR_ALIAS_FILE ${YELLOW}$DIR_ALIAS_FILE${NC}";
                return 1;
            }
        else
            DIR_ALIAS_FILE="$HOME/$(basename "$zshDirsFile")";
            [[ -f "$zshDirsFile" ]] && cp "$zshDirsFile" "$DIR_ALIAS_FILE";
        fi
        echoe "Created diralias file at ${YELLOW}$DIR_ALIAS_FILE${NC}";
        export "$DIR_ALIAS_FILE";
    fi
}

# Make `cd` more useful by allowing it to accept:
#   - numeric options of the form '-n', where n is an integer. With an option of this form,
#     cd will return n directories in the directory stack (given by the `dirs` command).
#
#   - a directory alias found in the $DIR_ALIAS_FILE (~/.zsh_dirs by default). These aliases
#     need not be prefaces with a '$' in order for `cd` to work.
cd() {
    # If no $DIR_ALIAS_FILE exists, create one from a template
    init_dir_alias_file;

    # Initial input checks
    if [[ $# -gt 1 ]]; then
        echo_err "Too many arguments to cd";
        return 1;
    fi

    # Figure out how to change directories
    if [[ $# -eq 0 ]]; then
        builtin cd;
    else
        local inputDirectory="$1";
        if [[ "$inputDirectory" == '-' ]]; then
            builtin cd -
        elif [[ "$inputDirectory" =~ \-[0-9]+ ]]; then
            local numDirsToGoBack="$(echo "$inputDirectory" | tr -d '-')";
            local rawDirFromHist="$(translate_dir_hist "$numDirsToGoBack")";
            local dirFromHist="$(eval "echo $rawDirFromHist")";
            builtin cd "$dirFromHist";
        elif [[ -d "$inputDirectory" ]]; then
            builtin cd "$inputDirectory";
        else
            local aliasedDirectory="$(cat "$DIR_ALIAS_FILE" | sed 's/^export\s*//g' \
                                    | sed 's/=.*$//g' \
                                    | grep -F -w --color=never "$inputDirectory"
                                    )";
            if [[ "$aliasedDirectory" != "" ]]; then
                local aliasedDirectoryRawValue="$(cat "$DIR_ALIAS_FILE" | sed 's/^export\s*//g' \
                                                | grep --color=never "^$aliasedDirectory=" \
                                                | sed "s/^$aliasedDirectory=//g" \
                                                | tr -d "'\";"
                                                )";

                # Make sure to expand any variable values in the matching diralias
                aliasedDirectoryValue="$(eval "echo $aliasedDirectoryRawValue")";
                if [[ -d "$aliasedDirectoryValue" ]]; then
                    builtin cd "$aliasedDirectoryValue";
                else
                    echo_err "Aliased value ${YELLOW}$aliasedDirectoryValue${NC} for diralias" \
                        "${YELLOW}$aliasedDirectory${NC} could not be resolved to a directory";
                    return 3;
                fi
            else
                echo_err "${YELLOW}$inputDirectory${NC} is not a directory or dir alias";
                return 4;
            fi
        fi
    fi
}

# Translates a numeric argument n into a directory in the directory stack visited n directories
# ago.
translate_dir_hist() {
    if [[ -z "$1" || ! "$1" =~ [0-9]+ ]]; then
        echo_err "No numeric argument given to translate_dir_hist";
        return 1;
    fi

    local numDirsToGoBack="$1";
    local dirFromStack;
    local index=0;

    for dirFromStack in $(dirs); do
        if [[ "$index" == "$numDirsToGoBack" ]]; then
            echo "$dirFromStack";
            break;
        fi
        ((index++));
    done

    # If the user entered a number that was too large, or if the directory was not matched for
    # some other reason, return 2
    return 2;
}

# Create a cd-able alias from a given name and directory (or a name and the current directory
# if no second argument is given)
diralias() {
    # If no $DIR_ALIAS_FILE exists, create it from template
    init_dir_alias_file;

    # Initial input checks
    if [[ -z "$1" ]]; then
        echo_err "No argument to diralias.";
        return 1;
    fi

    local dirAlias="$1";
    local dirToAlias;
    local dirAliasString;

    if [[ -n "$2" ]]; then
        dirToAlias="$2";
    else
        dirToAlias="$(pwd)";
    fi

    local existingDirAlias="$(cat "$DIR_ALIAS_FILE" \
                            | sed 's/^export\s*//g' \
                            | sed 's/=.*$//g' \
                            | grep -F -w --color=never "$dirAlias" \
                            )";

    if [[ "$existingDirAlias" == "$dirAlias" ]]; then
        echo_err "Duplicate name ${YELLOW}$dirAlias${NC}";
        return 2;
    fi

    local shortPath="$(shortpath "$dirToAlias")";
    dirAliasString="export $dirAlias=\"$shortPath\";";

    # Strip off any trailing newlines from $DIR_ALIAS_FILE
    if [[ "$(tail -n 1 "$DIR_ALIAS_FILE")" == "" ]]; then
        sed -i -e :a -e '/^\n*$/{$d;N;};/\n$/ba' "$DIR_ALIAS_FILE";
    fi

    echo -en "\n$dirAliasString" >> "$DIR_ALIAS_FILE";
    source "$DIR_ALIAS_FILE";
}

shortpath() {
    local line;
    local dirArg;
    local shortPath;

    init_dir_alias_file;

    if [[ -z "$1" ]]; then
        dirArg="$(pwd)";
    else
        dirArg="$1";
    fi

    # Initial input checks
    if [[ ! -d "$dirArg" ]]; then
        echo_err "Argument to shortpath must be a valid directory.";
        return 2;
    fi

    # Strip trailing '/' if it is present from directory provided
    dirArg="$(echo "$dirArg" | sed 's:/$::')";

    shortPath="$dirArg";
    while IFS='' read -r line || [[ -n "$line" ]]; do
        # Skip this iteration if the current line is a comment or empty
        if [[ "$line" =~ ^\# || "$line" == "" ]]; then
            continue;
        fi

        # The name of the current line's diralias
        local dirAlias="$(echo "$line" | sed 's/^export\s*//g' \
                        | sed 's/=.*$//g' \
                        )";

        # The abbreviated path (with a possible variable prefix) represented by the current line
        local rawListedDir="$(echo "$line" | sed 's/^export.*=//g' \
                            | tr -d "'\";" \
                            )";

        # The full path (starting at root) represented by the current line
        local listedDir="$(eval "echo $rawListedDir" | sed 's:/$::')";

        # The first (i.e., variable beginning with '$') part of the shortpath name
        local shortPathPrefix="$(echo "$shortPath" | sed 's:/.*$::')";

        # If we find that the current diralias is equivalent to our directory of interest,
        # just return the current diralias
        if [[ "$listedDir" == "$dirArg" ]]; then
            shortPath="\$$dirAlias";
            break;
        # Otherwise, if the current diralias represents a larger portion of the directory argument
        # than the current prefix to the shortpath, make this diralias the prefix to the shortpath
        elif [[ -z "$shortPathPrefix" ]]; then
            if [[ "$(echo "$shortPath" | grep "$listedDir")" != "" ]]; then
                shortPath="$(echo "$dirArg" \
                            | sed "s:$listedDir:\$$dirAlias:")";
            fi
        elif [[ -n "$shortPathPrefix" ]]; then
            if [[ "$(echo "$dirArg" | grep "$listedDir")" != "" \
                    && "$(eval "echo $shortPathPrefix" | wc -m)" \
                        -le "$(echo "$listedDir" | wc -m)" ]]
            then
                shortPath="$(echo "$dirArg" \
                            | sed "s:$listedDir:\$$dirAlias:")";
            fi
        fi
    done < "$DIR_ALIAS_FILE";

    # If no shortpath was found, do not return any string and return an error code of 3
    if [[ "$shortPath" == "" ]]; then
        return 3;
    fi

    echo "$shortPath";
}
# }}}

# $PATH {{{
# Initialize a $PATH_FILE if one does not already exist
init_path_file() {
    if [[ ! -f "$PATH_FILE" ]]; then
        local pathFileToCreate;
        local pathFileTemplate="$DOTFILES_HOME/copy/dotfiles_to_copy/.zsh_path";
        if [[ -n "$1" ]]; then
            pathFileToCreate="$1";
        else
            pathFileToCreate="$HOME/.zsh_path";
        fi

        if [[ -f "$pathFileTemplate" ]]; then
            cp "$pathFileTemplate" "$pathFileToCreate";
        else
            echo_err "Could not locate ZSH \$PATH template file";
            return 1;
        fi
    fi
}

# Add a directory to $PATH
atp() {
    init_path_file;

    local dirArg="$1";
    if [[ -z "$dirArg" ]]; then
        dirArg="$(pwd)";
    fi

    local shortDirPath="$(shortpath "$dirArg")";
    local newPathString="PATH=\"$shortDirPath:\$PATH\";";

    echo -en "\n$newPathString" >> "$PATH_FILE";

    # Get rid of any empty lines
    sed -i '/^$/d' "$PATH_FILE";

    # Get rid of duplicate lines (if any exist)
    sort -u "$PATH_FILE" -o "$PATH_FILE";

    source "$PATH_FILE";
}
# }}}

# Searching {{{
# }}}

# Mode line for this file (LEAVE IT COMMENTED!)
# vim:foldmethod=marker
