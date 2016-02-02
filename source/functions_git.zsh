#!/usr/bin/env ksh

###########################################################################
#                                                                         #
#                                   General                               #
#                                                                         #
###########################################################################

# Determines whether or not the directory is in a git repository
is_repo () {
    if [[ $(git rev-parse --is-inside-work-tree) == "true" ]]; then
        return 0
    else
        return 1
    fi
}

###########################################################################
#                                                                         #
#                              Staging/Pushing                            #
#                                                                         #
###########################################################################

# Commit and push changes upstream. Can optionally add files from a specified directory:
gacp () {
        gacp_usage () {
        echo "Add, commit, and push files to remote repository" 1>&2
        echo "all in one command." 1>&2
        echo "" 1>&2
        echo "Usage:" 1>&2
        echo "gacp [-m (message) -b (branch name) <file names>]" 1>&2
        echo "" 1>&2
        echo "Options:" 1>&2
        echo "  -m <message> : the commit message (will be prompted for one if this arguemnet is omitted)" 1>&2
        echo "  -b <remote branch> : the name of the remote branch to push changes to (\"master\" by default)" 1>&2
    }

    if [[ "$1" == "--help" || "$1" == "-h" ]]; then
        gacp_usage
        return 1
    fi

    if [[ `isrepo` == "false" ]]; then
        echo "Not a git repository" 1>&2
        echo "Aborting" 1>&2
        return 1
    fi

    # Local variables
    local commit_message=""
    local remote_branch="master"

    local OPTIND o
        while getopts ":m:b:" o; do
            case "${o}" in
                m)
                    commit_message="${OPTARG}"
                    ;;
                b)
                    remote_branch="${OPTARG}"
                    ;;
                *)
                    gacp_usage
                    return
                    ;;
            esac
        done
        shift $((OPTIND-1))

    while [[ "$commit_message" == "" ]]; do
        echo "Type a commit message, then press ENTER:" 1>&2
        read commit_message
    done

    for file in "$@"; do
        git add "$file"
    done

    git commit -m "$commit_message"
    git push origin "$remote_branch"
}


###########################################################################
#                                                                         #
#                                Submodules                               #
#                                                                         #
###########################################################################

# removes a submodule from the current repo:
rm_submod () {
    if [[ "$#" -ne 1 ]]; then
        echoe "Must enter the path to the submodule"
        return 1
    fi

    mv "$1" "$1_tmp"
    git submodule deinit "$1"
    git rm --cached "$1"
    mv "$1_tmp" "$1"

    rm -rf "$(git rev-parse --git-dir)/modules/$1"
}

###########################################################################
#                                                                         #
#                                 Editor                                  #
#                                                                         #
###########################################################################

# Changes the designated git editor.
# Options are Sublime, Atom, and Vim:
switchgeditor () {
    if [[ "$#" -ne 1 ]]; then
        echoe "No editor name supplied."
        echo "Valid options are sublime, atom, and vim." 1>&2
        return 1
    fi

    case "${1}" in
        "sublime")
            git config --global core.editor "subl -w"
            ;;
        "atom")
            git config --global core.editor "atom --wait"
            ;;
        "vim")
            git config --global core.editor "vim"
            ;;
        *)
            echoe "Unknown operand $1."
            echo "Please enter sublime, atom, or vim as an" 1>&2
            echo "argument to this function." 1>&2
            return 1
            ;;
    esac
}