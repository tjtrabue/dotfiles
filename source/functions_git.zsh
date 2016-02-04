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

# Runs the logic for generating a new SSH key for GitHub and saving the pair:
ssh_gen () {
    local response=""
    while [[ response == "" ]]; do
        echo "Enter file name for key (dafault is ~/.ssh/id_rsa)" 1>&2
        read response
        response=${response:-~/.ssh/id_rsa}
    done
    ssh-keygen -t rsa -b 4096 -C "tom.trabue@gmail.com" | echo "$response" | echo "" | echo ""
    eval "$(ssh-agent -s)"
    ssh-add "$response"
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

    if [[ `is_repo` == "false" ]]; then
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

# Lists all submodules in a repo
ls_submods () {
    is_repo
    if [[ "$!" -eq 0 ]]; then
        local repo_home="$(dirname $(git rev-parse --git-dir))"
        grep path $repo_home/.gitmodules | sed 's/.*= //'
    else
        echoe "Not in a git repository" 1>&2
        return 1
    fi
}

# Creates a git submodule based on a git url.
# Alternately deletes a submodule in a repo:
submod () {
    if [[ "$1" == "-r" ]]; then
        if [[ -z "$2" ]]; then
            echoe "Must enter the path to the submodule"
            return 1
        fi

        local submods=($(ls_submods))
        for sub in "${submods[@]}"; do
            if [[ "$2" == "$sub" ]]; then
                echo "Removing submodule $2" 1>&2
                mv "$2" "$2_tmp"
                git submodule deinit "$2"
                git rm --cached "$2"
                mv "$2_tmp" "$2"
                rm -rf "$(git rev-parse --git-dir)/modules/$2"
            fi
        done
    else
        git submodule add "$1"
    fi
}

###########################################################################
#                                                                         #
#                                  Tracking                               #
#                                                                         #
###########################################################################




###########################################################################
#                                                                         #
#                                 Editor                                  #
#                                                                         #
###########################################################################

# Changes the designated git editor.
# Options are Sublime, Atom, and Vim:
swged () {
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