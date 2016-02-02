#!/usr/bin/env zsh


#########################################
##           File Manipulation         ##
#########################################


#########################################
##       Directory Manipulation        ##
#########################################

shortpath ()
{
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
diralias ()
{
    local short_path="`shortpath "$(pwd)"`";
    sed -i "" "/export $1=/d" "$DIR_ALIAS_FILE";
    echo "export $1=\"$short_path\"" >> "$DIR_ALIAS_FILE";
    sdirs
}

#########################################
##              Navigation             ##
#########################################

# Wrapper for the cd function that adds some memory
# to it for retracing directories.
cd ()
{
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
    pushd "$the_new_dir" > /dev/null;
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

    echo $PATH | grep -q "$to_add"

    if [ $? -ne 0 ]; then
        PATH=$PATH:$to_add
        sed -i "" "/^export\ PATH=/d" ~/.path
        echo "export PATH=$PATH" >> ~/.path
        source ~/.path
        src
    fi
}


#########################################
##             Information             ##
#########################################

# Lists all scope extensions for Sublime snippets:
subscopes ()
{
    cat ~/.automation/snippet_scopes
}

# Used for printing errors:
echoe ()
{
    echo "${RED}Error${NC}: $@" 1>&2
}

# Prints useful network information regarding open
# connections.
netinfo () {
    if [[ "$1" == "-l" ]]; then
        lsof -i | grep -E "(LISTEN|ESTABLISHED)" | awk '{print $1, $8, $9}'
    elif [[ "$#" -gt 0 && "$1" != "-l" ]]; then
        echoe "Unknown operand $1"
        return 1
    else
        lsof -i | grep -E "(LISTEN|ESTABLISHED)"
    fi
}