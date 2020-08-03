#!/usr/bin/env bash

# Adds a path to the $PATH environment variable.
atp() {
  local pathToAdd="${1:-$(pwd)}";
  pathToAdd="$(shortpath "$pathToAdd")";

  if ! __evaluate_paths | grep -Fxq "$(eval echo "$pathToAdd")" ; then
    echo "$pathToAdd" >> "$PATH_FILE";
  fi
}

# Prefix a given path with any existing aliases
shortpath() {
  local dir_alias;
  local best_var;
  local to_replace;
  local best_to_replace;
  local input_path="$1";

  egrep "^\s*export" "$DIR_ALIAS_FILE" | sed 's:^ *export *::' | sed 's:=.*::' | {
      while read -s var; do
          dir_alias="`env | egrep "^${var}\b"`";
          if [[ -n $dir_alias ]]; then
              to_replace="`echo "$dir_alias" | sed -e "s:${var}=::" -e 's:"::g'`";
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

# Source the path file.
spath() {
  local constructedPath=""
  constructedPath="$(__evaluate_paths | tr '\n' ':' | sed 's/:$//')"

  PATH="$constructedPath"
  export PATH
}

# Print paths in $PATH file with all environment variables evaluated
__evaluate_paths() {
   # Make sure that paths are evaluated in reverse order from their listing in
   # the .path file since we want more recently added paths to take precedence
   # over older ones.
  eval echo "$(xargs -a "$PATH_FILE")" | tr ' ' '\n' | tac
}

# vim:foldenable:foldmethod=indent::foldnestmax=1
