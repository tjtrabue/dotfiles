#!/usr/bin/env bash

# Create
mksource() {
  local bashSourceDir="$DOTFILES_HOME/bash/source";
  local sourceFileName="$*";

  while [[ -z "$sourceFileName" ]]; do
    echoe "Please enter name of new source file:";
    read -er sourceFileName;
  done

  if [[ ! "$sourceFileName" =~ (aliases)|(functions).* ]]; then
    err "Source file name must begin with \"aliases\" or \"functions\"";
    return 2;
  fi

  # Remove file extension from file name
  sourceFileName="${sourceFileName/.@(bash|sh)/}";

  local response="";
  while [[ ! "$response" =~ [YyNn] ]]; do
    echoe "Create file ${sourceFileName}? [Y/n]";
    read -sn1 response;
    response="${response:-y}";
  done
  [[ "$response" =~ [Nn] ]] && return 1;

  local targetFile="${bashSourceDir}/${sourceFileName}.bash"

  # Exit if the file already exists.
  if [[ -f "$targetFile" ]]; then
    err "File $targetFile already exists.";
    return 3;
  fi

  touch "$targetFile";
  echo -e "#!/usr/bin/env bash\n\n\n" >> "$targetFile";

  if [[ "$sourceFileName" =~ aliases.* ]]; then
    echo "# vim:foldenable:foldmethod=marker:" >> "$targetFile";
  elif [[ "$sourceFileName" =~ functions.* ]]; then
    echo "# vim:foldenable:foldmethod=indent::foldnestmax=1" >> "$targetFile";
  fi
}

mkinit() {
  local newInit="$1";
  local initTemplate="$DOTFILES_HOME/copy/templates/mkinit.bash";

  if [[ -z "$newInit" ]]; then
    err "Must provide name for new init file";
    return 1;
  fi

  local initFileName="$newInit";
  if [[ ! "$initFileName" =~ ^init_ ]]; then
    initFileName="init_${initFileName}";
  fi
  if [[ ! "$initFileName" =~ .bash$ ]]; then
    initFileName="${initFileName}";
  fi
  local initFilePath="${DOTFILES_HOME}/init/${initFileName}";

  cp "$initTemplate" "$initFilePath";
  chmod 755 "$initFilePath";
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
