#!/bin/sh

# General purpose printing function
echoe() {
  printf "%s\n" "$*" 1>&2
}

log() {
  local logType="$1"
  local msg="$2"
  local info="$(__get_log_message_info)"
  local infoColor="$(__get_log_message_info "color")"
  local logTypeColor="$(__get_log_type_with_color "$logType")"
  local output="[${logType}${info}] $msg"
  local outputColor="[${logTypeColor}${infoColor}] $msg"
  local LOG_TO_FILE="${LOG_TO_FILE:-""}"

  echoe "${outputColor}"
  if [ -f "${LOG_TO_FILE}" ]; then
    echoe "${output}" >>"${LOG_TO_FILE}"
  fi
}

# Print error message to log output.
err() {
  log "ERROR" "$*"
}

# Print warning message to log output.
warn() {
  log "WARNING" "$*"
}

# Print success message to log output.
succ() {
  log "SUCCESS" "$*"
}

# Print general information to log output.
log_info() {
  log "INFO" "$*"
}

# Print a large, ornamented heading message to stderr.
print_header() {
  local msg="$*"
  local termwidth="$(tput cols)"
  local spacePadding="$(printf '%0.1s' ' '{1..500})"
  local bookend="##"

  # Print the top layer of '#'s
  printf '#%.0s' $(seq -s ' ' 1 "$termwidth")
  printf '\n'

  # Print the message, padded by spaces and a '##' on both ends
  printf '%s%0.*s %s %0.*s%s\n' \
    "$bookend" \
    "$(((termwidth - 2 - ${#msg}) / 2 - ${#bookend}))" \
    "$spacePadding" \
    "$msg" \
    "$(((termwidth - 1 - ${#msg}) / 2 - ${#bookend}))" \
    "$spacePadding" \
    "$bookend"

  # Print the bottom layer of '#'s
  printf '#%.0s' $(seq -s ' ' 1 "$termwidth")
  printf '\n'
}

# Set the parent program's logging file to a given file
log_to_file() {
  local logFile="$1"
  if [[ -z "$logFile" ]]; then
    err "No file provided for logging!"
    return 1
  fi
  export LOG_TO_FILE="$logFile"
}

# Turn off file logging
no_log_to_file() {
  unset LOG_TO_FILE
}

# Return the current line number, which is trickier than it sounds. This
# operation is highly shell-dependent.
__get_line_number() {
  local currentShell="$(currentshell)"

  if [ "${currentShell}" = "bash" ]; then
    echo "${BASH_LINENO[3]}"
  elif [ "${currentShell}" = "zsh" ]; then
    echo "${funcfiletrace[4]##*:}"
  else
    echo "${LINENO}"
  fi
}

# Return the name of the calling function based on the shell currently in use.
__get_function_name() {
  local currentShell="$(currentshell)"

  if [ "${currentShell}" = "bash" ]; then
    echo "${FUNCNAME[4]}"
  elif [ "${currentShell}" = "zsh" ]; then
    echo "$funcstack[5]"
  else
    echo "${FUNCNAME[4]}"
  fi
}

# Return the file name of the calling function based on the shell currently in
# use.
__get_file_name() {
  local currentShell="$(currentshell)"

  if [ "${currentShell}" = "bash" ]; then
    echo "${BASH_SOURCE[4]##*/}"
  elif [ "${currentShell}" = "zsh" ]; then
    echo "${funcfiletrace[4]}" | sed -r 's/.*\/([^/:]*):.*/\1/'
  else
    basename "$(test -L "$0" && readlink "$0" || echo "$0")"
  fi
}

__get_log_message_info() {
  local outputInColor="${1:-''}"
  local info=""
  local sep="|"
  local fileName="$(__get_file_name)"
  local funcName="$(__get_function_name)"
  local lineNo="$(__get_line_number)"

  if [ -n "$fileName" ]; then
    if [ -n "$outputInColor" ]; then
      info+="${sep}${BLUE}${fileName}${NC}"
    else
      info+="${sep}${fileName}${NC}"
    fi
  fi

  if [ -n "$funcName" ]; then
    if [ -n "$outputInColor" ]; then
      info+="${sep}${MAGENTA}${funcName}${NC}"
    else
      info+="${sep}${funcName}"
    fi
  fi

  if [ -n "$lineNo" ]; then
    if [ -n "$outputInColor" ]; then
      info+="${sep}${CYAN}${lineNo}${NC}"
    else
      info+="${sep}${lineNo}"
    fi
  fi
  echo "$info"
}

__get_log_type_with_color() {
  local logType="$1"
  local color=""
  case "$logType" in
  "INFO")
    color="${GREEN}"
    ;;
  "WARNING")
    color="${YELLOW}"
    ;;
  "ERROR")
    color="${RED}"
    ;;
  "SUCCESS")
    color="${GREEN}"
    ;;
  *)
    echo "ERROR: Unknown log type ${logType}" 1>&2
    return 1
    ;;
  esac
  echo "${color}${logType}${NC}"
}

# Test all logging functions
__log_test() {
  log_info "info"
  warn "warning"
  err "error"
  succ "success"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
