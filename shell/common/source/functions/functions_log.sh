#!/bin/sh

# Logging Level Information {{{
# The environment variable LOG_LEVEL controls the log output verbosity.
# The table below details the possible values of LOG_LEVEL and their respective
# output levels.
#
# ------------------------------
# | Level Number | Ouput Level |
# ------------------------------
# |            1 | error       |
# |            2 | warning     |
# |            3 | info        |
# |            4 | debug       |
# ------------------------------
# }}}

# General purpose printing function
echoe() {
  printf "%s\n" "$*" 1>&2
}

# Primary log function {{{
# The primary logging function entry point. This function takes care of adding
# all of the proper logging prefixes to the message based on the log type, and
# ensures that logging statements only show up if the established logging level
# permits.
log() {
  local logType="$1"
  local msg="$2"
  local info
  local infoColor
  local logTypeColor
  local output
  local outputColor

  if __should_log "${logType}"; then
    info="$(__get_log_message_info)"
    infoColor="$(__get_log_message_info "color")"
    logTypeColor="$(__get_log_type_with_color "${logType}")"
    output="[${logType}${info}] ${msg}"
    outputColor="[${logTypeColor}${infoColor}] ${msg}"
    __log "${output}" "${outputColor}"
  fi
}
# }}}

# Semantic logging functions {{{

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

# Print lower level debugging information to log output.
log_debug() {
  log "DEBUG" "$*"
}
# }}}

# Heading printing {{{
# Print a large, ornamented heading message to stderr.
print_header() {
  local msg="$*"
  local termwidth="$(tput cols)"
  local spacePadding="$(printf '%0.1s' ' '{1..500})"
  local bookend="##"

  # Print the top layer of '#'s
  printf '#%.0s' $(seq -s ' ' 1 "${termwidth}")
  printf '\n'

  # Print the message, padded by spaces and a '##' on both ends
  printf '%s%0.*s %s %0.*s%s\n' \
    "${bookend}" \
    "$(((termwidth - 2 - ${#msg}) / 2 - ${#bookend}))" \
    "${spacePadding}" \
    "$msg" \
    "$(((termwidth - 1 - ${#msg}) / 2 - ${#bookend}))" \
    "${spacePadding}" \
    "${bookend}"

  # Print the bottom layer of '#'s
  printf '#%.0s' $(seq -s ' ' 1 "${termwidth}")
  printf '\n'
}
# }}}

# Logging control functions {{{
# Set the parent program's logging file to a given file
log_to_file() {
  local logFile="$1"
  if [ -z "$logFile" ]; then
    err "No file provided for logging!"
    return 1
  fi
  export LOG_TO_FILE="$logFile"
}

# Turn off file logging
no_log_to_file() {
  unset LOG_TO_FILE
}
# }}}

# Private functions {{{
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
  local functionName

  if [ "${currentShell}" = "bash" ]; then
    functionName="${FUNCNAME[4]}"
  elif [ "${currentShell}" = "zsh" ]; then
    functionName="$funcstack[5]"
  else
    functionName="${FUNCNAME[4]}"
  fi

  functionName="${functionName}()"

  echo "${functionName}"
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

# Get the timestamp for the instant the logging function runs. Very useful for
# debugging purposes.
__get_timestamp() {
  date +'%Y-%m-%dT%H:%M:%S'
}

__get_log_message_info() {
  local outputInColor="${1:-''}"
  local info=""
  local sep="|"
  local fileName="$(__get_file_name)"
  local funcName="$(__get_function_name)"
  local lineNo="$(__get_line_number)"
  local timestamp="$(__get_timestamp)"

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

  if [ -n "$timestamp" ]; then
    if [ -n "$outputInColor" ]; then
      info+="${sep}${YELLOW}${timestamp}${NC}"
    else
      info+="${sep}${timestamp}"
    fi
  fi

  echo "$info"
}

__get_log_type_with_color() {
  local logType="$1"
  local color=""
  case "$logType" in
    "DEBUG")
      color="${MAGENTA}"
      ;;
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

# Predicate function for determining whether a message should be logged,
# depending on the currently set log level.
__should_log() {
  local logType="$1"
  local logLevel="${LOG_LEVEL:-3}"

  if ([ "${logType}" = "ERROR" ] && [ "${logLevel}" -ge 1 ]) ||
  ([ "${logType}" = "WARNING" ] && [ "${logLevel}" -ge 2 ]) ||
  ([ "${logType}" = "INFO" ] && [ "${logLevel}" -ge 3 ]) ||
  ([ "${logType}" = "SUCCESS" ] && [ "${logLevel}" -ge 3 ]) ||
  ([ "${logType}" = "DEBUG" ] && [ "${logLevel}" -ge 4 ]); then
    return 0
  fi
  return 1
}

# Actually do the logging!
__log() {
  local output="$1"
  local outputColor="$2"
  local logToFile="${LOG_TO_FILE:-""}"

  echoe "${outputColor}"
  if [ -f "${logToFile}" ]; then
    echoe "${output}" >>"${logToFile}"
  fi
}

# Test all logging functions
__log_test() {
  log_info "info"
  warn "warning"
  err "error"
  succ "success"
}
# }}}

# vim:foldenable:foldmethod=marker:foldlevel=0
