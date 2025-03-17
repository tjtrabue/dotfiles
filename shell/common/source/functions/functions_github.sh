#!/bin/sh

# We want to make sure we have the GitHub CLI tools installed before including
# these functions in our profile.
if [ -x "$(command -v gh)" ]; then
  # Convenience function for creating a GitHub pull request (PR) prefixed
  # with a project identifier and a JIRA task number.
  # This function parses the current Git branch for the project identifier and
  # task number.
  ghpr() {
    local title="${*}"
    local projectIdentifier="$(__get_project_identifier)"
    local taskNumber="$(__get_task_number)"
    local finalTitle

    if [ -z "${projectIdentifier}" ]; then
      err "Could not determine project identifier."
      return 1
    elif [ -z "${taskNumber}" ]; then
      err "Could not determine task number."
      return 2
    elif [ -z "${title}" ]; then
      err "PR title must not be empty."
      return 3
    fi

    finalTitle="${projectIdentifier}-${taskNumber}: ${title}"

    log_info "Creating PR with title: ${CYAN}${finalTitle}${NC}"
    gh pr create --title "${finalTitle}"
  }
fi

# Download the latest release of a GitHub project asset release from the
# project's GitHub Releases page. This tool is VERY experimental and limited,
# and it may not work for every use case.
#
# The first argument is the GitHub account and repository name in 'account/repo'
# name format.
#
# The second argument is a string representing what the name of the release
# ENDS with, so something like 'x86_64.tar.gz'. Note that this arg is a string,
# not a regexp.
#
# A third optional argument is the download directory for the release asset,
# which defaults to the current working directory.
#
# Usage:
#   download_latest_github_release 'account/project' '.tar.gz' /tmp/
download_latest_github_release() {
  local repo="$1"
  local namePattern="$2"
  local releasesUrl="https://api.github.com/repos/${repo}/releases/latest"
  local downloadDir="${3:-$(pwd)}"

  if [ -z "$(command -v wget)" ]; then
    err "'wget' CLI tool is required to complete this command"
    return 1
  elif [ -z "$(command -v jq)" ]; then
    err "'jq' CLI tool is required to complete this command"
    return 1
  fi

  log_info "Attempting to downlod the latest release of ${CYAN}${repo}${NC}" \
    "using URL: ${BLUE}${releasesUrl}${NC}"

  wget --content-disposition -P "${downloadDir}" \
    "$(wget -q -O - "${releasesUrl}" |
      jq -r ".assets[] | select(.name | endswith(\"${namePattern}\")) | .browser_download_url" |
      head -1)"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1