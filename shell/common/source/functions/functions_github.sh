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

    gh pr create --title "${projectIdentifier}-${taskNumber}: ${title}"
  }
fi

# vim:foldenable:foldmethod=indent:foldnestmax=1