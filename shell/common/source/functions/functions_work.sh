#!/bin/sh

get_current_projects() {
  do_multiple "git clone" "$CURRENT_PROJECTS_FILE"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1
