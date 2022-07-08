#!/bin/sh

# Generates a new example email address using a UUID for the username and
# "example.com" for the domain.
new_example_email() {
  printf "%s@example.com\n" "$(uuidgen -r)"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1