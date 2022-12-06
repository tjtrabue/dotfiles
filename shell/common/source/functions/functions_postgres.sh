#!/bin/sh

# Creates a local PostgreSQL cluster.
init_postgres() {
  local prefix="/usr/local"
  local varDir="${prefix}/var"
  local dataDir="${varDir}/postgres"

  log_info "Creating new data parent dir: ${BLUE}${varDir}${NC}"
  sudo mkdir -p "${varDir}"
  sudo chmod 775 "${varDir}"

  log_info "Initializing new PostgreSQL data directory: ${BLUE}${dataDir}${NC}"
  initdb -D "${dataDir}"
}

# Start the PostgreSQL server with the specified local cluster.
start_postgres() {
  local prefix="/usr/local"
  local dataDir="${prefix}/var/postgres"

  log_info "Starting PostgreSQL server for data dir: ${BLUE}${dataDir}${NC}"
  pg_ctl -D "${dataDir}" start
}

# Stop the PostgreSQL server.
stop_postgres() {
  local prefix="/usr/local"
  local dataDir="${prefix}/var/postgres"

  log_info "Stopping PostgreSQL server"
  pg_ctl -D "${dataDir}" stop
}

# vim:foldenable:foldmethod=indent:foldnestmax=1