#!/bin/sh

# Accepts an input PDF file, and produces a compressed version with a given
# output PDF file name.
compress_pdf() {
  local input="${1%%.pdf}.pdf"
  local output="${2%%.pdf}.pdf"

  if [ -z "$(command -v gs)" ]; then
    err "GhostScript executable (gs) not found on PATH."
    return 1
  fi

  if [ -z "${input}" ]; then
    err "No input PDF file provided"
    return 1
  elif [ ! -f "${input}" ]; then
    err "No such file: ${BLUE}${input}${NC}"
    return 1
  elif [ -z "${output}" ]; then
    err "No output PDF name provided"
    return 2
  fi

  log_info "Compressing PDF: ${BLUE}${input}${NC}"

  command gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/screen \
    -dNOPAUSE -dQUIET -dBATCH \
    -sOutputFile="${output}" \
    "${input}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1