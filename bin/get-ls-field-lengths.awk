#!/usr/sbin/awk -f

BEGIN {
  # Colors
  red = "\033[01;31m"
  green = "\033[01;32m"
  yellow = "\033[01;33m"
  blue = "\033[01;34m"
  purple = "\033[01;35m"
  cyan = "\033[01;36m"
  light_grey = "\033[01;37m"
  # Reset sequence to remove colorization
  reset = "\033[0m"
}

# Print first record as-is
NR == 1 {
  skip
}

# Main output processor
NR > 1 {
  perms = $1
  num_hardlinks = $2
  user = $3
  size = $4
  month_edited = $5
  day_edited = $6
  time_edited = $7

  $1=$2=$3=$4=$5=$6=$7=""

  printf "%s%s %s%2s %s%s %s%s %s%s %s %s%s ", red, perms, blue, num_hardlinks, \
         green, user, blue, size, purple, month_edited, day_edited, time_edited, \
         reset
  print $0
}

END {

}
