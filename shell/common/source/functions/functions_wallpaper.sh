#!/bin/sh

# Find duplicate wallpaper images in the designated wallpaper directory
# (usually ~/wallpaper).
find_wallpaper_dupes() {
  # Directory containing wallpaper images.
  local wallpaperDir="${WALLPAPER_DIR:-${HOME}/wallpaper}"
  # Database file used to speed up successive duplicate image searches.
  local wallpaperFingerprintDb="${WALLPAPER_FINGERPRINT_DB:-${HOME}/.wallpaper_fingerprints}"

  if [ ! -d "${wallpaperDir}" ]; then
    err "No wallpaper directory found at: ${BLUE}${wallpaperDir}${NC}"
    return 1
  fi

  findimagedupes --fingerprints "${wallpaperFingerprintDb}" \
    --prune \
    "${wallpaperDir}"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1