#!/bin/sh

# Find duplicate wallpaper images in the designated wallpaper directory
# (usually ~/wallpaper).
find_wallpaper_dupes() {
  # Directory containing wallpaper images.
  local wallpaperDir="${WALLPAPER_DIR:-${HOME}/wallpaper}"
  # Database file used to speed up successive duplicate image searches.
  local wallpaperFingerprintDb="${WALLPAPER_FINGERPRINT_DB:-${HOME}/.wallpaper_fingerprints}"

  if [ ! -x "$(command -v findimagedupes)" ]; then
    err "${BLUE}findimagedupes${NC} is necessary for finding duplicate images."
    return 1
  fi

  if [ ! -d "${wallpaperDir}" ]; then
    err "No wallpaper directory found at: ${BLUE}${wallpaperDir}${NC}"
    return 1
  fi

  findimagedupes --fingerprints "${wallpaperFingerprintDb}" \
    --prune \
    "${wallpaperDir}"
}

# Create a compressed TAR archive of the desktop wallpaper directory.
create_wallpaper_archive() {
  local wallpaperArchiveFile="${1:-${HOME}/wallpaper.tar.gz}"
  local wallpaperDir="${WALLPAPER_DIR:-${HOME}/wallpaper}"

  if [ -f "${wallpaperArchiveFile}" ]; then
    # Backup .tar.gz file if one is already present.
    mv "${wallpaperArchiveFile}" "${wallpaperArchiveFile}.bak"
  fi

  tar -czvf "${wallpaperArchiveFile}" "${wallpaperDir}"
}

# Create a compressed TAR archive of the desktop wallpaper directory, and upload
# the archive to Google drive.
update_wallpaper_in_google_drive() {
  local wallpaperArchiveFile="${HOME}/wallpaper.tar.gz"

  if [ ! -x "$(command -v rclone)" ]; then
    err "${CYAN}rclone${NC} is required to upload files to Google Drive."
    return 1
  fi

  # Compress wallpaper into a .tar.gz archive.
  create_wallpaper_archive "${wallpaperArchiveFile}"

  # Upload the new archive
  # NOTE: The name of the configured Google Drive rclone remote must be 'drive'.
  rclone copy "${wallpaperArchiveFile}" \
    "drive:$(basename "${wallpaperArchiveFile}")"
}

# vim:foldenable:foldmethod=indent:foldnestmax=1