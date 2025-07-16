#!/usr/bin/env bash

# Check if source directory is provided
if [ $# -ne 1 ]; then
  echo "Usage: $0 <source_directory>"
  exit 1
fi

SOURCE_DIR="$1"
MP3_BACKUP_DIR="${SOURCE_DIR}/mp3_duplicates"

# Check if source directory exists
if [ ! -d "$SOURCE_DIR" ]; then
  echo "Error: Directory '$SOURCE_DIR' does not exist"
  exit 1
fi

# Create backup directory for duplicate MP3s
mkdir -p "$MP3_BACKUP_DIR"

echo "Scanning for duplicate songs..."
echo "================================"

# Create associative arrays to track files
declare -A song_files
declare -A has_flac
declare -A has_mp3

# Find all music files and group by base name
while IFS= read -r -d '' file; do
  # Get relative path from source directory
  rel_path="${file#"$SOURCE_DIR"/}"

  # Skip files already in the mp3_duplicates directory
  if [[ "$rel_path" == mp3_duplicates/* ]]; then
    continue
  fi

  # Get filename without path
  filename=$(basename "$file")

  # Get base name without extension
  base_name="${filename%.*}"

  # Get extension in lowercase
  ext="${filename##*.}"
  ext_lower=$(echo "$ext" | tr '[:upper:]' '[:lower:]')

  # Store file path
  if [ -z "${song_files[$base_name]}" ]; then
    song_files[$base_name]="$file"
  else
    song_files[$base_name]="${song_files[$base_name]}|$file"
  fi

  # Track which formats exist
  if [ "$ext_lower" = "flac" ]; then
    has_flac[$base_name]=1
  elif [ "$ext_lower" = "mp3" ]; then
    has_mp3[$base_name]=1
  fi
done < <(find "$SOURCE_DIR" -type f \( -iname "*.mp3" -o -iname "*.flac" \) -print0)

# Process each song
moved_count=0
kept_count=0

for base_name in "${!song_files[@]}"; do
  IFS='|' read -ra files <<<"${song_files[$base_name]}"

  # If we have both FLAC and MP3 versions
  if [ "${has_flac[$base_name]}" = "1" ] && [ "${has_mp3[$base_name]}" = "1" ]; then
    echo "Found duplicate: $base_name (has both FLAC and MP3)"

    # Move MP3 files to backup directory
    for file in "${files[@]}"; do
      ext="${file##*.}"
      ext_lower=$(echo "$ext" | tr '[:upper:]' '[:lower:]')

      if [ "$ext_lower" = "mp3" ]; then
        # Preserve directory structure in backup
        rel_path="${file#"$SOURCE_DIR"/}"
        rel_dir=$(dirname "$rel_path")

        # Create subdirectory if needed
        if [ "$rel_dir" != "." ]; then
          mkdir -p "$MP3_BACKUP_DIR/$rel_dir"
        fi

        echo "  Moving MP3: $rel_path -> mp3_duplicates/$rel_path"
        mv "$file" "$MP3_BACKUP_DIR/$rel_path"
        ((moved_count++))
      fi
    done
  else
    # Keep single instance files
    for file in "${files[@]}"; do
      rel_path="${file#"$SOURCE_DIR"/}"
      echo "Keeping single instance: $rel_path"
      ((kept_count++))
    done
  fi
done

echo "================================"
echo "Summary:"
echo "  Files moved to backup: $moved_count"
echo "  Files kept in place: $kept_count"
echo "  MP3 duplicates moved to: $MP3_BACKUP_DIR"
