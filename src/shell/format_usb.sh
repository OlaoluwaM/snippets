#!/usr/bin/env bash

# IMPORTANT: Always run `lsblk` first and verify that USB points to the actual
# flash drive. Using the wrong device here will erase that drive.

# To find the USB device, run the following command before and after plugging in the USB drive:
lsblk -o NAME,SIZE,TYPE,FSTYPE,LABEL,MOUNTPOINTS,MODEL

# Change this to the USB drive you want to erase.
USB="/dev/sda"

# Unmount any mounted partitions on the USB.
sudo umount "${USB}"* 2>/dev/null || true

# Remove old filesystem/partition signatures, such as a NixOS ISO image.
sudo wipefs -a "$USB"

# Create a new GPT partition table.
sudo parted "$USB" --script mklabel gpt

# Create one partition using essentially the entire USB drive.
# Starting at 1 MiB keeps the partition properly aligned.
sudo parted "$USB" --script mkpart primary 1MiB 100%

# Tell Linux to reread the new partition table.
sudo partprobe "$USB"

# Format the new partition as exFAT.
# Change "USB" to whatever volume label you want.
sudo mkfs.exfat -n USB "${USB}1"

# Show the final drive/partition layout and filesystem.
lsblk -f "$USB"
