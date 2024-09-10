#!/bin/bash

# Set the source and destination directories
src_dir="/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/figs_to_make_animation/Figures_raw/CuGr3-4"

dest_dir="/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/figs_to_make_animation/CuGr3_4_animation_pa/rearranged_figs/"

# Create the destination directory if it doesn't exist
mkdir -p "$dest_dir"

# Loop through all subdirectories in the source directory
for subdir in "$src_dir"/*; do
    if [ -d "$subdir" ]; then
        # Find the "ens_pa.pdf" file in the subdirectory
        pdf_file="$subdir/ens_pa.pdf"
        if [ -f "$pdf_file" ]; then
            # Copy the file to the destination directory with the subdirectory name as the new filename
            dest_file="$dest_dir/$(basename "$subdir").pdf"
            cp "$pdf_file" "$dest_file"
            echo "Copied $pdf_file to $dest_file"
        fi
    fi
done
