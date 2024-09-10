#!/bin/bash

# Set the source directory
src_dir="/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/figs_to_make_animation/CuOpt_animation_pb/rearranged_figs/"

# Set the output directory
output_dir="/home/mahle68/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/figs_to_make_animation/CuOpt_animation_pb/pngs_pa/"

# Create the output directory if it doesn't exist
mkdir -p "$output_dir"

# Loop through all files in the source directory
for filename in "$src_dir"/*.pdf; do
    if [ -f "$filename" ]; then
        # Get the base name of the PDF file (without the extension)
        base_name=$(basename "$filename" .pdf)

        # Convert the PDF file to PNG
        pdftoppm -png "$filename" "$output_dir/$base_name"
        echo "Converted $filename to PNG files in $output_dir"
    fi
done
