#!/bin/bash

# Your server and username
server="g0.complang.tuwien.ac.at"
username="f51822845"

# Local directory where your .hs files are
local_dir="."

# Remote directory on the server where you want to upload the files
remote_dir="."

for file in "$local_dir"/*.hs; do
    scp "$file" "$username@$server:$remote_dir"
    
    if [ $? -eq 0 ]; then
        echo "Copied $file to $server:$remote_dir"
    else
        echo "Failed to copy $file to $server:$remote_dir"
    fi
done