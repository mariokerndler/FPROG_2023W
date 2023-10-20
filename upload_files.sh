#!/bin/bash

# Your server and username
server="g0.complang.tuwien.ac.at"
username="f51822845"

# Local directory where your .hs files are
local_dir="."

# Remote directory on the server where you want to upload the files
remote_dir="."

scp -r "$local_dir"/*.hs "$username@$server:$remote_dir"