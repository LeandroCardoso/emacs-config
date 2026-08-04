#!/usr/bin/env bash

mandir="$HOME/.emacs.d/windows/share/man"
srcurl="http://http.us.debian.org/debian/pool/main/g/git"
datafile="data.tar.xz"

# Ensure man directory exists
if [ ! -d "$mandir" ]; then
    echo "Error: directory '$mandir' does not exist." >&2
    exit 1
fi

# Find the latest git-man .deb file
file=$(wget -q -O - "$srcurl/" | grep -oP 'href="\K[^"]+' | grep 'git-man.*\.deb' | sort | tail -n 1)

if [ -z "$file" ]; then
    echo "Error: could not find git-man .deb in $srcurl." >&2
    exit 2
fi

cd "$mandir" || exit 3

echo "Downloading: $file from $srcurl to $mandir"
wget --continue --no-verbose "$srcurl/$file"

echo "Extracting $datafile from $file"
ar x "$file" "$datafile"

echo "Extracting man pages from $datafile"
tar --extract --xz --verbose --wildcards \
    --transform='s|^./usr/share/man/|./|' \
    --file="$datafile" './usr/share/man/*.gz'

echo "Cleaning up: $file and $datafile"
rm -f "$file" "$datafile"
