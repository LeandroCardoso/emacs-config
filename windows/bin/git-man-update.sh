#!/usr/bin/sh
mandir=~/.emacs.d/windows/share/man/
giturl=http://http.us.debian.org/debian/pool/main/g/git/
file=$(wget -q -O - $giturl | grep -oP 'href="\K[^"]+' | grep 'git-man.*\.deb' | sort | tail -n 1)
datafile=data.tar.xz

if [ ! -d $mandir ]; then
    echo Error: $mandir does not exist
    exit 1
fi

if [ -z $file ]; then
    echo $Error: git man page not found in $giturl
    exit 2
fi

cd $mandir

echo Downloading: $file from $giturl to $mandir
wget --continue --no-verbose $giturl/$file

echo Extracting $file
ar x $file $datafile

echo Extracing $datafile
tar --extract --xz --verbose --wildcards --transform 's|^./usr/share/man/|./|' --file $datafile './usr/share/man/*.gz'

echo Removing $file and $datafile
rm $file $datafile
