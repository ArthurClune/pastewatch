#!/bin/sh

echo -n "This script drops files all over your machine. You may wish to "
echo "run this script in a disposable VM. Delete these lines to run"
echo
echo "This script requires fpm to run"
echo "$ sudo apt-get install rubygems"
echo "$ sudo gem install fpm"
exit 1

VERSION=1.0

rm pastewatch*deb


if [ ! -d pastewatch ]; 
then 
   git checkout https://github.com/ArthurClune/pastewatch.git
fi

cd pastewatch
git pull
# if you have installed and then removed the .deb, you may need this next line
# to get a build next time
#sudo cabal --reinstall --force-reinstall --datadir=/usr/share install ekg
sudo cabal --datadir=/usr/share install
sudo cp dist/build/pastewatch/pastewatch /usr/bin/pastewatch
sudo cp sample.config /etc/pastewatch.config
sudo cp extras/ubuntu_init_script.sh /etc/init.d/pastewatch

sudo rm -f /usr/share/ekg-*/assets/*
sudo cp assets/ekg/assets/* /usr/share/ekg-*/assets/

cd ..
fpm -s dir -t deb -n "pastewatch" -v $VERSION --license="GPLv3" --vendor="clune.org" --directories=/opt/pastewatch -m "arthur@clune.org" --description="Watch paste sites for interesting stuff" --url="https://github.com/ArthurClune/pastewatch"  /usr/bin/pastewatch /etc/init.d/pastewatch /etc/pastewatch.config /usr/share/ekg-*


