#!/bin/sh

echo -n "This script drops files all over your machine. You may wish to "
echo "run this script in a disposable VM. Delete these lines to run"
echo
echo "This script requires fpm to run"
echo "$ sudo apt-get install rubygems"
echo "$ sudo gem install fpm"
exit 1

rm pastewatch*deb


if [ ! -d pastewatch ];
then
   git clone https://github.com/ArthurClune/pastewatch.git
fi

cd pastewatch
git pull
VERSION=`grep "^version:" pastewatch.cabal| awk '{split($0,a," "); print a[2]}' `
# if you have installed and then removed the .deb, you may need this next line
# to get a build next time
#sudo cabal --reinstall --force-reinstall --datadir=/usr/share install ekg
sudo cabal --datadir=/usr/share install
sudo cp dist/build/pastewatch/pastewatch /usr/bin/pastewatch
sudo cp sample.config /etc/pastewatch.conf
sudo cp extras/ubuntu_init_script.sh /etc/init.d/pastewatch

sudo rm -f /usr/share/ekg-*/assets/*
sudo cp -r assets/ekg/assets/* /usr/share/ekg-*/assets/

cd ..
fakeroot fpm -s dir -t deb -n "pastewatch" -v $VERSION --license="GPLv3" --vendor="clune.org" -m "arthur@clune.org" \
    --description="Watch paste sites for interesting stuff" \
    --url="https://github.com/ArthurClune/pastewatch" \
    --config-files=/etc/pastewatch.conf \
    /usr/bin/pastewatch /etc/init.d/pastewatch /etc/pastewatch.conf /usr/share/ekg-*


