#cd BlocksWorld
git stash
git pull

cd ../eta
git stash
git pull
cp -f ../config.lisp .

cd ../quicklisp/local-projects/ulf2english
git pull
cd ../cl-util
git pull
cd ../ulf-lib
git pull
cd ../ttt
git pull
