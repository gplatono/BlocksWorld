#cd BlocksWorld
git stash
git pull

cd ../eta
git stash
git pull
sed -i 's/sophie/david-qa/g' config.lisp
sed -i 's/*live-mode* NIL/*live-mode* T/g' config.lisp
sed -i 's/*perceptive-mode* NIL/*perceptive-mode* T/g' config.lisp
sed -i 's/*responsive-mode* NIL/*responsive-mode* T/g' config.lisp
#cp -f ../config.lisp .

cd ../quicklisp/local-projects/ulf2english
git pull
cd ../cl-util
git pull
cd ../ulf-lib
git pull
cd ../ttt
git pull
