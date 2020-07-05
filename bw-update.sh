#cd BlocksWorld
echo Updating the main BW codebase...
git stash
git pull

echo Updating the Eta dialog manager...
cd ../eta
git stash
git pull
sed -i 's/sophie/david-qa/g' config.lisp
sed -i 's/\*live-mode\* NIL/\*live-mode\* T/g' config.lisp
sed -i 's/\*perceptive-mode\* NIL/\*perceptive-mode\* T/g' config.lisp
sed -i 's/\*responsive-mode\* NIL/\*responsive-mode\* T/g' config.lisp
#cp -f ../config.lisp .

echo Updating ulf2english...
cd ../quicklisp/local-projects/ulf2english
git pull

echo Updating cl-util...
cd ../cl-util
git pull

echo Updating ulf-lib...
cd ../ulf-lib
git pull

echo Updating ttt...
cd ../ttt
git pull
