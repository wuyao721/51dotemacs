mv ~/.emacs ~/.emacs.bak
mv ~/.screenrc ~/.screenrc.bak
mv ~/.emacs.d/elisp ~/.emacs.d/elisp.bak

cp .emacs ~/.emacs
cp .screenrc ~/.screenrc
cp -r .emacs.d/elisp ~/.emacs.d/

echo 'Done'
