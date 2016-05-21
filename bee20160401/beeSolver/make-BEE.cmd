@echo off
echo ==============================
echo ===== COMPILE  BumbleBEE =====
echo ==============================
cd ..
del BumbleBEE.exe /q
swipl.exe -G0 -T0 -L0 -f none -F none -g true -t "consult(['beeSolver/bSolver.pl']),qsave_program('BumbleBEE.exe',[goal='bSolver',toplevel=prolog,init_file=none])"
copy satsolver\pl-crypminisat.dll .\
echo "**** BumbleBEE Requires Prolog files: swipl.dll, pthreadVC.dll ****"
echo "****                 PL-CryptoMinisat file: pl-crypminisat.dll ****"
echo ---- DONE -----
