all:
	@echo ""
	@echo "Please specify your requirement:"
	@echo "----------------------------------------------"
	@echo "'make plminisat'         to compile PL-MiniSAT."
	@echo "'make plglucose'         to compile PL-Glucose v2.2."
	@echo "'make plglucose4'        to compile PL-Glucose v4.0."
	@echo "'make plcryptominisat'   to compile PL-CryptoMinisat."
	@echo "'make satSolvers'        to compile all PL-SAT solvers."
	@echo ""

satSolvers: clean plminisat plglucose plglucose4 plcryptominisat

plcryptominisat: satSolverInit
	tar -xf plCryptoMinisat_src.tar.gz -C ../satsolver
	(cd ../satsolver/prologinterface && ./configure && make)
	mv ../satsolver/prologinterface/pl-crypminisat.so ../satsolver/pl-crypminisat.so
	rm -r -f ../satsolver/cryptominisat-2.5.1
	rm -r -f ../satsolver/prologinterface

plminisat: satSolverInit
	tar -xf plMiniSAT_src.tar.gz -C ../satsolver
	(cd ../satsolver/prologinterface && ./configure && make)
	mv ../satsolver/prologinterface/pl-minisat.so ../satsolver/pl-minisat.so
	rm -r -f ../satsolver/minisat-2.0.2
	rm -r -f ../satsolver/prologinterface

plglucose: satSolverInit
	tar xf ../satsolver_src/plGlucose_src.tar.gz -C ../satsolver
	(cd ../satsolver/prologinterface && ./configure && make)
	mv ../satsolver/prologinterface/pl-glucose.so ../satsolver/pl-glucose.so
	rm -r -f ../satsolver/glucose-2.2
	rm -r -f ../satsolver/prologinterface

plglucose4: satSolverInit
	tar xf ../satsolver_src/plGlucose4_src.tar.gz -C ../satsolver
	(cd ../satsolver/prologinterface && ./configure && make)
	mv ../satsolver/prologinterface/pl-glucose4.so ../satsolver/pl-glucose4.so
	rm -r -f ../satsolver/glucose-4
	rm -r -f ../satsolver/prologinterface

satSolverInit:
	mkdir -p ../satsolver
	tar xf ../satsolver_src/plSATsolver.tar.gz -C ../satsolver

clean:
	rm -r -f ../satsolver
