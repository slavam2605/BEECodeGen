beeSolver: cleanBEE
	swipl -G0 -T0 -L0 -f none -F none -g true -t \
	"['bSolver'],qsave_program('BumbleBEE',[stand_alone(true),goal(bSolver),toplevel(prolog),init_file(none),foreign(save)])"
	mv BumbleBEE ../BumbleBEE
	cp ../satsolver/pl-crypminisat.so ../pl-crypminisat.so

cleanBEE:
	rm -f BumbleBEE

clean: cleanBEE
