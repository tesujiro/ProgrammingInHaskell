recent:
	ghc `ls -tr *.hs|tail -1` && ./`ls -tr | tail -1`

clean:
	find . -perm +100 -type f -name ch\*.\*_\? -delete
	rm ./*.hi ./*.o
