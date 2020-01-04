recent:
	ghc `ls -tr *.hs|tail -1` && ./`ls -tr | tail -1`

clean:
	find . -perm +100 -type f -name ex\[1-9\] -delete
	- rm ./*.hi ./*.o 2>/dev/null
