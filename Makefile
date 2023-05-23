Main: Main.hs TH.hs
	ghc Main.hs -outputdir target

.PHONY: clean
clean:
	rm -rf Main target
