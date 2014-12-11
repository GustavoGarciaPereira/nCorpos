Testado no Linux Ubuntu com o Haskell Platform instalado

Baixar do Repositório
  git clone https://github.com/vfpereira/nCorpos.git
  
ncorpos.hs
	Compilação
		ghc ncorpos.hs -threaded
	Execução
		./ncorpos +RTS -Nx (x = numero de processadores) 
		ou
		./ncorpos a b c +RTS -Nx (a = quantidade de corpos, b = numero de movimentos, c = numero de sparks, x=numero de processadores)
		Exemplos
		./ncorpos +RTS -N4 (utilizando 4 núcleos)
		ou
		./ncorpos 100 50 4 +RTS -N4 (100 corpos realizando 50 movimentos e lançando 4 sparks utilizando 4 núcleos)

ncorposOpengl.hs
	Compilação	
		ghc ncorposOpengl.hs
	Execução
		./ncorposOpengl (botão + controla os movimentos)
		
BarnesHut.hs 
	Compilação 
		ghc BarnesHut.hs -threaded
	Execução	
		./BarnesHut +RTS -Nx (x = numero de processadores)
		ou
		./BarnesHut a b c -Nx (a = quantidade de corpos, b = numero de movimentos, c = numero de sparks, x=numero de processadores)
		Exemplos
		./BarnesHut +RTS -N4 (utilizando 4 núcleos)
		ou
		./BarnesHut 100 50 4 +RTS -N4 (100 corpos realizando 50 movimentos e lançando 4 sparks utilizando 4 núcleos)