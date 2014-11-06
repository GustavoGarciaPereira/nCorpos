--{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE TemplateHaskell #-}
--import Control.DeepSeq.TH
--import GHC.Generics
--import Control.DeepSeq.Generics
import System.Environment
--import StateUtil
--import Graphics.Rendering.OpenGL
import Data.IORef
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GLU
import Control.Parallel.Strategies
import Data.List
import Data.List.Split
import Foreign.C.Types
import System.Random
import Data.Time.Clock
import Control.DeepSeq
import Debug.Trace
import Control.Exception


type X  = GLdouble
type Y  = GLdouble
type Posicao = (X, Y)
type Velocidade = (X, Y)
type Massa = GLdouble 
type Corpo  =  (Posicao,Massa,Velocidade)
type Corpo2 =  (Posicao,Massa,Velocidade,Forca)
type Forca  = (X,Y)
type Aceleracao = (X,Y)
type Tempo = GLdouble
type Quadrante = Int
type Coordenadas = ((X,X),(Y,Y),Quadrante)

rdeepPar :: NFData a => Strategy a
rdeepPar pontos = rpar(force pontos)


g = 6.674287*10**(-11)
instance NFData CDouble -- Necessário para usar deepseq em GLfloat




forcaGravitacional m1 m2 r = if r == 0  then  0 
							 else if r<0 then -g*m1*m2/r**2
								  else g*m1*m2/r**2

velocidadefinal velocidadeinicial dtempo forca massa =velocidadeinicial+dtempo*forca/massa
-- dv = distancia/tempo
-- aceleracao = dv/dt
-- vfinal-vinicial = dt*aceleracao
-- aceleracao = f/massa
-- vfinal= vinicial +dt*f/massa

raiovetorial :: (X,Y)->GLdouble
raiovetorial v1  = sqrt (((fst v1 )**2)+(((snd v1)**2)))

angulovetorial :: (X,Y)->GLdouble
angulovetorial v1 =  atan((snd v1)/(fst v1 ))

subtracaovetorial :: (X,Y)->(X,Y)->(X,Y)
subtracaovetorial v1 v2 = (fst v1 - fst v2,snd v1 -snd v2)


data QuadTree m coordenadas limites = Empty [Corpo]
              | Nodo m coordenadas limites
			 	(QuadTree  m coordenadas limites ) 
				(QuadTree  m coordenadas limites ) 
				(QuadTree  m coordenadas limites ) 
				(QuadTree  m coordenadas limites )
				deriving (Show, Eq)


instance (NFData m,NFData coordenadas,NFData limites) =>NFData (QuadTree m coordenadas limites) where
	rnf (Empty z) =  ()
	rnf (Nodo a1 a2 a3 a4 a5 a6 a7) = rnf a1 `seq ` rnf a2 `seq ` rnf a3 `seq ` rnf a4 `seq ` rnf a5 `seq ` rnf a6 `seq ` rnf a7




			  
populaArvore ::  Int-> Int->[Corpo]->Coordenadas->Eval (QuadTree Massa Posicao Coordenadas)
populaArvore _ _ [] _ =return $ Empty []
populaArvore _ 1 corpo c1  = do 
	let massaTotal =  foldl (\a (x,y,z)->(a +y)) (0) corpo 
	let centroMassa= fstTripla(head corpo)
	return $ Nodo massaTotal centroMassa c1 (Empty []) (Empty []) (Empty []) (Empty [])
populaArvore nTotalCorpos _ corpo c1 = do  

								  let xmin = fst(fstTripla c1)
								  let xmax = snd (fstTripla c1)
								  let ymin = fst (sndTripla c1)
								  let ymax = snd (sndTripla c1)
								  
								  let coord1 = ((xmin,(xmin+xmax)/2),(((ymin+ymax)/2),ymax),1)
								  let coord2 = (((xmin+xmax)/2,xmax),((ymin+ymax)/2,ymax),2)
								  let coord3 = ((xmin,(xmin+xmax)/2),(ymin,(ymin+ymax)/2),3)
								  let coord4 = (((xmin+xmax)/2,xmax),(ymin,(ymin+ymax)/2),4)							  
								  
								  let p1tupla = partition (\(x,y,z)-> fst x < snd( fstTripla coord1) && snd x>= fst (sndTripla coord1)) corpo
								  let p1 = fst p1tupla
								  
								  let p2tupla = partition (\(x,y,z)-> fst x >= fst(fstTripla coord2) && snd x >= fst(sndTripla coord2)) (snd p1tupla)
								  let p2 = fst p2tupla
								  
								  let p3tupla = partition (\(x,y,z)-> fst x< snd(fstTripla coord3) && snd x < snd(sndTripla coord3)) (snd p2tupla)
								  let p3 = fst p3tupla
								  
								  let p4 = snd p3tupla
								  
								  
								  
								 

																		  
								  
								  if (((xmax-xmin))<100 && ((ymax-ymin))<100 ) then do
									let massaTotal =  foldl (\a (x,y,z)->(a +y)) (0) corpo 
									let centroMassaTotal= foldl (\a (x,y,z)->(fst a+(fst x)*y ,snd a+(snd x)*y )) (0,0) corpo 
									let centroMassa = (fst centroMassaTotal/massaTotal,snd centroMassaTotal/massaTotal)
									return $ Nodo massaTotal centroMassa c1 (Empty p1) (Empty p2) (Empty p3) (Empty p4)
								  else do
                                    --a' <- populaArvore (length p1) p1 coord1 
									--a0 <-rpar $ force (a' :: QuadTree Massa Posicao Coordenadas)
									
									a1' <-populaArvore nTotalCorpos (length p1) p1 coord1  
									a1 <- if length p1 > (nTotalCorpos `div` 2) then  rpar $  force (a1' :: QuadTree Massa Posicao Coordenadas)
										  else rseq $  a1'
									a2' <-populaArvore nTotalCorpos (length p2) p2 coord2	  
									a2 <- if length p2 > (nTotalCorpos `div` 2) then rpar $  force (a2' :: QuadTree Massa Posicao Coordenadas)
										  else rseq(a2') 
									a3'<-populaArvore nTotalCorpos (length p3) p3 coord3
									a3 <- if length p3 > (nTotalCorpos `div` 2) then  rpar $  force (a3' :: QuadTree Massa Posicao Coordenadas)
										  else rseq(a3') 
									
									a4' <-populaArvore nTotalCorpos (length p4) p4 coord4  
									a4 <- if length p4 > (nTotalCorpos `div` 2) then  rpar $  force (a4' :: QuadTree Massa Posicao Coordenadas)
										  else rseq(a4') 


									
									let (Nodo massaTotal1 centroMassa1 a b c d e )= 
										if a1 == (Empty []) then (Nodo 0 (0,0) ((0,0), (0,0),0) (Empty []) (Empty []) (Empty []) (Empty []) )
										else a1
									let (Nodo massaTotal2 centroMassa2 a b c d e )= 
										if a2 == (Empty []) then (Nodo 0 (0,0) ((0,0), (0,0),0) (Empty []) (Empty []) (Empty []) (Empty []) )
										else a2
									let (Nodo massaTotal3 centroMassa3 a b c d e )= 
										if a3 == (Empty []) then (Nodo 0 (0,0) ((0,0), (0,0),0) (Empty []) (Empty []) (Empty []) (Empty []) )
										else a3
									let (Nodo massaTotal4 centroMassa4 a b c d e )= 
										if a4 == (Empty []) then (Nodo 0 (0,0) ((0,0), (0,0),0) (Empty []) (Empty []) (Empty []) (Empty []) )
										else a4
									

									let massaTotal = massaTotal1+massaTotal2+massaTotal3+massaTotal4
									let centroMassaTotal = ((fst centroMassa1*massaTotal1)+(fst centroMassa2*massaTotal2)+(fst centroMassa3*massaTotal3)+(fst centroMassa4*massaTotal4),(snd centroMassa1*massaTotal1)+(snd centroMassa2*massaTotal2)+(snd centroMassa3*massaTotal3)+(snd centroMassa4*massaTotal4))
									--let massaTotal =  foldl (\a (x,y,z)->(a +y)) (0) corpo 
									--let centroMassaTotal= foldl (\a (x,y,z)->(fst a+(fst x)*y ,snd a+(snd x)*y )) (0,0) corpo 
									--let centroMassa = (fst centroMassaTotal/massaTotal,snd centroMassaTotal/massaTotal)
									let centroMassa = (fst centroMassaTotal/massaTotal,snd centroMassaTotal/massaTotal)
									return $ Nodo massaTotal centroMassa c1 (  a1 ) ( a2) ( a3) (  a4)
  
  
  
forcaIndividual :: [Corpo] -> Corpo -> [Forca]
forcaIndividual f corpo = map (\(posicao,massa,velocidade) -> do 
																let distancia = subtracaovetorial  posicao (fstTripla corpo) 
																let raio = raiovetorial distancia
																let angulo = if (fst distancia) == 0 then pi/2 
																			 else abs (angulovetorial distancia)
																
																let forca = forcaGravitacional massa (sndTripla corpo) raio
																--(forca,0)
																let s1 = if (fst distancia) < 0 then -1
																		 else 1
																
																let s2 = if (snd distancia) <0 then -1
																		 else  1
																
																(s1*forca*cos angulo,s2*forca*sin angulo) 

																) f


forcaTotalUmCorpo :: [Forca] -> Forca
forcaTotalUmCorpo f  = foldl (\a (x,y)->(fst a +x, snd a+y)) (0,0) f 
   


somaForcas::(QuadTree Massa Posicao Coordenadas)->Corpo ->Forca
somaForcas (Empty listacorpos) c = forcaTotalUmCorpo (forcaIndividual listacorpos c)

somaForcas (Nodo m coordenadas limites (Empty _) (Empty _) (Empty _) (Empty _)) corpo = if (pertence limites corpo)== True then (0,0)
																		else calculoForcas  m coordenadas corpo 
somaForcas (Nodo m coordenadas limites a1 a2 a3 a4) corpo = do 
				if (pertence limites corpo)== True then
					(fst (somaForcas a1 corpo) + fst (somaForcas a2 corpo) + fst (somaForcas a3 corpo) + fst (somaForcas a4 corpo) ,  snd (somaForcas a1 corpo) + snd (somaForcas a2 corpo) + snd (somaForcas a3 corpo) + snd (somaForcas a4 corpo))
				else calculoForcas m coordenadas corpo  


calculoForcas::Massa-> Posicao->Corpo->Forca
calculoForcas m coordenadas corpo = do 
						let distancia = subtracaovetorial  coordenadas (fstTripla corpo) 
						let raio = raiovetorial distancia
						let angulo = if (fst distancia) == 0 then pi/2 
									 else abs (angulovetorial distancia)

						let forca = forcaGravitacional m (sndTripla corpo) raio
						let s1 = if (fst distancia) < 0 then -1
								 else 1

						let s2 = if (snd distancia) <0 then -1
								 else  1
						(s1*forca*cos angulo,s2*forca*sin angulo) 


pertence:: Coordenadas->Corpo->Bool
pertence limites corpo =    
							if thdTripla(limites) == 1 then 
								if fst(fstTripla corpo)>= fst(fstTripla limites) && fst(fstTripla corpo)< snd(fstTripla limites) && snd(fstTripla corpo)>= fst(sndTripla limites) && snd(fstTripla corpo)<= snd(sndTripla limites) then
									True
								else False
							else if thdTripla(limites) == 2 then 
									if fst(fstTripla corpo)>= fst(fstTripla limites) && fst(fstTripla corpo)<= snd(fstTripla limites) && snd(fstTripla corpo)>= fst(sndTripla limites) && snd(fstTripla corpo)<= snd(sndTripla limites) then
										True
									else False
							else if thdTripla(limites) == 3 then 
									if fst(fstTripla corpo)>= fst(fstTripla limites) && fst(fstTripla corpo)< snd(fstTripla limites) && snd(fstTripla corpo)>= fst(sndTripla limites) && snd(fstTripla corpo)< snd(sndTripla limites) then
										True
									else False
							else 
									if fst(fstTripla corpo)>= fst(fstTripla limites)  && fst (fstTripla corpo)<= snd(fstTripla limites) && snd(fstTripla corpo)>= fst(sndTripla limites) && snd(fstTripla corpo)< snd(sndTripla limites) then
										True
									else False






eixoX:: [Corpo] ->[X]
eixoX [] = []
eixoX (c:cs) = [fst(fstTripla c)]++eixoX cs 

eixoY:: [Corpo] ->[Y]
eixoY [] = []
eixoY (c:cs) = [snd(fstTripla c)]++eixoY cs 

numeroMovimentos :: Int->Int ->Tempo->[Forca]->[Corpo]->[Corpo]
numeroMovimentos _ 0 _ _ _ =[]
numeroMovimentos chunks n dtempo forca corpo = do
										 let particao = linhaNormal  corpo chunks
										 let coord0 =   eixoX corpo
										 let coord1 =   eixoY corpo
										 --let coord = ((minimum coord0,maximum coord0),(minimum coord1,maximum coord1),2)
										 let coord = ((-1.5*10^11,1.5*10^11),(-1.5*10^11,1.5*10^11),2) -- distancia máxima possivel entre corpos para distancia maiores 0
										 let arvore = force $ runEval $populaArvore (length corpo) (length corpo) corpo  coord
										 let a2 = force (arvore)
										 let forcaTotal = runEval( forcaTotalParalelo (force $ runEval $populaArvore (length corpo) (length corpo) corpo  coord) particao)
										 let m1 = runEval $  movimentoParalelo (60*60) (forcaNormal forcaTotal chunks)  particao
										 m1++numeroMovimentos chunks (n-1) dtempo forca (m1)

movimentoParalelo :: Tempo->[[Forca]]->[[Corpo]]->Eval [Corpo]
movimentoParalelo _ [] _ = return []
movimentoParalelo dtempo (forca:forcaxs) (corposAntes:corposAntesxs) = do 
	p1 <- rpar(force $ movimento dtempo forca corposAntes) 
	p2 <- movimentoParalelo dtempo forcaxs corposAntesxs
	rseq p1
	return (p1++p2)  

movimento :: Tempo->[Forca]->[Corpo]->[Corpo]
movimento _ [] _ = []
movimento _ _ [] = []
movimento dtempo (forca:forcaxs) (corposAntes:corposAntesxs) = do 
										--let forcax = (raiovetorial forca (0,0))*cos(angulovetorial forca (0,0))
										--let forcay = (raiovetorial forca (0,0))*sin(angulovetorial forca (0,0))
										let massa = sndTripla corposAntes
										let newVelocidadex =( velocidadefinal (fst(thdTripla corposAntes)) dtempo (fst forca) massa)
										let newVelocidadey = ( velocidadefinal (snd(thdTripla corposAntes)) dtempo (snd forca) massa)
										let newVelocidade = (newVelocidadex,newVelocidadey)
										let newPosicaox= fst(fstTripla(corposAntes))+ newVelocidadex*dtempo
										let newPosicaoy =snd(fstTripla(corposAntes))+ newVelocidadey*dtempo
										let newPosicao = (newPosicaox,newPosicaoy) 
										[(newPosicao,massa,newVelocidade)]++movimento dtempo  forcaxs  corposAntesxs


forcaTotalTodosCorpos :: [Corpo]->(QuadTree Massa Posicao Coordenadas)->[Forca]
forcaTotalTodosCorpos corpos arvore = map (\c ->somaForcas arvore c) corpos										
										
								
linhaNormal :: [Corpo]->Int-> [[Corpo]]
linhaNormal c tamanhoChunk= chunksOf tamanhoChunk c 

forcaNormal :: [Forca]->Int-> [[Forca]]
forcaNormal c tamanhoChunk= chunksOf tamanhoChunk c 




forcaTotalParalelo :: (QuadTree Massa Posicao Coordenadas)->[[Corpo]]->Eval [ Forca]
forcaTotalParalelo  _ [] = return []
forcaTotalParalelo	arvore (c:cs) = do
							p1 <- rpar(force $ forcaTotalTodosCorpos c arvore) 
							p2 <- forcaTotalParalelo arvore cs 
							rseq p1
							return (p1++p2)  




fstTripla (a,b,c) = a
sndTripla (a,b,c)  = b 
thdTripla (a,b,c)  = c


randomBodies ::  Int -> IO [Corpo]
randomBodies len = 
   if len==0
     then return []
     else do
          b <- do mass <- getStdRandom (randomR (1e15,8e30))
                  x    <- getStdRandom (randomR (-1.5*10^11,1.5*10^11))
                  y    <- getStdRandom (randomR (-1.5*10^11,1.5*10^11))
                  --z    <- getStdRandom (randomR (-8e8,8e8))
                  --dx   <- getStdRandom (randomR (-5e3,5e3))
                  --dy   <- getStdRandom (randomR (-5e3,5e3))
                  --dz   <- getStdRandom (randomR (-5e3,5e3))
                  return ((x,y),(mass),(0,0))
                 
          l <- randomBodies (len-1)
          return $ (b:l) 


main :: IO ()
main  =
   do
	args <- getArgs
	--args<- getArgs
  	let (numeroCorpos,qtdMovimentos,cores) = processaArgs args
	let coord = ((-1.5*10^11,1.5*10^11),(-1.5*10^11,1.5*10^11),2)
	let tempo = 60*60
	t0 <- getCurrentTime
	corpos <- randomBodies numeroCorpos

	--let arvore = runEval $ populaArvore (length corpos) (length corpos) corpos  coord
	
	t2 <- getCurrentTime
	let particao = linhaNormal  corpos (numeroCorpos `div` cores)
	let t10 = (runEval $ populaArvore (length corpos) (length corpos) corpos  coord)
	--t3 <-  t10 `deepseq` getCurrentTime
	
	--putStrLn $ ((show $ diffUTCTime t3 t2))
	--let forcaTotal = runEval( forcaTotalParalelo arvore particao)
	let chunks = numeroCorpos `div` cores
	t4 <- getCurrentTime
	t5 <-  runEval( forcaTotalParalelo (force $runEval $ populaArvore (length corpos) (length corpos) corpos  coord) particao) `deepseq` getCurrentTime--( numeroMovimentos chunks qtdMovimentos (tempo) [] corpos) `deepseq` getCurrentTime
  
	t1 <- ( numeroMovimentos chunks qtdMovimentos (tempo) [] corpos) `deepseq` getCurrentTime
	
	putStrLn $ ((show $ diffUTCTime t1 t0) ++ ", " ++ (show $ (numeroCorpos))++ ", " ++ (show $ (cores))++ ", " ++ (show $ (qtdMovimentos)))  
	where
   -- define parâmetros default caso não sejam especificados
   processaArgs args = case args of
     (a:b:c:_) -> (read a,read b,read c)
     _           -> (100,50,2)							   
								  
								  
