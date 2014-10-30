import System.Environment
--import StateUtil
import Graphics.Rendering.OpenGL
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


data QuadTree a m coordenadas limites = Empty
              | Nodo m coordenadas limites
			 	(QuadTree  a m coordenadas limites ) 
				(QuadTree  a m coordenadas limites ) 
				(QuadTree  a m coordenadas limites ) 
				(QuadTree  a m coordenadas limites )
              deriving (Eq,Ord,Show,Read)
	
	
			  
populaArvore :: Int->[Corpo]->Coordenadas->(QuadTree a Massa Posicao Coordenadas)
populaArvore _ [] _ = Empty
populaArvore 1 corpo c1  = do 
	let massaTotal =  foldl (\a (x,y,z)->(a +y)) (0) corpo 
	let centroMassa= fstTripla(head corpo)
	Nodo massaTotal centroMassa c1 (Empty) (Empty) (Empty) (Empty)
populaArvore _ corpo c1 = do  
							
								  let xmin = fst(fstTripla c1)
								  let xmax = fst (sndTripla c1)
								  let ymin = snd (fstTripla c1)
								  let ymax = snd (sndTripla c1)
								  
								  let coord1 = ((xmin,((xmax-xmin)/2)),(((ymax-ymin)/2),ymax),1)
								  let coord2 = (((xmax-xmin)/2,xmax),((ymax-ymin)/2,ymax),2)
								  let coord3 = ((xmin,(xmax-xmin)/2),(ymin,(ymax-ymin)/2),3)
								  let coord4 = (((xmax-xmin)/2,xmax),(ymin,(ymax-ymin)/2),4)							  
								  
								  let p1tupla = partition (\(x,y,z)-> fst x < fst( sndTripla coord1) && snd x>= snd (fstTripla coord1)) corpo
								  let p1 = fst p1tupla
								  
								  let p2tupla = partition (\(x,y,z)-> fst x >= fst(fstTripla coord2) && snd x >= snd(fstTripla coord2)) (snd p1tupla)
								  let p2 = fst p2tupla
								  
								  let p3tupla = partition (\(x,y,z)-> fst x< fst(sndTripla coord3) && snd x < snd(sndTripla coord3)) (snd p2tupla)
								  let p3 = fst p3tupla
								  
								  let p4 = snd p3tupla
								  let massaTotal =  foldl (\a (x,y,z)->(a +y)) (0) corpo 

								  let centroMassaTotal= foldl (\a (x,y,z)->(fst a+(fst x)*y ,snd a+(snd x)*y )) (0,0) corpo 
								  let centroMassa = (fst centroMassaTotal/massaTotal,snd centroMassaTotal/massaTotal)
								  Nodo massaTotal centroMassa c1 (populaArvore (length p1) p1 coord1 ) (populaArvore (length p2) p2 coord2) (populaArvore (length p3) p3 coord3) (populaArvore (length p4) p4 coord4)
	

	
somaForcas::(QuadTree a Massa Posicao Coordenadas)->Corpo ->Forca	
somaForcas Empty _ = (0,0)
somaForcas (Nodo m coordenadas limites Empty Empty Empty Empty) corpo = if (pertence limites corpo)== True then (0,0)
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
									


forcaTotalTodosCorpos :: [Corpo]->(QuadTree a Massa Posicao Coordenadas)->[Forca]
forcaTotalTodosCorpos corpos arvore = map (\c ->somaForcas arvore c) corpos


eixoX:: [Corpo] ->[X]
eixoX [] = []
eixoX (c:cs) = [fst(fstTripla c)]++eixoX cs 

eixoY:: [Corpo] ->[Y]

eixoY (c:cs) = [snd(fstTripla c)]++eixoY cs 
		
numeroMovimentos :: Int->Int ->Tempo->[Forca]->[Corpo]->[Corpo]
numeroMovimentos _ 0 _ _ _ =[]
numeroMovimentos chunks n dtempo forca corpo = do
										 let particao = linhaNormal  corpo chunks
										 let coord0 =   eixoX corpo
										 let coord1 =   eixoY corpo
										 --let coord = ((minimum coord0,maximum coord0),(minimum coord1,maximum coord1),2)
										 let coord = ((-1.5*10^51,1.5*10^51),(-1.5*10^51,1.5*10^51),2)
										 let arvore = populaArvore (length corpo) corpo  coord
										 let forcaTotal = runEval( forcaTotalParalelo arvore particao)
										 let teste = movimento (60*60) forcaTotal  corpo
										 teste++numeroMovimentos chunks (n-1) dtempo forca (teste)												
	

movimento :: Tempo->[Forca]->[Corpo]->[Corpo]
movimento _ [] [] = []
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


linhaNormal :: [Corpo]->Int-> [[Corpo]]
linhaNormal c tamanhoChunk= chunksOf tamanhoChunk c 
										
forcaTotalParalelo :: (QuadTree a Massa Posicao Coordenadas)->[[Corpo]]->Eval [ Forca]
forcaTotalParalelo  _ [] = return []
forcaTotalParalelo	arvore (c:cs) = do
							p1 <- rpar(forcaTotalTodosCorpos c arvore) 
							p2 <- forcaTotalParalelo arvore cs 			
							rdeepseq p1
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
	(_, args) <- getArgsAndInitialize 
	--args<- getArgs
  	let (numeroCorpos,qtdMovimentos,cores) = processaArgs args
	let coord = ((-1.5*10^11,1.5*10^11),(-1.5*10^11,1.5*10^11),2)
	let tempo = 60*60
	t0 <- getCurrentTime
	corpos <- randomBodies numeroCorpos

	let arvore = populaArvore (length corpos) corpos  coord
	let particao = linhaNormal  corpos (numeroCorpos `div` cores)
	let forcaTotal = runEval( forcaTotalParalelo arvore particao)
	let chunks = numeroCorpos `div` cores
	t1 <- ( numeroMovimentos chunks qtdMovimentos (tempo) forcaTotal corpos) `deepseq` getCurrentTime
  
	putStrLn $ (show $ diffUTCTime t1 t0)  
	where
   -- define parâmetros default caso não sejam especificados
   processaArgs args = case args of
     (a:b:c:_) -> (read a,read b,read c)
     _           -> (100,50,2)							   
								  
								  