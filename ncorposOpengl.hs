import System.Environment
import StateUtil
import Graphics.Rendering.OpenGL
import Data.IORef
import Graphics.UI.GLUT
import Control.Parallel.Strategies
import Data.List
import Data.List.Split
import Foreign.C.Types
--g = 6.674287*10**(-11)
instance NFData CDouble -- Necessário para usar deepseq em GLfloat
g=1
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
 
forcaTotalTodosCorpos :: [Corpo]->[Corpo]->[Forca]
forcaTotalTodosCorpos corpos corposIterados = do 
													let f1 = map(\c -> forcaIndividual corpos c) corposIterados
													map (\forcas ->forcaTotalUmCorpo forcas   ) f1



													
													
													
movimento :: Tempo->[Forca]->[Corpo]->[Corpo]
movimento _ [] [] = []
movimento dtempo (forca:forcaxs) (corposAntes:corposAntesxs) = do 
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
										
forcaTotalParalelo :: [[Corpo]]->Eval [ Forca]
forcaTotalParalelo  [] = return []
forcaTotalParalelo	(c:cs) = do
							p1 <- rpar(forcaTotalTodosCorpos c c)
							p2 <- forcaTotalParalelo cs 			
							rdeepseq p1
							return (p1++p2)  

								
fstTripla (a,b,c) = a
sndTripla (a,b,c)  = b 
thdTripla (a,b,c)  = c


keyboard radius (Char '+') Down _ _ = do
  r <- get radius
  --radius $= r+0.05
  let particao = linhaNormal  r 4
  let forcaTotal = runEval( forcaTotalParalelo particao)
  radius $= movimento 0.001 forcaTotal  r
  postRedisplay Nothing
--keyboard radius (Char '-') Down _ _ = do
--  r <- get radius
--  radius $= r-0.05
--  postRedisplay Nothing
keyboard _ _ _ _ _ = return ()

myPoints :: [(GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12)) | k <- [1..12] ]

display  radius p= do 
  --print p
  clear [ColorBuffer]
  a <- get radius
  renderPrimitive Points $
     mapM_ (\(x, y, z) -> vertex $ Vertex2 ((fst x)::GLdouble) ((snd x)::GLdouble) ) a
  flush
	
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "nCorpos"
  let c1 = ((0,0),(1::GLdouble),(0,0)) 
  let c2 = ((0.1,0),(5::GLdouble),(0,0)) --5
  let c3 = ((0.2,0),(1::GLdouble),(0,0))
  let c4 = ((0,0.1),(1::GLdouble),(0,0))
  
  let corpos = [c1,c2,c3,c4]
  
  let tamanhoChunks = (length(corpos) `div` (4))
  
  
  
 -- let corpos = [c1,c2,c3,c4,c5,c6,c7,c8,c11,c12,c13,c14,c15,c16,c17,c18]
  --let sol  = ((0,(1.5*10^11)),(2*10**30),(0,0)  )
  --let terra = ((0,0), (6*10**24), (0,0) )
  --let lua  = ((3.8*10^8,0),(7.3 * 10**22 ),(0,0))
  --let corpos = [terra,sol,lua]
  print corpos
  let escala = corpos --map (\(posicao,massa,velocidade) -> ((((fst posicao)/5),((snd posicao)/5)),massa,velocidade)  ) corpos   
  print escala
  radius <- new corpos
  displayCallback $= display radius escala
  keyboardMouseCallback $= Just (keyboard radius)
  mainLoop
	