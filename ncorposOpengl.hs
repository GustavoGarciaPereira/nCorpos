import System.Environment
import StateUtil
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

g = 6.674287*10**(-11)
instance NFData CDouble -- Necessário para usar deepseq em GLfloat

forcaGravitacional m1 m2 r = 
	if r == 0  then  0 
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
							
numeroMovimentos :: Int ->Tempo->[Forca]->[Corpo]->[Corpo]
numeroMovimentos 0 _ _ _ =[]
numeroMovimentos n dtempo forca corpo = do
	let particao = linhaNormal  corpo (200 `div` 2)
	let forcaTotal = runEval( forcaTotalParalelo corpo particao)
	let teste = movimento (60*60) forcaTotal  corpo
	teste++numeroMovimentos (n-1) dtempo forca (teste)												
													
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
										
forcaTotalParalelo :: [Corpo]->[[Corpo]]->Eval [ Forca]
forcaTotalParalelo  _ [] = return []
forcaTotalParalelo	c0 (c:cs) = do
	p1 <- rpar( force (forcaTotalTodosCorpos c0 c)) 
	p2 <- forcaTotalParalelo c0 cs 			
	--rdeepseq p1
	return (p1++p2)  

								
fstTripla (a,b,c) = a
sndTripla (a,b,c)  = b 
thdTripla (a,b,c)  = c


keyboard radius (Char '+') Down _ _ = do
  r <- get radius
  --radius $= r+0.05
  let particao = linhaNormal  r (1000 `div` 4)
  let forcaTotal = runEval( forcaTotalParalelo r particao)
  radius $= movimento (60*60) forcaTotal  r
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
  
     mapM_ (\(x, y, z)-> preservingMatrix $ do 
		vertex $ Vertex2 ((fst x)::GLdouble) ((snd x)::GLdouble) 
		--translate $ Vector3 ((fst x)::GLdouble) ((snd x)::GLdouble) (0::GLdouble)
	
		--renderObject Solid (Sphere' (10**20) 8 8)
		) a
		--translate $ Vector3 x y (0::GLdouble)
		
  flush
	
	
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
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "nCorpos"

  corpos <- randomBodies 200

  t0 <- getCurrentTime
  let particao = linhaNormal  corpos (200 `div` 2)
  let forcaTotal = runEval( forcaTotalParalelo corpos particao)
  
  t1 <- ( numeroMovimentos 50 (60*60) forcaTotal corpos) `deepseq` getCurrentTime

  let escala = corpos 

  initialWindowSize $= Size 512 512
  let x0 = (-1.6*10^11)
  let xf = 1.6*10^11
  let y0 = (-1.6*10^11)
  let yf = 1.6*10^11
	

  ortho2D x0 xf y0 yf
  putStrLn $ (show $ diffUTCTime t1 t0) 	
  radius <- new corpos
  displayCallback $= display radius escala
  keyboardMouseCallback $= Just (keyboard radius)
  mainLoop
	

	
