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

g = 6.674287*10**(-11) -- constante gravitacional
instance NFData CDouble -- Necessário para usar deepseq em GLfloat


forcaGravitacional m1 m2 r =  -- calcula da força gravitacional entre dois corpos
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


-- força que todos os corpos vizinhos exercem em um corpo individual
forcaIndividual :: [Corpo] -> Corpo -> [Forca]
forcaIndividual f corpo = map (\(posicao,massa,velocidade) -> do 
	let distancia = subtracaovetorial  posicao (fstTripla corpo) 
	let raio = raiovetorial distancia
	let angulo = if (fst distancia) == 0 then pi/2 
				 else abs (angulovetorial distancia)
															
	let forca = forcaGravitacional massa (sndTripla corpo) raio
	let s1 = if (fst distancia) < 0 then -1
			 else 1
																
	let s2 = if (snd distancia) <0 then -1
			 else  1
																
	(s1*forca*cos angulo,s2*forca*sin angulo) 															
	) f

-- somatorio das forças em um corpo
forcaTotalUmCorpo :: [Forca] -> Forca
forcaTotalUmCorpo f  = foldl (\a (x,y)->(fst a +x, snd a+y)) (0,0) f 

--chama as funções anteriores para realizar em todos os corpos
forcaTotalTodosCorpos :: [Corpo]->[Corpo]->[Forca]
forcaTotalTodosCorpos corpos corposIterados = do 
	let f1 = map(\c -> forcaIndividual corpos c) corposIterados
	map (\forcas ->forcaTotalUmCorpo forcas   ) f1
		
-- quantidade de movimentos que os corpos realizam , onde cada movimento representa 60 minutos		
numeroMovimentos :: Int->Int ->Tempo->[Forca]->[Corpo]->[Corpo]
numeroMovimentos _ 0 _ _ _ =[]
numeroMovimentos chunks n dtempo forca corpo = do
	let particao = linhaNormal  corpo (chunks)
	let forcaTotal = runEval( forcaTotalParalelo corpo particao)
	let teste = movimento (60*60) forcaTotal  corpo
	teste++numeroMovimentos chunks (n-1) dtempo forca (teste)												
		
--realização dos movimentos de cada corpo 		
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

--particiona os dados para depois realizar o processameto paralelo
linhaNormal :: [Corpo]->Int-> [[Corpo]]
linhaNormal c tamanhoChunk= chunksOf tamanhoChunk c 
		
--realização do cálculo das forças em paralelo		
forcaTotalParalelo :: [Corpo]->[[Corpo]]->Eval [ Forca]
forcaTotalParalelo  _ [] = return []
forcaTotalParalelo	c0 (c:cs) = do
	p1 <- rpar( force (forcaTotalTodosCorpos c0 c)) 
	p2 <- forcaTotalParalelo c0 cs 			
	rseq p1
	return (p1++p2)  

								
fstTripla (a,b,c) = a
sndTripla (a,b,c)  = b 
thdTripla (a,b,c)  = c

--geração dos corpos de forma randômica
randomBodies ::  Int -> IO [Corpo]
randomBodies len = 
   if len==0
     then return []
     else do
          b <- do mass <- getStdRandom (randomR (1e15,8e30))
                  x    <- getStdRandom (randomR (-1.5*10^11,1.5*10^11))
                  y    <- getStdRandom (randomR (-1.5*10^11,1.5*10^11))
                  return ((x,y),(mass),(0,0))
                 
          l <- randomBodies (len-1)
          return $ (b:l) 
	  
main :: IO ()
main = do 
  (_, args) <- getArgsAndInitialize 
  let (numeroCorpos,qtdMovimentos,cores) = processaArgs args
  corpos <- randomBodies numeroCorpos
  
  
  t0 <- getCurrentTime
  let particao = linhaNormal  corpos (numeroCorpos `div` cores)
  let forcaTotal = runEval( forcaTotalParalelo corpos particao)
  
  t1 <- ( numeroMovimentos (numeroCorpos `div` cores) qtdMovimentos (60*60) forcaTotal corpos) `deepseq` getCurrentTime

  putStrLn $ ( (show $ diffUTCTime t1 t0)++ ", " ++ (show $ (numeroCorpos))++ ", " ++ (show $ (cores))++ ", " ++ (show $ (qtdMovimentos))) 	
  where
  processaArgs args = case args of
    (a:b:c:_) -> (read a,read b,read c)
    _           -> (100,50,2)		 