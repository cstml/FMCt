{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module FMCt.TypeChecker2 where 

import qualified Control.Lens as L
import Control.Lens (view,_1,makePrisms)
import FMCt.Syntax
import FMCt.Parsing
import FMCt.TypeChecker (Derivation(..), freshVarTypes, splitStream, TError(..), pShow, normaliseT)
import Control.Monad
-- data Derivation
--     = Star !Judgement
--     | Variable !Judgement
--     | Abstraction !Judgement !Derivation
--     | Application !Judgement !Derivation
--     | Fusion !Judgement !Derivation !Derivation


-- -- | FMC Terms Type
-- data Tm
--     = -- | Variable
--       V Vv Tm
--     | -- | Application or Push: [M]a.N
--       P Tm Lo Tm
--     | -- | Abstraction or Pop:  a\<x:t\>.N
--       B Vv T Lo Tm
--     | -- | Star
--       St
--     deriving (Eq, Ord)


type Context = [(Vv, T)]

type Judgement = (Context, Term, T)

type Term = Tm

makePrisms ''Tm
makePrisms ''Derivation


emptyCx :: Context
emptyCx = [("*",mempty :=> mempty)]

derive1 :: Term -> Derivation
derive1 = (derive1' freshVarTypes [])
  where
    
    derive1' :: [T] -> Context -> Term -> Derivation
    derive1' stream exCx = \case

      St -> Star (emptyCx, St, ty)
        where
          ty = either (error.show) id $ getType St exCx 
      
      xx@(V x St) -> Variable ((x,ty):emptyCx, xx, ty)
        where
          ty = either (error.show) id $ getType xx exCx 
            
      xx@(V bi tm) -> Fusion (mCx, xx, ty) derivL derivR
        where
          ty = head stream
          (lStr,rStr) = splitStream $ tail stream
          derivL = (derive1' lStr exCx (V bi St))
          lCx = getContext derivL
          derivR = (derive1' rStr lCx tm)          
          rCx = getContext derivR
          mCx = makeSet $ lCx ++ rCx

      xx@(B bi bTy lo St) -> Abstraction (aboveCx, xx, ty) deriv
        where
          ty = TLoc lo bTy :=> mempty
          nCx = (bi,bTy) : exCx
          deriv = derive1' (tail stream) nCx (V bi St)
          aboveCx = getContext deriv
            
      xx@(B bi bTy lo tm) -> Fusion (mCx, xx, ty) derivL derivR
        where
          ty = head stream
          (lStr,rStr) = splitStream $ tail stream
          derivL = (derive1' lStr exCx (B bi bTy lo St))
          lCx = getContext derivL
          derivR = derive1' rStr lCx tm
          rCx = getContext derivR
          mCx = makeSet $ lCx ++ rCx

      xx@(P ptm lo St) -> Application (aboveCx, xx, ty) deriv 
        where
          ty = mempty :=> TLoc lo abvT
          deriv = derive1' (tail stream) exCx ptm
          abvT = getDerivationT  deriv
          aboveCx = getContext deriv

      xx@(P ptm lo tm) -> Fusion (mCx, xx, ty) derivL derivR
        where
          ty = head stream
          (lStr,rStr) = splitStream $ tail stream
          derivL = derive1' lStr exCx (P ptm lo St)
          lCx = getContext derivL
          derivR = derive1' rStr lCx tm
          rCx = getContext derivR
          mCx = makeSet $ lCx ++ rCx

testD1 :: Term -> IO ()
testD1 = putStrLn . pShow . derive1

type TSubs = (T,T)

consume :: [TSubs] -> T -> T -> ([TSubs],T)
consume exSubs x y =
  let
    x' = applyTSub exSubs x
    y' = applyTSub exSubs y
  in
    case x' of
      
      TEmp -> (exSubs,mempty)
      
      TVec [] -> (exSubs,mempty)
      
      TCon _ -> case y' of
        TEmp -> (exSubs,x')
        TCon _ -> if x' == y' then mempty else error "cannot consume"
        TVec [] -> (exSubs,x')
        TVec (yy': yys') -> 
          let
            (intSubs,intX) = consume exSubs x' yy'
          in
            consume intSubs intX (TVec yys')
        t1 :=> t2 ->  error "cannot consume higher type"
        TVar _ -> ((y',x'):exSubs,mempty)
        TLoc _ _ -> (exSubs, y')
      
          
        
          
          
        
      
applyTSub :: [TSubs] -> T -> T
applyTSub = \case
  [] -> id
  xx@((xi,xo):xs) -> \case
     y@(TCon _ ) -> y
     TLoc l t -> TLoc l (applyTSub xx t)
     TVec y -> TVec $ applyTSub xx <$> y
     yi :=> yo -> applyTSub xx yi :=> applyTSub xx yo
     y@(TVar _) -> if y == xi then xo else applyTSub xs y

{-
The first derivation always can find a way to derive the term. Now we need a way
 to specialise it. Given that we are not allowing for types to be re-cast we can
 merge the context by creating our first round of substitutions.
-}

mergeContexts0 :: Derivation -> Subs
mergeContexts0 = \case
  x@(Star _  ) -> []
  x@(Variable _  ) -> []
  x@(Application (cx,tm,ty) d) -> [(ty,nt)]
    where
      nt' = getDerivationT d
      nt =  mempty :=> TLoc loc nt'
      loc = getLocation tm
  x@(Abstraction (cx,tm,ty) d) -> [(ty,nt)]
    where
      nt' = getDerivationT d
      nt =  TLoc loc nt' :=> mempty
      loc = getLocation tm
  Fusion _ dL dR -> (mergeContexts0 dL) ++ (mergeContexts0 dR)
  

testM0 :: Term -> IO ()
testM0 term = putStrLn . pShow $ deriv2
  where
    deriv1 = derive1 term
    subs = mergeContexts0 deriv1
    deriv2 = applySubsD subs deriv1

       
type Subs = [(T,T)]

mergeContexts1 :: Derivation -> Subs
mergeContexts1 = mergeContexts1' . makeSet . allCtx
  where    
    mergeContexts1' :: Context -> Subs
    mergeContexts1' [] = []
    mergeContexts1' (x:xs) =
       if uniqueV x xs
       then mergeContexts1' xs
       else firstPair x xs : mergeContexts1' xs
     where
       uniqueV :: (Vv,T) -> [(Vv,T)] -> Bool
       uniqueV _ [] = True
       uniqueV z@(zv,_) ((yv,_):ys)
         | zv == yv = False
         | otherwise = uniqueV z ys
   
       firstPair :: (Vv,T) -> [(Vv,T)] -> (T,T)
       firstPair z@(zv,zt) ((yv,yt):ys)
         | zv == yv = (zt,yt)
         | otherwise = firstPair z ys
       firstPair _ [] = error "This Should not happer - have a look at the mergeContexts1 uniqueV implementation"

testM1 :: Term -> IO ()
testM1 term = putStrLn $ pShow deriv2
  where
    deriv1 = derive1 term
    subs1 = mergeContexts1 deriv1
    deriv2 = applySubsD subs1 deriv1  

getType :: Term -> Context -> Either TError T
getType = \case
    t@(V b St) -> \case
        [] ->
            Left $
                ErrUndefT $
                    mconcat
                        [ "Cannot Find type for binder: "
                        , show b
                        , " in context. Have you defined it prior to calling it ?"
                        ]
        ((b', ty) : xs) -> if b == b' then pure ty else getType t xs
    St -> \_ -> pure $ mempty :=> mempty
    t -> \_ ->
        Left . ErrNotBinder $
            mconcat ["Attempting to get type of:", show t]

getContext :: Derivation -> Context
getContext = \case
  Star (c,_,_) -> c
  Variable (c,_,_) -> c
  Abstraction (c,_,_) _ -> c
  Fusion (c,_,_) _ _ -> c
  Application (c,_,_) _ -> c


applySubsT :: Subs -> T -> T
applySubsT [] x = x
applySubsT ((ti,to):xs) x = if ti == x then applySubsT xs to else applySubsT xs x 

applySubsCx :: Subs -> Context -> Context
applySubsCx x y = applySubsVvT x <$> y

applySubsVvT :: Subs -> (Vv,T) -> (Vv,T)
applySubsVvT [] y = y
applySubsVvT ((ti,to):xs) y@(yv,yt)
  | yt == ti  = applySubsVvT xs (yv,to)
  | otherwise = applySubsVvT xs y
  
applySubsD :: Subs -> Derivation -> Derivation
applySubsD subs = \case
  Star (cx,tm,ty) -> Star (applySubsCx subs cx, tm ,applySubsT subs ty)
  Variable (cx,tm,ty) -> Variable (applySubsCx subs cx, tm ,applySubsT subs ty)
  Abstraction (cx,tm,ty) de -> Abstraction (makeSet $ applySubsCx subs cx, tm ,applySubsT subs ty) (applySubsD subs de)
  Application (cx,tm,ty) de -> Application (makeSet $ applySubsCx subs cx, tm ,applySubsT subs ty) (applySubsD subs de)
  Fusion (cx,tm,ty) dL dR -> Fusion (makeSet $ applySubsCx subs cx, tm ,applySubsT subs ty) (applySubsD subs dL) (applySubsD subs dR)
 
allCtx :: Derivation -> Context
allCtx = \case
  x@(Star _) -> getContext x
  x@(Variable _) -> getContext x
  x@(Application _ d) -> getContext x ++ allCtx d
  x@(Abstraction _ d) -> getContext x ++ allCtx d
  x@(Fusion _ dL dR) -> getContext x ++ allCtx dR ++ allCtx dL

makeSet :: Eq a => [a] -> [a]
makeSet []     = [] 
makeSet (x:xs) = if elem x xs then makeSet xs else x : makeSet xs

getDerivationT :: Derivation -> T
getDerivationT = \case 
  Star (_,_,t) -> t
  Variable (_,_,t) -> t
  Application (_,_,t) _ -> t
  Abstraction (_,_,t) _ -> t
  Fusion (_,_,t) _ _ -> t

getLocation :: Term -> Lo
getLocation = \case
  P _ l _ -> l
  B _ _ l _ -> l
  x -> error $ "should't be reaching for location in term: " ++ show x
