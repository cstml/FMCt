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

          (lStr,rStr) = splitStream $ tail stream
          derivL = (derive1' lStr exCx (V bi St))
          lCx = getContext derivL
          derivR = (derive1' rStr lCx tm)          
          rCx = getContext derivR
          mCx = makeSet $ lCx ++ rCx
          
          tL = getDerivationT derivL
          tR = getDerivationT derivR
          
          (_,ty) = fuse tL tR

      xx@(B bi bTy lo St) -> Abstraction (aboveCx, xx, ty) deriv
        where
          ty = TLoc lo bTy :=> mempty
          nCx = (bi,bTy) : exCx
          deriv = derive1' (tail stream) nCx (V bi St)
          aboveCx = getContext deriv
            
      xx@(B bi bTy lo tm) -> Fusion (mCx, xx, ty) derivL derivR
        where
--          ty = head stream
          (lStr,rStr) = splitStream $ tail stream
          derivL = (derive1' lStr exCx (B bi bTy lo St))
          lCx = getContext derivL
          derivR = derive1' rStr lCx tm
          rCx = getContext derivR
          mCx = makeSet $ lCx ++ rCx
                    
          tL = getDerivationT derivL
          tR = getDerivationT derivR
          
          (_,ty) = fuse tL tR

      xx@(P ptm lo St) -> Application (aboveCx, xx, ty) deriv 
        where
          ty = mempty :=> TLoc lo abvT
          deriv = derive1' (tail stream) exCx ptm
          abvT = getDerivationT  deriv
          aboveCx = getContext deriv

      xx@(P ptm lo tm) -> Fusion (mCx, xx, ty) derivL derivR
        where
--          ty = head stream
          (lStr,rStr) = splitStream $ tail stream
          derivL = derive1' lStr exCx (P ptm lo St)
          lCx = getContext derivL
          derivR = derive1' rStr lCx tm
          rCx = getContext derivR
          mCx = makeSet $ lCx ++ rCx
                    
          tL = getDerivationT derivL
          tR = getDerivationT derivR
          
          (_,ty) = fuse tL tR

testD1 :: Term -> IO ()
testD1 = putStrLn . pShow . derive1

type TSubs = (T,T)

consume :: [TSubs]       -- ^ Substitutions to be made in both types.
        -> T             -- ^ The consuming Type.
        -> T             -- ^ The consumed Type.
        -> ([TSubs],T,T) -- ^ The new list of substitutions, remaining from the consuming, remaining from the consumed.
consume exSubs x y =
  let
    x' = applyTSub exSubs x -- we use the already subtituted form when consuming
    y' = applyTSub exSubs y -- for both terms
  in
    case x' of      
      TEmp -> (exSubs,mempty,y')    -- mempty doesn't change anything
      
      TVec [] -> (exSubs,mempty,y') -- synonym for mempty
      
      TCon _ -> case y' of          -- 
        TEmp -> (exSubs,x',mempty)
        TVec [] -> (exSubs,x',mempty)
        TCon _ -> if x' == y' then (exSubs, mempty, mempty) else error "cannot consume"
        TVec (yy': yys') -> 
          let
            (interSubs,interX,remainY) = consume exSubs x' yy'
            (finalSubs,finalX,finalY) = consume interSubs interX (TVec yys')
          in
            (finalSubs,finalX,remainY <> finalY)
        t1 :=> t2 ->  error $ show x' ++ " cannot consume higher type " ++ show y'
        -- if a constant tries to consume a variable, it creates a substitution and gets fully consumed
        TVar _ -> ((y',x') : exSubs, mempty, mempty) 
        TLoc _ _ -> (exSubs,x',y')

      TVar _ -> case y' of
        TEmp -> (exSubs,x',mempty)
        TVec [] -> (exSubs,x',mempty)
        -- if consumed by anything, the Variable gets cast and changes all the
        -- other appearances of itself.
        _ -> (((x',y'):exSubs),mempty,mempty)

      TLoc xl' xt' -> case y' of
        TEmp -> (exSubs,x',mempty)
        TVec [] -> (exSubs,x',mempty)
        TCon _ -> (exSubs,x',y) -- home row and locations don't interact
        TVar _ -> (exSubs,x',y) -- home row variable and locations don't interact
        TVec (yy': yys') -> 
          let
            (interSubs,interX,remainY) = consume exSubs x' yy'
            (finalSubs,finalX,finalY) = consume interSubs interX (TVec yys')
          in
            (finalSubs,finalX,remainY <> finalY)
        TLoc yl' yt' -> if yl' == xl'
                        then
                          let
                            (finalSubs, finalX', finalY') = consume exSubs xt' yt'
                          in
                            (finalSubs, TLoc xl' finalX', TLoc yl' finalY')
                        else
                          (exSubs,x',y)
        _ :=> _ -> (exSubs,x',y)
        
      ix' :=> ox' -> case y' of
        TEmp -> (exSubs,x',mempty)
        TVec [] -> (exSubs,x',mempty)
        TCon _ -> error $ "cannot fuse higer type " ++ show x' ++ " with " ++ show y'
        TVar _ -> (((y',x'):exSubs),mempty,mempty)
        TLoc _ _ -> (exSubs,x',y')
        TVec (yy' : yys') ->
          let
            (interSubs, interX', interYY') = consume exSubs x' yy'
            (finalSubs, finalX', finalYY') = consume interSubs interX' (TVec yys')
          in
            (finalSubs, finalX', interYY' <> finalYY')
        iy' :=> oy' ->
          if x' == y'
             then (exSubs, mempty, mempty)
             else let
               (intSubs, leftIX', leftIY') =  consume exSubs ix' iy'
               (finalSubs, rightIX', rightIY') =  consume intSubs ox' oy'
               (finalL,finalR) = (normaliseT $ leftIX' <> leftIY', normaliseT $ rightIX' <> rightIY')
               res = (finalSubs,mempty,mempty)
               in case res of
                    (_,TEmp,TEmp) -> res
                    _ -> error $ show x' ++ " cannot consume " ++ show y' ++ "fusion result: " ++ show res
        
      TVec (xx':xxs') -> case y' of
        TEmp -> (exSubs,x',mempty)
        TVec [] -> (exSubs,x',mempty)
        TVec (_:_) ->
          let
            (interSubs, interXX', interY') = consume exSubs xx' y'
            (finalSubs, finalXXs', finalY') = consume interSubs (TVec xxs') interY'
          in
            (finalSubs, interXX' <> finalXXs', finalY')
            
        -- The same way for all the other types. We recursively try the first of
        -- the first vector with all of the second. 
        _ -> 
          let
            (interSubs, interXX', interY') = consume exSubs xx' y'
            (finalSubs, finalXXs', finalY') = consume interSubs (TVec xxs') interY'
          in
            (finalSubs, interXX' <> finalXXs', finalY')

diffLoc :: T -> T -> Bool
diffLoc = \case
  x@(TCon _) -> \case
    TCon _ -> False
    TVar _ -> False -- Unsure about this
    TEmp -> True
    TVec [] -> True
    TVec y -> diffLoc x `all` y
    TLoc _ _ -> True
    _ -> False 
  x@(TVar _) -> \case
    TLoc _ _ -> True
    TEmp -> True
    TVec [] -> True
    TVec y -> diffLoc x `all` y
    _ -> False
  TVec [] -> const True
  TEmp -> const True
  xx@(TVec (x:xs)) -> \case
    y@(TVec yy) -> diffLoc x `all` yy && (diffLoc (TVec xs) y)
    y -> diffLoc y xx -- flip it
  TLoc l _ -> \case
    TLoc k _ -> if l == k then False else True
    _ -> True
  x@(_ :=> _) -> \y -> diffLoc y x
    
fuse :: T -> T -> ([TSubs],T)
fuse = \case
  x@(xi :=> xo) -> \case
    y@(yi :=> yo) ->
      let
        (subs, remainX, remainY) = consume [] yi xo
        res = (subs, normaliseT remainX, normaliseT remainY)
      in
        case res of
          (_,TEmp,_) -> (,) subs ((xi <> remainY) :=> yo)
          (_,_,TEmp) -> (,) subs (xi :=> (yo <> remainX))
          _ -> if snd $ aux diffLoc [remainX, remainY, xi, yi]
               then (,) subs ((xi <> remainY) :=> (yo <> remainX))
               else error $ "cannot fuse " ++ show x ++ " "  ++ show y ++ " result: " ++ show res
            where
              aux :: Monoid a => (a -> a -> Bool) -> [a] -> (a,Bool)
              aux f []  = (mempty,True)
              aux f (z:zs)  = (z, (f z (fst $ aux f zs)) && (snd $ aux f zs))
    y -> error $ "cannot fuse " ++ show x ++ " and " ++ show y
  x -> \y -> error $ "cannot fuse " ++ show x ++ " and " ++ show y
      
applyTSub :: [TSubs] -> T -> T
applyTSub = \case
  [] -> id
  xx@((xi,xo):xs) -> \case
     TEmp -> TEmp
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
