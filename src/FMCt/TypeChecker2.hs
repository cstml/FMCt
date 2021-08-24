{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module FMCt.TypeChecker2 where 
import FMCt.Syntax
import FMCt.Parsing
import FMCt.TypeChecker (
  Derivation(..),
  freshVarTypes,
  splitStream,
  TError(..),
  normaliseT,
  buildContext,
  Operations(..),
  )
import Control.Monad
import FMCt.Aux.Pretty (pShow,Pretty)
type Context = [(Vv, T)]

type Judgement = (Context, Term, T)

type Term = Tm

emptyCx :: Context
emptyCx = [("*",mempty :=> mempty)]

derive1 :: Term -> Derivation
derive1 term = derive1' freshVarTypes preBuildCtx term
  where
    preBuildCtx = either (const emptyCx) id $ buildContext emptyCx term
    
    derive1' :: [T] -> Context -> Term -> Derivation
    derive1' stream exCx = \case

      St -> Star (exCx, St, ty)
        where
          ty = either (error.show) id $ getType St exCx 
      
      xx@(V x St) -> Variable (exCx, xx, ty)
        where          
          ty = either (error.show) id $ getType xx exCx 
            
      xx@(V bi tm) -> applyTSubsD tCasts $ Fusion (mCx, xx, ty) derivL derivR
        where

          (lStr,rStr) = splitStream $ tail stream
          
          derivL = derive1' lStr exCx (V bi St)
          lCx = getContext derivL
          derivR = (derive1' rStr lCx tm)          
          rCx = getContext derivR
          mCx = makeSet $ lCx ++ rCx
          
          tL = getDerivationT derivL
          tR = getDerivationT derivR
          
          (tCasts,ty) = fuse tL tR

      xx@(B bi bTy lo St) -> Abstraction (aboveCx, xx, ty) deriv
        where
          ty = TLoc lo bTy :=> mempty
          nCx = (bi,bTy) : exCx
          deriv = derive1' (tail stream) nCx (V bi St)
          aboveCx = getContext deriv
            
      xx@(B bi bTy lo tm) -> applyTSubsD tCasts $ Fusion (mCx, xx, ty) derivL derivR
        where
          (lStr,rStr) = splitStream $ tail stream
          derivL = (derive1' lStr exCx (B bi bTy lo St))
          lCx = getContext derivL
          derivR = derive1' rStr lCx tm
          rCx = getContext derivR
          mCx = makeSet $ lCx ++ rCx
                    
          tL = getDerivationT derivL
          tR = getDerivationT derivR
          
          (tCasts,ty) = fuse tL tR

      xx@(P ptm lo St) -> Application (aboveCx, xx, ty) deriv 
        where
          ty = mempty :=> TLoc lo abvT
          deriv = derive1' (tail stream) exCx ptm
          abvT = getDerivationT  deriv
          aboveCx = getContext deriv

      xx@(P ptm lo tm) -> applyTSubsD tCasts $ Fusion (mCx, xx, ty) derivL derivR
        where

          (lStr,rStr) = splitStream $ tail stream

          derivL = derive1' lStr exCx (P ptm lo St)
          lCx = getContext derivL

          derivR = derive1' rStr lCx tm
          rCx = getContext derivR

          mCx = makeSet $ lCx ++ rCx
                    
          tL = getDerivationT derivL
          tR = getDerivationT derivR
          
          (tCasts,ty) = fuse tL tR

testD1 :: Term -> IO ()
testD1 = putStrLn . pShow . derive1

fresh :: T -> T -> (T,T)
fresh x y = (x',y')
  where
    
    x' = applyTSub subsX' x
    y' = applyTSub subsY' y
    
    (restFresh, subsX') = aux' freshVarTypes x

    (_, subsY') = aux' restFresh y
    
    aux' :: [T] -> T -> ([T],[TSubs])
    aux' stream = \case
      TEmp -> (stream,[])
      TCon _ -> (stream,[])
      TVec [] -> (stream,[])
      TVec (x:xs) -> do
        let (leftStream, subs1) = aux' stream x
        let (leftStream', subs2) = aux' leftStream (TVec xs)
        (leftStream', subs1 ++ subs2)
      TVar x -> (tail stream, [(TVar x, head stream)])
      TLoc l x -> do
        let (lStream, subs) = aux' stream x
        

type TSubs = (T,T)

consume :: [TSubs]       -- ^ Substitutions to be made in both types.
        -> T             -- ^ The consuming Type.
        -> T             -- ^ The consumed Type.
        -> ([TSubs],T,T) -- ^ The result containing: (new list of substitutions,
                         -- unconsumed types remaining from the consuming type,
                         -- unconsumed types remaining from the consumed type).
consume exSubs x y =
  let
    x' = normaliseT $ applyTSub  exSubs x -- we use the already subtituted form when consuming
    y' = normaliseT $ applyTSub  exSubs y -- for both terms
  in
    case x' of      
      TEmp -> case y' of
        _ ->  (exSubs,mempty,y') -- mempty doesn't change anything else 
      
      TVec [] -> (exSubs,mempty,y') -- synonym for mempty
      
      TCon _ -> case y' of         
        TEmp -> (exSubs,x',mempty)
        TVec [] -> (exSubs,x',mempty)
        TCon _ -> if x' == y'
                  then (exSubs, mempty, mempty)
                  else error $ "cannot consume! - type " ++ show y' ++ " should be " ++ show x'
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
        TEmp -> ((x',y'):exSubs,mempty,mempty)
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
          if normaliseT x' == normaliseT y'
             then (exSubs, mempty, mempty)
             else let
               (intSubs, leftIX', leftIY') =  consume exSubs ix' iy'
               (finalSubs, rightIX', rightIY') =  consume intSubs ox' oy'
               (finalL,finalR) = (normaliseT $ leftIX' <> leftIY', normaliseT $ rightIX' <> rightIY')
               res = (finalSubs, finalL, finalR)
               in case res of
                    (_,TEmp,TEmp) -> res
                    
                    _ -> error $ show x' ++ " cannot consume " ++ show y' ++ " fusion result: " ++ show res

diffLoc :: T -> T -> Bool
diffLoc = \case
  x@(TCon _) -> \case
    TCon _ -> False
    TVar _ -> False -- Unsure about this.
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
        (subs, remainY, remainX) = consume [] yi xo
        res                      = (subs, normaliseT remainX, normaliseT remainY)
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
    y@(TVar _) ->
      ([(y,x)],mempty)
    y -> error $ "cannot fuse " ++ show x ++ " and " ++ show y ++ ". Wrong type Types - Use Function Types"
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

getType :: Term -> Context -> Either TError T
getType = \case
    t@(V b St) -> \case
        [] -> Left $ ErrUndefT $
              mconcat [ "Cannot Find type for binder: ", show b
                      , " in context. Have you defined it prior to calling it ?" ]
        ((b', ty) : xs) -> if b == b' then pure ty else getType t xs
    St -> \_ -> pure $ mempty :=> mempty
    t -> \_ -> Left . ErrNotBinder $ mconcat ["Attempting to get type of:", show t]

getContext :: Derivation -> Context
getContext = \case
  Star (c,_,_) -> c
  Variable (c,_,_) -> c
  Abstraction (c,_,_) _ -> c
  Fusion (c,_,_) _ _ -> c
  Application (c,_,_) _ -> c

applyTSubsD :: [TSubs] -> Derivation -> Derivation
applyTSubsD subs = \case 
  Fusion (cx,tm,ty) dL dR -> Fusion ( (\(x,y)-> (x,applyTSub subs y)) <$> cx, nTm , applyTSub subs ty) dL dR
    where
      nTm = case tm of
        B v ty' l t' -> B v (applyTSub subs ty') l t'
        x -> x          
  _ -> error "You are not allowed to cast or substitute here! - this should never happen."
  
allCtx :: Derivation -> Context
allCtx = \case
  x@(Star _) -> getContext x
  x@(Variable _) -> getContext x
  x@(Application _ d) -> getContext x ++ allCtx d
  x@(Abstraction _ d) -> getContext x ++ allCtx d
  x@(Fusion _ dL dR) -> getContext x ++ allCtx dR ++ allCtx dL

makeSet :: Eq a => [a] -> [a]
makeSet = \case
  [] -> []
  (x:xs) -> if elem x xs then makeSet xs else x : makeSet xs

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
  x -> error $ "should't be reaching for location in term: " ++ show x ++ ".This should never happen."
