{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE TupleSections #-}
module FMCt.TypeChecker2 where 
import FMCt.Syntax
import FMCt.Parsing
import FMCt.TypeChecker (
--  Derivation(..),
  freshVarTypes,
  splitStream,
  TError(..),
  normaliseT,
  buildContext,
  Operations(..),
  )
import Control.Monad
import FMCt.Aux.Pretty (pShow,Pretty)
import Data.Set
import Control.Exception
import Control.Lens hiding (Context)
import Data.List (nub)

type Context = [(Vv, T)]

type Judgement = (Context, Term, T)

type Term = Tm

data Derivation
    = Star        !Judgement
    | Variable    !Judgement !Derivation
    | Abstraction !Judgement !Derivation
    | Application !Judgement !Derivation !Derivation
    deriving (Show, Eq)

emptyCx :: Context
emptyCx = [("*",mempty :=> mempty)]

normalForm :: T -> T
normalForm = \x -> case x of 
  TEmp -> TEmp
  TVar _ -> x
  TCon _ -> x
  TVec [] -> TEmp
  TVec (m:n:p) -> case m of
    TLoc l t -> case n of
      TLoc k t' -> if l < k then TLoc l (normalForm t) <> normalForm (TVec (n:p))
                  else TLoc k (normalForm t') <> normalForm (TVec (m:p))
      _ -> (normalForm n) <> normalForm (TVec (m:p))
    _ -> (normalForm m) <> normalForm (TVec (n:p))
  TVec [x'] -> normalForm x'
  TLoc l t -> TLoc l (normalForm t)
  m :=> n -> normalForm m :=> normalForm n

flat :: Either TError (Either TError a) -> Either TError a
flat = either Left id 
  
derive0 :: Term -> Derivation
derive0 term = derive0' freshVarTypes term
  where

    pBCx = either (const emptyCx) id $ buildContext emptyCx term
    
    exCx = []    
    derive0' :: [T] -> Term -> Derivation
    derive0' stream = \case
      
      St -> Star (pBCx, St, ty)
        where ty = TEmp :=> TEmp
      
      x@(V bi t') -> Variable (pBCx', x, ty') nDeriv
        where
          ty = normaliseT $ head stream
          ty' = either (const ty) id $ getType x pBCx
          pBCx' :: [(Vv,T)]
          pBCx' = toList $ fromList pBCx `union` singleton (bi,ty')
          nDeriv = derive0' (tail stream) t'

      x@(B bi bTy lo t') -> Abstraction (nCx, x, ty) nDeriv
        where
          ty = TLoc lo bTy :=> mempty
          nCx = [(bi,bTy)]
          nDeriv = derive0' (tail stream) t'
            
      xx@(P ptm lo t') -> Application (exCx, xx, ty) deriv nDeriv
        where
          ty = mempty :=> TLoc lo abvT
          deriv = derive0' (tail stream) ptm
          abvT = getDerivationT  deriv
          nDeriv = derive0' (tail stream) t'
  
derive1 :: Term -> Derivation
derive1 term = snd $ derive1' freshVarTypes pBCx emptySb term
  where
    emptySb = []
    pBCx1   = either (const emptyCx) id $ buildContext emptyCx term -- add constants
    pBCx2   = parseBinders term
    pBCx    = chkUnique $ pBCx1 ++ pBCx2
    chkUnique :: Context -> Context
    chkUnique x = if length x == length (nub $ fmap fst x) then x else error "Variable double bind."
                            
    parseBinders = \case
      St          -> []
      B bi t _ t' -> (bi,t) : parseBinders t'
      P t _ t'    -> parseBinders t ++ parseBinders t'
      V _ t'      -> parseBinders t'

    derive1' :: [T] -> Context -> [TSubs] -> Term -> ([TSubs],Derivation)
    derive1' stream exCx exSb = \case
      
      St -> (exSb,Star (pBCx, St, ty))
        where ty = TEmp :=> TEmp
      
      x@(V bi t') -> (,) nSb (Variable (nCx, x, rTy') nDeriv)
        where
          uRes   = derive1' (tail stream) exCx exSb t'
          nDeriv = snd $ uRes
          upSb   = fst $ uRes
          
          upCx   = applySubsC upSb exCx
          ty     = either (error.show) id $ getType (V bi St) upCx
          
          upType = getDType nDeriv

          fusion = ty `fuse` upType
          
          cast   = either (error.show) fst $ fusion
          rTy    = either (error.show) snd $ fusion

          nSb    = upSb ++ cast

          nCx    = applySubsC nSb upCx
          rTy'   = applyTSub  nSb rTy

          
      x@(B bi _ lo t') -> (,) nSb (Abstraction (nCx, x, nTy) nDeriv)
        where
          uRes   = derive1' (tail stream) exCx exSb t'
          nDeriv = snd uRes
          upSb   = fst uRes
          
          upCx   = applySubsC upSb exCx
          upType = getDType nDeriv
          
          ty'    = either (error.show) id $ getType (V bi St) upCx
          ty     = TLoc lo ty' :=> mempty
          
          nTy    = either (error.show) (snd) $ ty  `fuse` upType
          cast   = either (error.show) (fst) $ ty' `fuse` upType
          
          nCx    = applySubsC cast upCx
          nSb    = exSb ++ cast
            
      xx@(P pTm lo sTm) -> (,) cSb (Application (sCx, xx, nTy') pDeriv sDeriv)
        where
          pRes    = derive1' (tail stream) exCx exSb pTm
          pDeriv  = snd pRes
          pSb     = fst pRes

          sRes    = derive1' (tail stream) exCx pSb sTm
          sDeriv  = snd sRes
          sSb     = fst sRes
          
          sTy     = getDType sDeriv
          pTy     = getDType pDeriv

          npTy    = applyTSub sSb pTy          
          
          npTy'   = TEmp :=> TLoc lo npTy

          nTy     = either (error.show) snd $ npTy' `fuse` sTy
          cast    = either (error.show) fst $ npTy' `fuse` sTy

          cSb     = sSb ++ cast
          sCx     = applySubsC cSb exCx
          nTy'    = applyTSub cSb nTy
          

testD1 :: String -> IO ()
testD1 = putStrLn . pShow . derive1 . parseFMC

{-
unionC :: Context -> Context -> Context
unionC x y = applySubsC (mergeCx x y) x
  where
      mergeCx []     y = []
      mergeCx (x:xs) y = aux x y ++ mergeCx xs y

      aux x [] = []
      aux x@(bi,t) ((bi',t'):ys)
        | bi == bi'  = view _1 (merge [] t t')
        | otherwise  = aux x ys

{-
unionS :: Derivation -> Derivation
unionS x = case x of
  Fusion (cx,t,ty) dL dR -> Fusion (nCx,t,ty) dL'' dR''
    where
      dL' = unionS dL
      dR' = unionS dR
      cL = getContext dL'
      cR = getContext dR'
      nCx = cL `unionC` cR
      dL'' = setContextR dL' nCx
      dR'' = setContextR dR' nCx    
  y -> y
-}

testU :: String -> IO ()
testU x = do
  term  <- return $ parseFMC x
  deriv <- return $ derive0 term
  putStrLn . pShow $ deriv
--  putStrLn . pShow  $ unionS deriv


unionD :: Derivation -> Either TError Derivation
unionD = \case
  deriv@(Fusion (cx, x, ty) dL dR) -> fuseResult
    where
      dL' = unionD dL
      dR' = unionD dR
      fuseResult  = case (dL',dR') of
        (Left e, _ )   -> Left e
        (_,Left e)     -> Left e
        (Right ndL, Right ndR) -> nfuseResult
          where
            tL = getDType ndL
            tR = getDType ndR

            fusion = fuse tL tR
  
            nfuseResult = either Left aux $ fusion 
  
            aux :: ([TSubs],T) -> Either TError Derivation
            aux (s,t) = Right $ applyTSubsD s nDeriv 
              where
                nDeriv = Fusion(cx,x,t) dL dR
  Abstraction (cx,x,ty) n -> Abstraction (cx,x,ty) <$> unionD n
  Application (cx,x,ty) n -> Application (cx,x,ty) <$> unionD n
  x -> pure x

  
derive1 :: Term -> Either TError Derivation
derive1 term = derive1' freshVarTypes pBCx term
  where
    pBCx = either (const emptyCx) id $ buildContext emptyCx term
    
    derive1' :: [T] -> Context -> Term -> Either TError Derivation
    derive1' stream exCx = \case

      St -> result
        where
          ty =  getType St exCx
          result =  (Star . (exCx, St, )) <$> ty
      
      x@(V bi St) -> result
        where
          ty = either (const $ head stream) id $ getType x exCx
          nCx = toList . fromList $ (bi,ty) : exCx 
          result = pure $ Variable  (nCx, x, ty)
            
      x@(V bi tm) -> result
        where
          (lStr,rStr) = splitStream $ tail stream
          
          derivL = derive1' lStr exCx (V bi St)          
          lCx = getContext <$> derivL
          
          derivR = flat $ derive1' rStr <$> lCx <*> pure tm          

          rCx = getContext <$> derivR
          
          tL = getDerivationT <$> derivL
          tR = getDerivationT <$> derivR
          
          fusionR = flat $ pure fuse <*> tL <*> tR
          ty = snd <$> fusionR
          sub = fst <$> fusionR
          
          resultD = pure Fusion <*> ((,x,) <$> rCx <*> ty) <*> derivL <*> derivR
          result = applyTSubsD <$> sub <*> resultD

      x@(B bi bTy lo St) -> result
        where
          ty = TLoc lo bTy :=> mempty
          nCx = (bi,bTy) : exCx
          uD = Variable (nCx, V bi St, bTy)
          result = pure $ Abstraction (nCx, x,ty) uD

      x@(B bi bTy lo tm) -> result 
        where
          (lStr,rStr) = splitStream $ tail stream
          
          derivL = derive1' lStr exCx (B bi bTy lo St)          

          lCx = getContext <$> derivL
          
          derivR = flat $ derive1' rStr <$> lCx <*> pure tm          

          rCx = getContext <$> derivR
          
          tL = getDerivationT <$> derivL
          tR = getDerivationT <$> derivR
          
          fusionR = flat $ pure fuse <*> tL <*> tR
          ty = snd <$> fusionR
          sub = fst <$> fusionR
          
          resultD = pure Fusion <*> ((,x,) <$> rCx <*> ty) <*> derivL <*> derivR
          result = applyTSubsD <$> sub <*> resultD
          
      x@(P ptm lo St) -> result
        where
          aboveD = derive1' stream exCx ptm

          aboveT = getDerivationT <$> aboveD
          
          ty = (\z -> mempty :=> TLoc lo z) <$> aboveT
          
          aboveCx = getContext <$> aboveD

          result = pure Application <*> ((,x,) <$> aboveCx <*> ty) <*> aboveD
          
      x@(P ptm lo tm) -> result
        where
          (lStr,rStr) = splitStream $ tail stream
          
          derivL = derive1' lStr exCx (P ptm lo St)          

          lCx = getContext <$> derivL
          
          derivR = flat $ derive1' rStr <$> lCx <*> pure tm          

          rCx = getContext <$> derivR
          
          tL = getDerivationT <$> derivL
          tR = getDerivationT <$> derivR
          
          fusionR = flat $ pure fuse <*> tL <*> tR
          ty = snd <$> fusionR
          sub = fst <$> fusionR
          
          resultD = pure Fusion <*> ((,x,) <$> rCx <*> ty) <*> derivL <*> derivR
          result = applyTSubsD <$> sub <*> resultD
-}
testD0 :: String -> IO ()
testD0 = putStrLn . pShow . derive0 . parseFMC
{-
testD1 :: String -> IO ()
testD1 x = either (putStrLn . show) (putStrLn . pShow) <$> unionD $ derive0 $ parseFMC x

testD1' :: String -> IO ()
testD1' x = either (putStrLn . show) (putStrLn . pShow) <$>  derive1 $ parseFMC x
-}
           
type TSubs = (T,T)

merge :: [TSubs]         -- ^ Substitutions to be made in both types.
        -> T             -- ^ The consuming Type.
        -> T             -- ^ The merged Type.
        -> ([TSubs],T,T) -- ^ The result containing: (new list of substitutions,
                         -- unmerged types remaining from the consuming type,
                         -- unmerged types remaining from the merged type).
merge exSubs x y =
  let
    x' = normalForm . normaliseT . (applyTSub  exSubs) $ x -- we use the already subtituted form when consuming
    y' = normalForm . normaliseT . (applyTSub  exSubs) $ y -- for both terms
  in
    case x' of      
      TEmp -> case y' of
--        TVar _ ->  ((y',mempty):exSubs,mempty,y')
        _      ->  (exSubs,mempty,y') -- mempty doesn't change anything else        
      
      TVec [] -> merge exSubs TEmp y
      
      TCon _ -> case y' of         
        TEmp             -> (exSubs,x',mempty)
        TVec []          -> (exSubs,x',mempty)
        TCon _           -> if x' == y' then (exSubs, mempty, mempty) else (exSubs,x',y')
        t1 :=> t2        -> (exSubs,x',y')
        TVar _           -> ((y',x') : exSubs, mempty, mempty) 
        TLoc _ _         -> (exSubs,x',y')
        TVec (yy': yys') -> (finalSubs,finalX,remainY <> finalY)
          where
            (interSubs,interX,remainY) = merge exSubs x' yy'
            (finalSubs,finalX,finalY) = merge interSubs interX (TVec yys')         

      TVar _ -> case y' of
        TVar _           -> if x' == y' then (exSubs,mempty,mempty) else ((x',y'):exSubs,mempty,mempty)
        _                -> ((x',y'):exSubs, mempty, mempty)

      TLoc xl' xt' -> case y' of
        TEmp             -> (exSubs,x',mempty)
        TVec []          -> (exSubs,x',mempty)
        TCon _           -> (exSubs,x',y) -- home row and locations don't interact
        TVar _           -> (exSubs,x',y) -- home row variable and locations don't interact
        TVec (yy': yys') -> (finalSubs,finalX,remainY <> finalY)
          where
            (interSubs,interX,remainY) = merge exSubs x' yy'
            (finalSubs,finalX,finalY) = merge interSubs interX (TVec yys')
            
        TLoc yl' yt'     -> if xl' == yl' then (finalSubs, TLoc xl' finalX', TLoc yl' finalY')
                            else (exSubs,x',y')
                              where (finalSubs, finalX', finalY') = merge exSubs xt' yt'                                                      
        _ :=> _          -> (exSubs,x',y')
        
      TVec (xx':xxs') -> case y' of
        TEmp             -> (exSubs,x',mempty)
        TVec []          -> (exSubs,x',mempty)
        TVec (_:_)       -> (finalSubs, interXX' <> finalXXs', finalY')
                              where
                                (interSubs, interXX', interY')  = merge exSubs xx' y'
                                (finalSubs, finalXXs', finalY') = merge interSubs (TVec xxs') interY'            
        _                -> (finalSubs, interXX' <> finalXXs', finalY')
                              where
                                (interSubs, interXX', interY')  = merge exSubs xx' y'
                                (finalSubs, finalXXs', finalY') = merge interSubs (TVec xxs') interY'
                                
      ix' :=> ox' -> case y' of
        TEmp             -> (exSubs,x',mempty)
        TVec []          -> (exSubs,x',mempty)
        TCon _           -> (exSubs,x',y')
        TVar _           -> ((y',x'):exSubs,mempty,mempty)
        TLoc _ _         -> (exSubs,x',y')
        TVec (yy':yys')  -> (finalSubs, finalX', interYY' <> finalYY')
                              where
                                (interSubs, interX', interYY') = merge exSubs x' yy'
                                (finalSubs, finalX', finalYY') = merge interSubs interX' (TVec yys')
                                
        iy' :=> oy'      -> if x'' == y'' then (exSubs, mempty, mempty)
                            else if (finalSubs, finalL, finalR) == (finalSubs, TEmp, TEmp)
                                 then (finalSubs, mempty, mempty)
                                 else (exSubs, x'', y'')
                              where
                                x'' = normalForm x'
                                y'' = normalForm y'
                                (intSubs,   leftIX',  leftIY' ) = merge exSubs  ix' iy'
                                (finalSubs, rightIX', rightIY') = merge intSubs ox' oy'
                                finalL                          = normaliseT $ leftIX'  <> leftIY' 
                                finalR                          = normaliseT $ rightIX' <> rightIY'
                                
-- | Assess if two terms have no common unsaturated location
diffLoc :: T -> T -> Bool
diffLoc x y = (loc' x `intersection` loc' y) == empty
  where
    loc' = loc . normaliseT . normalForm
  
loc :: T -> Set Lo
loc = \case
  TEmp -> empty
  TVec [] -> empty
  TCon _ -> singleton Ho
  TVar _ -> singleton Ho
  _ :=> _ -> singleton Ho
  TVec (x:xs) -> loc x `union` loc (TVec xs)
  TLoc l _ -> singleton l

fuse :: T -> T -> Either TError ([TSubs],T)
fuse = \case
  x@(xi :=> xo) -> \case
    y@(yi :=> yo) ->
      let
          res = merge [] yi xo
      in
        case res of
          (subs,rY,TEmp) -> pure $ (,) subs ((xi <> rY) :=> yo)
          (subs,TEmp,rX) -> pure $ (,) subs (xi :=> (yo <> rX))
          (subs,rX,rY)   -> if diffLoc rX rY
                            then Right $ (,) subs ((xi <> rY) :=> (yo <> rX))
                            else Left . ErrFuse $ "cannot fuse " ++ show x ++ " "  ++ show y ++ " result: " ++ show res
    y@(TVar _) -> Right ([(y,x)],mempty)
    y          -> Left . ErrFuse $ "cannot fuse " ++ show x ++ " and " ++ show y ++ ". Wrong type Types - Use Function Types"
  x -> \y      -> Left . ErrFuse $ "cannot fuse " ++ show x ++ " and " ++ show y
      
applyTSub :: [TSubs] -> T -> T
applyTSub subs ty = normaliseT $ aux subs ty 
  where
    aux = \case
      [] -> id
      xx@((xi,xo):xs) -> \case
        TEmp -> TEmp
        y@(TCon _ ) -> y
        TLoc l t -> TLoc l (applyTSub xx t)
        TVec y -> TVec $ applyTSub xx <$> y
        yi :=> yo -> applyTSub xx yi :=> applyTSub xx yo
        y@(TVar _) -> if y == xi then applyTSub xs xo else applyTSub xs y         

getType :: Term -> Context -> Either TError T
getType = \case
    t@(V b St) -> \case
        [] -> Left $ ErrUndefT $
              mconcat [ "Cannot Find type for binder: ", show b
                      , " in context. Have you defined it prior to calling it ?" ]
        ((b', ty) : xs) -> if b == b' then pure ty else getType t xs
    St -> \_ -> pure $ mempty :=> mempty
    t -> \_ -> Left . ErrNotBinder $ mconcat ["Attempting to get type of:", show t]

getDType :: Derivation -> T
getDType = \case 
  Star  (_,_,t) -> t
  Variable  (_,_,t) _ -> t
  Abstraction  (_,_,t) _ -> t
--  Fusion  (_,_,t) _ _ -> t
  Application (_,_,t) _ _ -> t

getContext :: Derivation -> Context
getContext = \case
  Star (c,_,_)            -> c
  Variable (c,_,_) _      -> c
  Abstraction (c,_,_) _   -> c
--  Fusion (c,_,_) _ _      -> c
  Application (c,_,_) _ _ -> c

setContext :: Derivation -> Context -> Derivation
setContext = \case
  Star        (c,a,b)     -> \c' -> Star        (c',a,b)
  Variable    (c,a,b) n   -> \c' -> Variable    (c',a,b) n
  Abstraction (c,a,b) n   -> \c' -> Abstraction (c',a,b) n
  Application (c,a,b) u r -> \c' -> Application (c',a,b) u r
--  Fusion      (c,a,b) l r -> \c' -> Fusion      (c',a,b) l r

setContextR :: Derivation -> Context -> Derivation
setContextR = \case
  Star        (c,a,b)     -> \c' -> Star        (c',a,b)
  Variable    (c,a,b) n   -> \c' -> Variable    (c',a,b) (setContextR n c')
  Abstraction (c,a,b) n   -> \c' -> Abstraction (c',a,b) (setContextR n c')
  Application (c,a,b) u r -> \c' -> Application (c',a,b) (setContextR u c') (setContextR r c')
--  Fusion      (c,a,b) l r -> \c' -> Fusion      (c',a,b) (setContextR l c') (setContextR r c')


applyTSubsD :: [TSubs] -> Derivation -> Derivation
applyTSubsD subs = {-applyDTypeSubs subs .-}  applyDCxSubs subs

applyDCxSubs :: [TSubs] -> Derivation -> Derivation
applyDCxSubs s d = res 
  where
    ctx    = getContext d
    newCtx = applySubsC s ctx
    res    = setContext d newCtx
  
applySubsC :: [TSubs] -> Context -> Context
applySubsC x y = (\(b,bt) -> (b, applyTSub x bt)) <$> y 

{-
    applyDTypeSubs :: [TSubs] -> Derivation -> Derivation
    applyDTypeSubs s = \case
      d@(Star _) -> newD
        where
          oldT = getDerivationT d 
          newT = applyTSub s oldT
          newD = setDerivationT d newT

      d@(Variable _ _) -> newD
        where
          oldT = getDerivationT d 
          newT = applyTSub s oldT
          newD = setDerivationT d newT
          
      d@(Abstraction _ _) -> newD
        where
          oldT = getDerivationT d 
          newT = applyTSub s oldT
          newD = case setDerivationT d newT of
            Abstraction d' n -> Abstraction d' (applyDTypeSubs s n)
            _ -> error "This never happens"
          
      d@(Application _ _ _) -> newD
        where
          oldT = getDerivationT d 
          newT = applyTSub s oldT
          newD = case setDerivationT d newT of
            Application d' n -> Application d' (applyDTypeSubs s n) (applyDTypeSubs s n)
            _ -> error "This never happens"
-}          
allCtx :: Derivation -> Context
allCtx = \case
  x@(Star _)          -> getContext x
  x@(Variable _ _)    -> getContext x
  x@(Application _ u r) -> getContext x ++ allCtx u  ++ allCtx r
  x@(Abstraction _ d) -> getContext x ++ allCtx d
--  x@(Fusion _ dL dR)  -> getContext x ++ allCtx dR ++ allCtx dL

getDerivationT :: Derivation -> T
getDerivationT = \case 
  Star (_,_,t)            -> t
  Variable (_,_,t)    _   -> t
  Application (_,_,t) _ _ -> t
  Abstraction (_,_,t) _   -> t
--  Fusion (_,_,t) _ _      -> t

setDerivationT :: Derivation -> T -> Derivation
setDerivationT = \case 
  Star        (a,b,t)      -> \t' -> Star        (a,b,t') 
  Variable    (a,b,t) n    -> \t' -> Variable    (a,b,t') n
  Application (a,b,t) u r  -> \t' -> Application (a,b,t') u r 
  Abstraction (a,b,t) n    -> \t' -> Abstraction (a,b,t') n
--  Fusion      (a,b,t) l r  -> \t' -> Fusion      (a,b,t') l r 

getLocation :: Term -> Lo
getLocation = \case
  P _ l _ -> l
  B _ _ l _ -> l
  x -> error $ "should't be reaching for location in term: " ++ show x ++ ".This should never happen."


-- Show Instance
-- Inspired by previous CW.
instance Pretty Derivation where
    pShow d = unlines (reverse strs)
      where
        (_, _, _, strs) = showD d
        showT :: T -> String
        showT = pShow
        showC :: Context -> String
        showC =
            let sCtx (x, t) = show x ++ ":" ++ showT t ++ ", "
             in \case
                    [] -> []
                    c -> (flip (++) " ") . mconcat $ sCtx <$> c
        showJ :: Judgement -> String
        showJ (cx, n, t) = mconcat $ showC cx : "|- " : pShow n : " : " : showT t : []
        showL :: Int -> Int -> Int -> String
        showL l m r = mconcat $ replicate l ' ' : replicate m '-' : replicate r ' ' : []
        showD :: Derivation -> (Int, Int, Int, [String])
        showD (Star j) = (0, k, 0, [s, showL 0 k 0]) where s = showJ j; k = length s
        showD (Variable j d') =  addrule (showJ j) (showD d')
        showD (Abstraction j d') = addrule (showJ j) (showD d')
        showD (Application j d' e) = addrule (showJ j) (sidebyside (showD d') (showD e))
        addrule :: String -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
        addrule x (l, m, r, xs)
            | k <= m =
                (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL l m r : xs)
            | k <= l + m + r =
                (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL ll k rr : xs)
            | otherwise =
                (0, k, 0, x : replicate k '-' : [replicate (- ll) ' ' ++ y ++ replicate (- rr) ' ' | y <- xs])
          where
            k = length x; i = div (m - k) 2; ll = l + i; rr = r + m - k - i
        extend :: Int -> [String] -> [String]
        extend i strs' = strs' ++ repeat (replicate i ' ')
        sidebyside :: (Int, Int, Int, [String]) -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
        sidebyside (l1, m1, r1, d1) (l2, m2, r2, d2)
            | length d1 > length d2 =
                (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ "  " ++ y | (x, y) <- zip d1 (extend (l2 + m2 + r2) d2)])
            | otherwise =
                (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ " " ++ y | (x, y) <- zip (extend (l1 + m1 + r1) d1) d2])


pShow' :: Derivation -> String
pShow' d = unlines (reverse strs)
  where
    (_, _, _, strs) = showD d
    showT :: T -> String
    showT = pShow
    showJ :: Judgement -> String
    showJ (cx, n, t) = mconcat $ "Γ " : "|- " : show n : " : " : showT t : []
    showL :: Int -> Int -> Int -> String
    showL l m r = mconcat $ replicate l ' ' : replicate m '-' : replicate r ' ' : []
    showD :: Derivation -> (Int, Int, Int, [String])
    showD (Star j) = (0, k, 0, [s, showL 0 k 0]) where s = showJ j; k = length s
    showD (Variable j d') =  addrule (showJ j) (showD d')
    showD (Abstraction j d') = addrule (showJ j) (showD d')
    showD (Application j d' e) = addrule (showJ j) (sidebyside (showD d') (showD e))
--        showD (Fusion j d' e) = addrule (showJ j) (sidebyside (showD d') (showD e))
    addrule :: String -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
    addrule x (l, m, r, xs)
        | k <= m =
            (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL l m r : xs)
        | k <= l + m + r =
            (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL ll k rr : xs)
        | otherwise =
            (0, k, 0, x : replicate k '-' : [replicate (- ll) ' ' ++ y ++ replicate (- rr) ' ' | y <- xs])
      where
        k = length x; i = div (m - k) 2; ll = l + i; rr = r + m - k - i
    extend :: Int -> [String] -> [String]
    extend i strs' = strs' ++ repeat (replicate i ' ')
    sidebyside :: (Int, Int, Int, [String]) -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
    sidebyside (l1, m1, r1, d1) (l2, m2, r2, d2)
        | length d1 > length d2 =
            (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ "  " ++ y | (x, y) <- zip d1 (extend (l2 + m2 + r2) d2)])
        | otherwise =
            (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ " " ++ y | (x, y) <- zip (extend (l1 + m1 + r1) d1) d2])
