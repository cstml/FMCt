{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE TupleSections #-}

module FMCt.TypeChecker2 
  (
    derive2,
    getTermType,
  ) where
import FMCt.Syntax
import FMCt.Parsing
import FMCt.TypeChecker (
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
           
type TSubs = (T,T)

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

type Result a = Either TError a

-- | Same as "derive1" but safe, and applies all substitutions at the end.
derive2 :: Term -> Result Derivation
derive2 term = do
  let (ppTerm,lTStream) = replaceInfer freshVarTypes term
  bCx           <- pBCx ppTerm                             -- pre build context
  result        <- derive2' lTStream bCx emptySb ppTerm    -- derive
  let derivation = snd result                              -- take final derivation
  let casts      = fst result                              -- take the final casts 
  return $ applyTSubsD casts derivation                    -- apply them to the derivation and return it 
  
  where
    emptySb = []

    -- | Pre builds the context by adding the constants and the binder types to the context.
    pBCx termR  =  do
      t1 <- buildContext emptyCx termR -- add constants
      let t2 = parseBinders termR      
      chkUnique $ t1 ++ t2

    -- | Replace the infer types with new fresh types so they do not overlap. 
    replaceInfer :: [T] -> Term -> (Term,[T]) -- ^ Return a Tuple formed out of the new pre-processed term and the stream left.
    replaceInfer stream t = case t of
      St      -> (St    , stream)
      V a n   -> (V a nN, rStr)
        where
          sStr = splitStream stream
          lStr = fst sStr
          rStr = snd sStr
          nN   = fst $ replaceInfer lStr n
      P p l n -> (P nP l nN, lStr)
        where
          sStr  = splitStream stream
          lStr  = snd sStr
          sStr' = splitStream . fst $ sStr
          str1  = fst sStr'
          str2  = snd sStr'
          nP    = fst $ replaceInfer str1 p
          nN    = fst $ replaceInfer str2 n

      B b ty l n -> (B b nT l nN, rStr)
        where
          sStr = splitStream stream
          lStr = fst sStr
          rStr = snd sStr
          nT = case ty of
            TVar "inferA" :=> TVar "inferB" -> head lStr
            _ -> ty
          nN = fst $ replaceInfer (tail lStr) n            
    
    chkUnique :: Context -> Result Context
    chkUnique x = if length x == length (nub $ fmap fst x)
                  then pure x
                  else  Left $ ErrOverride "Variable double bind."
                            
    parseBinders = \case
      St          -> []
      B bi t _ t' -> (bi,t) : parseBinders t'
      P t _ t'    -> parseBinders t ++ parseBinders t'
      V _ t'      -> parseBinders t'

    derive2' :: [T] -> Context -> [TSubs] -> Term -> Result ([TSubs],Derivation)
    derive2' stream exCx exSb = \case
      
      St -> do 
        let ty = TEmp :=> TEmp
        let pbC = exCx
        return $ (,) exSb (Star (pbC, St, ty))
      
      x@(V bi t') -> do
        uRes      <- derive2' (tail stream) exCx exSb t'
        let nDeriv = snd uRes
        let upSb   = fst uRes                
        let upCx   = applySubsC upSb exCx
        ty        <- getType (V bi St) upCx
        let upType = getDType nDeriv    
        fusion    <- ty `fuse` upType
        let cast   = fst fusion
        let rTy    = snd fusion
        let nSb    = upSb ++ cast
        let nCx    = applySubsC nSb upCx
        let rTy'   = applyTSub  nSb rTy                               
        return $ (,) nSb (Variable (nCx, x, rTy') nDeriv)          

          
      x@(B bi _ lo t') -> do
        uRes   <- derive2' (tail stream) exCx exSb t'
        let nDeriv = snd uRes
        let upSb   = fst uRes        
        let upCx   = applySubsC upSb exCx
        let upType = getDType nDeriv
        ty'       <- getType (V bi St) upCx
        let ty     = TLoc lo ty' :=> mempty
        nTy       <- snd <$> ty  `fuse` upType
        cast      <- fst <$> ty' `fuse` upType
        let nCx    = applySubsC cast upCx
        let nSb    = exSb ++ cast      
        return $ (,) nSb (Abstraction (nCx, x, nTy) nDeriv)
            
      xx@(P pTm lo sTm) -> do
        pRes       <- derive2' (tail stream) exCx exSb pTm
        let pDeriv  = snd pRes
        let pSb     = fst pRes
        sRes       <- derive2' (tail stream) exCx pSb sTm
        let sDeriv  = snd sRes
        let sSb     = fst sRes          
        let sTy     = getDType sDeriv
        let pTy     = getDType pDeriv            
        let npTy    = applyTSub sSb pTy                                
        let npTy'   = TEmp :=> TLoc lo npTy            
        nTy        <- snd <$> npTy' `fuse` sTy
        cast       <- fst <$> npTy' `fuse` sTy            
        let cSb     = sSb ++ cast
        let sCx     = applySubsC cSb exCx
        let nTy'    = applyTSub cSb nTy                    
        return $ (,) cSb (Application (sCx, xx, nTy') pDeriv sDeriv)
          
testD1 :: String -> IO ()
testD1 = putStrLn . pShow . derive1 . parseFMC

testD2 :: String -> IO ()
testD2 str = do
  term       <- return $ parseFMC str
  derivation <- return $ derive2 term
  either (putStrLn . show) (putStrLn) $ pShow <$> derivation

testD0 :: String -> IO ()
testD0 = putStrLn . pShow . derive0 . parseFMC

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
        TVar _ ->  ((y',mempty):exSubs,mempty,y')
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
                      , " in context. Have you defined it prior to calling it?" ]
        ((b', ty) : xs) -> if b == b' then pure ty else getType t xs
    St -> \_ -> pure $ mempty :=> mempty
    t -> \_ -> Left . ErrNotBinder $ mconcat ["Attempting to get type of:", show t]

getDType :: Derivation -> T
getDType = \case 
  Star        (_,_,t)     -> t
  Variable    (_,_,t) _   -> t
  Abstraction (_,_,t) _   -> t
  Application (_,_,t) _ _ -> t

setDType :: Derivation -> T -> Derivation
setDType d t = case d of 
  Star        (a,b,_)     -> Star (a,b,t)
  Variable    (a,b,_) c   -> Variable (a,b,t) c
  Abstraction (a,b,_) c   -> Abstraction (a,b,t) c
  Application (a,b,_) c e -> Application (a,b,t) c e

getContext :: Derivation -> Context
getContext = \case
  Star        (c,_,_)     -> c
  Variable    (c,_,_) _   -> c
  Abstraction (c,_,_) _   -> c
  Application (c,_,_) _ _ -> c

setContext :: Derivation -> Context -> Derivation
setContext = \case
  Star        (c,a,b)     -> \c' -> Star        (c',a,b)
  Variable    (c,a,b) n   -> \c' -> Variable    (c',a,b) n
  Abstraction (c,a,b) n   -> \c' -> Abstraction (c',a,b) n
  Application (c,a,b) u r -> \c' -> Application (c',a,b) u r

setContextR :: Derivation -> Context -> Derivation
setContextR = \case
  Star        (c,a,b)     -> \c' -> Star        (c',a,b)
  Variable    (c,a,b) n   -> \c' -> Variable    (c',a,b) (setContextR n c')
  Abstraction (c,a,b) n   -> \c' -> Abstraction (c',a,b) (setContextR n c')
  Application (c,a,b) u r -> \c' -> Application (c',a,b) (setContextR u c') (setContextR r c')

applyTSubsD :: [TSubs] -> Derivation -> Derivation
applyTSubsD subs = subCx subs . subTy subs
  where
    subCx :: [TSubs] -> Derivation -> Derivation
    subCx s d = do
      let cx = getContext d
      let nc = applySubsC s cx
      setContextR d nc
      
    subTy :: [TSubs] -> Derivation -> Derivation
    subTy s d = case d of 
      Star _                  -> d
      Variable    (a,b,t) n   -> Variable    (a,b, applyTSub s t) (subTy s n)
      Abstraction (a,b,t) n   -> Abstraction (a,b, applyTSub s t) (subTy s n)
      Application (a,b,t) p n -> Application (a,b, applyTSub s t) (subTy s p) (subTy s n)                                 

getTermType :: Term -> Result T
getTermType t = do
  deriv <- derive2 t
  return $ getDType deriv
      
applyDCxSubs :: [TSubs] -> Derivation -> Derivation
applyDCxSubs s d = res 
  where
    ctx    = getContext d
    newCtx = applySubsC s ctx
    res    = setContext d newCtx
  
applySubsC :: [TSubs] -> Context -> Context
applySubsC x y = (\(b,bt) -> (b, applyTSub x bt)) <$> y 

allCtx :: Derivation -> Context
allCtx x = case x of 
  Star _            -> getContext x
  Variable _ _      -> getContext x
  Application _ u r -> getContext x ++ allCtx u  ++ allCtx r
  Abstraction _ d   -> getContext x ++ allCtx d

getDerivationT :: Derivation -> T
getDerivationT = \case 
  Star (_,_,t)            -> t
  Variable (_,_,t)    _   -> t
  Application (_,_,t) _ _ -> t
  Abstraction (_,_,t) _   -> t

setDerivationT :: Derivation -> T -> Derivation
setDerivationT = \case 
  Star        (a,b,t)      -> \t' -> Star        (a,b,t') 
  Variable    (a,b,t) n    -> \t' -> Variable    (a,b,t') n
  Application (a,b,t) u r  -> \t' -> Application (a,b,t') u r 
  Abstraction (a,b,t) n    -> \t' -> Abstraction (a,b,t') n

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
