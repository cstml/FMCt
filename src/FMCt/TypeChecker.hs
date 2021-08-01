{-#LANGUAGE TypeSynonymInstances#-}
{-#LANGUAGE FlexibleInstances#-}
module FMCt.TypeChecker
  ( TError(..)
  , typeCheck
  , typeCheckP
  , derive
  , fuse
  , consumes
  , consume
  , Derivation
  )
where

import FMCt.Parsing
import FMCt.Syntax
import Control.Exception
import Text.Read (readMaybe)

-- | Typechecking Errors.
data TError
  = ErrSimple    String          -- ^ A Simple Error.
  | ErrUndefT    String          -- ^ An undefined Type.
  | ErrMerge     String          -- ^ A merge Error.
  | ErrOverride  String          -- ^ Attempting to override declared variable.
  | ErrWrongT    String          -- ^ Attemptin to use the wrong types
  | ErrNotBinder String
  deriving Show

instance Exception TError

getErrMsg :: TError -> String
getErrMsg = \case
  ErrSimple    x -> x
  ErrUndefT    x -> x       -- ^ An undefined Type.
  ErrMerge     x -> x       -- ^ A merge Error.
  ErrOverride  x -> x       -- ^ Attempting to override declared variable.
  ErrWrongT    x -> x       -- ^ Attemptin to use the wrong types
  ErrNotBinder x -> x

type Context = [(Vv, T)]
type Judgement = (Context, Term, T)

data Derivation
  = Star        !Judgement
  | Variable    !Judgement
  | Abstraction !Judgement !Derivation
  | Application !Judgement !Derivation
  | Fusion      !Judgement !Derivation !Derivation

type Term = Tm

--------------------------------------------------------------------------------
-- TypeCheck Function

typeCheck :: Tm -> Either TError Derivation
typeCheck = derive

-- | Typecheck Print - useful for debugging.
typeCheckP :: Tm -> IO ()
typeCheckP t = do
  res  <- try $ evaluate $ (show . derive) t 
  either (\x -> (putStrLn . getErrMsg) (x :: TError)) putStrLn res   

--------------------------------------------------------------------------------
-- Aux Functions
getJudgement :: Derivation -> Judgement
getJudgement = \case
  Star j -> j
  Variable j -> j
  Application j _ -> j
  Abstraction j _ -> j
  Fusion j _ _ -> j

freshTypeVar :: [T]
freshTypeVar = TCon <$> [ mconcat $ [[x],[z],show y]
                        | y <- [1..] :: [Integer]
                        , x <- ['A'..'Z']
                        , z <- ['A'..'Z']
                        ]

freshVarTypes :: [T]
freshVarTypes =  TVec . (:[]) <$> freshTypeVar

splitStream :: [a] -> ([a],[a])
splitStream x = (,) l r
  where
    l = snd <$> (filter ( odd . fst ) $ zip ([1..] :: [Integer]) x)
    r = snd <$> (filter ( not . odd . fst ) $ zip ([1..] :: [Integer]) x)              

--------------------------------------------------------------------------------

fuseTypesD :: Derivation -> Derivation -> Either TError T
fuseTypesD dL dR = ty 
  where
    tL = (getJType . getJudgement) dL :: T 
    tR = (getJType . getJudgement) dR :: T 
    ty = fuse tL tR 

getJType :: Judgement -> T
getJType (_,_,x) = x

-- | Merge contexts
mergeCtx :: Context -> Context -> Context
mergeCtx ox oy = makeSet ox oy
  where
    makeSet [] x  = x
    makeSet (t:xs) [] = t : makeSet xs oy 
    makeSet t@((term,ty):xs) ((term',ty'):ys)
      = if term /= term' then makeSet t ys
        else if ty == ty' then makeSet xs oy
             else throw $ ErrOverride $ "Type Conflict between: "
                  ++ show term ++ ":" ++ show ty ++ " and "
                  ++ show term' ++ ":" ++ show ty'


-- | Second step is to add our known variables to the context
derive :: Term -> Either TError Derivation
derive p = derive' freshVarTypes (buildContext emptyCtx p) p
  where
    emptyCtx = [("*", TCon [] :=> TCon [])]
    
    -- | Get the context 
    gCtx :: Derivation -> Context
    gCtx = \case
      Star        (c,_,_)     -> c
      Variable    (c,_,_)     -> c
      Abstraction (c,_,_) _   -> c
      Application (c,_,_) _   -> c
      Fusion      (c,_,_) _ _ -> c

    -- | Get type of term from Context.
    getType :: Term -> Context -> Either TError T
    getType = \case
      t@(V b St) -> \case
        [] -> Left $ ErrUndefT $
              mconcat ["Cannot Find type for binder: ", show b
                      , " in context. Have you defined it prior to calling it ?"]
        ((b',ty):xs) ->
          if b == b'
          then Right ty
          else getType t xs
      St -> \_ -> Right $ mempty :=> mempty
      t  -> \_ -> Left . ErrNotBinder $
                  mconcat ["Attempting to get type of:", show t]

    -- | Get the type from a Derivation
    getUpperType :: Derivation -> T
    getUpperType = \case 
      Star        (_,_,t)     -> t 
      Variable    (_,_,t)     -> t 
      Abstraction (_,_,t) _   -> t
      Application (_,_,t) _   -> t
      Fusion      (_,_,t) _ _ -> t        
    
    derive' :: [T] -> Context -> Term -> Either TError Derivation
    derive' stream ctx term 
      = case term of  
          St ->
            case getType term ctx of
              Right ty -> Right $ Star (ctx, St, ty)
              Left e -> Left e 

          V _ St ->
            case (getType term ctx) of
              Right z -> Right $ Variable (ctx, term, ty)
                where
                  ty = mempty :=> TLoc Ho z
              Left e -> Left e 

          V x t  ->
            case derive' nStreamL ctx (V x St) of
              Right dLeft ->
                case derive' nStreamR ctx t of
                  Right dRight ->
                    case fuseTypesD dLeft dRight of
                      Right ty -> Right $ Fusion (ctx, term, ty) dLeft dRight
                      Left e   -> Left e
                  Left e -> Left e
              Left e -> Left e 
            where
              nStream             = tail stream
              (nStreamL,nStreamR) = splitStream nStream
              
          B x t lo St ->
            case derive' nStream ctx' (V x St) of
              Right nDeriv -> Right $ Abstraction (ctx', term, ty) nDeriv
              Left e       -> Left e 
            where
              t'      = normaliseT $ mempty :=>  TLoc Ho t
              ty      = TLoc lo t' :=> TCon []
              nStream = tail stream
              ctx'    = (x,t') : ctx
              
          B x t lo t' ->
            case derive' nStreamL ctx (B x t lo St) of
              Right dLeft ->
                case derive' nStreamR ctx' t' of
                  Right dRight ->
                    case fuseTypesD dLeft dRight of
                      Right ty -> Right $ Fusion (ctx', term, ty) dLeft dRight
                      Left e -> Left e
                  Left e -> Left e
              Left e -> Left e 
            where
              ctx'     = (x, t) : ctx
              nStream  = tail stream
              (nStreamL, nStreamR) = splitStream nStream

          P t lo St ->
            case derive' nStream ctx t of
              Right nDeriv -> Right $ Application (ctx, term, ty) nDeriv
                where
                  uty = getUpperType nDeriv
                  ty = TCon [] :=> TLoc lo uty
              Left e -> Left e 
            where
              nStream = tail stream

          P t lo t' ->
            case derive' nStreamL ctx (P t lo St) of
              Right dLeft ->
                case derive' nStreamR ctx t' of
                  Right dRight ->
                    case fuseTypesD dLeft dRight of
                      Right ty -> Right $ Fusion (nctx, term, ty) dLeft dRight
                        where
                          nctx =  mergeCtx (gCtx dLeft) (gCtx dRight)                  
                      Left e -> Left e
                  Left e -> Left e
              Left e -> Left e 
            where
              nStream = tail stream
              (nStreamL, nStreamR) = splitStream nStream


fuse :: T -> T -> Either TError T
fuse = \case
  TCon "" -> Right . normaliseT

  TVec [] -> Right . normaliseT
  
  x@(TCon _)  -> \case
    TCon ""          -> Right x
    y@(TCon _)       -> Right $ TVec [x,y]
    TVec []          -> Right $ TVec [x]
    TVec (y:ys)      -> case (fuse x y) of
                          Right z -> fuse z (TVec ys)
                          Left e -> Left e
    y@(TLoc _ _)     -> Right $ TVec [x,y]
    y@(_ :=> _)      -> case consume y (mempty :=> x) of
                          Right z -> Right $ normaliseT z
                          Left e -> Left e 
    
  xxx@(TVec xx@(x:_))  -> \case
    TCon ""          -> Right xxx
    y@(TCon _)       -> Right . TVec $ xx ++ [y]
    TVec []          -> Right xxx
    TVec (y:ys)      -> case fuse xxx y of
                          Right z -> fuse z (TVec ys) 
                          Left e -> Left e
    y@(TLoc _ _)     -> Right $ TVec [x,y]
    y@(_ :=> _)      -> case consume y (mempty :=> xxx) of
                          Right z -> (Right . normaliseT) z
                          Left e -> Left e 
    
  x@(TLoc l t) -> \case
    TCon ""          -> Right x
    y@(TCon _)       -> Right . TVec $ x : y : []
    TVec []          -> Right x
    TVec (y:ys)      -> case (fuse x y) of
                          Right z -> fuse z (TVec ys)
                          Left e -> Left e
    y@(TLoc l' t')   -> if l == l'
                        then case fuse t t' of
                               Right z -> Right $ TLoc l z
                               Left e -> Left e
                        else Right $ TVec [x,y]
    y@(_ :=> _)    -> normaliseT <$> consume y (mempty :=> x)

  x@(_ :=> _)  -> \case
    TCon ""          -> Right x
    y@(TCon _)       -> fuse (mempty :=> y) x
    TVec []          -> Right x
    TVec (y:ys)      -> case (fuse x y) of
                          Right z -> fuse z (TVec ys)
                          Left e -> Left e 
    y@(TLoc _ _)     -> fuse (mempty :=> y) x
    y@(_ :=> _)      -> normaliseT <$> consume x y

consumes :: T
          -- ^ Requires - What the term will consume
          -> T
          -- ^ Receives - what the term is getting 
          -> Either TError (T,T)
          -- ^ (Left from Requires, Left from Receives)
consumes = \case
  TCon "" -> Right . (,) mempty . id

  TVec [] -> Right . (,) mempty . id

  xx@(TCon x)  -> \case
    TCon ""     -> Right $ (,) xx mempty
    yy@(TCon y) -> if x == y
                   then Right $ (,) mempty mempty
                   else Left $ ErrMerge $
                          mconcat [ "cannot merge ", show xx , " " , show yy ]
    TVec []     -> Right $ (,) xx mempty
    TVec (y:ys) ->
      case consumes xx y of 
        Right (res1, left1) ->
          case consumes res1 (TVec ys) of
            Right (res2, left2) -> Right $ (,) res2 (left1 <> left2)
            Left e -> Left e
        Left e -> Left e 
    yy          -> Right (xx,yy)

  xxx@(TVec (x:xs)) -> \case
    TCon ""     -> Right $ (,) xxx mempty
    TVec []     -> Right $ (,) xxx mempty
    TVec (y:ys) ->
      case consumes xxx y of 
        Right (res1,left1) ->
          case consumes res1 (TVec ys) of
            Right (res2,left2) -> Right $ (,) res2 (left1 <> left2)
            e -> e
        e -> e 
            
    y ->
      case  consumes x y of
        Right (res1,left1) ->
          case consumes (TVec xs) left1 of
            Right (res2,left2) -> Right $ (,) (res1 <> res2) (left2)
            e -> e
        e -> e 

  x@(TLoc l t) -> \case
    TCon "" -> Right $ (,) x mempty
    TVec [] -> Right $ (,) x mempty
    y@(TLoc l' t') ->
      -- If the location are the same we need to check what happens with the
      -- interaction between the types.
      if l == l'
      then case  consumes t t' of
          Right (res, left) ->
            let               
              left' = if left == mempty || left == TVec []
                      then mempty
                      else TLoc l left
            in
              Right $ (,) res left'
          e -> e 

        -- Otherwise they don't interact 
      else Right $ (,) x y

    -- TLoc doesn't interact with any other type.
    y       -> Right $ (,) x y

  x@(_ :=> _) ->
    \y ->
      if x == y
      then Right $ (,) mempty mempty
      else Right $ (,) x y 

-- | Normalise gets rid of empty Types at locations.
normaliseT :: T -> T
normaliseT t
  | t == normalisedT = normalisedT
  | otherwise        = normaliseT normalisedT
  where
    
    normalisedT = normaliseT' t
    
    normaliseT' = \case
      TVec []           -> mempty
      TLoc _ (TVec [])  -> mempty
      TLoc _ (TCon "")  -> mempty
      TLoc l (TVec (x:xs)) -> TLoc l x <> (TLoc l $ TVec xs)
      TVec ([x])        -> normaliseT x
      t1 :=> t2         -> normaliseT t1 :=> normaliseT t2
      TVec (x:xs)       -> normaliseT x <> (normaliseT $ TVec xs)
      x -> x -- Just to be sure it gets through.

-- | Consume                                 
consume :: T -> T -> Either TError T
consume t1@(tIn :=> tOut) t2@(tIn' :=> tOut') = result  
  where
   intermediary :: Either TError (T,T)
   intermediary = tIn' `consumes` tOut

   result :: Either TError T
   result = case intermediary of
     Right (TCon "", TCon "") -> Right $ tIn :=> tOut'
     Right (TCon "", TVec []) -> Right $ tIn :=> tOut'
     Right (TCon "", x)       -> Right $ tIn :=> (x <> tOut')
     Right (TVec [], TCon "") -> Right $ tIn :=> tOut'
     Right (TVec [], TVec []) -> Right $ tIn :=> tOut'
     Right (TVec [], x)       -> Right $ tIn :=> (x <> tOut')
     Right (x       ,TVec []) -> Right $ (tIn<>x) :=> tOut'
     Right (x       ,TCon "") -> Right $ (tIn<>x) :=> tOut'
     Right _ -> Left $ ErrMerge $ mconcat $
                [ "error could not merge "
                , show t1
                , " with "
                , show t2
                , ". Resulting Type: "
                , show intermediary
                ]
consume _ _ = (throw . ErrWrongT) $ "Cannot consume types which are not of the form (* => *)"

data Operations = Add
                | Subtract
                | If
                 deriving (Eq, Ord)

instance Read Operations where
  readsPrec _ = \case
    "+"  -> return (Add, mempty)
    "-"  -> return (Subtract, mempty)
    "if" -> return (If,mempty)
    i    -> return (error "", i)

instance Show Operations where
  show = \case
    Add -> "+"
    Subtract -> "-"
    If -> "if"

-- | Pre parses the Term for primitives and adds their type to the context.
buildContext :: Context -> Term -> Context
buildContext eCtx =
  let
    opType :: Operations -> T
    opType = \case
      Add      -> TVec [TCon "Int", TCon "Int"] :=> TCon "Int"
      Subtract -> TVec [TCon "Int", TCon "Int"] :=> TCon "Int"
      If       -> error "Not yet implemented!"
  in \case
    V x St -> do 
      let int    = (readMaybe x) :: Maybe Int
      let bool   = (readMaybe x) :: Maybe Bool
      let op     = (readMaybe x) :: Maybe Operations
      let nCtx   = maybe [] (const [(x,TCon "Int")]) int
      let nCtx'  = maybe [] (const [(x,mempty :=> TCon "Bool")]) bool
      let nCtx'' = maybe [] ((:[]).((,) x) . opType ) op 
      foldr1 mergeCtx $ eCtx : nCtx : nCtx' : nCtx'' : []

    V x t' -> do
      let lCtx = buildContext eCtx (V x St)
      let rCtx = buildContext eCtx t'
      mergeCtx lCtx rCtx

    P t _ t' -> do
      let lCtx = buildContext eCtx t
      let rCtx = buildContext eCtx t'
      mergeCtx lCtx rCtx      

    B _ _ _ t -> buildContext eCtx t
      
    St -> eCtx
    
-- Show Instance
-- Inspired by previous CW.
instance Show Derivation where
  show d = unlines (reverse strs)
    where      
      (_, _, _, strs) = showD d
      showT :: T -> String
      showT = show 
        
      showC :: Context -> String
      showC = let sCtx (x,t) =  show x ++ ":" ++ showT t ++ ", " in \case
        [] -> []
        c  -> (flip (++) " ") . mconcat $ sCtx <$> c

      showJ :: Judgement -> String
      showJ (cx,n,t) = mconcat $ showC cx : "|- " : show n : " : " : showT t : []

      showL :: Int -> Int -> Int -> String
      showL l m r = mconcat $ replicate l ' ' : replicate m '-' : replicate r ' ' : []
      
      showD :: Derivation -> (Int,Int,Int,[String])
      showD (Star j) = (0,k,0,[s,showL 0 k 0]) where s = showJ j; k = length s
      showD (Variable j) = (0,k,0,[s,showL 0 k 0]) where s = showJ j; k = length s
      showD (Abstraction j d') = addrule (showJ j) (showD d')
      showD (Application j d') = addrule (showJ j) (showD d')
      showD (Fusion j d' e) = addrule (showJ j) (sidebyside (showD d') (showD e))

      addrule :: String -> (Int,Int,Int,[String]) -> (Int,Int,Int,[String])
      addrule x (l,m,r,xs)
        | k <= m
        = (ll,k,rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL  l m r : xs)

        | k <= l+m+r
        = ( ll , k , rr , (replicate ll ' ' ++ x ++ replicate rr ' ') : showL ll k rr : xs )

        | otherwise
        = ( 0 , k , 0 , x : replicate k '-' : [ replicate (-ll) ' ' ++ y ++ replicate (-rr) ' ' | y <- xs ] )
        where k = length x; i = div (m - k) 2; ll = l+i; rr = r+m-k-i

      extend :: Int -> [String] -> [String]
      extend i strs' = strs' ++ repeat (replicate i ' ')

      sidebyside :: (Int,Int,Int,[String]) -> (Int,Int,Int,[String]) -> (Int,Int,Int,[String])
        
      sidebyside (l1,m1,r1,d1) (l2,m2,r2,d2)
        | length d1 > length d2
        = ( l1, m1+r1+2+l2+m2, r2, [ x ++ "  " ++ y | (x,y) <- zip d1 (extend (l2+m2+r2) d2)])
        
        | otherwise
        = ( l1 , m1+r1+2+l2+m2 , r2 , [ x ++ " " ++ y | (x,y) <- zip (extend (l1+m1+r1) d1) d2])


