import PGF2

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad (forever)
import Control.Monad.State.Lazy
import System.IO(hFlush,stdout)

--------------------------------------------------------------------------------

type Quality = String
type Item    = String

type World = [(Item,[Quality])]

emptyWorld = []

updateWorld :: Item -> Quality -> StateT World IO ()
updateWorld item qual = do
  world <- get
  case lookup item world of
     Nothing -> modify ((item,[qual]) :)
     Just qs -> put $ add world item (qual:qs)

add :: World -> Item -> [Quality] -> World
add world item quals = insert (item,quals) rest
 where 
  rest = deleteBy fstEq (item,[]) world  :: World
  fstEq (a,_) (b,_) = a==b

--------------------------------------------------------------------------------

main = do 
  pgf  <- readPGF "Foods.pgf"
  eng  <- getConcr' pgf "FoodsEng"
  flip execStateT emptyWorld $ forever $ do puts "> " ; liftIO $ hFlush stdout
                                            checkWorld eng =<< liftIO getLine
                                            --printl =<< get


checkWorld lang sent = do
  let Right p = parse lang "Comment" sent
  let firstSent = fst $ head p
  let (item:qual:_) = delMod firstSent
  updateWorld item qual

  case findMod firstSent of
    (_:presup:_) -> do
       world <- get
       case lookup item world of
         Just qs -> if presup `elem` qs
                      then putln $ "you're right, " ++ item ++ " is indeed " ++ presup
                      else putln $ item ++ " is " ++ (intercalate ", " qs) ++ ", but not " ++ presup
         Nothing    -> putln "checkWorld: Nothing" 

    _            -> putln $ "ok, not supposing anything about " ++ item

--------------------------------------------------------------------------------
--copied from GF/src/runtime/haskell-bind/examples/pgf-shell.hs

getConcr' :: (Monad m) => PGF -> ConcName -> m Concr
getConcr' pgf lang =
    maybe (fail $ "Concrete syntax not found: "++show lang) return $
    Map.lookup lang (languages pgf)


printl xs = liftIO $ putl $ map show xs
putl ls = liftIO . putStr $ unlines ls
putln s = liftIO $ putStrLn s
puts s = liftIO $ putStr s

--------------------------------------------------------------------------------

delMod :: Expr -> [Fun]
delMod e = 
 case unApp e of      
        Nothing       -> []
        Just (cid,es) -> concatMap help es
 where 
  help e = case unApp e of
             Nothing       -> error "findHeads.help: Nothing"
             Just ("Mod",x:xs)  -> concatMap help xs
             Just (cid,[]) -> [cid]
             Just (cid,xs) -> [unwords (cid : concatMap help xs)]
           

findMod e =
  case unApp e of      
        Nothing       -> []
        Just (cid,es) -> concat $ concatMap help es
  where 
   help e = case unApp e of
              Nothing            -> error "findMod.help: Nothing" 
              Just ("Mod",x:xs)  -> concatMap modFound xs : [[show x]]
              Just (cid,[])      -> []
              Just (cid,xs)      -> concatMap help xs
   modFound e = case unApp e of
              Nothing            -> error "findMod.modFound: Nothing" 
              Just (cid,[])      -> [cid]
              Just (cid,xs)      -> []
