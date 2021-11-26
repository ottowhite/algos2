{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test where

import Control.DeepSeq
import Control.Exception
import System.Environment
import System.IO
import System.Timeout (timeout)
import Control.Monad
import Unsafe.Coerce
import Data.List (intercalate)
import System.Directory
import System.FilePath
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor (second, Bifunctor (first, bimap))
import Text.Printf (printf)

data Test input result = input :=> result
  deriving (Show, Read)

apply :: (i -> r) -> Test i r -> Test i r
apply f (i :=> r) = i :=> f i

-- type Question = Int

newtype Question = Question (String, Int)
    deriving(Eq, Ord)

unQuestion :: Question -> (String, Int)
unQuestion (Question x) = x

showQNum :: Question -> String
showQNum (Question (a,_)) =  a

type Result = (String, Question, Int, Int)
    -- ^ (Name, QNum, Score, Max)

resName  :: Result -> String
resName  (x,_,_,_) = x
resQNum  :: Result -> Question
resQNum  (_,x,_,_) = x
resScore :: Result -> Int
resScore (_,_,x,_) = x
resMax   :: Result -> Int
resMax   (_,_,_,x) = x


data LogLevel = Verbose | Silent
  deriving Eq

type TestM a = WriterT [Result] (ReaderT LogLevel IO) a

runTests :: TestM a -> IO a
runTests thing_inside = do
  args <- getArgs
  let lvl = case args of
              ("-v" : _) -> Verbose
              _ -> Silent
  (r, results) <- runReaderT (runWriterT thing_inside) lvl
  printResults lvl results
  return r

forceTest :: NFData b => Test a b -> IO (Test a (Either SomeException b))
forceTest t@(a :=> b) = timeout' (1 * 10^6) (let !_ = force b in return (a :=> Right b))  -- wait a second
  `catch` (\(e :: SomeException) -> return (a :=> Left e))
  where timeout' :: Int -> IO a -> IO a
        timeout' n thing_inside =
          timeout n thing_inside >>= \case
            Nothing -> error "timeout"
            Just a -> pure a

single :: String -> Int -> Int -> TestM ()
single name score possible
  = tell [(name, error "no qnum", score, possible)]

test
  :: (Show i, Show r, NFData r)
  => String -- ^ Name
  -> Question -- ^ Question Number
  -> (i -> r)
  -> (r -> r -> Bool)
  -> [Test i r]
  -> TestM ()
test name qnum f cmp fp = do
  lvl <- lift ask
  result <- lift $ lift $ doTest lvl name qnum f cmp fp
  tell [result]

testFromFile
  :: (Read i, Show i, Show r, Read r, NFData r)
  => String -- ^ Name
  -> Question -- ^ Question Number
  -> (i -> r)
  -> (r -> r -> Bool)
  -> FilePath
  -> TestM ()
testFromFile name qnum f cmp fp = do
  lvl <- lift ask
  result <- lift $ lift $ doTestFromFile lvl name qnum f cmp fp
  tell [result]

doTestFromFile
  :: (Read i, Show i, Show r, Read r, NFData r)
  => LogLevel
  -> String -- ^ test name
  -> Question -- ^ question number
  -> (i -> r)
  -> (r -> r -> Bool)
  -> FilePath
  -> IO Result
doTestFromFile lvl name qnum f cmp fp = do
  tests <- map read . lines <$> readFile fp
  outputs <- mapM (forceTest . apply f) tests
  result <- doTest lvl name qnum f cmp tests
  when (lvl == Verbose) $ do
    let write_log = do
        tmp <- getTemporaryDirectory
        createDirectoryIfMissing True (tmp ++ takeDirectory fp)
        let res_file = tmp ++ fp
        writeFile res_file (unlines $ map show outputs)
        hPutStrLn stderr ("Output written to: " ++ res_file)
    write_log
      `catch` (\(_ :: SomeException) -> return ())
  return result

doTest
  :: (Show i, Show r, NFData r)
  => LogLevel
  -> String -- ^ test name
  -> Question -- ^ question number
  -> (i -> r)
  -> (r -> r -> Bool)
  -> [Test i r]
  -> IO Result
doTest lvl name qnum f cmp tests = do
  outputs <- mapM (forceTest . apply f) tests
  let check _ (_ :=> Left _) = False
      check (_ :=> exp) (_ :=> Right act) = cmp exp act
      results = zipWith check tests outputs
      score = length (filter id results)
      possible = length tests
  when (lvl == Verbose) $
    forM_ (zip tests outputs) $ \(test, output) ->
      unless (check test output) $ do
        hPutStrLn stderr (replicate 80 '-')
        hPutStrLn stderr (grey ++ "Error in " ++ reset ++ name ++ grey ++ " for test case: " ++ show (input test) ++ reset)
        hPutStrLn stderr (grey ++ "Expected: " ++ green ++ show (result test) ++ grey ++ " Got: " ++ red ++ show (result output) ++ reset)
  return (name, qnum, score, possible)

printResults :: LogLevel -> [Result] -> IO ()
printResults lvl results = do
    when (lvl == Verbose) $ do
        hPutStrLn stderr (replicate 80 '-')
        hPutStrLn stderr "Test summary:\n "
        hPutStrLn stderr (prettyAutoScores rs)
    --   putStrLn (jsonTestResult results)
    putStrLn (jsonAutoScoreslabts rs)
    where
        rs = generateAutoScores results
--   putStrLn (prettyAutoScores $ generateAutoScores results)

result :: Test i r -> r
result (_ :=> r) = r

input :: Test i r -> i
input (i :=> _) = i

prettyTestResult :: [Result] -> String
prettyTestResult results = unlines (map showResult results)
  where
    tag_len (tag, _, _, _) = length tag
    max_tag_len = maximum (map tag_len results)
    width = max_tag_len + 20
    showResult (name, qnum, score, possible)
      = let colour = if score == possible then green else red
        in  concat [ name, replicate (width - length name) ' ', colour, show score, "/", show possible, reset]

red,green,grey,reset :: String
red = "\x1b[31m"
green = "\x1b[32m"
grey = "\x1b[37m"
reset = "\x1b[0m"

jsonTestResult :: [Result] -> String
jsonTestResult results = "[" ++ intercalate ", " (map showResult results) ++ "]"
  where
    showKV k v = show k ++ ": " ++ show v
    showResult (name, qnum, score, possible)
      = concat [ "{"
               , intercalate ", " [ showKV "name" name
                                  , showKV "score" score
                                  , showKV "possible" possible
                                  ]
               , "}"
               ]

type ResultsPerQ = [(Int, Double)]
type ResultsPerTest = Map Question ResultRow
type ResultRow = (Double, [(Result, Double, Double)])
    -- ReulstRow (QTotal, [(Result, Score, Possible)])
    {- Map: Key - Question Number
        Stores: (Double,  -- Total Marks for the question
            [(String,     -- Question Name
                Double)]  -- Marks per test set for that question    
    -}

jsonAutoScoreslabts :: [(Question, ResultRow)] -> String
jsonAutoScoreslabts results = "[" ++ intercalate ", " (map showResultRow results) ++ "]"
  where
    showKV :: String -> Double -> String
    showKV k v = show k ++ ": " ++ printMark v

    showResultRow :: (Question, ResultRow) -> String
    showResultRow (q, (_, rs)) = (intercalate ", " . map showResult) rs

    showResult :: (Result, Double, Double) -> String
    showResult ((name, Question (qnum, _), score, possible), mrk, scmax) = concat ["{", intercalate ", "
        [
            showKV "score" mrk,
            concat ["\"name\": \"Q", qnum, " ", name, "\""],
            showKV "possible" scmax
        ], "}"
        ]

generateAutoScores :: [Result] -> [(Question, ResultRow)]
generateAutoScores = Map.toList . weightScore . qstnScore . resByQstn
    where
    resByQstn :: [Result] -> Map Question [Result]
    resByQstn = Map.fromListWith (++) . map (\r@(_, q, _, _) -> (q, [r]))

    qstnScore :: Map Question [Result] -> Map Question (Int, Int, [Result])
    qstnScore = Map.map $ \rs ->
        let (a,b) = foldr (\r@(_,_,a,b) (x,y) -> (a+x,b+y)) (0,0) rs
        in  (a,b,rs)

    weightScore :: Map Question (Int, Int, [Result])
                -> Map Question (Double, [(Result, Double, Double)])
    weightScore = --Map.map scoreCalc
        Map.mapWithKey scoreCalc

    scoreCalc :: Question -> (Int, Int, [Result]) -> ResultRow
    scoreCalc q (tosc, max, rs) = (weight q tosc max,
        map (\r@(_,_,sc, qmx) -> (r, weight q sc max, weight q qmx max)) rs)

    weight :: Question -> Int -> Int -> Double
    weight q x y = fromIntegral (x * marksPerQ q) / fromIntegral y

marksPerQ :: Question -> Int
marksPerQ (Question (_,x)) = x

marksPerQD :: Question -> Double
marksPerQD = fromIntegral . marksPerQ

calcTotalScore :: [(Question, ResultRow)] -> (Double, Double)
calcTotalScore rs = (totalScore, totalMax)
    where
    totalScore = (sum . map (fst . snd)) rs
    totalMax = fromIntegral $ (sum . map (snd . unQuestion . fst)) rs

prettyAutoScores :: [(Question, ResultRow)] -> String
prettyAutoScores rs = concatMap (uncurry $ prettyResultRow max_tag_len) rs ++ summaryLine
    where
    tag_len :: (Result, Double, Double) -> Int
    tag_len ((tag, _, _, _), _, _) = length tag

    row_tag_len :: ResultRow -> Int
    row_tag_len (_, rs) = maximum (map tag_len rs)
    max_tag_len = maximum (map (row_tag_len . snd) rs)

    (totalScore, totalMax) = calcTotalScore rs
    -- totalScore = foldr (\((qnm, mrk), (n, _)) (ttl, m) -> (mrk + ttl, n + m)) (0,0) rs
    -- totalScore = bimap id id  $ map (second fst) rs
    summaryLine = concat ["\nTotal Score: ", scoreColour totalScore totalMax, printMark totalScore, "/", printMark totalMax, reset, "\n"]

scoreColour :: Eq a => a -> a -> String
scoreColour act exp = if act == exp then green else red

prettyResultRow :: Int -> Question -> ResultRow -> String
prettyResultRow max_tag_len qn@(Question (_,qtotal)) (total, rs) = unlines (summaryLine : map showResult rs)
    where
    summaryLine = let colour = scoreColour total (marksPerQD qn) in
        concat ["Question ", showQNum qn, ":  ", colour, printMark total, "/", show qtotal, reset]
    width = max_tag_len + 5

    showResult :: (Result, Double, Double) -> String
    showResult ((name, _, score, possible), sc, scmax)
        = let colour = scoreColour score possible
            in  concat [ " -", name, replicate (width - length name) ' ', colour, show score, "/", show possible, "  ", printMark sc, reset]

printMark :: Double -> String
printMark = printf "%.1f"
