module DFA where

import qualified SemanticTypeDef as Sem
import CFGTypeDef
import CFG
import AddrTypeDef
import DFATypeDef
import qualified Control.Monad.State as St
import qualified Control.Monad.Reader as Re
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List


doDFA :: OptAST -> OptAST
doDFA ast = map doDFAEx ast

doDFAEx :: OptEx -> OptEx
doDFAEx (OptFunc a b cfg) =
    let defs = (Re.runReader (St.execStateT (dfaCheck (initialGen cfg)) M.empty) cfg) -- 到達可能定義集合
        ret = constFold cfg defs
    in (OptFunc a b ret)
doDFAEx x = x

dfaCheck :: [(Int,Def)] -> WithCFG [a]
dfaCheck [] = return []
dfaCheck (x:queue) = do
  dict <- St.get -- 今の辞書
  cfg <- Re.ask -- cfg全体
  let oldset = case (M.lookup (fst x,In) dict) of -- 今見てる行にすでにある定義
                 (Just s) -> s
                 (Nothing) -> S.empty
      newdef = (snd x) -- 今挿入したい定義
      nowline = (fst x) -- 今挿入したい行
      newbefore = (S.insert newdef oldset)
      newset = (kill (S.insert newdef oldset) nowline cfg) -- 今の定義集合に新しい定義を追加した上で、死ぬ定義を殺した定義集合
      nextline = findNext nowline cfg -- 次の行の集合 -- ここをprevにしたら逆向きの解析ができる
      nextdefs = S.toList (S.difference newset oldset) -- 差分をSetからListにする
  St.modify (M.insert (nowline,In) newbefore)
  St.modify (M.insert (nowline,Out) newset) -- 今の行と新しい定義集合とをつないだやつを、元のやつを削除した上で挿入
  dfaCheck (queue ++ (zip nextline nextdefs)) -- 新しく伝搬することが確定した定義集合と次の行の集合をzipしたやつをまたcheckにかける

initialGen :: [OptNode] -> [(Int,Def)]
initialGen cfg = (concat (map gen (concat (map (\x -> (inter x)) cfg)))) -- assign-stmtの各行に紐づけられた、各々の行での定義のリストを返す

gen :: OptIn -> [(Int,Def)]
gen i = case (stmt i) of
          (AddrAssign d _) -> [((line i),((Sem.name d),(line i)))] -- assign-stmtやったらその行に紐付けて、その行での定義を返す
          _ -> [] -- それ以外はいらない

kill :: Defs -> Int -> [OptNode] -> Defs
kill defs num cfg = case stmt(findLine num cfg) of
                      (AddrAssign d _) -> killStr defs (Sem.name d) num
                      _ -> defs

killStr :: Defs -> String -> Int -> Defs
killStr defs str num = S.filter (\x -> (((fst x) /= str) || ((snd x) == num))) defs -- 変数名が違うor今見てる行で生成された定義は生き残る


------------------------------------------------------------------------

constFold :: [OptNode] -> Dict -> [OptNode]
constFold cfg dict = (Re.runReader (St.execStateT (mapM constFoldEach (concat (map inter cfg))) cfg) dict)

constFoldEach :: OptIn -> WithConst [a]
constFoldEach internal = do
  mayChanged <- constAssignFold internal
  St.modify (updateCFG mayChanged)
  return []

updateCFG :: OptIn -> [OptNode] -> [OptNode]
updateCFG inline cfg =
    let theNode = (findNodeByLine (line inline) cfg)
        idx = (elemIndex theNode cfg)
    in case idx of
         (Just num) -> ((take num cfg)
                       ++ [(replaceInternal inline theNode)]
                       ++ (drop (num + 1) cfg))
         Nothing -> cfg

replaceInternal :: OptIn -> OptNode -> OptNode
replaceInternal inline node = (OptNode (name node) (prev node) ((filter (\x -> (line x < line inline)) (inter node))
                                                            ++ [inline]
                                                            ++ (filter (\x -> (line x > line inline)) (inter node)))(next node))

constAssignFold :: OptIn -> WithConst OptIn
constAssignFold opt =
  case (stmt opt) of
    (AddrAssign d1 right) ->
        do
          cfg <- St.get
          dict <- Re.ask
          case (constFoldRight cfg dict right (line opt)) of
            Nothing -> return opt
            (Just num) -> return (OptIn (AddrAssign d1 (AddrLit (fromIntegral num))) (line opt))
    _ -> return opt

lookupDict :: String -> Defs -> [Def]
lookupDict str defs = S.toList (S.filter (\x -> (fst x) == str) defs)

searchInterFormDecl :: [OptNode] -> Dict -> Int -> Sem.Decl -> [OptIn]
searchInterFormDecl cfg defs num d =
  let itsDef = lookupDict (Sem.name d) (defs M.! (num,In)) -- 現在行の到達可能定義集合
  in if (length itsDef == 1) -- その変数名の定義が一つしかなければ
      then let line =[findLine (snd $ (head itsDef)) cfg] -- その行を返す
           in line
      else [] -- そうでなければ空で返す


constFoldRight :: [OptNode] -> Dict -> AddrIn -> Int -> Maybe Int
constFoldRight cfg dict optinter num =
    case optinter of
      (AddrAssign d right) -> constFoldRight cfg dict right num
      (AddrVar d) ->
          let internal = searchInterFormDecl cfg dict num d
          in if(null internal)
             then Nothing
             else (constFoldRight cfg dict (stmt (head internal)) (line (head internal)))

      (AddrLit num) -> (Just (fromIntegral num))
      (AddrGT d1 d2) ->
        let inter1 = searchInterFormDecl cfg dict num d1
            inter2 = searchInterFormDecl cfg dict num d2
        in if(null inter1 || null inter2)
           then Nothing
           else
            let num1 = constFoldRight cfg dict (stmt (head inter1)) (line (head inter1))
                num2 = constFoldRight cfg dict (stmt (head inter2)) (line (head inter2))
            in case num1 of
                 Nothing -> Nothing
                 (Just n1) -> case num2 of
                                Nothing -> Nothing
                                (Just n2) -> if(n1 > n2)
                                             then (Just 1)
                                             else (Just 0)

      (AddrST d1 d2) ->
          let inter1 = searchInterFormDecl cfg dict num d1
              inter2 = searchInterFormDecl cfg dict num d2
          in if(null inter1 || null inter2)
             then Nothing
             else
                 let num1 = constFoldRight cfg dict (stmt (head inter1)) (line (head inter1))
                     num2 = constFoldRight cfg dict (stmt (head inter2)) (line (head inter2))
                 in case num1 of
                      Nothing -> Nothing
                      (Just n1) -> case num2 of
                                     Nothing -> Nothing
                                     (Just n2) -> if(n1 < n2)
                                                  then (Just 1)
                                                  else (Just 0)

      (AddrGE d1 d2) ->
          let inter1 = searchInterFormDecl cfg dict num d1
              inter2 = searchInterFormDecl cfg dict num d2
          in if(null inter1 || null inter2)
             then Nothing
             else
                 let num1 = constFoldRight cfg dict (stmt (head inter1)) (line (head inter1))
                     num2 = constFoldRight cfg dict (stmt (head inter2)) (line (head inter2))
                 in case num1 of
                      Nothing -> Nothing
                      (Just n1) -> case num2 of
                                     Nothing -> Nothing
                                     (Just n2) -> if(n1 >= n2)
                                                  then (Just 1)
                                                  else (Just 0)

      (AddrSE d1 d2) ->
          let inter1 = searchInterFormDecl cfg dict num d1
              inter2 = searchInterFormDecl cfg dict num d2
          in if(null inter1 || null inter2)
             then Nothing
             else
                 let num1 = constFoldRight cfg dict (stmt (head inter1)) (line (head inter1))
                     num2 = constFoldRight cfg dict (stmt (head inter2)) (line (head inter2))
                 in case num1 of
                      Nothing -> Nothing
                      (Just n1) -> case num2 of
                                     Nothing -> Nothing
                                     (Just n2) -> if(n1 <= n2)
                                                  then (Just 1)
                                                  else (Just 0)
      (AddrEQ d1 d2) ->
          let inter1 = searchInterFormDecl cfg dict num d1
              inter2 = searchInterFormDecl cfg dict num d2
          in if(null inter1 || null inter2)
             then Nothing
             else
                 let num1 = constFoldRight cfg dict (stmt (head inter1)) (line (head inter1))
                     num2 = constFoldRight cfg dict (stmt (head inter2)) (line (head inter2))
                 in case num1 of
                      Nothing -> Nothing
                      (Just n1) -> case num2 of
                                     Nothing -> Nothing
                                     (Just n2) -> if(n1 == n2)
                                                  then (Just 1)
                                                  else (Just 0)

      (AddrNE d1 d2) ->
          let inter1 = searchInterFormDecl cfg dict num d1
              inter2 = searchInterFormDecl cfg dict num d2
          in if(null inter1 || null inter2)
             then Nothing
             else
                 let num1 = constFoldRight cfg dict (stmt (head inter1)) (line (head inter1))
                     num2 = constFoldRight cfg dict (stmt (head inter2)) (line (head inter2))
                 in case num1 of
                      Nothing -> Nothing
                      (Just n1) -> case num2 of
                                     Nothing -> Nothing
                                     (Just n2) -> if(n1 /= n2)
                                                  then (Just 1)
                                                  else (Just 0)

      (AddrAdd d1 d2) ->
          let inter1 = searchInterFormDecl cfg dict num d1
              inter2 = searchInterFormDecl cfg dict num d2
          in if(null inter1 || null inter2)
             then Nothing
             else
                 let num1 = constFoldRight cfg dict (stmt (head inter1)) (line (head inter1))
                     num2 = constFoldRight cfg dict (stmt (head inter2)) (line (head inter2))
                 in case num1 of
                      Nothing -> Nothing
                      (Just n1) -> case num2 of
                                     Nothing -> Nothing
                                     (Just n2) -> (Just (fromIntegral (n1 + n2)))

      (AddrSub d1 d2) ->
          let inter1 = searchInterFormDecl cfg dict num d1
              inter2 = searchInterFormDecl cfg dict num d2
          in if(null inter1 || null inter2)
             then Nothing
             else
                 let num1 = constFoldRight cfg dict (stmt (head inter1)) (line (head inter1))
                     num2 = constFoldRight cfg dict (stmt (head inter2)) (line (head inter2))
                 in case num1 of
                      Nothing -> Nothing
                      (Just n1) -> case num2 of
                                     Nothing -> Nothing
                                     (Just n2) -> (Just (fromIntegral (n1 - n2)))

      (AddrMul d1 d2) ->
          let inter1 = searchInterFormDecl cfg dict num d1
              inter2 = searchInterFormDecl cfg dict num d2
          in if(null inter1 || null inter2)
             then Nothing
             else
                 let num1 = constFoldRight cfg dict (stmt (head inter1)) (line (head inter1))
                     num2 = constFoldRight cfg dict (stmt (head inter2)) (line (head inter2))
                 in case num1 of
                      Nothing -> Nothing
                      (Just n1) -> case num2 of
                                     Nothing -> Nothing
                                     (Just n2) -> (Just (fromIntegral (n1 * n2)))

      (AddrDiv d1 d2) ->
          let inter1 = searchInterFormDecl cfg dict num d1
              inter2 = searchInterFormDecl cfg dict num d2
          in if(null inter1 || null inter2)
             then Nothing
             else
                 let num1 = constFoldRight cfg dict (stmt (head inter1)) (line (head inter1))
                     num2 = constFoldRight cfg dict (stmt (head inter2)) (line (head inter2))
                 in case num1 of
                      Nothing -> Nothing
                      (Just n1) -> case num2 of
                                     Nothing -> Nothing
                                     (Just n2) -> (Just (fromIntegral (div n1 n2)))
      _ -> Nothing
