module CFG where

import AddrTypeDef
import CFGTypeDef

makeCFG :: AddrAST -> OptAST
makeCFG ast = (map makeCFGEx ast)

makeCFGEx :: AddrEx -> OptEx
makeCFGEx (ex@(AddrVarDeclGlobal _)) = (OptVar ex)
makeCFGEx (AddrFunDef d a is) =
    let before = setLineNum is 0
        first = (makeCFGIn [] [] before)
        second = if (null first)
                 then []
                 else ((makeNodePrevSingle (head first) [Begin] first):(makeNodePrevIn first (tail first)))
        comp = rmNeedless second
    in (OptFunc d a comp)

setLineNum :: [AddrIn] -> Int -> [OptIn]
setLineNum [] _ = []
setLineNum (x:xs) n = (OptIn x n) : (setLineNum xs (n+1))

makeCFGIn :: [AddrIn] -> [OptIn] -> [OptIn] -> [OptNode]
makeCFGIn _ _ [] = []
makeCFGIn ls is ((opt@(OptIn x _)):xs) = case x of
                           (AddrLabel _) -> if (null is)
                                                  then (makeCFGIn (ls++[x]) is xs)
                                                  else (makeNode ls is x) : (makeCFGIn [x] [] xs)
                           (AddrIf _ _ _) -> (makeNode ls (is ++ [opt]) x) : (makeCFGIn [] [] xs)
                           (AddrGoto _) -> (makeNode ls (is ++ [opt]) x) : (makeCFGIn [] [] xs)
                           (AddrReturn _) -> (makeNode ls (is ++ [opt]) x) : (makeCFGIn [] [] xs)
                           (AddrVReturn) -> (makeNode ls (is ++ [opt]) x) : (makeCFGIn [] [] xs)
                           _ -> makeCFGIn ls (is ++ [opt]) xs


makeNode :: [AddrIn] -> [OptIn] -> AddrIn -> OptNode -- 自分の名前になるラベルのリストと、中身の文のリストと、次のノードを示す文を受け取って、
makeNode ls is togo = (OptNode nodename [] is nextnodes) -- Nodeにして返す
                       where nodename = if (null ls)
                                        then [None] -- 空ならnone
                                        else (makeNodeName ls) -- 名前になるラベルのリストから名前を作ってもらう
                             nextnodes = case togo of -- 次のノードを示す文が
                                           (AddrIf _ (AddrLabel str1) (AddrLabel str2)) -> [(Label str1),(Label str2)] -- ifならtlabelとelabelの示すNode
                                           (AddrGoto (AddrLabel str)) -> [(Label str)]  -- gotoならlabelの示すNode
                                           (AddrReturn _) -> [End] -- returnならend
                                           (AddrVReturn) -> [End] -- returnならend
                                           (AddrLabel str) -> [(Label str)] -- labelなら、そのラベル
                                                                              -- それ以外なら落ちてもらって構わない


makeNodeName :: [AddrIn] -> [NodeName] -- ラベルのリストを受け取って、NodeNameのリストにして返す
makeNodeName [] = []
makeNodeName ((AddrLabel str):xs) = (Label str) : makeNodeName xs

makeNodePrevIn ::[OptNode] -> [OptNode] -> [OptNode]
makeNodePrevIn _ [] = []
makeNodePrevIn every (x:xs) = now : (makeNodePrevIn every xs) -- 関数内の全Nodeのリストと、今見てるノードを使って
                          where now = if((name x) == [None]) -- 名前がNoneなら
                                      then x -- 何もせず、
                                      else (makeNodePrevSingle x [] every) -- それ以外なら、その名前をnextに持つノードを探してprevに入れて返す

makeNodePrevSingle :: OptNode -> [NodeName] -> [OptNode] -> OptNode -- 今見てるNodeと、prevになることが確定したノードの名前と、prevになるかまだ確認できてないノードのリストを受け取って、
makeNodePrevSingle node prevs [] = (OptNode (name node) prevs (inter node) (next node))  -- 確認できてないノードがなくなれば、確定してるprevを入れて返す
makeNodePrevSingle node prevs (x:xs) = -- まだ残ってるなら
    let mem = map (\y -> elem y (next x)) (name node) -- nodeの名前の一部でもnextの一部に含まれているなら
    in if(foldl (||) False mem)
       then makeNodePrevSingle node (prevs ++ (name x)) xs -- そのノードをprevに入れる
       else makeNodePrevSingle node prevs xs -- そうでないなら入れないで先に進む


rmNeedless :: [OptNode] -> [OptNode] -- prevがない=入ってこれないNodeを削除する
rmNeedless [] = []
rmNeedless (x:xs) = if(prev x == [])
                    then rmNeedless xs
                    else (x : rmNeedless xs)


-----------------------------------
-----以下解析用の便利関数------------
-----------------------------------

-- 最終的に、ある行が与えられた時に、その前後の行が欲しい


-- Nodeを与えたら、最初の行と最後の行の番号を取ってくる

findFirst :: OptNode -> Int
findFirst node = fst $ findEach node


findLast :: OptNode -> Int
findLast node = snd $ findEach node


findEach :: OptNode -> (Int,Int)
findEach node = ((line (head (inter node))) , (line (last (inter node))))

-- nodeNameとCFGを受け取ってNodeを返す
findNodeByName :: [OptNode] -> NodeName -> OptNode
findNodeByName (x:xs) n = if (elem n (name x))
                   then x
                   else findNodeByName xs n

-- 行番号投げたら、その行の入ってるNodeを取ってくる
-- 設計上、最初の行と最後の行の間にあったらそのNodeの中にいるはず
findNodeByLine :: Int -> [OptNode] -> OptNode
findNodeByLine num (x:xs) = if(((findFirst x) <= num) && ((findLast x) >= num))
                      then x
                      else findNodeByLine num xs

-- 行番号投げたらその前の行の番号を取ってくる
findPrev :: Int -> [OptNode] -> [Int]
findPrev n cfg
    | n == (findFirst x) = map (findLast . (findNodeByName cfg)) (filter (\a -> (a /= Begin)) (prev x)) -- そのノードの先頭なら、prevに入ってるBegin以外のノードの最後の行を集めてくる
    | otherwise = [(n-1)] -- それ以外なら前の行
    where
      x = findNodeByLine n cfg -- その行を含むノード


-- 行番号投げたらその後ろの行の番号を取ってくる
findNext :: Int -> [OptNode] -> [Int]
findNext n cfg
    | n == (findLast x) = map (findFirst . (findNodeByName cfg)) (filter (\a -> (a /= End)) (next x)) -- そのノードの末尾なら、nextに入ってるEnd以外のノードの最初の行を集めてくる
    | otherwise = [(n+1)] -- それ以外なら次の行
    where
      x = findNodeByLine n cfg -- その行を含むノード

-- 行番号投げたらその行の中身(OptIn)を取ってくる
findLine :: Int -> [OptNode] -> OptIn
findLine num cfg = findLineInNode num (inter (findNodeByLine num cfg))

findLineInNode :: Int -> [OptIn] -> OptIn
findLineInNode num (x:xs) = if(line x == num)
                            then x
                            else findLineInNode num xs
