module Gen where 

import SemanticTypeDef
import AddrTypeDef
import GenTypeDef
    
generate :: AddrAST -> Mips
generate addr = concat (map genFunc addr) ++ [(MipsData (genData addr))] 

                
genFunc :: AddrEx -> [MipsEx]
genFunc (AddrFunDef dec _ i) = case (kind dec) of
                                 Proto -> []
                                 _ -> [(MipsFunc dec (concat $ map (genIn dec) i))]
genFunc _ = []

            
genData :: [AddrEx] -> [Decl]
genData [] = []
genData (x:xs) = case x of
                   (AddrVarDeclGlobal d) -> case d of
                                              (AddrDecl _ _ _ _) -> (d:(genData xs))
                                              _ -> (genData xs)
                   _ -> (genData xs)                   

genIn :: Decl -> AddrIn -> [MipsIn]
genIn _ (AddrVarDecl _) = []
genIn _ (AddrAssign d inter) = hoge ++ fuga ++ argWrite
    where fuga = case inter of
                   (AddrVar d1) -> -- 右辺が普通の変数で(a = bの形)
                       let (reg,_) = (genReg T0 d1)
                       in if((decfp d) == -1) -- 左辺がglobal変数やったら
                          then [(La T2 (name d)), -- 左辺の変数名でlaしてとってきたアドレスに
                                (Sw reg (Emit T2 0))] -- 書き込む
                          else [(Sw reg (Emit Fp (decfp d)))] -- それ以外やったら、fp+ofsに書き込む
                   _ -> case (decfp d) of -- それ以外で左辺が
                          -1 -> [(La T2 (name d)), -- globalはlaでアドレス取ってきて
                                 (Sw T0 (Emit T2 0))] -- そこにT0を入れる
                          _ -> [(Sw T0 (Emit Fp (decfp d)))] -- それ以外ならアドレスに書き込む
          argWrite = case (decfp d) of
                        0 -> [(Move A0 T0)]-- 1~4番目の引数やったら、Aの方のレジスタも書き換える
                        4 -> [(Move A1 T0)]
                        8 -> [(Move A2 T0)]
                        12 -> [(Move A3 T0)]
                        _ -> []
          hoge = case inter of
                 (AddrAdd d1 d2) ->
                     let (reg1,m1) = (genReg T0 d1)
                         (reg2,m2) = (genReg T1 d2)
                     in m1 ++ m2 ++
                          [(Add T0 reg1 reg2)] 
                 (AddrSub d1 d2) ->
                     let (reg1,m1) = (genReg T0 d1)
                         (reg2,m2) = (genReg T1 d2)
                     in m1 ++ m2 ++
                          [(Sub T0 reg1 reg2)] 
                 (AddrMul d1 d2) ->
                     let (reg1,m1) = (genReg T0 d1)
                         (reg2,m2) = (genReg T1 d2)
                     in m1 ++ m2 ++
                          [(Mul T0 reg1 reg2)] 
                 (AddrDiv d1 d2) ->
                     let (reg1,m1) = (genReg T0 d1)
                         (reg2,m2) = (genReg T1 d2)
                     in m1 ++ m2 ++
                          [(Div T0 reg1 reg2)] 
                 (AddrGT d1 d2) ->
                     let (reg1,m1) = (genReg T0 d1)
                         (reg2,m2) = (genReg T1 d2)
                     in m1 ++ m2 ++
                          [(Sgt T0 reg1 reg2)] 
                 (AddrST d1 d2) ->
                     let (reg1,m1) = (genReg T0 d1)
                         (reg2,m2) = (genReg T1 d2)
                     in m1 ++ m2 ++
                          [(Slt T0 reg1 reg2)] 
                 (AddrGE d1 d2) ->
                     let (reg1,m1) = (genReg T0 d1)
                         (reg2,m2) = (genReg T1 d2)
                     in m1 ++ m2 ++
                          [(Sge T0 reg1 reg2)] 
                 (AddrSE d1 d2) ->
                     let (reg1,m1) = (genReg T0 d1)
                         (reg2,m2) = (genReg T1 d2)
                     in m1 ++ m2 ++
                          [(Sle T0 reg1 reg2)] 
                 (AddrEQ d1 d2) ->
                     let (reg1,m1) = (genReg T0 d1)
                         (reg2,m2) = (genReg T1 d2)
                     in m1 ++ m2 ++
                          [(Seq T0 reg1 reg2)] 
                 (AddrNE d1 d2) ->
                     let (reg1,m1) = (genReg T0 d1)
                         (reg2,m2) = (genReg T1 d2)
                     in m1 ++ m2 ++
                          [(Sne T0 reg1 reg2)] 
                 (AddrLit i) -> [(Li T0 i)]                                
                 (AddrVar d1) -> -- 右辺がarrayかpointerやったらアドレスをとってくる -- pointerやったらすでに中身が入ってるはずやからarrayのみ……かな
                            case (t d1) of
                              (SArray _ _) -> -- 配列で
                                  if((decfp d1) == -1) -- globalなら
                                  then [(La T0 (name d1))] -- アドレスとってくる
                                  else [(Addiu T0 Fp (decfp d1))] -- fpとofsの和をT0に入れる
                              _ -> let (_,m1) = (genReg T0 d1) -- それ以外ならなんかのレジスタにd1の中身を入れる
                                   in m1 
                 (AddrAddr d1) -> if((decfp d1) == (-1)) -- globalなら
                                  then [(La T0 (name d1))] -- laする
                                  else [(Move T0 Fp), -- それ以外ならFp取ってきて
                                        (Addiu T0 T0 (decfp d1))] -- ofsの値を足す(=番地を返す)
-- それ以外がきたら落ちてもらう

genIn dec (AddrIf d lab1 lab2) =
    let l1 = (genIn dec lab1)
        l2 = (genIn dec lab2)
        (reg1, m1) = (genReg T0 d)
    in m1 ++
           [(Beqz reg1 (head l2)),
            (B (head l1))]
genIn _ (AddrLabel str) = [(Label str)]
genIn dec (AddrGoto l) =
    let lab = (genIn dec l)
    in [(B (head lab))]
genIn _  (AddrWrite d1 d2) =
    let (reg2,m2) = genReg T0 d2
    in m2 ++ [(Lw T1 (Emit Fp (decfp d1))), -- d1の中に入ってるアドレスをとってきて
              (Sw reg2 (Emit T1 0))] -- そのアドレスにreg2の中身を入れる
genIn _ (AddrRead d1 d2) =
     [(Lw T0 (Emit Fp (decfp d2))), -- d2の中に入ってる値(メモリ)を取り出す
      (Lw T1 (Emit T0 0)), -- そのメモリにアクセスして、中身を取り出す
      (Sw T1 (Emit Fp (decfp d1)))] -- 取り出した中身をd1のメモリに入れる
genIn dec (AddrVReturn) =
    [(Lw Fp (Emit Sp 0)), -- fpを退避したものに戻す
     (Lw Ra (Emit Sp 4)), -- raも退避したものに戻す
     (Addiu Sp Sp (-((decfp dec) - 4 * (fromIntegral $ length (fargs (t dec))) - 8))),-- 最初に下げた分spを戻す
     (Jr Ra)]
genIn dec (AddrReturn d) =
    let (reg,m) = (genReg V0 d)
    in if(reg == V0)
       then m ++ [(Lw Fp (Emit Sp 0)), -- fpを退避したものに戻す
                  (Lw Ra (Emit Sp 4)), -- raも退避したものに戻す
                  (Addiu Sp Sp (-((decfp dec) - 4 * (fromIntegral $ length (fargs (t dec))) - 8))),-- 最初に下げた分spを戻す
                  (Jr Ra)]
       else m ++ [(Lw Fp (Emit Sp 0)), -- fpを退避したものに戻す
                  (Lw Ra (Emit Sp 4)), -- raも退避したものに戻す
                  (Addiu Sp Sp (-((decfp dec) - 4 * (fromIntegral $ length (fargs (t dec))) - 8))),-- 最初に下げた分spを戻す
                  (Move V0 reg), -- 値をV0に入れ直す
                  (Jr Ra)]

genIn _ (AddrPrint d) =
    let (reg,m) = (genReg A0 d)
        m2 = if(reg == A0)
             then []
             else [(Move A0 reg)]
    in [(Move T3 A0)] ++
        m ++ m2 ++ 
       [(Li V0 1),
        (Syscall),
        (Li V0 4),
        (La A0 "newline"),
        (Syscall),
        (Move A0 T3)]

genIn _ (AddrCall d1 d2 args) =
    (genArgs args (fromIntegral $ length args) 0) ++ -- spの引数の個数*4bit向こうから順番に引数を入れていく
    [(Jal (name d2))] -- 関数呼び出し
    ++ (returnArgs (length args)) -- 上書きしたりした引数を戻す
    ++[(Sw V0 (Emit Fp (decfp d1)))] -- 戻り値を束縛
    
genIn _ (AddrVCall d args) =
    (genArgs args (fromIntegral $ length args) 0) ++ -- spの引数の個数*4bit向こうから順番に引数を入れていく
    [(Jal (name d))]
-- genIn _ _ = []

genArgs :: [Decl] -> Integer -> Int -> [MipsIn]
genArgs [] _  _= []
genArgs _ 0 _ = []
genArgs (x:xs) num hoge =
    let regist = case hoge of
                   0 -> [(Lw A0 (Emit Fp (decfp x)))]
                   1 -> [(Lw A1 (Emit Fp (decfp x)))]
                   2 -> [(Lw A2 (Emit Fp (decfp x)))]
                   3 -> [(Lw A3 (Emit Fp (decfp x)))]
                   _ -> []     
    in regist ++ [(Lw T0 (Emit Fp (decfp x))),
     (Sw T0 (Emit Sp (num * (-4))))]
    ++ (genArgs xs (num - 1) (hoge + 1))

genReg :: Reg -> Decl -> (Reg,[MipsIn])
genReg reg d = if ((decfp d) == -1) -- global変数なら
               then (reg,[(La T2 (name d)), -- laでアドレス引っ張ってきて
                          (Lw reg (Emit T2 0))]) -- 引っ張ってきたアドレスの中身をとってくる
               else case (decfp d) of
                      0 -> (A0,[]) -- 1~4番目の引数なら
                      4 -> (A1,[]) -- レジスタ渡ししているので、そこから取る
                      8 -> (A2,[])
                      12 -> (A3,[])
                      _ -> (reg,[(Lw reg (Emit Fp (decfp d)))]) -- どちらでもなければメモリから取ってくる

returnArgs :: Int -> [MipsIn]
returnArgs 0 = []
returnArgs num =
    let regist = case num of
                   1 -> [(Lw A0 (Emit Fp 0))]
                   2 -> [(Lw A1 (Emit Fp 4))]
                   3 -> [(Lw A2 (Emit Fp 8))]
                   4 -> [(Lw A3 (Emit Fp 12))]
                   _ -> []     
    in regist ++ (returnArgs (num - 1))

