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
genIn _ (AddrAssign d inter) = hoge ++ fuga
    where fuga = case inter of
                   (AddrVar d1) ->
                       let (reg,_) = (genReg T0 d1)
                       in if((decfp d) == -1)
                          then [(La T2 (name d)),
                                (Sw reg (Emit T2 0))]
                          else [(Sw reg (Emit Fp (decfp d)))]
                   _ -> case (decfp d) of
                          4 -> [(Sw T0 (Emit Fp (decfp d))),
                                (Move A0 T0)]
                          8 -> [(Sw T0 (Emit Fp (decfp d))),
                                (Move A1 T0)]
                          12 -> [(Sw T0 (Emit Fp (decfp d))),
                                 (Move A2 T0)]
                          16 -> [(Sw T0 (Emit Fp (decfp d))),
                                 (Move A3 T0)]
                          -1 -> [(La T2 (name d)), -- globalはlaでアドレス取ってきて
                                 (Sw T0 (Emit T2 0))] -- そこにT2を入れる
                          _ -> [(Sw T0 (Emit Fp (decfp d)))]
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
                 (AddrVar d1) ->
                     let (_,m1) = (genReg T0 d1)
                     in m1 
                 (AddrAddr d1) -> if((decfp d1) == (-1)) -- globalなら
                                  then [(La T0 (name d1))] -- laする
                                  else [(Move T0 Fp), -- それ意外ならFp取ってきて
                                        (Addiu T0 T0 (decfp d1))] -- ofsの値を足して
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
    let (reg2,m2) = genReg T1 d2
        m1 = if((decfp d1) == (-1))
             then [(La T0 (name d1))]
             else [(Lw T0 (Emit Fp (decfp d1)))]
    in m1 ++ m2 ++ [(Sw T1 (Emit reg2 0))]
genIn _ (AddrRead d1 d2) =
    let hoge = if((decfp d2) > 0 && (decfp d2) <= 16)
               then [(La T0 (name d2))]
               else [(Lw T0 (Emit Fp (decfp d2)))]
    in hoge ++ [(Lw T1 (Emit T0 0)),
                (Sw T1 (Emit Fp (decfp d1)))]
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
                  (Move V0 reg),
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
    [(Jal (name d2)),
     (Sw V0 (Emit Fp (decfp d1))) -- 戻り値を束縛
    ]
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
                      4 -> (A0,[]) -- 1~4番目の引数なら
                      8 -> (A1,[]) -- レジスタ渡ししているので、そこから取る
                      12 -> (A2,[])
                      16 -> (A3,[])
                      _ -> (reg,[(Lw reg (Emit Fp (decfp d)))]) -- どちらでもなければメモリから取ってくる
