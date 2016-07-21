module GenTypeDef where

import Data.List
import SemanticTypeDef   

type Mips = [MipsEx]

data MipsEx = MipsCom String
            | MipsFunc Decl [MipsIn]
            | MipsData [Decl]
              deriving (Eq,Show)

data MipsIn = Label {labelName :: String}
            | Add Reg Reg Reg
            | Addiu Reg Reg Integer
            | Sub Reg Reg Reg
            | Mul Reg Reg Reg
            | Div Reg Reg Reg
            | Sgt Reg Reg Reg
            | Slt Reg Reg Reg
            | Sge Reg Reg Reg
            | Sle Reg Reg Reg              
            | Seq Reg Reg Reg
            | Sne Reg Reg Reg
            | B MipsIn -- Blanch系のInは全部Label
            | Beqz Reg MipsIn 
            | Li Reg Integer
            | Lw Reg Emit -- lw,swのIntegerはaddressだから、n($fp)となる(はず)
            | Sw Reg Emit
            | Move Reg Reg
            | La Reg String
            | Jr Reg
            | Jal String
            | Syscall
              deriving (Eq,Show)

data Emit = Emit Reg Integer
          deriving (Eq,Show)

data Reg = A0 | A1 | A2 | A3
         | T0 | T1 | T2 | T3 | T4
         | V0 | Fp | Sp | Ra
           deriving (Eq,Show)

header :: Mips
header = [(MipsCom "text"),(MipsCom "globl main")]    

showData :: Decl -> String
showData d = case (t d) of
               (SArray _ num) -> (name d) ++ ": .space " ++ (show (num * 4))
               _ -> (name d) ++ ": .word 0"

funcHead :: Decl -> [MipsIn]
funcHead d = [(Addiu Sp Sp ((decfp d) - ((fromIntegral $ length (fargs (t d)))*4) - 8)),-- 最初にspを下げる
               (Sw Fp (Emit Sp 0)), -- fpを保存
               (Sw Ra (Emit Sp 4)),  -- raを保存
              (Addiu Fp Sp (-(decfp d) + 8))] -- SpからFpとRaの退避先と、ローカル変数分上がったところがFp

funcFoot :: Decl -> [MipsIn]
funcFoot d = [(Lw Fp (Emit Sp 0)), -- fpを退避したものに戻す
              (Lw Ra (Emit Sp 4)), -- raも退避したものに戻す
              (Addiu Sp Sp (-((decfp d) - 4 * (fromIntegral $ length (fargs (t d))) - 8))),-- 最初に下げた分spを戻す
              (Jr Ra) -- 帰る
             ]

showMips :: Mips -> String
showMips m = intercalate "\n" (map showEx (header ++ m))

showEx :: MipsEx -> String
showEx (MipsCom str) = "." ++ str
showEx (MipsFunc d inter) = (name d) ++ ":\n\t" ++ intercalate "\n\t" (map showMipsIn ((funcHead d) ++  inter　++ (funcFoot d)))
showEx (MipsData d) = ".data\n\t" ++ "newline: .asciiz \"\\n\"\n\t"++ (intercalate "\n\t" (map showData d)) 

showMipsIn ::  MipsIn -> String
showMipsIn (Label str) = str ++ ":"
showMipsIn (Add r1 r2 r3) = "add " ++ showReg r1 ++ "," ++ showReg r2  ++ "," ++ showReg r3
showMipsIn (Addiu r1 r2 num) = "addiu " ++ showReg r1 ++ "," ++ showReg r2  ++ "," ++ show num
showMipsIn (Sub r1 r2 r3) = "sub " ++ showReg r1 ++ "," ++ showReg r2  ++ "," ++ showReg r3
showMipsIn (Mul r1 r2 r3) = "mul " ++ showReg r1 ++ "," ++ showReg r2  ++ "," ++ showReg r3
showMipsIn (Div r1 r2 r3) = "div " ++ showReg r1 ++ "," ++ showReg r2  ++ "," ++ showReg r3
showMipsIn (Sgt r1 r2 r3) = "sgt " ++ showReg r1 ++ "," ++ showReg r2  ++ "," ++ showReg r3
showMipsIn (Slt r1 r2 r3) = "slt " ++ showReg r1 ++ "," ++ showReg r2  ++ "," ++ showReg r3
showMipsIn (Sge r1 r2 r3) = "sge " ++ showReg r1 ++ "," ++ showReg r2  ++ "," ++ showReg r3
showMipsIn (Sle r1 r2 r3) = "sle " ++ showReg r1 ++ "," ++ showReg r2  ++ "," ++ showReg r3
showMipsIn (Seq r1 r2 r3) = "seq " ++ showReg r1 ++ "," ++ showReg r2  ++ "," ++ showReg r3
showMipsIn (Sne r1 r2 r3) = "sne " ++ showReg r1 ++ "," ++ showReg r2  ++ "," ++ showReg r3
showMipsIn (B l) = "b " ++ labelName l
showMipsIn (Beqz r l) = "beqz " ++ showReg r ++ "," ++ labelName l
showMipsIn (Li r num) = "li " ++ showReg r ++ "," ++ show num
showMipsIn (Lw r emit) = "lw " ++ showReg r ++ "," ++ showEmit emit
showMipsIn (Sw r emit) = "sw " ++ showReg r ++ "," ++ showEmit emit
showMipsIn (Move r1 r2) = "move " ++ showReg r1 ++ "," ++ showReg r2
showMipsIn (La r str) = "la " ++ showReg r ++ "," ++ str
showMipsIn (Jr r) = "jr " ++ showReg r
showMipsIn (Jal str) = "jal " ++ str
showMipsIn Syscall = "syscall"                      

showEmit :: Emit -> String
showEmit (Emit r i) = (show i) ++ "(" ++ (showReg r) ++ ")"

showReg :: Reg -> String
showReg A0 = "$a0"
showReg A1 = "$a1"
showReg A2 = "$a2" 
showReg A3 = "$a3"
showReg T0 = "$t0"
showReg T1 = "$t1"
showReg T2 = "$t2"
showReg T3 = "$t3"
showReg T4 = "$t4"
showReg V0 = "$v0"
showReg Fp = "$fp"
showReg Sp = "$sp"
showReg Ra = "$ra"
             
