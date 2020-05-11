{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGrusGrus where
import AbsGrusGrus
import LexGrusGrus
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ((Maybe (Int, Int), Integer))
	| HappyAbsSyn5 ((Maybe (Int, Int), LIdent))
	| HappyAbsSyn6 ((Maybe (Int, Int), UIdent))
	| HappyAbsSyn7 ((Maybe (Int, Int), Body (Maybe (Int, Int))))
	| HappyAbsSyn8 ((Maybe (Int, Int), [Decl (Maybe (Int, Int))]))
	| HappyAbsSyn9 ((Maybe (Int, Int), Decl (Maybe (Int, Int))))
	| HappyAbsSyn10 ((Maybe (Int, Int), TypedIdent (Maybe (Int, Int))))
	| HappyAbsSyn11 ((Maybe (Int, Int), [TypedIdent (Maybe (Int, Int))]))
	| HappyAbsSyn12 ((Maybe (Int, Int), [TypeAlgConstr (Maybe (Int, Int))]))
	| HappyAbsSyn13 ((Maybe (Int, Int), Exp (Maybe (Int, Int))))
	| HappyAbsSyn22 ((Maybe (Int, Int), [Exp (Maybe (Int, Int))]))
	| HappyAbsSyn24 ((Maybe (Int, Int), Case (Maybe (Int, Int))))
	| HappyAbsSyn25 ((Maybe (Int, Int), [Case (Maybe (Int, Int))]))
	| HappyAbsSyn26 ((Maybe (Int, Int), Boolean (Maybe (Int, Int))))
	| HappyAbsSyn27 ((Maybe (Int, Int), Unit (Maybe (Int, Int))))
	| HappyAbsSyn28 ((Maybe (Int, Int), ParserType (Maybe (Int, Int))))
	| HappyAbsSyn30 ((Maybe (Int, Int), [ParserType (Maybe (Int, Int))]))
	| HappyAbsSyn32 ((Maybe (Int, Int), TypeAlgConstr (Maybe (Int, Int))))

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,626) ([0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,32768,0,11117,56,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,2048,0,0,0,0,16,16,0,0,0,12288,3,0,0,0,5,0,0,0,33856,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,59392,49170,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,2,2356,224,0,0,0,0,2,0,128,19712,14338,0,0,4,4712,448,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,32,0,0,0,8192,0,0,0,0,0,4096,0,0,1024,0,0,0,0,0,0,1,0,0,4096,0,0,0,4096,0,0,0,0,0,0,2048,0,0,2,2356,224,0,4096,40960,1,7,0,128,3328,14336,0,0,4,104,448,0,8192,16384,3,14,0,256,6656,28672,0,0,8,208,896,0,16384,32768,6,28,0,512,13312,57344,0,0,16,416,1792,0,32768,0,13,56,0,1024,26624,49152,1,0,32,832,3584,0,0,1,26,112,0,1024,0,0,0,0,8,8,0,0,0,38912,1,0,0,0,3264,0,0,0,5120,0,0,0,0,160,0,0,0,0,5,0,0,0,10240,0,0,0,0,1058,0,0,0,4096,33,0,0,0,512,0,0,0,0,16,0,0,0,32768,0,0,0,0,16384,0,0,0,0,64,0,0,0,0,16,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,128,0,0,0,2048,0,0,0,0,4096,0,0,4,4712,448,0,0,0,0,0,0,256,39424,28676,0,0,8,168,512,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,1024,21504,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,512,0,0,8192,0,0,0,0,128,19712,14338,0,0,4,0,0,0,0,64,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,16,18848,1792,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,64,0,0,0,0,0,0,64,1344,4096,0,0,0,0,16,0,0,32,0,0,0,0,0,512,0,0,256,0,0,0,8192,16384,147,14,0,0,0,0,0,0,144,0,0,0,16384,16384,5,16,0,0,0,0,0,0,0,0,0,0,32768,32768,10,32,0,0,0,0,0,0,32,672,2048,0,0,0,0,0,0,2048,53248,32804,3,0,64,9856,7169,0,0,32,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,5376,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,32,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,2048,43008,0,2,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pBody_internal","Integer","LIdent","UIdent","Body","ListDecl","Decl","TypedIdent","ListTypedIdent","ListTypeAlgConstr","Exp","Exp2","Exp3","Exp4","Exp5","Exp6","Exp7","Exp8","Exp9","ListExp","Exp1","Case","ListCase","Boolean","Unit","ParserType","ParserType2","ListParserType","ParserType1","TypeAlgConstr","'!='","'%'","'&&'","'('","')'","'*'","'+'","','","'-'","'->'","'/'","':'","';'","'<'","'<='","'='","'=='","'>'","'>='","'Bool'","'False'","'Int'","'True'","'Unit'","'\\\\'","'alg'","'case'","'else'","'fun'","'if'","'of'","'put'","'then'","'val'","'{'","'|'","'||'","'}'","'~>'","L_integ","L_LIdent","L_UIdent","%eof"]
        bit_start = st * 75
        bit_end = (st + 1) * 75
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..74]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (7) = happyGoto action_3
action_0 (8) = happyGoto action_4
action_0 _ = happyReduce_5

action_1 (72) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (75) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (36) = happyShift action_21
action_4 (53) = happyShift action_22
action_4 (55) = happyShift action_23
action_4 (56) = happyShift action_24
action_4 (58) = happyShift action_25
action_4 (59) = happyShift action_26
action_4 (61) = happyShift action_27
action_4 (62) = happyShift action_28
action_4 (64) = happyShift action_29
action_4 (66) = happyShift action_30
action_4 (72) = happyShift action_2
action_4 (73) = happyShift action_31
action_4 (74) = happyShift action_32
action_4 (4) = happyGoto action_5
action_4 (5) = happyGoto action_6
action_4 (6) = happyGoto action_7
action_4 (9) = happyGoto action_8
action_4 (13) = happyGoto action_9
action_4 (14) = happyGoto action_10
action_4 (15) = happyGoto action_11
action_4 (16) = happyGoto action_12
action_4 (17) = happyGoto action_13
action_4 (18) = happyGoto action_14
action_4 (19) = happyGoto action_15
action_4 (20) = happyGoto action_16
action_4 (21) = happyGoto action_17
action_4 (23) = happyGoto action_18
action_4 (26) = happyGoto action_19
action_4 (27) = happyGoto action_20
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_43

action_6 _ = happyReduce_46

action_7 _ = happyReduce_47

action_8 _ = happyReduce_6

action_9 _ = happyReduce_4

action_10 (69) = happyShift action_55
action_10 _ = happyReduce_52

action_11 (35) = happyShift action_54
action_11 _ = happyReduce_22

action_12 (33) = happyShift action_52
action_12 (49) = happyShift action_53
action_12 _ = happyReduce_24

action_13 (46) = happyShift action_48
action_13 (47) = happyShift action_49
action_13 (50) = happyShift action_50
action_13 (51) = happyShift action_51
action_13 _ = happyReduce_27

action_14 (39) = happyShift action_46
action_14 (41) = happyShift action_47
action_14 _ = happyReduce_32

action_15 (34) = happyShift action_43
action_15 (38) = happyShift action_44
action_15 (43) = happyShift action_45
action_15 _ = happyReduce_35

action_16 (36) = happyShift action_42
action_16 _ = happyReduce_39

action_17 _ = happyReduce_41

action_18 _ = happyReduce_20

action_19 _ = happyReduce_44

action_20 _ = happyReduce_45

action_21 (36) = happyShift action_21
action_21 (53) = happyShift action_22
action_21 (55) = happyShift action_23
action_21 (56) = happyShift action_24
action_21 (57) = happyShift action_41
action_21 (59) = happyShift action_26
action_21 (62) = happyShift action_28
action_21 (72) = happyShift action_2
action_21 (73) = happyShift action_31
action_21 (74) = happyShift action_32
action_21 (4) = happyGoto action_5
action_21 (5) = happyGoto action_6
action_21 (6) = happyGoto action_7
action_21 (13) = happyGoto action_40
action_21 (14) = happyGoto action_10
action_21 (15) = happyGoto action_11
action_21 (16) = happyGoto action_12
action_21 (17) = happyGoto action_13
action_21 (18) = happyGoto action_14
action_21 (19) = happyGoto action_15
action_21 (20) = happyGoto action_16
action_21 (21) = happyGoto action_17
action_21 (23) = happyGoto action_18
action_21 (26) = happyGoto action_19
action_21 (27) = happyGoto action_20
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_58

action_23 _ = happyReduce_57

action_24 _ = happyReduce_59

action_25 (74) = happyShift action_32
action_25 (6) = happyGoto action_39
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (36) = happyShift action_21
action_26 (53) = happyShift action_22
action_26 (55) = happyShift action_23
action_26 (56) = happyShift action_24
action_26 (59) = happyShift action_26
action_26 (62) = happyShift action_28
action_26 (72) = happyShift action_2
action_26 (73) = happyShift action_31
action_26 (74) = happyShift action_32
action_26 (4) = happyGoto action_5
action_26 (5) = happyGoto action_6
action_26 (6) = happyGoto action_7
action_26 (13) = happyGoto action_38
action_26 (14) = happyGoto action_10
action_26 (15) = happyGoto action_11
action_26 (16) = happyGoto action_12
action_26 (17) = happyGoto action_13
action_26 (18) = happyGoto action_14
action_26 (19) = happyGoto action_15
action_26 (20) = happyGoto action_16
action_26 (21) = happyGoto action_17
action_26 (23) = happyGoto action_18
action_26 (26) = happyGoto action_19
action_26 (27) = happyGoto action_20
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (73) = happyShift action_31
action_27 (5) = happyGoto action_37
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (36) = happyShift action_21
action_28 (53) = happyShift action_22
action_28 (55) = happyShift action_23
action_28 (56) = happyShift action_24
action_28 (59) = happyShift action_26
action_28 (62) = happyShift action_28
action_28 (72) = happyShift action_2
action_28 (73) = happyShift action_31
action_28 (74) = happyShift action_32
action_28 (4) = happyGoto action_5
action_28 (5) = happyGoto action_6
action_28 (6) = happyGoto action_7
action_28 (13) = happyGoto action_36
action_28 (14) = happyGoto action_10
action_28 (15) = happyGoto action_11
action_28 (16) = happyGoto action_12
action_28 (17) = happyGoto action_13
action_28 (18) = happyGoto action_14
action_28 (19) = happyGoto action_15
action_28 (20) = happyGoto action_16
action_28 (21) = happyGoto action_17
action_28 (23) = happyGoto action_18
action_28 (26) = happyGoto action_19
action_28 (27) = happyGoto action_20
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (36) = happyShift action_21
action_29 (53) = happyShift action_22
action_29 (55) = happyShift action_23
action_29 (56) = happyShift action_24
action_29 (59) = happyShift action_26
action_29 (62) = happyShift action_28
action_29 (72) = happyShift action_2
action_29 (73) = happyShift action_31
action_29 (74) = happyShift action_32
action_29 (4) = happyGoto action_5
action_29 (5) = happyGoto action_6
action_29 (6) = happyGoto action_7
action_29 (13) = happyGoto action_35
action_29 (14) = happyGoto action_10
action_29 (15) = happyGoto action_11
action_29 (16) = happyGoto action_12
action_29 (17) = happyGoto action_13
action_29 (18) = happyGoto action_14
action_29 (19) = happyGoto action_15
action_29 (20) = happyGoto action_16
action_29 (21) = happyGoto action_17
action_29 (23) = happyGoto action_18
action_29 (26) = happyGoto action_19
action_29 (27) = happyGoto action_20
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (73) = happyShift action_31
action_30 (5) = happyGoto action_33
action_30 (10) = happyGoto action_34
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_2

action_32 _ = happyReduce_3

action_33 (44) = happyShift action_80
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (48) = happyShift action_79
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (45) = happyShift action_78
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (65) = happyShift action_77
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (36) = happyShift action_76
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (63) = happyShift action_75
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (48) = happyShift action_74
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (37) = happyShift action_73
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (73) = happyShift action_31
action_41 (5) = happyGoto action_33
action_41 (10) = happyGoto action_71
action_41 (11) = happyGoto action_72
action_41 _ = happyReduce_12

action_42 (36) = happyShift action_21
action_42 (53) = happyShift action_22
action_42 (55) = happyShift action_23
action_42 (56) = happyShift action_24
action_42 (59) = happyShift action_26
action_42 (62) = happyShift action_28
action_42 (72) = happyShift action_2
action_42 (73) = happyShift action_31
action_42 (74) = happyShift action_32
action_42 (4) = happyGoto action_5
action_42 (5) = happyGoto action_6
action_42 (6) = happyGoto action_7
action_42 (13) = happyGoto action_69
action_42 (14) = happyGoto action_10
action_42 (15) = happyGoto action_11
action_42 (16) = happyGoto action_12
action_42 (17) = happyGoto action_13
action_42 (18) = happyGoto action_14
action_42 (19) = happyGoto action_15
action_42 (20) = happyGoto action_16
action_42 (21) = happyGoto action_17
action_42 (22) = happyGoto action_70
action_42 (23) = happyGoto action_18
action_42 (26) = happyGoto action_19
action_42 (27) = happyGoto action_20
action_42 _ = happyReduce_49

action_43 (36) = happyShift action_21
action_43 (53) = happyShift action_22
action_43 (55) = happyShift action_23
action_43 (56) = happyShift action_24
action_43 (72) = happyShift action_2
action_43 (73) = happyShift action_31
action_43 (74) = happyShift action_32
action_43 (4) = happyGoto action_5
action_43 (5) = happyGoto action_6
action_43 (6) = happyGoto action_7
action_43 (20) = happyGoto action_68
action_43 (21) = happyGoto action_17
action_43 (26) = happyGoto action_19
action_43 (27) = happyGoto action_20
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (36) = happyShift action_21
action_44 (53) = happyShift action_22
action_44 (55) = happyShift action_23
action_44 (56) = happyShift action_24
action_44 (72) = happyShift action_2
action_44 (73) = happyShift action_31
action_44 (74) = happyShift action_32
action_44 (4) = happyGoto action_5
action_44 (5) = happyGoto action_6
action_44 (6) = happyGoto action_7
action_44 (20) = happyGoto action_67
action_44 (21) = happyGoto action_17
action_44 (26) = happyGoto action_19
action_44 (27) = happyGoto action_20
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (36) = happyShift action_21
action_45 (53) = happyShift action_22
action_45 (55) = happyShift action_23
action_45 (56) = happyShift action_24
action_45 (72) = happyShift action_2
action_45 (73) = happyShift action_31
action_45 (74) = happyShift action_32
action_45 (4) = happyGoto action_5
action_45 (5) = happyGoto action_6
action_45 (6) = happyGoto action_7
action_45 (20) = happyGoto action_66
action_45 (21) = happyGoto action_17
action_45 (26) = happyGoto action_19
action_45 (27) = happyGoto action_20
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (36) = happyShift action_21
action_46 (53) = happyShift action_22
action_46 (55) = happyShift action_23
action_46 (56) = happyShift action_24
action_46 (72) = happyShift action_2
action_46 (73) = happyShift action_31
action_46 (74) = happyShift action_32
action_46 (4) = happyGoto action_5
action_46 (5) = happyGoto action_6
action_46 (6) = happyGoto action_7
action_46 (19) = happyGoto action_65
action_46 (20) = happyGoto action_16
action_46 (21) = happyGoto action_17
action_46 (26) = happyGoto action_19
action_46 (27) = happyGoto action_20
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (36) = happyShift action_21
action_47 (53) = happyShift action_22
action_47 (55) = happyShift action_23
action_47 (56) = happyShift action_24
action_47 (72) = happyShift action_2
action_47 (73) = happyShift action_31
action_47 (74) = happyShift action_32
action_47 (4) = happyGoto action_5
action_47 (5) = happyGoto action_6
action_47 (6) = happyGoto action_7
action_47 (19) = happyGoto action_64
action_47 (20) = happyGoto action_16
action_47 (21) = happyGoto action_17
action_47 (26) = happyGoto action_19
action_47 (27) = happyGoto action_20
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (36) = happyShift action_21
action_48 (53) = happyShift action_22
action_48 (55) = happyShift action_23
action_48 (56) = happyShift action_24
action_48 (72) = happyShift action_2
action_48 (73) = happyShift action_31
action_48 (74) = happyShift action_32
action_48 (4) = happyGoto action_5
action_48 (5) = happyGoto action_6
action_48 (6) = happyGoto action_7
action_48 (18) = happyGoto action_63
action_48 (19) = happyGoto action_15
action_48 (20) = happyGoto action_16
action_48 (21) = happyGoto action_17
action_48 (26) = happyGoto action_19
action_48 (27) = happyGoto action_20
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (36) = happyShift action_21
action_49 (53) = happyShift action_22
action_49 (55) = happyShift action_23
action_49 (56) = happyShift action_24
action_49 (72) = happyShift action_2
action_49 (73) = happyShift action_31
action_49 (74) = happyShift action_32
action_49 (4) = happyGoto action_5
action_49 (5) = happyGoto action_6
action_49 (6) = happyGoto action_7
action_49 (18) = happyGoto action_62
action_49 (19) = happyGoto action_15
action_49 (20) = happyGoto action_16
action_49 (21) = happyGoto action_17
action_49 (26) = happyGoto action_19
action_49 (27) = happyGoto action_20
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (36) = happyShift action_21
action_50 (53) = happyShift action_22
action_50 (55) = happyShift action_23
action_50 (56) = happyShift action_24
action_50 (72) = happyShift action_2
action_50 (73) = happyShift action_31
action_50 (74) = happyShift action_32
action_50 (4) = happyGoto action_5
action_50 (5) = happyGoto action_6
action_50 (6) = happyGoto action_7
action_50 (18) = happyGoto action_61
action_50 (19) = happyGoto action_15
action_50 (20) = happyGoto action_16
action_50 (21) = happyGoto action_17
action_50 (26) = happyGoto action_19
action_50 (27) = happyGoto action_20
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (36) = happyShift action_21
action_51 (53) = happyShift action_22
action_51 (55) = happyShift action_23
action_51 (56) = happyShift action_24
action_51 (72) = happyShift action_2
action_51 (73) = happyShift action_31
action_51 (74) = happyShift action_32
action_51 (4) = happyGoto action_5
action_51 (5) = happyGoto action_6
action_51 (6) = happyGoto action_7
action_51 (18) = happyGoto action_60
action_51 (19) = happyGoto action_15
action_51 (20) = happyGoto action_16
action_51 (21) = happyGoto action_17
action_51 (26) = happyGoto action_19
action_51 (27) = happyGoto action_20
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (36) = happyShift action_21
action_52 (53) = happyShift action_22
action_52 (55) = happyShift action_23
action_52 (56) = happyShift action_24
action_52 (72) = happyShift action_2
action_52 (73) = happyShift action_31
action_52 (74) = happyShift action_32
action_52 (4) = happyGoto action_5
action_52 (5) = happyGoto action_6
action_52 (6) = happyGoto action_7
action_52 (17) = happyGoto action_59
action_52 (18) = happyGoto action_14
action_52 (19) = happyGoto action_15
action_52 (20) = happyGoto action_16
action_52 (21) = happyGoto action_17
action_52 (26) = happyGoto action_19
action_52 (27) = happyGoto action_20
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (36) = happyShift action_21
action_53 (53) = happyShift action_22
action_53 (55) = happyShift action_23
action_53 (56) = happyShift action_24
action_53 (72) = happyShift action_2
action_53 (73) = happyShift action_31
action_53 (74) = happyShift action_32
action_53 (4) = happyGoto action_5
action_53 (5) = happyGoto action_6
action_53 (6) = happyGoto action_7
action_53 (17) = happyGoto action_58
action_53 (18) = happyGoto action_14
action_53 (19) = happyGoto action_15
action_53 (20) = happyGoto action_16
action_53 (21) = happyGoto action_17
action_53 (26) = happyGoto action_19
action_53 (27) = happyGoto action_20
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (36) = happyShift action_21
action_54 (53) = happyShift action_22
action_54 (55) = happyShift action_23
action_54 (56) = happyShift action_24
action_54 (72) = happyShift action_2
action_54 (73) = happyShift action_31
action_54 (74) = happyShift action_32
action_54 (4) = happyGoto action_5
action_54 (5) = happyGoto action_6
action_54 (6) = happyGoto action_7
action_54 (16) = happyGoto action_57
action_54 (17) = happyGoto action_13
action_54 (18) = happyGoto action_14
action_54 (19) = happyGoto action_15
action_54 (20) = happyGoto action_16
action_54 (21) = happyGoto action_17
action_54 (26) = happyGoto action_19
action_54 (27) = happyGoto action_20
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (36) = happyShift action_21
action_55 (53) = happyShift action_22
action_55 (55) = happyShift action_23
action_55 (56) = happyShift action_24
action_55 (72) = happyShift action_2
action_55 (73) = happyShift action_31
action_55 (74) = happyShift action_32
action_55 (4) = happyGoto action_5
action_55 (5) = happyGoto action_6
action_55 (6) = happyGoto action_7
action_55 (15) = happyGoto action_56
action_55 (16) = happyGoto action_12
action_55 (17) = happyGoto action_13
action_55 (18) = happyGoto action_14
action_55 (19) = happyGoto action_15
action_55 (20) = happyGoto action_16
action_55 (21) = happyGoto action_17
action_55 (26) = happyGoto action_19
action_55 (27) = happyGoto action_20
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (35) = happyShift action_54
action_56 _ = happyReduce_21

action_57 (33) = happyShift action_52
action_57 (49) = happyShift action_53
action_57 _ = happyReduce_23

action_58 (46) = happyShift action_48
action_58 (47) = happyShift action_49
action_58 (50) = happyShift action_50
action_58 (51) = happyShift action_51
action_58 _ = happyReduce_25

action_59 (46) = happyShift action_48
action_59 (47) = happyShift action_49
action_59 (50) = happyShift action_50
action_59 (51) = happyShift action_51
action_59 _ = happyReduce_26

action_60 (39) = happyShift action_46
action_60 (41) = happyShift action_47
action_60 _ = happyReduce_31

action_61 (39) = happyShift action_46
action_61 (41) = happyShift action_47
action_61 _ = happyReduce_29

action_62 (39) = happyShift action_46
action_62 (41) = happyShift action_47
action_62 _ = happyReduce_30

action_63 (39) = happyShift action_46
action_63 (41) = happyShift action_47
action_63 _ = happyReduce_28

action_64 (34) = happyShift action_43
action_64 (38) = happyShift action_44
action_64 (43) = happyShift action_45
action_64 _ = happyReduce_34

action_65 (34) = happyShift action_43
action_65 (38) = happyShift action_44
action_65 (43) = happyShift action_45
action_65 _ = happyReduce_33

action_66 (36) = happyShift action_42
action_66 _ = happyReduce_37

action_67 (36) = happyShift action_42
action_67 _ = happyReduce_36

action_68 (36) = happyShift action_42
action_68 _ = happyReduce_38

action_69 (40) = happyShift action_99
action_69 _ = happyReduce_50

action_70 (37) = happyShift action_98
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (40) = happyShift action_97
action_71 _ = happyReduce_13

action_72 (71) = happyShift action_96
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_48

action_74 (74) = happyShift action_32
action_74 (6) = happyGoto action_93
action_74 (12) = happyGoto action_94
action_74 (32) = happyGoto action_95
action_74 _ = happyReduce_15

action_75 (67) = happyShift action_92
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (73) = happyShift action_31
action_76 (5) = happyGoto action_33
action_76 (10) = happyGoto action_71
action_76 (11) = happyGoto action_91
action_76 _ = happyReduce_12

action_77 (36) = happyShift action_21
action_77 (53) = happyShift action_22
action_77 (55) = happyShift action_23
action_77 (56) = happyShift action_24
action_77 (59) = happyShift action_26
action_77 (62) = happyShift action_28
action_77 (72) = happyShift action_2
action_77 (73) = happyShift action_31
action_77 (74) = happyShift action_32
action_77 (4) = happyGoto action_5
action_77 (5) = happyGoto action_6
action_77 (6) = happyGoto action_7
action_77 (13) = happyGoto action_90
action_77 (14) = happyGoto action_10
action_77 (15) = happyGoto action_11
action_77 (16) = happyGoto action_12
action_77 (17) = happyGoto action_13
action_77 (18) = happyGoto action_14
action_77 (19) = happyGoto action_15
action_77 (20) = happyGoto action_16
action_77 (21) = happyGoto action_17
action_77 (23) = happyGoto action_18
action_77 (26) = happyGoto action_19
action_77 (27) = happyGoto action_20
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_7

action_79 (36) = happyShift action_21
action_79 (53) = happyShift action_22
action_79 (55) = happyShift action_23
action_79 (56) = happyShift action_24
action_79 (59) = happyShift action_26
action_79 (62) = happyShift action_28
action_79 (72) = happyShift action_2
action_79 (73) = happyShift action_31
action_79 (74) = happyShift action_32
action_79 (4) = happyGoto action_5
action_79 (5) = happyGoto action_6
action_79 (6) = happyGoto action_7
action_79 (13) = happyGoto action_89
action_79 (14) = happyGoto action_10
action_79 (15) = happyGoto action_11
action_79 (16) = happyGoto action_12
action_79 (17) = happyGoto action_13
action_79 (18) = happyGoto action_14
action_79 (19) = happyGoto action_15
action_79 (20) = happyGoto action_16
action_79 (21) = happyGoto action_17
action_79 (23) = happyGoto action_18
action_79 (26) = happyGoto action_19
action_79 (27) = happyGoto action_20
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (36) = happyShift action_85
action_80 (52) = happyShift action_86
action_80 (54) = happyShift action_87
action_80 (56) = happyShift action_88
action_80 (74) = happyShift action_32
action_80 (6) = happyGoto action_81
action_80 (28) = happyGoto action_82
action_80 (29) = happyGoto action_83
action_80 (31) = happyGoto action_84
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_66

action_82 _ = happyReduce_11

action_83 (42) = happyShift action_113
action_83 _ = happyReduce_71

action_84 _ = happyReduce_62

action_85 (36) = happyShift action_85
action_85 (52) = happyShift action_86
action_85 (54) = happyShift action_87
action_85 (56) = happyShift action_88
action_85 (74) = happyShift action_32
action_85 (6) = happyGoto action_81
action_85 (28) = happyGoto action_112
action_85 (29) = happyGoto action_83
action_85 (31) = happyGoto action_84
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_64

action_87 _ = happyReduce_63

action_88 _ = happyReduce_65

action_89 (45) = happyShift action_111
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (60) = happyShift action_110
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (37) = happyShift action_109
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (36) = happyShift action_21
action_92 (53) = happyShift action_22
action_92 (55) = happyShift action_23
action_92 (56) = happyShift action_24
action_92 (59) = happyShift action_26
action_92 (62) = happyShift action_28
action_92 (72) = happyShift action_2
action_92 (73) = happyShift action_31
action_92 (74) = happyShift action_32
action_92 (4) = happyGoto action_5
action_92 (5) = happyGoto action_6
action_92 (6) = happyGoto action_7
action_92 (13) = happyGoto action_106
action_92 (14) = happyGoto action_10
action_92 (15) = happyGoto action_11
action_92 (16) = happyGoto action_12
action_92 (17) = happyGoto action_13
action_92 (18) = happyGoto action_14
action_92 (19) = happyGoto action_15
action_92 (20) = happyGoto action_16
action_92 (21) = happyGoto action_17
action_92 (23) = happyGoto action_18
action_92 (24) = happyGoto action_107
action_92 (25) = happyGoto action_108
action_92 (26) = happyGoto action_19
action_92 (27) = happyGoto action_20
action_92 _ = happyReduce_54

action_93 (36) = happyShift action_105
action_93 _ = happyReduce_72

action_94 (45) = happyShift action_104
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (68) = happyShift action_103
action_95 _ = happyReduce_16

action_96 (7) = happyGoto action_102
action_96 (8) = happyGoto action_4
action_96 _ = happyReduce_5

action_97 (73) = happyShift action_31
action_97 (5) = happyGoto action_33
action_97 (10) = happyGoto action_71
action_97 (11) = happyGoto action_101
action_97 _ = happyReduce_12

action_98 _ = happyReduce_40

action_99 (36) = happyShift action_21
action_99 (53) = happyShift action_22
action_99 (55) = happyShift action_23
action_99 (56) = happyShift action_24
action_99 (59) = happyShift action_26
action_99 (62) = happyShift action_28
action_99 (72) = happyShift action_2
action_99 (73) = happyShift action_31
action_99 (74) = happyShift action_32
action_99 (4) = happyGoto action_5
action_99 (5) = happyGoto action_6
action_99 (6) = happyGoto action_7
action_99 (13) = happyGoto action_69
action_99 (14) = happyGoto action_10
action_99 (15) = happyGoto action_11
action_99 (16) = happyGoto action_12
action_99 (17) = happyGoto action_13
action_99 (18) = happyGoto action_14
action_99 (19) = happyGoto action_15
action_99 (20) = happyGoto action_16
action_99 (21) = happyGoto action_17
action_99 (22) = happyGoto action_100
action_99 (23) = happyGoto action_18
action_99 (26) = happyGoto action_19
action_99 (27) = happyGoto action_20
action_99 _ = happyReduce_49

action_100 _ = happyReduce_51

action_101 _ = happyReduce_14

action_102 (37) = happyShift action_125
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (74) = happyShift action_32
action_103 (6) = happyGoto action_93
action_103 (12) = happyGoto action_124
action_103 (32) = happyGoto action_95
action_103 _ = happyReduce_15

action_104 _ = happyReduce_10

action_105 (36) = happyShift action_85
action_105 (52) = happyShift action_86
action_105 (54) = happyShift action_87
action_105 (56) = happyShift action_88
action_105 (74) = happyShift action_32
action_105 (6) = happyGoto action_81
action_105 (28) = happyGoto action_122
action_105 (29) = happyGoto action_83
action_105 (30) = happyGoto action_123
action_105 (31) = happyGoto action_84
action_105 _ = happyReduce_68

action_106 (71) = happyShift action_121
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (45) = happyShift action_120
action_107 _ = happyReduce_55

action_108 (70) = happyShift action_119
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (42) = happyShift action_118
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (36) = happyShift action_21
action_110 (53) = happyShift action_22
action_110 (55) = happyShift action_23
action_110 (56) = happyShift action_24
action_110 (59) = happyShift action_26
action_110 (62) = happyShift action_28
action_110 (72) = happyShift action_2
action_110 (73) = happyShift action_31
action_110 (74) = happyShift action_32
action_110 (4) = happyGoto action_5
action_110 (5) = happyGoto action_6
action_110 (6) = happyGoto action_7
action_110 (13) = happyGoto action_117
action_110 (14) = happyGoto action_10
action_110 (15) = happyGoto action_11
action_110 (16) = happyGoto action_12
action_110 (17) = happyGoto action_13
action_110 (18) = happyGoto action_14
action_110 (19) = happyGoto action_15
action_110 (20) = happyGoto action_16
action_110 (21) = happyGoto action_17
action_110 (23) = happyGoto action_18
action_110 (26) = happyGoto action_19
action_110 (27) = happyGoto action_20
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_8

action_112 (37) = happyShift action_115
action_112 (40) = happyShift action_116
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (36) = happyShift action_85
action_113 (52) = happyShift action_86
action_113 (54) = happyShift action_87
action_113 (56) = happyShift action_88
action_113 (74) = happyShift action_32
action_113 (6) = happyGoto action_81
action_113 (28) = happyGoto action_114
action_113 (29) = happyGoto action_83
action_113 (31) = happyGoto action_84
action_113 _ = happyFail (happyExpListPerState 113)

action_114 _ = happyReduce_60

action_115 _ = happyReduce_67

action_116 (36) = happyShift action_85
action_116 (52) = happyShift action_86
action_116 (54) = happyShift action_87
action_116 (56) = happyShift action_88
action_116 (74) = happyShift action_32
action_116 (6) = happyGoto action_81
action_116 (28) = happyGoto action_122
action_116 (29) = happyGoto action_83
action_116 (30) = happyGoto action_131
action_116 (31) = happyGoto action_84
action_116 _ = happyReduce_68

action_117 _ = happyReduce_18

action_118 (36) = happyShift action_85
action_118 (52) = happyShift action_86
action_118 (54) = happyShift action_87
action_118 (56) = happyShift action_88
action_118 (74) = happyShift action_32
action_118 (6) = happyGoto action_81
action_118 (28) = happyGoto action_130
action_118 (29) = happyGoto action_83
action_118 (31) = happyGoto action_84
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_19

action_120 (36) = happyShift action_21
action_120 (53) = happyShift action_22
action_120 (55) = happyShift action_23
action_120 (56) = happyShift action_24
action_120 (59) = happyShift action_26
action_120 (62) = happyShift action_28
action_120 (72) = happyShift action_2
action_120 (73) = happyShift action_31
action_120 (74) = happyShift action_32
action_120 (4) = happyGoto action_5
action_120 (5) = happyGoto action_6
action_120 (6) = happyGoto action_7
action_120 (13) = happyGoto action_106
action_120 (14) = happyGoto action_10
action_120 (15) = happyGoto action_11
action_120 (16) = happyGoto action_12
action_120 (17) = happyGoto action_13
action_120 (18) = happyGoto action_14
action_120 (19) = happyGoto action_15
action_120 (20) = happyGoto action_16
action_120 (21) = happyGoto action_17
action_120 (23) = happyGoto action_18
action_120 (24) = happyGoto action_107
action_120 (25) = happyGoto action_129
action_120 (26) = happyGoto action_19
action_120 (27) = happyGoto action_20
action_120 _ = happyReduce_54

action_121 (36) = happyShift action_21
action_121 (53) = happyShift action_22
action_121 (55) = happyShift action_23
action_121 (56) = happyShift action_24
action_121 (59) = happyShift action_26
action_121 (62) = happyShift action_28
action_121 (72) = happyShift action_2
action_121 (73) = happyShift action_31
action_121 (74) = happyShift action_32
action_121 (4) = happyGoto action_5
action_121 (5) = happyGoto action_6
action_121 (6) = happyGoto action_7
action_121 (13) = happyGoto action_128
action_121 (14) = happyGoto action_10
action_121 (15) = happyGoto action_11
action_121 (16) = happyGoto action_12
action_121 (17) = happyGoto action_13
action_121 (18) = happyGoto action_14
action_121 (19) = happyGoto action_15
action_121 (20) = happyGoto action_16
action_121 (21) = happyGoto action_17
action_121 (23) = happyGoto action_18
action_121 (26) = happyGoto action_19
action_121 (27) = happyGoto action_20
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (40) = happyShift action_127
action_122 _ = happyReduce_69

action_123 (37) = happyShift action_126
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_17

action_125 _ = happyReduce_42

action_126 _ = happyReduce_73

action_127 (36) = happyShift action_85
action_127 (52) = happyShift action_86
action_127 (54) = happyShift action_87
action_127 (56) = happyShift action_88
action_127 (74) = happyShift action_32
action_127 (6) = happyGoto action_81
action_127 (28) = happyGoto action_122
action_127 (29) = happyGoto action_83
action_127 (30) = happyGoto action_134
action_127 (31) = happyGoto action_84
action_127 _ = happyReduce_68

action_128 _ = happyReduce_53

action_129 _ = happyReduce_56

action_130 (67) = happyShift action_133
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (37) = happyShift action_132
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (42) = happyShift action_136
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (7) = happyGoto action_135
action_133 (8) = happyGoto action_4
action_133 _ = happyReduce_5

action_134 _ = happyReduce_70

action_135 (70) = happyShift action_138
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (36) = happyShift action_85
action_136 (52) = happyShift action_86
action_136 (54) = happyShift action_87
action_136 (56) = happyShift action_88
action_136 (74) = happyShift action_32
action_136 (6) = happyGoto action_81
action_136 (28) = happyGoto action_137
action_136 (29) = happyGoto action_83
action_136 (31) = happyGoto action_84
action_136 _ = happyFail (happyExpListPerState 136)

action_137 _ = happyReduce_61

action_138 _ = happyReduce_9

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 ((Just (tokenLineCol happy_var_1), read (prToken happy_var_1))
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 ((Just (tokenLineCol happy_var_1), LIdent (prToken happy_var_1))
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 ((Just (tokenLineCol happy_var_1), UIdent (prToken happy_var_1))
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  7 happyReduction_4
happyReduction_4 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ((fst happy_var_1, AbsGrusGrus.Body (fst happy_var_1)(reverse (snd happy_var_1)) (snd happy_var_2))
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  8 happyReduction_5
happyReduction_5  =  HappyAbsSyn8
		 ((Nothing, [])
	)

happyReduce_6 = happySpecReduce_2  8 happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  9 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn13  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.DPut (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 5 9 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.DVal (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 10 9 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.DFun (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4)(snd happy_var_7)(snd happy_var_9))
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 5 9 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.DAlg (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn10
		 ((fst happy_var_1, AbsGrusGrus.TypedIdent (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  11 happyReduction_12
happyReduction_12  =  HappyAbsSyn11
		 ((Nothing, [])
	)

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  12 happyReduction_15
happyReduction_15  =  HappyAbsSyn12
		 ((Nothing, [])
	)

happyReduce_16 = happySpecReduce_1  12 happyReduction_16
happyReduction_16 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn12
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn12
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 6 13 happyReduction_18
happyReduction_18 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.EIfte (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4)(snd happy_var_6))
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 6 13 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.ECase (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_5))
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, snd happy_var_1)
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  14 happyReduction_21
happyReduction_21 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EOr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, snd happy_var_1)
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  15 happyReduction_23
happyReduction_23 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EAnd (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, snd happy_var_1)
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EEq (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  16 happyReduction_26
happyReduction_26 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.ENeq (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, snd happy_var_1)
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  17 happyReduction_28
happyReduction_28 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.ELt (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  17 happyReduction_29
happyReduction_29 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EGt (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  17 happyReduction_30
happyReduction_30 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.ELe (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  17 happyReduction_31
happyReduction_31 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EGe (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  17 happyReduction_32
happyReduction_32 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, snd happy_var_1)
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  18 happyReduction_33
happyReduction_33 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EAdd (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  18 happyReduction_34
happyReduction_34 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.ESub (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, snd happy_var_1)
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  19 happyReduction_36
happyReduction_36 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EMult (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  19 happyReduction_37
happyReduction_37 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EDiv (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  19 happyReduction_38
happyReduction_38 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EMod (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  19 happyReduction_39
happyReduction_39 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, snd happy_var_1)
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happyReduce 4 20 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.ECall (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_1  20 happyReduction_41
happyReduction_41 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, snd happy_var_1)
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happyReduce 6 21 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.ELambda (Just (tokenLineCol happy_var_1)) (snd happy_var_3)(snd happy_var_5))
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_1  21 happyReduction_43
happyReduction_43 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EInt (fst happy_var_1)(snd happy_var_1))
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  21 happyReduction_44
happyReduction_44 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EBool (fst happy_var_1)(snd happy_var_1))
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  21 happyReduction_45
happyReduction_45 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EUnit (fst happy_var_1)(snd happy_var_1))
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  21 happyReduction_46
happyReduction_46 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EVar (fst happy_var_1)(snd happy_var_1))
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  21 happyReduction_47
happyReduction_47 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsGrusGrus.EAlg (fst happy_var_1)(snd happy_var_1))
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  21 happyReduction_48
happyReduction_48 _
	(HappyAbsSyn13  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn13
		 ((Just (tokenLineCol happy_var_1), snd happy_var_2)
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_0  22 happyReduction_49
happyReduction_49  =  HappyAbsSyn22
		 ((Nothing, [])
	)

happyReduce_50 = happySpecReduce_1  22 happyReduction_50
happyReduction_50 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn22
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  22 happyReduction_51
happyReduction_51 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn22
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  23 happyReduction_52
happyReduction_52 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, snd happy_var_1)
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  24 happyReduction_53
happyReduction_53 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn24
		 ((fst happy_var_1, AbsGrusGrus.Case (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_0  25 happyReduction_54
happyReduction_54  =  HappyAbsSyn25
		 ((Nothing, [])
	)

happyReduce_55 = happySpecReduce_1  25 happyReduction_55
happyReduction_55 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn25
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  25 happyReduction_56
happyReduction_56 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn25
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  26 happyReduction_57
happyReduction_57 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.BTrue (Just (tokenLineCol happy_var_1)))
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  26 happyReduction_58
happyReduction_58 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.BFalse (Just (tokenLineCol happy_var_1)))
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  27 happyReduction_59
happyReduction_59 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.Unit (Just (tokenLineCol happy_var_1)))
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  28 happyReduction_60
happyReduction_60 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 ((fst happy_var_1, AbsGrusGrus.PTArrow (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 7 28 happyReduction_61
happyReduction_61 ((HappyAbsSyn28  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.PTArrowMult (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4)(snd happy_var_7))
	) `HappyStk` happyRest

happyReduce_62 = happySpecReduce_1  28 happyReduction_62
happyReduction_62 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 ((fst happy_var_1, snd happy_var_1)
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  29 happyReduction_63
happyReduction_63 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn28
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.PTInt (Just (tokenLineCol happy_var_1)))
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  29 happyReduction_64
happyReduction_64 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn28
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.PTBool (Just (tokenLineCol happy_var_1)))
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  29 happyReduction_65
happyReduction_65 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn28
		 ((Just (tokenLineCol happy_var_1), AbsGrusGrus.PTBool (Just (tokenLineCol happy_var_1)))
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  29 happyReduction_66
happyReduction_66 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn28
		 ((fst happy_var_1, AbsGrusGrus.PTAlg (fst happy_var_1)(snd happy_var_1))
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  29 happyReduction_67
happyReduction_67 _
	(HappyAbsSyn28  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn28
		 ((Just (tokenLineCol happy_var_1), snd happy_var_2)
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_0  30 happyReduction_68
happyReduction_68  =  HappyAbsSyn30
		 ((Nothing, [])
	)

happyReduce_69 = happySpecReduce_1  30 happyReduction_69
happyReduction_69 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn30
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  30 happyReduction_70
happyReduction_70 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn30
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  31 happyReduction_71
happyReduction_71 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 ((fst happy_var_1, snd happy_var_1)
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  32 happyReduction_72
happyReduction_72 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn32
		 ((fst happy_var_1, AbsGrusGrus.TAC (fst happy_var_1)(snd happy_var_1))
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happyReduce 4 32 happyReduction_73
happyReduction_73 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 ((fst happy_var_1, AbsGrusGrus.TACArgs (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 75 75 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 33;
	PT _ (TS _ 2) -> cont 34;
	PT _ (TS _ 3) -> cont 35;
	PT _ (TS _ 4) -> cont 36;
	PT _ (TS _ 5) -> cont 37;
	PT _ (TS _ 6) -> cont 38;
	PT _ (TS _ 7) -> cont 39;
	PT _ (TS _ 8) -> cont 40;
	PT _ (TS _ 9) -> cont 41;
	PT _ (TS _ 10) -> cont 42;
	PT _ (TS _ 11) -> cont 43;
	PT _ (TS _ 12) -> cont 44;
	PT _ (TS _ 13) -> cont 45;
	PT _ (TS _ 14) -> cont 46;
	PT _ (TS _ 15) -> cont 47;
	PT _ (TS _ 16) -> cont 48;
	PT _ (TS _ 17) -> cont 49;
	PT _ (TS _ 18) -> cont 50;
	PT _ (TS _ 19) -> cont 51;
	PT _ (TS _ 20) -> cont 52;
	PT _ (TS _ 21) -> cont 53;
	PT _ (TS _ 22) -> cont 54;
	PT _ (TS _ 23) -> cont 55;
	PT _ (TS _ 24) -> cont 56;
	PT _ (TS _ 25) -> cont 57;
	PT _ (TS _ 26) -> cont 58;
	PT _ (TS _ 27) -> cont 59;
	PT _ (TS _ 28) -> cont 60;
	PT _ (TS _ 29) -> cont 61;
	PT _ (TS _ 30) -> cont 62;
	PT _ (TS _ 31) -> cont 63;
	PT _ (TS _ 32) -> cont 64;
	PT _ (TS _ 33) -> cont 65;
	PT _ (TS _ 34) -> cont 66;
	PT _ (TS _ 35) -> cont 67;
	PT _ (TS _ 36) -> cont 68;
	PT _ (TS _ 37) -> cont 69;
	PT _ (TS _ 38) -> cont 70;
	PT _ (TS _ 39) -> cont 71;
	PT _ (TI _) -> cont 72;
	PT _ (T_LIdent _) -> cont 73;
	PT _ (T_UIdent _) -> cont 74;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 75 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pBody_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens

pBody = (>>= return . snd) . pBody_internal
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
