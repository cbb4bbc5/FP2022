{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Interface(mainLoop) where
    import Lib
    import Control.Monad.State
    import Control.Monad.Accum
    import Data.Functor
    import Data.Maybe
    import System.IO
    import UsingAxioms(AllAxioms)
    
    
    addProof :: (MonadMap m, MonadFail m) => String -> Proof -> m ()
    addProof name pr = do
        Just (GetProof (pr0,xs,_)) <- getMap name
        putMap name $ GetProof (pr, pr0 : xs, [])
    backProof :: (MonadMap m, MonadFail m) => String -> m ()
    backProof name = do
        Just (GetProof (x',x : xs, xs')) <- getMap name
        putMap name $ GetProof (x, xs, x' : xs')
    forwardProof :: (MonadMap m, MonadFail m) => String -> m ()
    forwardProof name = do
        Just (GetProof (x,xs, x' : xs')) <- getMap name
        putMap name $ GetProof (x', x : xs, xs')

    readProof :: (MonadInput m,MonadPlusPlus m,MonadLex m,MonadFail m,MonadMap m, ProofRead m) => m With
    readProof = do{
        ;name <- getLex
        ;Just (GetProof(Complete _,_,_)) <- getMap name
        ;local (Just name) readTheoremFun
        ;Just pr@(GetProof _) <- getMap name
        ;writeLine $ show pr
        ;return pr}
      <|>do{
        ;name <- getLex 
        ;"" <- reading lex
        ;Just (GetProof _) <- getMap name
        ;local (Just name) (proofLoop >> (readTheoremFun <|> return ())) <||> return ()
        ;Just pr@(GetProof _) <- getMap name
        ;writeLine $ show pr
        ;return pr}
      <|>  do{
        ;name <- getLex
        ;f <- input FormulaF
        ;let pr0 = proof [] f & trivial
        ;putMap name $ GetProof (pr0,[],[])
        ;local (Just name) (proofLoop >> (readTheoremFun <|> return ())) <||> return ()
        ;Just pr@(GetProof _) <- getMap name
        ;return pr}
      <|>do{
        ;name <- getLex 
        ;Just (GetProof _) <- getMap name
        ;local (Just name) (proofLoop >> (readTheoremFun <|> return ())) <||> return ()
        ;Just pr@(GetProof _) <- getMap name
        ;writeLine $ show pr
        ;return pr}
        
    readProof1 :: (MonadInput m,MonadPlusPlus m,MonadLex m,MonadFail m,MonadMap m, ProofRead m) => m With
    readProof1 = do
        name <- getLex
        Just (GetProof(Complete thm,_,_)) <- getMap name
        thm' <- readTheoremFun1 thm
        return (GetProof (Complete thm',[],[]))
      <|> do
        name <- getLex 
        "" <- reading lex
        Just (GetProof _) <- getMap name
        local (Just name) (proofLoop >> (readTheoremFun <|> return ())) <||> return ()
        Just pr@(GetProof _) <- getMap name
        return pr
      <|> do
        name <- getLex
        f <- input FormulaF
        let pr0 = proof [] f & trivial
        putMap name $ GetProof (pr0,[],[])
        local (Just name) (proofLoop >> (readTheoremFun <|> return ())) <||> return ()
        Just pr@(GetProof _) <- getMap name
        return pr
      <|> do
        name <- getLex 
        Just (GetProof _) <- getMap name
        local (Just name) (proofLoop >> (readTheoremFun <|> return ())) <||> return ()
        Just pr@(GetProof _) <- getMap name
        return pr
-- proof l1 "Ax.Ay. x=y -> y=x"
    
    readFunInner :: (MonadInput m, MonadFail m, MonadPlusPlus m, MonadLex m,MonadMap m, ProofRead m) => String -> Theorem -> m Theorem
    readFunInner command thm = case command of
            "forallI" -> forallIM thm
            "forallE" -> forallEM thm
            "forI" -> forallIM thm
            "forE" -> forallEM thm
            "foralls" -> forallEsM thm
            "for" -> forallEsM thm
            "andI" -> do
                thm2 <- readTheorem
                andIM thm thm2
            "andR" -> andRightEM thm
            "andL" -> andLeftEM thm
            "bottomE" -> bottomEM thm
            "botE" -> bottomEM thm
            "orR" -> orRightIM thm
            "orL" -> orLeftIM thm
            "orE" -> do
                thm1 <- readTheorem
                thm2 <- readTheorem
                orEM thm thm1 thm2
            "impliesI" -> impliesIM thm
            "impliesE" -> do
                thm2 <- readTheorem
                impliesEM thm thm2
            "impI" -> impliesIM thm
            "impE" -> do
                thm2 <- readTheorem
                impliesEM thm thm2
            "iffI" -> do
                thm2 <- readTheorem
                iffIM thm thm2
            "iffE" -> do
                thm2 <- readTheorem
                iffEM thm thm2
            "existsI" -> existsIM thm
            "existsE" -> do
                thm2 <- readTheorem
                existsEM thm thm2
            "exiI" -> existsIM thm
            "exiE" -> do
                thm2 <- readTheorem
                existsEM thm thm2
            "subst" -> substInTheoremM thm
            _ -> mzero

    readTheoremFun :: (MonadInput m,MonadPlusPlus m,MonadLex m,MonadFail m,MonadMap m,ProofRead m) =>  m ()
    readTheoremFun = do
        Just name <- ask
        Just (GetProof (Complete thm,_,_)) <- getMap name
        command <- reading lex
        thm0' <- readFunInner command thm
        addProof name (Complete thm0')
        readTheoremFun <|> return ()
    readTheoremFun1 :: (MonadInput m,MonadPlusPlus m,MonadLex m,MonadFail m,MonadMap m,ProofRead m) => Theorem -> m Theorem
    readTheoremFun1 thm = do
        command <- reading lex
        thm' <- readFunInner command thm
        readTheoremFun1 thm' <|> return thm'

    readTheoremMini :: (MonadInput m,MonadPlusPlus m,MonadLex m,MonadFail m,MonadMap m, ProofRead m) =>  m With    
    readTheoremMini = readT
      where
       readT = do
        thm <- do
                    Just name <- ask
                    Just (GetProof (pr,_,_)) <- getMap name
                    Just (as,_) <- return $ goal pr
                    label <- getLex
                    Just f <- return $ lookup label as
                    return $ aX f
                <|> (parse <&> (fromAxiom :: AllAxioms -> Theorem))
                <|> do
                        "ax" <- getLex 
                        axM
                <|> do
                        "topI" <- getLex
                        topIM
        thm' <- readTheoremFun1 thm <|> return thm
        return $ GetProof (Complete thm',[],[])
    readTheorem :: (MonadInput m,MonadPlusPlus m,MonadLex m,MonadFail m,MonadMap m, ProofRead m) => m Theorem
    readTheorem = parenLex readT
      where
       readT = do
        (GetProof (Complete thm,_,_)) <- readTheoremMini <|> readProof1
        return thm
    readValue :: (MonadInput m,MonadPlusPlus m,MonadLex m,MonadFail m,MonadMap m, ProofRead m) => m With
    readValue = do
        let format = do
                fmt@(_:^^:_) <- inputFormat
                return $ GetFormat fmt
        readTheoremMini <|> format <|> readProof1
            
    llet :: (MonadLex m, MonadInput m, MonadPlusPlus m, MonadMap m,ProofRead m, MonadFail m) => String -> m ()
    llet name' = do 
        "=" <- reading lex
        ff <- readValue
        writeLine $ show ff
        putMap name' ff
    gget :: (MonadMap m, MonadRL m, MonadFail m) => String -> m ()
    gget name' = do
        Just w <- getMap name'
        writeLine $ show w 
    topLoop :: (MonadInput m,MonadPlusPlus m,MonadLex m,MonadFail m, MonadHandler m, MonadMap m, MonadAccum String m, ProofRead m) =>  m ()
    topLoop = do
        command <- getLex
        case command of
            "proof" -> readProof >> topLoop
            "exit" -> do
                Nothing <- getHandle
                return ()
              <|> topLoop
            "list" -> do
                Nothing <- getHandle
                showMap >>= writeLine
              <|> return () >> topLoop
            "load" -> do
                        name <- getLex 
                        let name' | ('"':_) <- name = read name
                                  | otherwise = name
                        let load = do 
                                Nothing <- getHandle
                                handle <- liftIO $ openFile name' ReadMode
                                putHandle $ Just handle
                        load <|> return () >> topLoop
            "save" -> do
                        name <- getLex 
                        let name' | ('"':_) <- name = read name
                                  | otherwise = name
                        let save = do
                                Nothing <- getHandle
                                content <- look
                                liftIO $ writeFile name' content
                        save <|> return () >> topLoop
            name ->  ((llet name <|> gget name) <||> return ()) >> topLoop
                      


    proofLoop :: (MonadInput m,MonadLex m,MonadFail m,MonadMap m,MonadPlusPlus m,ProofRead m) => m ()
    proofLoop = do
      Just name <- ask
      Just (GetProof (pr,_,_)) <- getMap name
      when (isNothing $ qed pr) $ do
        command <- getLex
        let exec = (>>proofLoop). (<||> writeLine "Error") . ((addProof name <=< trivialM ) =<<)
        let pply = exec $ do
                val <- readValue
                writeLine $ show val
                case val of
                    GetProof (Complete thm,_,_) -> applyThmM thm pr
                    GetProof _ -> mzero
                    GetFormat fmt -> do
                        f <- evalFormat fmt readFormula
                        writeLine $ show f
                        applyM f pr
        case command of
            "apply" -> pply
            "app" -> pply
            "next" -> exec $ return $ next pr
            "add" -> exec $ do
                i <- reading reads
                f <- input FormulaF
                addFormulaM (toEnum i) f pr
            "choose" -> exec $ do
                i <- reading reads
                chooseFormulaM (toEnum i) pr
            "get" -> exec $ getM pr
            "gets" -> exec $ getsM pr
            "proof" -> do 
                (void readProof <||> writeLine "ErrorI" )>> proofLoop
            "back" -> backProof name >> proofLoop
            "forward" -> forwardProof name >> proofLoop
            "exit" -> return ()
            name' -> ((llet name' <|> gget name') <||> return ()) >> proofLoop

    mainLoop :: (MonadInput m, MonadPlusPlus m, MonadLex m, MonadFail m,MonadHandler m, MonadMap m, MonadAccum String m,ProofRead m) => m ()
    mainLoop = topLoop <||> (writeLine "top error" >> mainLoop)