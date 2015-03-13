module TypeDatabase where

import Data.Maybe
import Data.List

import Util
import AST
import Environment

data TypeNode = TN {
    symbol :: Symbol,
    subNodes :: [TypeNode]
} deriving (Eq)

arrayClass = CLS ["public"] "Array" (Just ["Object"]) [[]] [] [] [] (CLSI [] (AI "" 0 0 0 0) Nothing [])
arrayUnit = Comp (Just ["joosc native", "Array"]) [] arrayClass (CompI Nothing [])
nativeTypes = TN (PKG []) [TN (PKG "joosc native") [TN (CL ["public"] "Array" (TypeClass (Name ["joosc native", "Array"])) arrayUnit) [TN (SYM ["public"] ["joosc native", "Array"] "length" TypeInt) []]]]

isVisibleClassNode tn = case symbol tn of
                            PKG _ -> True
                            CL _ _ _ _ -> True
                            IT _ _ _ _ -> True
                            FUNC _ _ _ _ _ -> False
                            _ -> True -- False -> True

isConcreteNode tn = case symbol tn of
                            CL  _ _ _ _ -> True
                            IT _ _ _ _ -> True
                            _ -> False

isSYMFUNCNode tn = case symbol tn of
                            FUNC _ _ _ _ _ -> True
                            SYM _ _ _ _ -> True
                            _ -> False

instance Show TypeNode where
    show (TN sym nodes) =   "{\n" ++
                            "  " ++ (show sym) ++ "\n" ++
                            (indent 2 (intercalate "\n" lns)) ++ "\n" ++
                            "}\n"
        where
            lns = map show (filter isVisibleClassNode nodes)

getTypeEntry :: TypeNode -> [String] -> Maybe TypeNode
getTypeEntry tn name = case traverseTypeEntry tn name of
  Just node -> Just . head . subNodes $ node
  Nothing -> Nothing

inheritFromNodes :: TypeNode -> [TypeNode] -> TypeNode
inheritFromNodes (TN sym ch) nodes = (TN sym (syms ++ cons ++ funcs))
    where
        inherits = (concat $ map subNodes nodes)
        syms = nubBy (\(TN sym1 _) (TN sym2 _) -> localName sym1 == localName sym2) [TN sym ch | TN sym@(SYM _ _ _ _) ch <- ch ++ inherits]
        cons = [TN sym ch | TN sym@(FUNC mds _ _ _ _) ch <- ch, elem "cons" mds]
        funcs = nubBy (\(TN sym1 _) (TN sym2 _) -> (localName sym1, parameterTypes sym1) == (localName sym2, parameterTypes sym2)) [TN sym ch | TN sym@(FUNC mds _ _ _ _) ch <- ch ++ inherits, not $ elem "cons" mds]

inheritFromTypes :: TypeNode -> [String] -> [[String]] -> Maybe TypeNode
inheritFromTypes tn cname cnames = if and tycs then Just $ inheritFromNodes (fromJust mtar) (map fromJust msrcs) else Nothing
    where
        mtar = getTypeEntry tn cname
        msrcs = map (getTypeEntry tn) cnames
        tycs = map isJust (mtar:msrcs)

updateNode :: TypeNode -> [String] -> TypeNode -> Maybe TypeNode
updateNode cur [] n = if symbol cur == symbol n then Just n else Nothing
updateNode (TN sym nodes) (nm:remain) n = case conflict of
                                    [tar] -> case updateNode tar remain n of
                                                Just tar' -> Just $ TN sym (tar':remainNodes)
                                                Nothing -> Nothing
                                    _ -> Nothing
    where
        remainNodes = [node | node <- nodes, (localName . symbol) node /= nm]
        conflict = [node | node <- nodes, (localName . symbol) node == nm]

updateDBWithInheritance :: TypeNode -> [String] -> [[String]] -> Maybe TypeNode
updateDBWithInheritance root cname cnames = if isJust tar && isJust root' then root' else Nothing
    where
        tar = inheritFromTypes root cname cnames
        root' = updateNode root cname (fromJust tar)

updateDBWithInheritances :: TypeNode -> [([String], [[String]])] -> Maybe TypeNode
updateDBWithInheritances root [] = Just root
updateDBWithInheritances root ((cname, cnames):remain) = case updateDBWithInheritance root cname cnames of
                                                            Just root' -> updateDBWithInheritances root' remain
                                                            Nothing -> Nothing

dumpDB :: TypeNode -> [[String]]
dumpDB tn@(TN sym nodes) = map (typeToName . localType . symbol) (dumpDBNodes tn)

dumpDBNodes :: TypeNode -> [TypeNode]
dumpDBNodes tn@(TN sym nodes) = if isConcreteNode tn then [tn] else concat $ map dumpDBNodes nodes

traverseNodeEntry :: TypeNode -> [String] -> Maybe TypeNode
traverseNodeEntry tn [] = traverseTypeEntry tn []
traverseNodeEntry tn@(TN sym nodes) ["*"] = Just tn
traverseNodeEntry tn@(TN sym nodes) (nm:remain) = case [node | node <- nodes, (localName . symbol) node == nm] of
                                                [] -> Nothing
                                                [node] -> traverseNodeEntry node remain
                                                _ -> Nothing

traverseTypeEntry :: TypeNode -> [String] -> Maybe TypeNode
traverseTypeEntry tn [] = case symbol tn of
                            CL _ _ _ _ -> Just (TN (PKG []) [tn])
                            IT _ _ _  _ -> Just (TN (PKG []) [tn])
                            _ -> Nothing
traverseTypeEntry (TN sym nodes) ["*"] = let nodes' = filter isConcreteNode nodes in if nodes' == [] then Nothing else Just (TN (PKG []) (filter isConcreteNode nodes))
traverseTypeEntry (TN sym nodes) (nm:remain) = case [node | node <- nodes, (localName . symbol) node == nm] of
                                                [] -> Nothing
                                                [node] -> traverseTypeEntry node remain
                                                _ -> Nothing

traverseFieldEntryWithImports :: TypeNode -> [[String]] -> [String] -> [TypeNode]
traverseFieldEntryWithImports tn imps query = nub . concat $ (flds ++ funcs)
    where
        entries = map (traverseTypeEntry tn) imps
        entries' = map (\(mnode, imp) -> (fromJust mnode, imp)) (filter (isJust . fst) (zip entries imps))
        results = map (\(node, imp) -> (traverseTypeEntry node (init query), (init imp) ++ (init query))) ((tn, ["*"]):entries')
        fld = if query == [] then error "traverseFieldEntryWithImports" else last query
        flds = [[TN (SYM mds ls ln lt) ch | TN (SYM mds ls ln lt) ch <- subNodes node, elem "static" mds, ln == fld] | (Just (TN _ [node]), cname) <- results]
        funcs = [[TN (FUNC mds ls ln ps rt) ch | TN (FUNC mds ls ln ps rt) ch <- subNodes node, elem "static" mds, ln == fld] | (Just (TN _ [node]), cname) <- results]

traverseTypeEntryWithImports :: TypeNode -> [[String]] -> [String] -> [[String]]
traverseTypeEntryWithImports tn imps query = nub [cname | (Just node, cname) <- results]
    where
        entries = map (traverseTypeEntry tn) imps
        entries' = map (\(mnode, imp) -> (fromJust mnode, imp)) (filter (isJust . fst) (zip entries imps))
        results = map (\(node, imp) -> (traverseTypeEntry node query, (init imp) ++ query)) ((tn, ["*"]):entries')

traverseInstanceEntry :: TypeNode -> [TypeNode] -> [String] -> [TypeNode]
traverseInstanceEntry root nodes cname = concat nodes'
    where
        nodes' = map (\cur -> traverseInstanceEntry' root cur cname) nodes

traverseInstanceEntry' :: TypeNode -> TypeNode -> [String] -> [TypeNode]
traverseInstanceEntry' root cur [] = [cur]
traverseInstanceEntry' root (TN (SYM mds ls ln lt) _) (nm:cname) = traverseInstanceEntry' root root ((typeToName lt) ++ (nm:cname))
traverseInstanceEntry' root cur (nm:cname) = case [node | node <- subNodes cur, (localName . symbol) node == nm, (not $ isSYMFUNCNode node) || (not $ elem "static" ((symbolModifiers . symbol) node))] of
                                        []            -> []
                                        targets      -> concat $ map (\target -> traverseInstanceEntry' root target cname) targets

buildTypeEntryFromSymbol :: Symbol -> TypeNode
buildTypeEntryFromSymbol sym = TN sym []

buildTypeEntryFromEnvironments :: TypeNode -> [Environment] -> Maybe TypeNode
buildTypeEntryFromEnvironments tn [] = Just tn
buildTypeEntryFromEnvironments tn (env:envs) = case mtn of
                                                Nothing -> Nothing
                                                Just tn' -> buildTypeEntryFromEnvironments tn' envs
    where
        mtn = buildTypeEntry tn env

buildInstanceEntryFromEnvironments :: TypeNode -> [Environment] -> Maybe TypeNode
buildInstanceEntryFromEnvironments tn [] = Just tn
buildInstanceEntryFromEnvironments tn (env:envs) = case mtn of
                                                    Nothing -> Nothing
                                                    Just tn' -> buildInstanceEntryFromEnvironments tn' envs
    where
        mtn = buildInstanceEntry tn env

buildTypeEntry :: TypeNode -> Environment -> Maybe TypeNode
buildTypeEntry tn env = buildEntry tn env (elem "static")

buildInstanceEntry :: TypeNode -> Environment -> Maybe TypeNode
buildInstanceEntry tn env = buildEntry tn env (\mds -> True)

--watch out what is current node
buildEntry :: TypeNode -> Environment -> ([String] -> Bool) -> Maybe TypeNode
buildEntry tn ENVE cond = Just tn
buildEntry (TN sym nodes) env cond = case (sym, kind su) of
                            (PKG _, Package)  -> if isJust mpNode' then Just (TN sym ((fromJust mpNode'):remainNodes)) else Nothing
                            (PKG _, Class)  -> mcNode'
                            (PKG _, Interface)  -> mcNode'
                            _ -> Nothing
    where
        ENV su c = env
        ch = case c of
                [c'] -> c'
                _ -> error (show c)
        parent = inheritFrom su
        sym' = case symbolTable parent of
                [s] -> s
                a -> error (show (a, su))
        --[sym'] = symbolTable parent

        cname = (scope . semantic)  env
        nm = case cname of
            [] -> (show env)
            _ -> last cname
        --nm = (last . scope . semantic)  env
        remainNodes = [node | node <- nodes, (localName . symbol) node /= nm]
        conflict = [node | node <- nodes, (localName . symbol) node == nm]

        mcNode = buildEntry' sym' env cond
        mcNode' = case (conflict, isJust mcNode) of
                    ([], True) -> Just (TN sym ((fromJust mcNode):remainNodes))
                    _ -> Nothing

        mpNode' = case conflict of
                      [] -> buildEntry (TN (PKG nm) []) ch cond
                      [tar] -> buildEntry tar ch cond
                      _ -> Nothing

buildEntry' :: Symbol -> Environment -> ([String] -> Bool) -> Maybe TypeNode
buildEntry' sym ENVE cond = Nothing
buildEntry' sym env cond = case kind su of
                            Class -> Just (TN sym (map buildTypeEntryFromSymbol (funcs ++ sflds)))
                            Interface -> Just (TN sym (map buildTypeEntryFromSymbol (funcs)))
                            _ -> Nothing
    where
        ENV su ch = env
        syms = symbolTable su
        funcs = [(FUNC mds ls ln params lt) | (FUNC mds ls ln params lt) <- syms]
        sflds = [(SYM mds ls ln lt) | (SYM mds ls ln lt) <- syms, cond mds]

lookUpType :: TypeNode -> [[String]] -> [String] -> [[String]]
lookUpType db imps cname
    | length tpsExplicit > 0 = if length tpsFromPrefix > 0 then [] else tpsExplicit
    | length tpsSamePackage > 0 = tpsSamePackage
    | length tpsOnDemand > 1 = []
    | otherwise = tps'
        where
            self = head imps
            ps = map (\i -> (take i cname, drop i cname)) [1..(length cname)]
            tps' = traverseTypeEntryWithImports db imps cname
            tpsExplicit = nub [tp | tp <- tps', (elem tp imps) || (not $ elem ((init tp) ++ ["*"]) imps)]
            tpsFromPrefix = concat $ map (traverseTypeEntryWithImports db imps) (map fst (init ps))
            tpsSamePackage = nub [tp | tp <- tps', (init tp) == (init self)]
            tpsOnDemand = nub [tp | tp <- tps', not $ elem tp tpsExplicit]

refineTypeWithType :: TypeNode -> [[String]] -> Type -> Maybe Type
refineTypeWithType db imps (Object (Name nm)) = case lookUpType db imps nm of
                                                    [] -> Nothing
                                                    nm':_ -> Just (Object (Name nm'))
                                                    --r -> error (show (nm, r))
refineTypeWithType db imps (TypeClass (Name nm)) = case lookUpType db imps nm of
                                                    [] -> Nothing
                                                    nm':_ -> Just (TypeClass (Name nm'))
                                                    --r -> error (show (nm, r))
refineTypeWithType db imps (Array t) = case refineTypeWithType db imps t of
                                        Nothing -> Nothing
                                        Just t' -> Just (Array t')
refineTypeWithType db imps t = Just t

refineSymbolWithType :: TypeNode -> [[String]] -> Symbol -> Maybe Symbol
refineSymbolWithType db imps (SYM mds ls ln lt) = case refineTypeWithType db imps lt of
                                                Nothing -> Nothing
                                                Just lt' -> Just (SYM mds ls ln lt')
refineSymbolWithType db imps (CL mds ln lt unit) = case refineTypeWithType db imps lt of
                                                Nothing -> Nothing
                                                Just lt' -> Just (CL mds ln lt' unit)
refineSymbolWithType db imps (IT mds ln lt unit) = case refineTypeWithType db imps lt of
                                                Nothing -> Nothing
                                                Just lt' -> Just (IT mds ln lt' unit)
refineSymbolWithType db imps (FUNC mds ls ln params lt) = case (dropWhile isJust params', mlt') of
                                                        (_, Nothing) -> Nothing
                                                        ([], Just lt') -> Just (FUNC mds ls ln (map fromJust params') lt')
                                                        _ -> Nothing
    where
        params' = map (refineTypeWithType db imps) params
        mlt' = refineTypeWithType db imps lt
refineSymbolWithType db imps sym = Just sym

refineKindWithType  db imps (Method sym) = case refineSymbolWithType db imps sym of
                                            Nothing -> Nothing
                                            Just sym' -> Just (Method sym')
refineKindWithType  db imps (Field sym mexpr) = case refineSymbolWithType db imps sym of
                                            Nothing -> Nothing
                                            Just sym' -> Just (Field sym' mexpr)
refineKindWithType  db imps kd = Just kd

refineEnvironmentWithType :: TypeNode -> [[String]] -> SemanticUnit -> Environment -> Maybe Environment
refineEnvironmentWithType db imps _ ENVE = Just ENVE
refineEnvironmentWithType db imps parent (ENV su ch) = case (isJust k', dropWhile isJust syms', dropWhile isJust ch') of
                                                        (True, [], []) -> Just (ENV su' (map fromJust ch'))
                                                        _ -> Nothing
    where
        (SU cname k syms _) = su
        k' = refineKindWithType db imps k
        syms' = map (refineSymbolWithType db imps) syms
        su' = (SU cname (fromJust k') (map fromJust syms') parent)
        ch' = map (refineEnvironmentWithType db imps su') ch
