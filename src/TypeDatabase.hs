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

nativeTypes = TN (PKG []) [TN (PKG "joosc native") [TN (CL ["public"] "Array" (TypeClass (Name ["joosc native", "Array"]))) [TN (SYM ["public"] "length" TypeInt) []]]]

isVisibleClassNode tn = case symbol tn of
                            PKG _ -> True
                            CL _ _ _ -> True
                            IT _ _ _ -> True
                            FUNC _ _ _ _ -> False
                            _ -> True -- False -> True

isConcreteNode tn = case symbol tn of
                            CL _ _ _ -> True
                            IT _ _ _ -> True
                            _ -> False

instance Show TypeNode where
    show (TN sym nodes) =   "{\n" ++
                            "  " ++ (show sym) ++ "\n" ++
                            (indent 2 (intercalate "\n" lns)) ++ "\n" ++
                            "}\n"
        where
            lns = map show (filter isVisibleClassNode nodes)

traverseTypeEntry :: TypeNode -> [String] -> Maybe TypeNode
traverseTypeEntry tn [] = case symbol tn of
                            CL _ _ _ -> Just (TN (PKG []) [tn])
                            IT _ _ _ -> Just (TN (PKG []) [tn])
                            _ -> Nothing
traverseTypeEntry (TN sym nodes) ["*"] = Just (TN (PKG []) (filter isConcreteNode nodes))
traverseTypeEntry (TN sym nodes) (nm:remain) = case [node | node <- nodes, (localName . symbol) node == nm] of
                                                [] -> Nothing
                                                [node] -> traverseTypeEntry node remain

traverseFieldEntryWithImports :: TypeNode -> [[String]] -> [String] -> [TypeNode]
traverseFieldEntryWithImports tn imps query = nub . concat $ (flds ++ funcs)
    where
        entries = map (traverseTypeEntry tn) imps
        entries' = map (\(mnode, imp) -> (fromJust mnode, imp)) (filter (isJust . fst) (zip entries imps))
        results = map (\(node, imp) -> (traverseTypeEntry node (init query), (init imp) ++ (init query))) ((tn, ["*"]):entries')
        fld = last query
        flds = [[TN (SYM mds ln lt) ch | TN (SYM mds ln lt) ch <- subNodes node, elem "static" mds, ln == fld] | (Just (TN _ [node]), cname) <- results]
        funcs = [[TN (FUNC mds ln ps rt) ch | TN (FUNC mds ln ps rt) ch <- subNodes node, ln == fld] | (Just (TN _ [node]), cname) <- results]

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
traverseInstanceEntry' root (TN (SYM mds ln lt) _) (nm:cname) = traverseInstanceEntry' root root ((typeToName lt) ++ (nm:cname))
traverseInstanceEntry' root cur (nm:cname) = case [node | node <- subNodes cur, (localName . symbol) node == nm] of
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
        funcs = [(FUNC mds ln params lt) | (FUNC mds ln params lt) <- syms]
        sflds = [(SYM mds ln lt) | (SYM mds ln lt) <- syms, cond mds]

refineTypeWithType :: ([String] -> [[String]]) -> Type -> Maybe Type
refineTypeWithType querier (Object (Name nm)) = case querier nm of
                                                    [] -> Nothing
                                                    [nm'] -> Just (Object (Name nm'))
                                                    r -> error (show (nm, r))
refineTypeWithType querier (Array t) = case refineTypeWithType querier t of
                                        Nothing -> Nothing
                                        Just t' -> Just (Array t')
refineTypeWithType querier t = Just t

refineSymbolWithType :: ([String] -> [[String]]) -> Symbol -> Maybe Symbol
refineSymbolWithType querier (SYM mds ln lt) = case refineTypeWithType querier lt of
                                                Nothing -> Nothing
                                                Just lt' -> Just (SYM mds ln lt')
refineSymbolWithType querier (CL mds ln lt) = case refineTypeWithType querier lt of
                                                Nothing -> Nothing
                                                Just lt' -> Just (CL mds ln lt')
refineSymbolWithType querier (IT mds ln lt) = case refineTypeWithType querier lt of
                                                Nothing -> Nothing
                                                Just lt' -> Just (IT mds ln lt')
refineSymbolWithType querier (FUNC mds ln params lt) = case (dropWhile isJust params', mlt') of
                                                        (_, Nothing) -> Nothing
                                                        ([], Just lt') -> Just (FUNC mds ln (map fromJust params') lt')
                                                        _ -> Nothing
    where
        params' = map (refineTypeWithType querier) params
        mlt' = refineTypeWithType querier lt
refineSymbolWithType querier sym = Just sym

refineEnvironmentWithType :: ([String] -> [[String]]) -> SemanticUnit -> Environment -> Maybe Environment
refineEnvironmentWithType querier _ ENVE = Just ENVE
refineEnvironmentWithType querier parent (ENV su ch) = case (dropWhile isJust syms', dropWhile isJust ch') of
                                                        ([], []) -> Just (ENV su' (map fromJust ch'))
                                                        _ -> Nothing
    where
        (SU cname k syms _) = su
        syms' = map (refineSymbolWithType querier) syms
        su' = (SU cname k (map fromJust syms') parent)        
        ch' = map (refineEnvironmentWithType querier su') ch
