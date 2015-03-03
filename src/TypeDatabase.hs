module TypeDatabase where

import Data.Maybe
import Data.List

import Util
import AST
import Environment

data TypeNode = TN {
    symbol :: Symbol,
    subNodes :: [TypeNode]
}

isVisibleClassNode tn = case symbol tn of
                            PKG _ -> True
                            CL _ _ -> True
                            IT _ _ -> True
                            _ -> False

isConcreteNode tn = case symbol tn of
                            CL _ _ -> True
                            IT _ _ -> True
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
                            CL _ _ -> Just (TN (PKG []) [tn])
                            IT _ _ -> Just (TN (PKG []) [tn])
                            _ -> Nothing
traverseTypeEntry (TN sym nodes) ["*"] = Just (TN (PKG []) (filter isConcreteNode nodes))
traverseTypeEntry (TN sym nodes) (nm:remain) = case [node | node <- nodes, (localName . symbol) node == nm] of
                                                [] -> Nothing
                                                [node] -> traverseTypeEntry node remain

traverseTypeEntryWithImports :: TypeNode -> [[String]] -> [String] -> [[String]]
traverseTypeEntryWithImports tn imps query = case dropWhile (isNothing . fst) results of
                                                [] -> []
                                                results' -> map (\(Just node, imp) -> (init imp) ++ [(localName . symbol) node]) results'
    where
        entries = map (traverseTypeEntry tn) imps
        entries' = map (\(mnode, imp) -> (fromJust mnode, imp)) (filter (isJust . fst) (zip entries imps))
        results = map (\(node, imp) -> (traverseTypeEntry node query, imp)) ((tn, ["*"]):entries')

traverseInstanceEntry :: TypeNode -> [TypeNode] -> [String] -> [TypeNode]
traverseInstanceEntry root nodes cname = [node | Just node <- mnodes']
    where
        mnodes' = map (\cur -> traverseInstanceEntry' root cur cname) nodes

traverseInstanceEntry' :: TypeNode -> TypeNode -> [String] -> Maybe TypeNode
traverseInstanceEntry' root (TN (SYM mds ln lt) _) cname = traverseInstanceEntry' root root ((typeToName lt) ++ cname)
traverseInstanceEntry' root cur cname = case [node | node <- subNodes cur, (localName . symbol) node == nm] of
                                        []            -> Nothing
                                        [target]      -> Just target
    where
        nm = head cname

buildTypeEntryFromSymbol sym = TN sym []

buildTypeEntryFromEnvironments tn [] = Just tn
buildTypeEntryFromEnvironments tn (env:envs) = case mtn of
                                                Nothing -> Nothing
                                                Just tn' -> buildTypeEntryFromEnvironments tn' envs
    where
        mtn = buildTypeEntry tn env

buildTypeEntry :: TypeNode -> Environment -> Maybe TypeNode
buildTypeEntry tn env = buildEntry tn env (elem "static")

buildInstanceEntry :: TypeNode -> Environment -> Maybe TypeNode
buildInstanceEntry tn env = buildEntry tn env (\mds -> True)

buildEntry :: TypeNode -> Environment -> ([String] -> Bool) -> Maybe TypeNode
buildEntry tn ENVE cond = Just tn
buildEntry (TN sym nodes) env cond = case (sym, nodes, kind su) of
                            (PKG _, _, Package)  -> if isJust tarNode' then Just (TN sym nodes') else Nothing
                            (PKG _, [], Class)  -> buildEntry' sym' env cond
                            (PKG _, _, Class)  -> Nothing
                            (PKG _, [], Interface)  -> buildEntry' sym' env cond
                            (PKG _, _, Interface)  -> Nothing
                            _ -> Nothing
    where
        ENV su c = env
        ch = case c of
                [c'] -> c'
                _ -> error (show c)
        parent = inheritFrom su
        [sym'] = symbolTable parent
        
        cname = (scope . semantic)  env
        nm = case cname of
            [] -> (show env)
            _ -> last cname
        --nm = (last . scope . semantic)  env
        remainNodes = [node | node <- nodes, (localName . symbol) node /= nm]
        tarNode = case [node | node <- nodes, (localName . symbol) node == nm] of
            []            -> TN (PKG nm) []
            [target]      -> target
        tarNode' = buildTypeEntry tarNode ch
        nodes' = (fromJust tarNode'):remainNodes

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

refineTypeWithType :: ([String] -> Maybe [String]) -> Type -> Maybe Type
refineTypeWithType querier (Array t) = case refineTypeWithType querier t of
                                        Nothing -> Nothing
                                        Just t' -> Just (Array t')
refineTypeWithType querier (Object (Name nm)) = case querier nm of
                                                    Nothing -> Nothing
                                                    Just nm' -> Just (Object (Name nm'))
refineTypeWithType querier t = Just t

refineSymbolWithType :: ([String] -> Maybe [String]) -> Symbol -> Maybe Symbol
refineSymbolWithType querier (SYM mds ln lt) = case refineTypeWithType querier lt of
                                                Nothing -> Nothing
                                                Just lt' -> Just (SYM mds ln lt')
refineSymbolWithType querier (FUNC mds ln params lt) = case (dropWhile isJust params', lt') of
                                                        ([], _) -> Nothing
                                                        (_, Nothing) -> Nothing
                                                        _ -> Just (FUNC mds ln (map fromJust params') (fromJust lt'))
    where
        params' = map (refineTypeWithType querier) params
        lt' = refineTypeWithType querier lt
refineSymbolWithType querier sym = Just sym

refineEnvironmentWithType :: ([String] -> Maybe [String]) -> SemanticUnit -> Environment -> Maybe Environment
refineEnvironmentWithType querier _ ENVE = Just ENVE
refineEnvironmentWithType querier parent (ENV su ch) = case (dropWhile isJust syms', dropWhile isJust ch') of
                                                        ([], []) -> Just (ENV su' (map fromJust ch'))
                                                        _ -> Nothing
    where
        (SU cname k syms _) = su
        syms' = map (refineSymbolWithType querier) syms
        su' = (SU cname k (map fromJust syms') parent)
        
        ch' = map (refineEnvironmentWithType querier su') ch