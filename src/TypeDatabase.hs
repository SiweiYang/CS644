module TypeDatabase where

import Data.Maybe
import Data.List

import Util
import Environment

data TypeNode = TN {
    symbol :: Symbol,
    subNodes :: [TypeNode]
}

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
            lns = map show nodes

traverseTypeEntry :: TypeNode -> [String] -> Maybe TypeNode
traverseTypeEntry tn [] = case symbol tn of
                            CL _ _ -> Just (TN (PKG []) [tn])
                            IT _ _ -> Just (TN (PKG []) [tn])
                            _ -> Nothing
traverseTypeEntry (TN sym nodes) ["*"] = Just (TN (PKG []) (filter isConcreteNode nodes))
traverseTypeEntry (TN sym nodes) (nm:remain) = case [node | node <- nodes, (localName . symbol) node == nm] of
                                                [] -> Nothing
                                                [node] -> traverseTypeEntry node remain

traverseTypeEntryWithImports :: TypeNode -> [[String]] -> [String] -> Maybe [String]
traverseTypeEntryWithImports tn imps query = case dropWhile (isNothing .fst) results of
                                                [] -> Nothing
                                                (Just (TN sym [node]), imp):remain -> Just ((init imp) ++ query)
    where
        entries = map (traverseTypeEntry tn) imps
        entries' = map (\(mnode, imp) -> (fromJust mnode, imp)) (filter (isJust . fst) (zip entries imps))
        results = map (\(node, imp) -> (traverseTypeEntry node query, imp)) ((tn, ["*"]):entries')

buildTypeEntryFromSymbol sym = TN sym []

buildTypeEntryFromEnvironments tn [] = Just tn
buildTypeEntryFromEnvironments tn (env:envs) = case mtn of
                                                Nothing -> Nothing
                                                Just tn' -> buildTypeEntryFromEnvironments tn' envs
    where
        mtn = buildTypeEntry tn env

buildTypeEntry :: TypeNode -> Environment -> Maybe TypeNode
buildTypeEntry tn ENVE = Just tn
buildTypeEntry (TN sym nodes) env = case (sym, nodes, kind su) of
                            (PKG _, _, Package)  -> if isJust tarNode' then Just (TN sym nodes') else Nothing
                            (PKG _, [], Class)  -> buildTypeEntry' sym' env
                            (PKG _, _, Class)  -> Nothing
                            (PKG _, [], Interface)  -> buildTypeEntry' sym' env
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
        

buildTypeEntry' :: Symbol -> Environment -> Maybe TypeNode
buildTypeEntry' sym ENVE = Nothing
buildTypeEntry' sym env = case kind su of
                            Class -> Just (TN sym (map buildTypeEntryFromSymbol (funcs ++ sflds)))
                            Interface -> Just (TN sym (map buildTypeEntryFromSymbol (funcs)))
                            _ -> Nothing
    where
        ENV su ch = env
        syms = symbolTable su
        funcs = [(FUNC mds ln params lt) | (FUNC mds ln params lt) <- syms]
        sflds = [(SYM mds ln lt) | (SYM mds ln lt) <- syms, elem "static" mds]
