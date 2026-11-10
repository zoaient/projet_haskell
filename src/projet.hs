
import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck
import Data.Word (Word8,Word16)
import Data.Aeson (Value(Null))
import Data.List.Split


-- Stuctures réccursives

data Domain = 
    RootDom String |
    NodeDom String Domain


data Path = 
    RootPath String |
    NodePath String Path


data KeyVal = KeyVal String String

data Request = 
    RootReq KeyVal |
    NodeReq KeyVal Request


data URL = MkURL {
    protocole :: String,
    domain :: Domain,
    path :: Maybe Path,
    request :: Maybe Request,
    fragment :: Maybe String
}
    

-- Fonctions pour check si les strings sont valides

isLetter::Char -> Bool
isLetter c = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
 
isLetterOrNumber::Char -> Bool
isLetterOrNumber c = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9'))

onlyAlpha::String -> Bool
onlyAlpha [] = True
onlyAlpha (x:xs) | isLetter x  = onlyAlpha xs
onlyAlpha _ = False

onlyAlphaNum::String -> Bool
onlyAlphaNum [] = True
onlyAlphaNum (x:xs) | isLetterOrNumber x  = onlyAlphaNum xs
onlyAlphaNum _ = False


-- Fonction principale On sépare le protocole et on passe a l'étape suivante
-- Chaque étape garde dans ces parmaètres les résultats des fonctions précédentes
parseUrl:: String -> Either String URL
parseUrl str = case splitOn "://" str of
    [s1,_] | not (onlyAlpha s1) -> Left "Erreur domain"
    [s1,s2] -> parseUrl2 s1 s2

    _ -> Left "Erreur domain"


-- Par example parseUrl2 ici garde le protocole calculé dans parseUrl dans sont premier argument
-- parseUrl2 extarit le fragment
parseUrl2:: String -> String -> Either String URL
parseUrl2 prot str = case splitOn "#" str of
    [s1] -> parseUrl3 prot Nothing s1
    [_,s2] | not (onlyAlphaNum s2) -> Left "Fragment Invalide"
    [s1,s2] -> parseUrl3 prot (Just s2) s1
    _ -> Left "Fragment Invalide"


-- Fonction qui prend un tableau de String de requete et en fait une type Request (ou renvoie une erreur)
parseSplitedRequest:: [String] -> Either String Request
parseSplitedRequest tab = case tab of
    [s1]  -> case splitOn "=" s1 of
        [s1,s2] | not (onlyAlphaNum s1) || not (onlyAlphaNum s2)  -> Left "Erreur request"
        [s1,s2] -> Right (RootReq (KeyVal s1 s2))
        _ -> Left "Erreur request"
    (x:xs) -> case splitOn "=" x of
        [s1,s2] | not (onlyAlphaNum s1) || not (onlyAlphaNum s2)  -> Left "Erreur request"
        [s1,s2] -> 
            let next = parseSplitedRequest xs in
                case next of
                    Left err -> Left err
                    Right obj -> Right (NodeReq (KeyVal s1 s2) obj)
        _ -> Left "Erreur request"
    _ -> Left "Erreur request"


--Sépare les requetes mise ensebles et renvoie un objet Request (ou Erreur)
parseRequest:: String -> Either String Request
parseRequest str = parseSplitedRequest (splitOn "&" str)



-- Extarit les requetes de l'url puis passe a l'étape suivante
parseUrl3:: String -> Maybe String -> String->Either String URL
parseUrl3 prot frag str = case splitOn "?" str of 
    [s1] -> parseUrl4 prot frag Nothing s1
    [s1,s2] -> 
        let next = parseRequest s2 in
            case next of
                Left err -> Left err
                Right req -> parseUrl4 prot frag (Just req) s1
    _ -> Left "Erreur request"


-- Meme chose que pour parseSplitedRequest mais pour les domaines
parseSplitedDomain:: [String] -> Either String Domain
parseSplitedDomain tab = case tab of
    [s1] | not (onlyAlpha s1) -> Left "Erreur Domaine"
    [s1] -> Right (RootDom s1)
    (x:_) | not (onlyAlpha x) -> Left "Erreur Domaine"
    (x:xs) ->
        let next = parseSplitedDomain xs in
            case next of
                Left err -> Left err
                Right obj -> Right (NodeDom x obj)
    _ -> Left "Erreur Domaine"


parseSplitedPath:: [String] -> Either String Path
parseSplitedPath tab = case tab of
    [s1] | not (onlyAlpha s1) -> Left "Erreur Path"
    [s1] -> Right (RootPath s1)
    [s1,""] -> Right (RootPath s1)
    (x:_) | not (onlyAlpha x) -> Left "Erreur Path"
    ("":xs)-> parseSplitedPath xs
    (x:xs) ->
        let next = parseSplitedPath xs in
            case next of
                Left err -> Left err
                Right obj -> Right (NodePath x obj)
    _ -> Left "Erreur Path"

-- Fonction qui assemble touts les objet dans la structure URL
assemble:: String -> Maybe String -> Maybe Request -> Domain -> Maybe Path -> URL
assemble prot frag req dom path = MkURL{protocole = prot, domain = dom, path = path,request = req,fragment = frag}

-- Dèrnière étape on extarit le domaine et chemin
parseUrl4::String -> Maybe String -> Maybe Request -> String -> Either String URL
parseUrl4 prot frag req str = case splitOn "/" str of 
    [s1] -> 
        let next = parseSplitedDomain (splitOn "." s1) in
            case next of
                Left err -> Left err
                Right dom -> Right (assemble prot frag req dom Nothing)
    [s1,""] -> 
        let next = parseSplitedDomain (splitOn "." s1) in
            case next of
                Left err -> Left err
                Right dom -> Right (assemble prot frag req dom Nothing)
    (x:xs) ->
        let next = parseSplitedDomain (splitOn "." x) in
            let next2 = parseSplitedPath xs in
                case (next,next2) of
                    (Left err,_) -> Left err
                    (_,Left err) -> Left err
                    (Right dom,Right path) -> Right (assemble prot frag req dom (Just path))
    _ -> Left "Erreur Path + Domain "



-- Fonction Shows
instance Show Domain where
    show d = case d of
        RootDom s -> s
        NodeDom x xs -> x++"."++(show xs)

instance Show Path where
    show p = case p of
        RootPath s -> "/" ++ s
        NodePath s xs -> "/"++s++(show xs)

instance Show Request where
    show r = case r of 
        RootReq (KeyVal k v) -> k++"="++v
        NodeReq (KeyVal k v) xs -> k++"="++v++"&"++(show xs)



instance Show URL where
    show url = case (url.path,url.request,url.fragment) of
        (Nothing,Nothing,Nothing) -> ( url.protocole)++"://"++(show url.domain)
        (Nothing,Nothing,Just c) -> ( url.protocole)++"://"++(show url.domain)++"#"++c
        (Nothing,Just b,Nothing) -> ( url.protocole)++"://"++(show url.domain)++"?"++(show b)
        (Nothing,Just b,Just c) -> ( url.protocole)++"://"++(show url.domain)++"?"++(show b)++"#"++(c)
        (Just a,Nothing,Nothing) -> ( url.protocole)++"://"++(show url.domain)++(show a)
        (Just a,Nothing,Just c) -> ( url.protocole)++"://"++(show url.domain)++(show a)++"#"++(c)
        (Just a,Just b,Nothing) -> ( url.protocole)++"://"++(show url.domain)++(show a)++"?"++(show b)
        (Just a,Just b,Just c) -> ( url.protocole)++"://"++(show url.domain)++(show a)++"?"++(show b)++"#"++(c)
        

-- Tests Quickcheck

listWithLength :: (Int, Int) -> Gen a -> Gen [a]
listWithLength (minLen, maxLen) gen = do
  len <- choose (minLen, maxLen)
  vectorOf len gen

genAlphaWithLength :: (Int, Int) -> Gen String
genAlphaWithLength lenRange =
  listWithLength lenRange (elements ['a'..'z'])

genSchemeStr :: Gen String
genSchemeStr = elements ["http", "https", "ftp", "ssh"]

genDomainNameStr :: Gen String
genDomainNameStr = do
  tld <- genAlphaWithLength (2, 4)
  sld <- genAlphaWithLength (2, 10)
  subdomains <- frequency [
      (3, return []),
      (1, listWithLength (1, 5) $ genAlphaWithLength (2, 10))
    ]
  let subdomainsString = foldr (\sub acc -> acc ++ sub ++ ".") "" subdomains
  return $ subdomainsString ++ sld ++ "." ++ tld

genPathStr :: Gen String
genPathStr = frequency [
    (3, return ""),
    (1, do
      segments <- listWithLength (1, 5) $ genAlphaWithLength (2, 10)
      return $ foldr (\seg acc -> "/" ++ seg ++ acc) "" segments
    )
  ]

genRequestStr :: Gen String
genRequestStr = frequency [
    (3, return ""),
    (1, do
      k <- genAlphaWithLength (2, 10)
      v <- genAlphaWithLength (2, 10)
      rest <- listWithLength (0, 4) $ do
        key <- genAlphaWithLength (2, 10)
        value <- genAlphaWithLength (2, 10)
        return (key, value)
      return $ "?" ++ k ++ "=" ++ v ++ foldr (\(key, value) acc -> "&" ++ key ++ "=" ++ value ++ acc) "" rest
    )
  ]

genFragmentStr :: Gen String
genFragmentStr =
  frequency [
    (3, return ""),
    (1, ("#" ++) <$> genAlphaWithLength (2, 10))
  ]


-- Generateur d'url
genUrlStr :: Gen String
genUrlStr = do
  schemeStr <- genSchemeStr
  domainStr <- genDomainNameStr
  pathStr <- genPathStr
  requestStr <- genRequestStr
  fragmentStr <- genFragmentStr
  return $ schemeStr ++ "://" ++ domainStr ++ pathStr ++ requestStr ++ fragmentStr


-- Propriété a tester
testproprety::String -> Bool
testproprety s =
    let check = parseUrl s in 
        case check of
            Left _ -> False
            Right res -> (show res) == s 

-- Part2

-- Stuctures (récursives)

data ProtocolFilter = Star | Name String
    deriving (Show)

data DomainFilterType = StarDom | NameDom String
    deriving (Show)

data DomainFilterTree = 
    RootFil DomainFilterType |
    NodeFil DomainFilterType DomainFilterTree
    deriving (Show)

data DomainFilter = 
    DoubleStar DomainFilterTree|
    NoDoubleStar DomainFilterTree |
    JustDoubleStar
    deriving (Show)

data Filter = MkFilter{
    protocolFilter::ProtocolFilter,
    domainFilter::DomainFilter
    
}   deriving (Show)


-- Parser pour le filtre
parseFilter :: String -> Either String Filter
parseFilter str = case splitOn "://" str of
    [s1,s2] -> do
        res1 <- parseProtocolFilter s1
        res2 <- parseDomainFilterStar s2
        Right MkFilter{protocolFilter = res1,domainFilter = res2}

    _ -> Left "Error"

-- Parser pour le filtre de protocol
parseProtocolFilter :: String -> Either String ProtocolFilter
parseProtocolFilter s = case s of
    "*" -> Right Star
    s | not (onlyAlpha s) -> Left "Error Protocol"
    s -> Right (Name s)  



parseDomainFilter:: [String] -> Either String DomainFilterTree
parseDomainFilter s = case s of 
    ["*"] -> Right (RootFil StarDom)
    [x] | not (onlyAlpha x) -> Left "Error Domain"
    [x] -> Right (RootFil (NameDom x))
    ("*":xs) -> (\valid -> NodeFil StarDom valid) <$> (parseDomainFilter xs)
    (x:_) | not (onlyAlpha x) -> Left "Error Domain"
    (x:xs) -> (\valid -> NodeFil (NameDom x) valid) <$> (parseDomainFilter xs)
    _ -> Left "Error Domain"


-- Parser pour le filtre du domaine
parseDomainFilterStar:: String -> Either String DomainFilter
parseDomainFilterStar s = case splitOn "." s of
    ["**"] -> Right JustDoubleStar
    ("**":xs) -> (\valid -> DoubleStar valid) <$> (parseDomainFilter xs)
    x -> (\valid -> NoDoubleStar valid) <$> (parseDomainFilter x)

-- Main: Effectue les tests et parse 2 urls
main :: IO ()
main = do
    putStrLn (show (parseFilter "*://**.hello.*.com") ++ show (parseUrl "http://google.com/hello/world?key1=val1&bonjour=aurevoir#5555"))
    quickCheck (forAll genUrlStr testproprety)