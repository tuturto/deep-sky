{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Names ( greekMasculineNameM, greekFemineNameM, greekMasculineNames
             , greekFemineNames, greekNameM, greekNames, petNameM, petNames )
    where

import Import
import Control.Monad.Random ( Rand )
import qualified Prelude as P
import Data.List ( nub )
import Data.Text ( chunksOf, toTitle )
import System.Random

import Common ( safeTail )
import Markov ( Config, chainM, addStart, addLink, addEnd, emptyConfig
              , chains )
import People.Data ( FirstName(..), PetName(..) )


-- | Generate random name wrapped in specific type
name :: RandomGen g => (Text -> b) -> Config Text -> Rand g b
name t config = do
    s <- chainM config
    return $ (t . toTitle . concat) s


-- | Generate random names wrapped in specific type
names :: (RandomGen g, Eq b) => (Text -> b) -> Config Text -> g -> [b]
names t config g =
    nub $ (t . toTitle . concat) <$> chains config g


-- | Generated greek masculine name
greekMasculineNameM :: (RandomGen g) => Rand g FirstName
greekMasculineNameM =
    name FirstName greekMasculineConfig


-- | Infinite amount of greek masculine names
greekMasculineNames :: (RandomGen g) => g -> [FirstName]
greekMasculineNames =
    names FirstName greekMasculineConfig


-- | Generated greek femine name
greekFemineNameM :: (RandomGen g) => Rand g FirstName
greekFemineNameM =
    name FirstName greekFemineConfig


-- | Infinite amount of greek femine names
greekFemineNames :: (RandomGen g) => g -> [FirstName]
greekFemineNames =
    names FirstName greekFemineConfig


-- | Generated greek name
greekNameM :: (RandomGen g) => Rand g FirstName
greekNameM =
    name FirstName greekNameConfig


-- | Infinite amount of greek names
greekNames :: (RandomGen g) => g -> [FirstName]
greekNames =
    names FirstName greekNameConfig


-- | Generated pet name
petNameM :: (RandomGen g) => Rand g PetName
petNameM =
    name MkPetName greekNameConfig


-- | Infinite amount of pet names
petNames :: (RandomGen g) => g -> [PetName]
petNames =
    names MkPetName greekNameConfig


-- | Markov chain configuration for greek masculine names
greekMasculineConfig :: Config Text
greekMasculineConfig =
    nameConfig masculineGreek 2


-- | Markov chain configuration for greek femine names
greekFemineConfig :: Config Text
greekFemineConfig =
    nameConfig femineGreek 2


-- | Markov chain configuration for greek names
greekNameConfig :: Config Text
greekNameConfig =
    nameConfig (masculineGreek ++ femineGreek) 2


-- | Markov chain configuration for names with given element size
nameConfig :: [Text] -> Int -> Config Text
nameConfig xs n =
    foldr (addName n) emptyConfig xs


-- | Add new name into configuration with given element size
addName :: Int -> Text -> Config Text -> Config Text
addName n s config =
    links pairs $
            end elements $
            start elements config
    where
        elements = chunksOf n s
        pairs = zip elements (safeTail elements)


-- | Add first element of given list of elements as starting element into config
start :: [Text] -> Config Text -> Config Text
start [] config =
    config

start (x:_) config =
    addStart x config


-- | Given list of (previous, next) elements and config, create new config with them
-- added into it
links :: [(Text, Text)] -> Config Text -> Config Text
links xs config =
   foldr (\(prev, nxt) -> addLink prev nxt) config xs


-- | Add last element of given list of elements as ending element into config
end :: [Text] -> Config Text -> Config Text
end [] config =
    config

end xs config =
    addEnd (P.last xs) config


-- | Examples of greek masculine names
masculineGreek :: [Text]
masculineGreek =
    [ "acacius", "achaikos", "aeschylus", "aesop", "agapetos", "agapetus", "agapios"
    , "agathon", "akakios", "alcaeus", "alcibiades", "alexander", "alexandros", "alexios"
    , "alexis", "alexius", "alkaios", "alkibiades", "ambrosios", "ambrosius", "ampelios"
    , "ampelius", "amyntas", "anacletus", "anakletos", "anastasios", "anastasius"
    , "anatolios", "anatolius", "anaxagoras", "andreas", "androcles", "androkles"
    , "andronicus", "andronikos", "anicetus", "aniketos", "antigonos", "antigonus"
    , "antiochos", "antiochus", "antipater", "antipatros", "aphrodisios", "apollinaris"
    , "apollodoros", "apollonios", "arcadius", "archelaos", "archelaus", "archimedes"
    , "archippos", "argyros", "aristarchos", "aristarchus", "aristeides", "aristides"
    , "aristocles", "aristodemos", "aristokles", "ariston", "aristophanes", "aristoteles"
    , "aristotle", "arkadios", "arsenios", "arsenius", "artemidoros", "artemios"
    , "artemisios", "artemius", "artemon", "asklepiades", "athanas", "athanasios"
    , "athanasius", "auxentios", "auxentius", "basileios", "basilius", "bion", "callias"
    , "cassander", "chares", "chariton", "chrysanthos", "cleisthenes", "cleitus"
    , "cleon", "clitus", "cosmas", "cyrillus", "cyrus", "damianos", "damianus"
    , "dareios", "demetrios", "demetrius", "democritus", "demokritos", "demon"
    , "demosthenes", "diocles", "diodoros", "diodorus", "diodotos", "diodotus"
    , "diogenes", "diokles", "dion", "dionysios", "dionysius", "dionysodoros", "draco"
    , "drakon", "eirenaios", "epaphras", "epaphroditos", "epiktetos", "epiphanes"
    , "epiphanios", "epiphanius", "erasmos", "erastos", "euaristos", "euclid"
    , "eugenios", "eugenius", "eukleides", "euphemios", "euphranor", "euripides"
    , "eusebios", "eusebius", "eustachys", "eustathios", "eustathius", "eustorgios"
    , "eustorgius", "euthymios", "euthymius", "eutropios", "eutropius", "eutychios"
    , "eutychius", "eutychos", "evaristus", "gaios", "galenos", "gennadios"
    , "gennadius", "georgios", "georgius", "heliodoros", "heracleitus"
    , "heraclius", "herakleides", "herakleios", "herakleitos", "hermes", "hermogenes"
    , "hermokrates", "hermolaos", "hero", "herodes", "herodion", "herodotos"
    , "herodotus", "heron", "hesiod", "hesiodos", "hesperos", "hieronymos", "hieronymus"
    , "hilarion", "hippocrates", "hippokrates", "hippolytos", "homer", "homeros"
    , "hyacinthus", "hyakinthos", "hyginos", "hyginus", "hypatos", "iason", "irenaeus"
    , "ireneus", "isidoros", "isocrates", "isokrates", "kallias", "kallikrates"
    , "kallistos", "karpos", "kassandros", "kleisthenes", "kleitos", "kleon"
    , "kleopatros", "kosmas", "kyriakos", "kyrillos", "kyros", "leon", "leonidas"
    , "leontios", "leontius", "linos", "linus", "loukianos", "loukios", "lycurgus"
    , "lycus", "lykos", "lykourgos", "lysander", "lysandros", "lysimachos"
    , "lysimachus", "markos", "melanthios", "meliton", "methodios", "methodius"
    , "metrophanes", "miltiades", "mnason", "myron", "neophytos", "nereus", "nicanor"
    , "nicolaus", "nicomedes", "nicostratus", "nikandros", "nikanor", "nikephoros"
    , "niketas", "nikias", "nikodemos", "nikolaos", "nikomachos", "nikomedes"
    , "nikon", "nikostratos", "olympiodoros", "olympos", "onesimos", "onesiphoros"
    , "origenes", "pamphilos", "pancratius", "pankratios", "pantaleon", "panther"
    , "pantheras", "paramonos", "pelagios", "pelagius", "pericles", "perikles"
    , "phaedrus", "phaidros", "philandros", "philippos", "philo", "philokrates"
    , "philon", "philotheos", "phocas", "phoibos", "phokas", "photios", "plato"
    , "platon", "ploutarchos", "polycarp", "polykarpos", "porphyrios", "praxiteles"
    , "prochoros", "prokopios", "ptolemaios", "pyrrhos", "pyrrhus", "pythagoras"
    , "seleucus", "seleukos", "simonides", "socrates", "sokrates", "solon", "sophocles"
    , "sophokles", "sophos", "sophus", "sosigenes", "stephanos", "straton"
    , "telesphoros", "telesphorus", "thales", "themistocles", "themistokles"
    , "theocritus", "theodoros", "theodorus", "theodosios", "theodosius", "theodotos"
    , "theodotus", "theodoulos", "theodulus", "theokritos", "theophanes", "theophilos"
    , "theophilus", "theophylaktos", "theron", "thoukydides", "thucydides"
    , "timaeus", "timaios", "timon", "timoteus", "timotheos", "tryphon", "tycho"
    , "tychon", "xanthippos", "xenocrates", "xenokrates", "xenon", "xenophon", "zeno"
    , "zenobios", "zenon", "zephyros", "zopyros", "zosimos", "zosimus", "zoticus"
    , "zotikos"
    ]


-- | Examples of greek femine names
femineGreek :: [Text]
femineGreek =
    [ "agape", "agatha", "agathe", "agnes", "aikaterine", "alexandra", "alexis"
    , "ambrosia", "anastasia", "anthousa", "aphrodisia", "apollonia", "aristomache"
    , "artemisia", "aspasia", "athanasia", "athenais", "berenice", "berenike"
    , "charis", "charmion", "chloe", "chrysanthe", "cleopatra", "corinna", "demetria"
    , "demostrate", "doris", "eirene", "elpis", "euanthe", "eudocia", "eudokia"
    , "eudoxia", "eugeneia", "eugenia", "eulalia", "eumelia", "eunike", "euphemia"
    , "euphrasia", "eupraxia", "euthalia", "euthymia", "eutropia", "eutychia"
    , "gaiana", "gaiane", "galene", "hagne", "helena", "helene", "hypatia", "irene"
    , "isidora", "kallisto", "kallistrate", "kassandra", "kleopatra", "korinna"
    , "ligeia", "lysandra", "lysistrata", "lysistrate", "melissa", "melitta"
    , "menodora", "metrodora", "myrrine", "nike", "nikephoros", "nymphodora"
    , "olympias", "pelagia", "pherenike", "phile", "phoibe", "photina", "photine"
    , "ptolemais", "rhode", "roxana", "roxane", "sappho", "sophia", "sostrate"
    , "syntyche", "thais", "theodora", "theodosia", "theokleia", "theophania"
    , "theophila", "timo", "timothea", "tryphaina", "tryphosa", "xanthe", "xanthippe"
    , "xenia", "xeno", "zenais", "zenobia", "zoe", "zosime"]
