{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-| Data types for reports
-}
module Reports.Core(
  Report(..),
  ReportSection(..),
  Content(..),
  contentToHtml,
  toRevealHtml,
  renderToFile
) where

import           Control.Applicative     ((<|>))
import           Control.Monad.State     (MonadState, evalState, get, put)
import           Data.Aeson              (FromJSON (..), Value (..), encode,
                                          withObject, (.:))
import           Data.Foldable           (traverse_)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as Text.Lazy
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy.IO       as Text.IO
import           Lucid                   (Html)
import qualified Lucid                   as L
import           NeatInterpolation       (text)
import           Reports.Visualisation   (VegaLiteGraph (..))

data Report anchor content =
  Report
    { rpTitle    :: String
    , rpSubtitle :: Maybe String
    , rpSections :: [ReportSection anchor content]
    }
    deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Subsection anchor content =
  Subsection
    { ssTitle   :: Maybe String
    , ssAnchor  :: anchor
    , ssContent :: content
    }
    deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance FromJSON content => FromJSON (Subsection () content) where
  parseJSON =
    withObject "Subsection" $ \object -> do
      Subsection
        <$> (object .: "title" <|> pure Nothing)
        <*> pure ()
        <*> object .: "content"

data ReportSection anchor content =
  ReportSection
    { rsTitle       :: String
    , rsSubtitle    :: Maybe String
    , rsAnchor      :: anchor
    , rsSubsections :: [Subsection anchor content]
    }
    deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance FromJSON content => FromJSON (ReportSection () content) where
  parseJSON =
    withObject "reportSection" $ \object -> do
      ReportSection
        <$>  object .: "title"
        <*> (object .: "subtitle" <|> pure Nothing)
        <*>  pure ()
        <*>  object .: "subsections"

instance FromJSON content => FromJSON (Report () content) where
  parseJSON =
    withObject "report" $ \object -> do
      Report
        <$> object .: "title"
        <*> (object .: "subtitle" <|> pure Nothing)
        <*> object .: "sections"

meta :: Text -> Text -> Html ()
meta a t = L.meta_ [L.name_ a, L.content_ t]

newtype GenState = GenState Int -- Counting the IDs

newtype ElementID = ElementID{ getElementId :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype L.ToHtml

nextID :: MonadState GenState m => m ElementID
nextID = do
  GenState i <- get
  put $ GenState (succ i)
  pure $ ElementID $ Text.pack $ "elem-" <> show i

type ReportHTML a = (ElementID -> Html a)

toRevealHtml :: Report ElementID (ReportHTML ()) -> Html ()
toRevealHtml Report{rpTitle, rpSubtitle, rpSections} = do
  L.head_ $ do
    L.meta_ [L.charset_ "utf-8"]
    L.title_ (L.toHtml rpTitle)
    meta "viewport" "width=device-width, initial-scale=1.0"
    L.with (L.script_ "") [L.src_ "https://cdn.tailwindcss.com?plugins=forms,typography,aspect-ratio,line-clamp"]

    -- vega stuff
    L.with (L.script_ "") [L.src_ "https://cdn.jsdelivr.net/npm/vega@5"]
    L.with (L.script_ "") [L.src_ "https://cdn.jsdelivr.net/npm/vega-lite@5"]
    L.with (L.script_ "") [L.src_ "https://cdn.jsdelivr.net/npm/vega-embed@6"]

    -- graphviz stuff
    L.with (L.script_ "") [L.src_ "https://cdn.jsdelivr.net/npm/d3@7.6.1/dist/d3.min.js"]
    L.with (L.script_ "") [L.src_ "https://cdn.jsdelivr.net/npm/@hpcc-js/wasm/dist/index.min.js"]
    L.with (L.script_ "") [L.src_ "https://cdn.jsdelivr.net/npm/d3-graphviz@4.4.0/build/d3-graphviz.js"]
  L.body_ $ do
    L.div_ [L.class_ "bg-white dark:bg-slate-900"] $ do
      L.div_ [L.class_ "bg-slate-200 relative mx-auto flex max-w-8xl justify-center sm:px-2 lg:px-8 xl:px-12"] $ do
        L.div_ [L.class_ "hidden lg:relative lg:block lg:flex-none bg-slate-200"] $ do
          L.div_ [L.class_ "sticky top-[4.5rem] -ml-0.5 h-[calc(100vh-4.5rem)] overflow-y-auto overflow-x-hidden py-16 pl-0.5"] $ do
            L.nav_ [L.class_ "text-base lg:text-sm w-64 pr-8 xl:w-72 xl:pr-16"] $ do
              L.ul_ [L.class_ "space-y-9"] $ do
                flip traverse_ rpSections $ \ReportSection{rsTitle, rsSubsections, rsAnchor} -> do
                  L.li_ $ do
                    L.h2_ [L.class_ "font-display font-medium text-slate-900 dark:text-white"] $ do
                      L.a_ [L.href_ ("#" <> getElementId rsAnchor) ] (L.toHtml rsTitle)
                    L.ul_ [L.class_ "mt-2 space-y-2 border-l-2 border-slate-100 dark:border-slate-800 lg:mt-4 lg:space-y-4 lg:border-slate-200"] $
                      flip traverse_ rsSubsections $ \Subsection{ssTitle, ssAnchor} -> do
                        flip traverse_ ssTitle $ \txt -> do
                          L.li_ [L.class_ "relative"] $ L.a_ [L.href_ ("#" <> getElementId ssAnchor)] (L.toHtml txt)

        L.div_ [L.class_ "bg-white min-w-0 max-w-2xl flex-auto px-4 py-16 lg:max-w-none lg:pr-0 lg:pl-8 xl:px-16"] $ do
          L.article_ $ do
            L.div_ [L.class_ "prose prose-slate max-w-none dark:prose-invert dark:text-slate-400 prose-headings:scroll-mt-28 prose-headings:font-display prose-headings:font-normal lg:prose-headings:scroll-mt-[8.5rem] prose-lead:text-slate-500 dark:prose-lead:text-slate-400 prose-a:font-semibold dark:prose-a:text-sky-400 prose-a:no-underline prose-a:shadow-[inset_0_-2px_0_0_var(--tw-prose-background,#fff),inset_0_calc(-1*(var(--tw-prose-underline-size,4px)+2px))_0_0_var(--tw-prose-underline,theme(colors.sky.300))] hover:prose-a:[--tw-prose-underline-size:6px] dark:[--tw-prose-background:theme(colors.slate.900)] dark:prose-a:shadow-[inset_0_calc(-1*var(--tw-prose-underline-size,2px))_0_0_var(--tw-prose-underline,theme(colors.sky.800))] dark:hover:prose-a:[--tw-prose-underline-size:6px] prose-pre:rounded-xl prose-pre:bg-slate-900 prose-pre:shadow-lg dark:prose-pre:bg-slate-800/60 dark:prose-pre:shadow-none dark:prose-pre:ring-1 dark:prose-pre:ring-slate-300/10 dark:prose-hr:border-slate-800"] $ do
              L.h1_ (L.toHtml rpTitle)
              traverse_ L.toHtml rpSubtitle
              flip traverse_ rpSections $ \ReportSection{rsTitle, rsSubtitle, rsSubsections, rsAnchor} -> do
                  L.h2_ [L.id_ (getElementId rsAnchor)] (L.toHtml rsTitle)
                  traverse_ (L.h3_ . L.toHtml) rsSubtitle
                  flip traverse_ rsSubsections $ \Subsection{ssContent, ssAnchor} -> ssContent ssAnchor

renderToFile :: FilePath -> Report () Content -> IO ()
renderToFile fp = Text.IO.writeFile fp . L.renderText . toRevealHtml . fmap contentToHtml . addAnchors

addAnchors :: Report () a -> Report ElementID a
addAnchors r@Report{rpSections} =
  let sections' = evalState (traverse addSectionAnchor rpSections) (GenState 0)
      addSectionAnchor s@ReportSection{rsSubsections} = do
        rsAnchor <- nextID
        subsections' <- traverse addSubsectionAnchor rsSubsections
        pure s{rsAnchor, rsSubsections = subsections'}
      addSubsectionAnchor s = do
        ssAnchor <- nextID
        pure s{ssAnchor}
  in r{rpSections = sections'}

data Content =
  VegaLiteContent VegaLiteGraph
  | TextContent Text

contentToHtml :: Content -> ReportHTML ()
contentToHtml = \case
  TextContent t -> \(ElementID i) -> L.p_ [L.id_ i] (L.toHtml t)
  VegaLiteContent VegaLiteGraph{vlGraph, vlTitle} -> \(ElementID i) -> do
    L.h4_ (L.toHtml vlTitle)
    L.div_ [L.id_ i, L.alt_ vlTitle] (pure ())
    L.with (L.script_ $ vegaEmbed i $ Object vlGraph) [L.type_ "text/javascript", L.defer_ ""]

vegaEmbed :: Text -> Value -> Text
vegaEmbed elm spec_ =
  let spec = Text.Lazy.toStrict $ E.decodeUtf8 (encode spec_)
  in [text|vegaEmbed("#$elm", $spec, { "renderer": "svg" })|]
