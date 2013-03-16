{-# LANGUAGE ImplicitParams,DoRec, PatternGuards #-}
module Fenfire.Main where

-- Copyright (c) 2006-2007, Benja Fallenstein, Tuukka Hastrup
-- This file is part of Fenfire.
-- 
-- Fenfire is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- Fenfire is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
-- Public License for more details.
-- 
-- You should have received a copy of the GNU General
-- Public License along with Fenfire; if not, write to the Free
-- Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
-- MA  02111-1307  USA

import Fenfire.Utils
import Fenfire.Cairo hiding (Path, rotate)
import Fenfire.Vobs
import qualified Fenfire.Raptor as Raptor
import Fenfire.URN5
import Fenfire.RDF
import Fenfire.VanishingView
import Fenfire

import Paths_fenfire (getDataFileName)

import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.State

import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

--import Fenfire.GtkFixes
import Graphics.UI.Gtk hiding (Color, get, disconnect,
{-- GtkFixes overrides:
                               actionNew,
                               widgetGetStyle,
                               styleGetForeground, styleGetBackground, 
                               styleGetLight, styleGetMiddle, styleGetDark,
                               styleGetText, styleGetBase,
                               styleGetAntiAliasing -})
import Graphics.UI.Gtk.ModelView as New
import Graphics.UI.Gtk.Gdk.Events as E

import qualified Network.URI

import System.Directory (canonicalizePath)
import System.Environment (getArgs, getProgName)

interpretNode :: (?graph :: Graph) => String -> Node
interpretNode str | "<" `List.isPrefixOf` str && ">" `List.isSuffixOf` str = 
                        IRI $ tail $ init str
                  | isQname
                  , Just base <- Map.lookup ns (graphNamespaces ?graph) = 
                        IRI $ base ++ local
                  | isQname = error $ "No such namespace: \""++ns++"\""
                  | otherwise = IRI str
    where local = drop 1 $ dropWhile (/= ':') str
          ns = takeWhile (/= ':') str
          isQname = ns /= "" && (not $ any (`elem` local) [':', '/', '@'])

openFile :: (?vs :: ViewSettings) => FilePath -> 
            IO (Maybe (Graph, FilePath))
openFile fileName0 = do
    dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen
                                   [(stockCancel, ResponseCancel),
                                    (stockOpen, ResponseAccept)]
    when (fileName0 /= "") $ do fileChooserSetFilename dialog fileName0
                                return ()
    response <- dialogRun dialog
    widgetHide dialog
    case response of
        ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                             graph <- loadGraph fileName
                             return $ Just (graph, fileName)
        _              -> return Nothing
        
saveFile :: Graph -> FilePath -> Bool -> IO (FilePath,Bool)
saveFile graph fileName0 confirmSame = do
    dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionSave
                                   [(stockCancel, ResponseCancel),
                                    (stockSave, ResponseAccept)]
    fileChooserSetDoOverwriteConfirmation dialog True
    dialogSetDefaultResponse dialog ResponseAccept
    when (fileName0 /= "") $ do fileChooserSetFilename dialog fileName0
                                return ()
    onConfirmOverwrite dialog $ do 
        Just fileName <- fileChooserGetFilename dialog
        if fileName == fileName0 && not confirmSame
            then return FileChooserConfirmationAcceptFilename
            else return FileChooserConfirmationConfirm
    response <- dialogRun dialog
    widgetHide dialog
    case response of
        ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                             let fileName' = checkSuffix fileName
                             saveGraph graph fileName'
                             return (fileName', True)
        _              -> return (fileName0, False)
        
checkSuffix :: FilePath -> FilePath
checkSuffix s | List.isSuffixOf ".turtle" s = s
              | otherwise                        = s ++ ".turtle"

confirmSave :: (?vs :: ViewSettings, ?pw :: Window,
                ?views :: Views, ?uriMaker :: URIMaker) => 
               Bool -> HandlerAction FenState -> 
               HandlerAction FenState
confirmSave False action = action
confirmSave True action = do
    response <- liftIO $ do
        dialog <- makeConfirmUnsavedDialog
        response' <- dialogRun dialog
        widgetHide dialog
        return response'
    case response of ResponseClose  -> action
                     ResponseAccept -> do 
                         handleAction "save"
                         saved <- get >>= return . not . fsGraphModified
                         when (saved) action
                     _              -> return ()

confirmFix :: (?vs :: ViewSettings, ?pw :: Window,
                ?views :: Views, ?uriMaker :: URIMaker) => 
               String -> Bool -> HandlerAction FenState -> 
               HandlerAction FenState -> HandlerAction FenState
confirmFix _ False _ action = action
confirmFix title True fixaction action = do
    response <- liftIO $ do
        dialog <- makeDialog title
            [(stockCancel, ResponseCancel),
             (stockNo,     ResponseReject),
             (stockYes,    ResponseAccept)]
            ResponseAccept

        response' <- dialogRun dialog
        widgetHide dialog
        return response'
    case response of ResponseReject -> action
                     ResponseAccept -> action >> fixaction
                     _              -> return ()

confirmRevert :: (?vs :: ViewSettings, ?pw :: Window) => 
               Bool -> HandlerAction FenState -> 
               HandlerAction FenState
confirmRevert False action = action
confirmRevert True  action = do
    response <- liftIO $ do
        dialog <- makeConfirmRevertDialog
        response' <- dialogRun dialog
        widgetHide dialog
        return response'
    case response of ResponseClose  -> action
                     _              -> return ()

confirmString :: (?vs :: ViewSettings, ?pw :: Window) => 
               String -> String -> (String -> HandlerAction FenState) -> 
               HandlerAction FenState
confirmString title preset action = do
    (response,text) <- liftIO $ do 
        dialog <- makeDialog title
            [(stockCancel, ResponseCancel),
             (stockApply, ResponseAccept)]
            ResponseAccept
        entry <- entryNew
        set entry [ entryText := preset, entryActivatesDefault := True ]
        widgetShow entry
        vBox <- dialogGetUpper dialog
        boxPackStart vBox entry PackNatural 0
        response' <- dialogRun dialog
        text' <- entryGetText entry
        widgetHide dialog
        return (response',text')
    case response of ResponseAccept -> action text
                     _              -> return ()
handleEvent :: (?vs :: ViewSettings, ?pw :: Window, ?views :: Views,
                ?uriMaker :: URIMaker) => Handler Event FenState
handleEvent (Key { E.eventModifier=_mods, E.eventKeyName=key }) = do
    state <- get; let graph = fsGraph state; fileName = fsFilePath state
    case key of 
        x | x == "Up"    || x == "i"     -> handleAction "up"
        x | x == "Down"  || x == "comma" -> handleAction "down"
        x | x == "Left"  || x == "j"     -> handleAction "left"
        x | x == "Right" || x == "l"     -> handleAction "right"
        x | x == "Page_Up" -> handleAction "pageup"
        x | x == "Page_Down" -> handleAction "pagedown"
        x | x == "Home" -> handleAction "propup"
        x | x == "End" -> handleAction "propdown"
        "g" -> handleAction "goto"
        "v" -> handleAction "chgview"
        "p" -> handleAction "resetprop"
        "O" -> handleAction "open"
        "S" -> do (fp',saved) <- liftIO $ saveFile graph fileName False
                  let modified' = fsGraphModified state && not saved
                  put $ state { fsFilePath = fp', fsGraphModified = modified' }
        _   -> unhandledEvent
handleEvent _ = unhandledEvent

handleAction :: (?vs :: ViewSettings, ?pw :: Window, ?views :: Views,
                 ?uriMaker :: URIMaker) => Handler String FenState
handleAction action = do
    state@(FenState { fsGraph = graph, fsPath = path, fsMark = mark, 
                      fsFilePath = filepath, fsGraphModified = modified,
                      fsHasFocus=focus
                    }) <- get
    let ?graph = graph in do
    let rot@(Rotation node _) = fsRotation state
        b f x = maybeDo (f rot x) $ \rot' -> do 
                    putRotation rot'
                    modify $ \s -> s { fsGraphModified = modified }
        n f x = do state' <- liftIO (f x state); put state'; setInterp True
        o f x = do put (f x state); setInterp True
    case action of
        "up"    -> b tryRotate (-1) ; "down"  -> b tryRotate 1
        "pageup"-> b tryRotate (-10); "pagedown" -> b tryRotate 10
        "propup"-> b findChange (-1); "propdown" -> b findChange 1
        "left"  -> b tryMove Neg    ; "right" -> b tryMove Pos
        "nodel" -> n newNode Neg    ; "noder" -> n newNode Pos
        "connl" -> o connect Neg    ; "connr" -> o connect Pos
        "breakl"-> o disconnect Neg ; "breakr"-> o disconnect Pos
        "rmlit" -> putGraph (delLit node graph)
        "mark"  -> putMark $ toggleMark node mark
        "new"   -> confirmSave modified $ do
            (g', path') <- liftIO newGraph
            put $ newState g' path' "" focus
        "open"  -> confirmSave modified $ do 
            result <- liftIO $ openFile filepath
            maybeDo result $ \(g',fp') -> do
                let g'' = mergeGraphs g' $ containsInfoTriples g'
                put $ newState g'' (findStartPath Nothing g'') fp' focus
        "loadIRI" -> case node of 
                         IRI uri -> do 
                             g <- liftIO $ loadGraph uri
                             let graph' = delete' (Any,Any,Any,node) graph
                                 g' = mergeGraphs (mergeGraphs graph' g) $
                                      containsInfoTriples g
                                 s' = state {fsGraph=g',
                                             fsUndo=(graph,path):fsUndo state,
                                             fsRedo=[]}
                             put s'
                         _ -> unhandledEvent
        "revert" | filepath /= "" -> confirmRevert modified $ do
            g <- liftIO $ loadGraph filepath
            let graph' = delete' (Any,Any,Any,Dft) graph
                g' = mergeGraphs (mergeGraphs g graph') $ 
                     containsInfoTriples g
            put $ newState g' (findStartPath Nothing g') filepath focus
        "save" | filepath /= "" -> do 
                     liftIO $ saveGraph graph filepath
                     modify $ \s -> s { fsGraphModified = False }
               | otherwise      -> handleAction "saveas"
        "saveas"-> do
            (fp',saved) <- liftIO $ saveFile graph filepath True
            let modified' = modified && not saved
            modify $ \s -> s { fsFilePath = fp', fsGraphModified = modified' }
        "quit"  -> do confirmSave modified $ liftIO mainQuit
        "about" -> liftIO $ makeAboutDialog >>= widgetShow
        "chgview" -> do put $ state { fsView = (fsView state + 1) `mod` 
                                               (length ?views) }
                        setInterp True
        "addprop" -> do let uri = case node of IRI _ -> showNode 
                                                   (graphNamespaces graph) node
                                               _     -> ""
                        confirmString "Add property" uri $ \uri' ->
                            when (uri' /= "") $ do
                                let prop' = interpretNode uri'
                                    props = fsProperties state
                                put $ state { fsProperty = prop',
                                    fsProperties = Set.insert prop' props }
        "resetprop" -> when (fsProperty state /= rdfs_seeAlso) $
                           put $ state { fsProperty = rdfs_seeAlso }
        "changeIRI" -> case node of
                           IRI _ -> confirmString "New IRI" (showNode 
                               (graphNamespaces graph) node) $ \uri' ->
                                   put $ stateReplaceNode node
                                       (interpretNode uri') state
                           _     -> unhandledEvent
        "goto" -> confirmString "Go to node"
                      (showNode (graphNamespaces graph) node) $ \s -> do
                          let node' = interpretNode s
                              rot' = Rotation node' 0
                              noinfo = not $ iquery (node',Any,Any)
                                          || iquery (Any,Any,node')
                          confirmFix "Load info about node"
                              noinfo (handleAction "loadIRI")
                              $ putRotation rot'
        "undo" | (graph',path'):undos <- fsUndo state -> do
                   put state {fsGraph=graph', fsPath=path', 
                              fsUndo=undos, fsRedo=(graph,path):fsRedo state}
                   setInterp True
        "redo" | (graph',path'):redos <- fsRedo state -> do
                   put state {fsGraph=graph', fsPath=path', 
                              fsUndo=(graph,path):fsUndo state, fsRedo=redos}
                   setInterp True
        _       -> do liftIO $ putStrLn $ "Unhandled action: " ++ action
                      unhandledEvent
  where putGraph g        = do modify $ \s ->
                                   s { fsGraph=g, fsGraphModified=True,
                                       fsUndo=(fsGraph s, fsPath s):fsUndo s,
                                       fsRedo=[]}
                               setInterp True
        putRotation rot   = do modify $ \s -> s { fsPath = toPath' rot }
                               setInterp True
        putMark mk        = do modify $ \state -> state { fsMark=mk }
        delLit n graph = delete' (n, rdfs_label, Any) graph

makeActions actionGroup = do
    let actionentries = 
            [ ( "new"    , "", stockNew           , Nothing              )
            , ( "open"   , "", stockOpen          , Nothing              )
            , ( "save"   , "", stockSave          , Nothing              )
            , ( "saveas" , "", stockSaveAs        , Just "<Ctl><Shift>S" )
            , ( "revert" , "", stockRevertToSaved , Nothing              )
            , ( "quit"   , "", stockQuit          , Nothing              )
            , ( "about"  , "", stockAbout         , Nothing              )
            , ( "loadIRI", "_Load node's IRI",
                                    stockGoForward     , Just "<Ctl>L"        )
            , ( "undo"   , "", stockUndo          , Just "<Ctl>Z"        )
            , ( "redo"   , "", stockRedo          , Just "<Ctl><Shift>Z" )
            ]
    forM actionentries $ \(name,label',stock,accel) -> do 
        action <- actionNew name label' Nothing (Just stock)
        actionGroupAddActionWithAccel actionGroup action accel
--        actionSetAccelGroup action accelGroup

updateActions actionGroup stateRef = do
    state <- readIORef stateRef
    let readable = fsFilePath state /= ""
        modified = fsGraphModified state
        view = fst $ ?views !! (fsView state)

    Just save <- actionGroupGetAction actionGroup "save"
    actionSetSensitive save modified
    Just revert <- actionGroupGetAction actionGroup "revert"
    actionSetSensitive revert (modified && readable)
    Just undo <- actionGroupGetAction actionGroup "undo"
    actionSetSensitive undo (not $ null $ fsUndo state)
    Just redo <- actionGroupGetAction actionGroup "redo"
    actionSetSensitive redo (not $ null $ fsRedo state)
    Just changeView <- actionGroupGetAction actionGroup view
    toggleActionSetActive (castToToggleAction changeView) True
    
updatePropMenu propmenu actionGroup stateRef updateCanvas = do
    state <- readIORef stateRef
    Just addProp <- actionGroupGetAction actionGroup "addprop"
                
    menu <- menuNew
    forM (Set.toAscList $ fsProperties state) $ \prop -> do
        item <- let ?graph = fsGraph state
                 in menuItemNewWithLabel $ getTextOrIRI prop
        onActivateLeaf item $ do 
            modifyIORef stateRef $ \state' -> state' {fsProperty=prop}
            updateCanvas False
        menuShellAppend menu item
        widgetShow item
    sep <- separatorMenuItemNew
    menuShellAppend menu sep
    widgetShow sep
    item <- actionCreateMenuItem addProp
    menuShellAppend menu $ castToMenuItem item
    
    menuItemSetSubmenu propmenu menu

makeBindings actionGroup = do
    let bindingentries =
            [ ("noder"  , "_New node to right"         , 
               stockMediaForward  , Just "n"              )
            , ("nodel"  , "N_ew node to left"          , 
               stockMediaRewind   , Just "<Shift>N"       )
            , ("breakr" , "_Break connection to right" , 
               stockGotoLast      , Just "b"              )
            , ("breakl" , "B_reak connection to left"  , 
               stockGotoFirst     , Just "<Shift>B"       )
            , ("mark"   , "Toggle _mark"               ,
               stockOk            , Just "m"              )
            , ("connr"  , "_Connect marked to right"   ,
               stockGoForward     , Just "c"              )
            , ("connl"  , "C_onnect marked to left"    ,
               stockGoBack        , Just "<Shift>C"       )
            , ("rmlit"  , "Remove _literal text"       ,
               stockStrikethrough , Just "<Alt>BackSpace" )
            , ("addprop",  "_Add property"              ,
               stockAdd           , Just "<Ctl>P"         )
            , ("changeIRI", "Change node's _IRI"       ,
               stockRefresh       , Just "u"              )
            , ( "goto"  , "_Go to IRI"                 ,
               stockJumpTo        , Just "g"              )
            ]
    forM bindingentries $ \(name,label',stock,accel) -> do 
        action <- actionNew name label' Nothing (Just stock)
        actionGroupAddActionWithAccel actionGroup action accel
--        actionSetAccelGroup action bindings

makeMenus actionGroup root propmenu = addAll root menu where
    menu = [m "_File" [a "new", a "open", a "goto", a "loadIRI", sep,
                       a "save", a "saveas", a "revert", sep,
                       a "quit"],
            m "_Edit" [a "undo", a "redo", sep,
                       return propmenu, sep,
                       a "noder", a "nodel", sep,
                       a "breakr", a "breakl", sep,
                       a "mark", a "connr", a "connl", sep, 
                       a "changeIRI", a "rmlit"],
            m "_View" (map (a . fst) ?views),
            m "_Help" [a "about"]]
    addAll parent items = mapM_ (menuShellAppend parent) =<< sequence items
    m :: String -> [IO MenuItem] -> IO MenuItem
    m name children = do item <- menuItemNewWithMnemonic name
                         menu' <- menuNew
                         addAll menu' children
                         menuItemSetSubmenu item menu'
                         return item
    sep = liftM castToMenuItem separatorMenuItemNew
    a name = do
     actionM <- actionGroupGetAction actionGroup name
     action <- case actionM of
      Just action' -> return action'
      Nothing -> error $ "Action: " ++ name ++ " does not exist."
     item <- actionCreateMenuItem action
     return (castToMenuItem item)

makeToolbarItems actionGroup toolbar = do
    forM_ ["new", "open", "save", "", "undo", "redo",""] $ \name -> 
        if name == "" then do 
            item <- separatorToolItemNew
            toolbarInsert toolbar item (-1)
        else do
            Just action <- actionGroupGetAction actionGroup name
            item <- actionCreateToolItem action
            toolbarInsert toolbar (castToToolItem item) (-1)

handleException :: Control.Exception.SomeException -> IO ()
handleException e = do
    dialog <- makeMessageDialog "Exception in event" (show e)
    dialogRun dialog
    widgetHide dialog


main :: IO ()
main = do

    uriMaker <- newURIMaker

    -- initial state:

    args <- initGUI

    window <- windowNew
    style <- widgetGetStyle window

    bgColor <- styleGetBackground style StateSelected
    blurBgColor <- styleGetBackground style StateActive
    focusColor <- styleGetBase style StateSelected
    blurColor <- styleGetBase style StateActive
    textColor <- styleGetText style StateSelected
    blurTextColor <- styleGetText style StateActive
    
    canvasBgColor <- styleGetBackground style StateNormal

    let alpha x (Color r g b a) = Color r g b (x*a)

    let ?vs = ViewSettings { hiddenProps=[rdfs_label], maxCenter=3 }
        ?uriMaker = uriMaker in let
        ?views = [("Wheel view", vanishingView 20 30 
                       (alpha 0.7 $ fromGtkColor bgColor)
                           (alpha 0.7 $ fromGtkColor blurBgColor)
                       (fromGtkColor focusColor) (fromGtkColor blurColor)
                       (fromGtkColor textColor)  (fromGtkColor blurTextColor)),
                  ("Presentation view", presentationView)] in do

    let view s = snd (?views !! fsView s) s

    stateRef <- case args of 
        [] -> do 
            (g, rot) <- newGraph
            newIORef $ newState g rot "" False
        xs -> do
            let f x | List.isPrefixOf "http:" x = return x
                    | otherwise = canonicalizePath x
            fileName:fileNames <- mapM f xs
            g' <- loadGraph fileName
            gs <- mapM loadGraph fileNames
            let h x | List.isPrefixOf "http:" x = return x
                    | otherwise = Raptor.filenameToURI x
            uri <- h fileName
            let graph = foldl mergeGraphs g' (gs ++ map containsInfoTriples (g':gs))
            newIORef $ newState graph (findStartPath (Just uri) graph) fileName False

    -- start:

    makeWindow window canvasBgColor view stateRef
    widgetShowAll window

    mainGUI

makeWindow window canvasBgColor view stateRef = do

    -- main window:

    let ?pw = window in do
     rec
      logo <- getDataFileName "data-files/icon16.png"
      Control.Exception.catch (windowSetIconFromFile window logo)
       (\e -> putStr ("Opening "++logo++" failed: ") >> print (e::Control.Exception.IOException))

      windowSetTitle window "Fenfire"
      windowSetDefaultSize window 800 550

      -- textview for editing:
      
      textView <- textViewNew
      textViewSetAcceptsTab textView False
      textViewSetWrapMode textView WrapWordChar

      -- this needs to be called whenever the node or its text changes:
      let stateChanged _ state@(FenState { fsPath=Path n _, fsGraph=g }) = do
              buf <- textBufferNew Nothing
              textBufferSetText buf (let ?graph=g in maybe "" id $ getText n)
              afterBufferChanged buf $ do 
                  start <- textBufferGetStartIter buf
                  end   <- textBufferGetEndIter buf
                  text  <- textBufferGetText buf start end True
                  s@(FenState { fsGraph = g' }) <- readIORef stateRef
                  let g'' = setText n text g' -- buf corresponds to n, not to n'

                  writeIORef stateRef $
                      s { fsGraph=g'', fsGraphModified=True, fsRedo=[],
                          fsUndo=(fsGraph s, fsPath s):(fsUndo s) }
                  updateActions actionGroup stateRef
                  updateCanvas True

              textViewSetBuffer textView buf
              updatePropMenu propmenu actionGroup stateRef updateCanvas
              New.listStoreClear propList
              forM_ (Set.toAscList $ fsProperties state) $ \prop -> 
                  let ?graph = g in 
                          New.listStoreAppend propList (prop, getTextOrIRI prop)
              let activeIndex = List.elemIndex (fsProperty state) 
                                    (Set.toAscList $ fsProperties state)
              maybe (return ()) (New.comboBoxSetActive combo) activeIndex

              updateActions actionGroup stateRef

      -- canvas for view:
      
      (canvas, updateCanvas, canvasAction) <- 
          vobCanvas stateRef view handleEvent handleAction
                    stateChanged handleException (fromGtkColor canvasBgColor) 0.5

      onFocusIn canvas $ \_event -> do 
          modifyIORef stateRef $ \s -> s { fsHasFocus = True }
          forM_ bindingActions $ actionConnectAccelerator
          updateCanvas True
          return True
      onFocusOut canvas $ \_event -> do 
          modifyIORef stateRef $ \s -> s { fsHasFocus = False }
          forM_ bindingActions $ actionDisconnectAccelerator
          updateCanvas True
          return True

      -- action widgets:

--      accelGroup <- accelGroupNew
--      windowAddAccelGroup window accelGroup

      -- bindings are active only when the canvas has the focus:
 --     bindings <- accelGroupNew
--      windowAddAccelGroup window bindings
      -- fake bindings aren't used
--      fake <- accelGroupNew

      actionGroup <- actionGroupNew "main"
      bindingGroup <- actionGroupNew "bindings"

      makeActions actionGroup -- accelGroup
      makeBindings bindingGroup-- bindings
      makeBindings actionGroup-- fake

      actions <- actionGroupListActions actionGroup
      bindingActions <- actionGroupListActions bindingGroup

      forM_ (actions ++ bindingActions) $ \action -> do
          name <- actionGetName action
          onActionActivate action $ canvasAction name >> return ()
          
      viewActs <- forM (zip [0..] ?views) $ \(index, (name, _view)) -> do
          action <- radioActionNew name name Nothing Nothing index
          actionGroupAddAction actionGroup action
          onActionActivate action $ do
              i <- radioActionGetCurrentValue action
              state <- readIORef stateRef
              when (i /= fsView state) $ do
                  writeIORef stateRef $ state { fsView = i }
                  updateCanvas True
          return action
          
      forM_ (tail viewActs) $ \x -> radioActionSetGroup x (head viewActs)
      toggleActionSetActive (toToggleAction $ head viewActs) True

      -- user interface widgets:

      menubar <- menuBarNew
      propmenu <- menuItemNewWithMnemonic "Set _property"
      makeMenus actionGroup menubar propmenu

      toolbar <- toolbarNew
      makeToolbarItems actionGroup toolbar

      propList <- New.listStoreNew []
      combo <- New.comboBoxNew
      set combo [-- New.comboBoxModel := Just propList
                 New.comboBoxFocusOnClick := False ]
      renderer <- New.cellRendererTextNew
      New.cellLayoutPackStart combo renderer True
      New.cellLayoutSetAttributes combo renderer propList $ \row -> 
          [ New.cellText := snd row ]
      New.onChanged combo $ do 
          active <- New.comboBoxGetActive combo 
          case active of 
              -1 -> return ()
              i -> do 
                  (prop,_name) <- listStoreGetValue propList i
                  state' <- readIORef stateRef
                  writeIORef stateRef $ state' {fsProperty=prop}
                  when (fsProperty state' /= prop) $ updateCanvas False
                  
      comboLabel <- labelNew (Just "Property:  ")
                  
      comboVBox <- hBoxNew False 0
      boxPackStart comboVBox comboLabel PackNatural 0
      boxPackStart comboVBox combo PackNatural 0

      comboAlign <- alignmentNew 0.5 0.5 1 0
      containerAdd comboAlign comboVBox

      combotool <- toolItemNew
      containerAdd combotool comboAlign
      toolbarInsert toolbar combotool (-1)

      sepItem <- separatorToolItemNew
      toolbarInsert toolbar sepItem (-1)
      
      Just addpropAction <- actionGroupGetAction actionGroup "addprop"
      addpropItem <- actionCreateToolItem addpropAction
      toolbarInsert toolbar (castToToolItem addpropItem) (-1)

      -- layout:

      canvasFrame <- frameNew
      set canvasFrame [ containerChild := canvas
                        , frameShadowType := ShadowIn 
                        ]

      textViewFrame <- frameNew
      set textViewFrame [ containerChild := textView
                        , frameShadowType := ShadowIn 
                        ]

      paned <- vPanedNew
      panedAdd1 paned canvasFrame
      panedAdd2 paned textViewFrame

      vBox <- vBoxNew False 0
      boxPackStart vBox menubar PackNatural 0
      boxPackStart vBox toolbar PackNatural 0
      boxPackStart vBox paned PackGrow 0
      containerSetFocusChain vBox [toWidget paned]
      
      set paned [ panedPosition := 380, panedChildResize textViewFrame := False ]

      set window [ containerChild := vBox ]

      -- start:

      startState <- readIORef stateRef
      stateChanged (startState { fsProperties = Set.empty }) startState
      
      widgetGrabFocus canvas

      onDelete window $ \_event -> canvasAction "quit"
     return window


makeAboutDialog :: (?pw :: Window) => IO AboutDialog
makeAboutDialog = do
    dialog <- aboutDialogNew
{- pixbufNewFromFile has different signature on newer gtk2hs:
    logoFilename <- getDataFileName "data-files/logo.svg"
    pixbuf <- Control.Exception.catch (pixbufNewFromFile logoFilename)
                  (\e -> return $ Left (undefined, show e))
    logo <- case pixbuf of Left (_,msg)  -> do 
                               putStr ("Opening "++logoFilename++" failed: ")
                               putStrLn msg
                               return Nothing
                           Right pixbuf' -> return . Just =<< 
                               pixbufScaleSimple pixbuf'
                                   200 (floor (200*(1.40::Double))) 
                                   InterpHyper 
-}  logo <- return Nothing
    set dialog [ aboutDialogName := "Fenfire" 
               , aboutDialogVersion := "alpha version"
               , aboutDialogCopyright := "Licensed under GNU GPL v2 or later"
               , aboutDialogComments := 
                     "An application for notetaking and RDF graph browsing."
               , aboutDialogLogo := logo
               , aboutDialogWebsite := "http://fenfire.org"
               , aboutDialogAuthors := ["Benja Fallenstein", "Tuukka Hastrup"]
               , windowTransientFor := ?pw
               ]
    onResponse dialog $ \_response -> widgetHide dialog
    return dialog

makeDialog :: (?pw :: Window) => String -> [(String, ResponseId)] -> 
                                 ResponseId -> IO Dialog
makeDialog title buttons preset = do
    dialog <- dialogNew
    set dialog [ windowTitle := title
               , windowTransientFor := ?pw
               , windowModal := True
               , windowDestroyWithParent := True
               , dialogHasSeparator := False
               ]
    mapM_ (uncurry $ dialogAddButton dialog) buttons
    dialogSetDefaultResponse dialog preset
    return dialog

makeConfirmUnsavedDialog :: (?pw :: Window) => IO Dialog
makeConfirmUnsavedDialog = do 
    makeDialog "Confirm unsaved changes" 
        [("_Discard changes", ResponseClose),
         (stockCancel, ResponseCancel),
         (stockSave, ResponseAccept)]
        ResponseAccept

makeConfirmRevertDialog :: (?pw :: Window) => IO Dialog
makeConfirmRevertDialog = do
    makeDialog "Confirm revert"
        [(stockCancel, ResponseCancel),
         (stockRevertToSaved,ResponseClose)]
        ResponseCancel

makeMessageDialog primary secondary = do
    dialog <- dialogNew
    set dialog [ windowTitle := primary
               , windowModal := True
               , containerBorderWidth := 6
               , dialogHasSeparator := False
               ]
    image' <- imageNewFromStock stockDialogError IconSizeDialog
    set image' [ miscYalign := 0.0 ]
    label' <- labelNew $ Just $ "<span weight=\"bold\" size=\"larger\">"++
                  escapeMarkup primary++"</span>\n\n"++escapeMarkup secondary
    set label' [ labelUseMarkup := True
               , labelWrap := True
               , labelSelectable := True
               , miscYalign := 0.0
               ]
    hBox <- hBoxNew False 0
    set hBox [ boxSpacing := 12
             , containerBorderWidth := 6
             ]
    boxPackStart hBox image' PackNatural 0
    boxPackStart hBox label' PackNatural 0

    vBox <- dialogGetUpper dialog
    set vBox [ boxSpacing := 12 ]
    boxPackStart vBox hBox PackNatural 0

    dialogAddButton dialog stockOk ResponseAccept
    widgetShowAll hBox
    return dialog
