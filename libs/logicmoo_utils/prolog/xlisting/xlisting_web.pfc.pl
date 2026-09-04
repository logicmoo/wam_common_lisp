



%:- set_defaultAssertMt(xlisting_web).

:- set_fileAssertMt(xlisting_web).% WAS OFF  :- system:expects_dialect(pfc).

/*
:- baseKB:export(baseKB:never_assert_u/2).
:- xlisting_web:import(baseKB:never_assert_u/2).
:- baseKB:export(baseKB:never_retract_u/2).
:- xlisting_web:import(baseKB:never_retract_u/2).
*/

%% param_default_value( ?ARG1, ?ARG2) is det.
%
% Param Default Value.
%

% :- mpred_trace_exec.
:- kb_global(baseKB:param_default_value/2).
singleValueInArg(param_default_value,2).

:- kb_global(xlisting_web:combo_default_value/3).
singleValueInArg(combo_default_value,3).

combo_default_value(human_language,1,'EnglishLanguage').

%% baseKB:param_default_value( ?ARG1, ?ARG2) is det.
%
% Param Default Value.
%
baseKB:param_default_value(N,V):- combo_default_value(N,_,V).
% combo_default_value(Pred,Arity,_Value) ==> {kb_shared(Pred/Arity)}.
%:- brea.


%% human_language( ?ARG1) is det.
%
% Human Language.
%
:- kb_global(baseKB:human_language/1).
human_language("AlbanianLanguage").
human_language("ArabicLanguage").
human_language("BasqueLanguage").
human_language("CatalanLanguage").
human_language("ChineseLanguage").
human_language("DanishLanguage").
human_language("EnglishLanguage"). 
human_language("FarsiLanguage").
human_language("FinnishLanguage").
human_language("FrenchLanguage").
human_language("GalicianLanguage").
human_language("GermanLanguage").
human_language("HebrewLanguage").
human_language("IndonesianLanguage").
human_language("ItalianLanguage").
human_language("JapaneseLanguage").
human_language("MalayLanguage").
human_language("NorwegianBokmalLanguage").
human_language("NorwegianNorskLanguage").
human_language("PolishLanguage").
human_language("PortugueseLanguage").
human_language("SpanishLanguage").
human_language("ThaiLanguage").
human_language("de").


baseKB:param_default_value(request_uri,'/swish/lmxref/').
baseKB:param_default_value(olang,'CLIF').
baseKB:param_default_value(fa,'tHumanHead').

:- forall(
  member(N=V,[
     webproc=edit1term,
     'prover'='proverPTTP',
     'apply'='fa',
     'term'='',
     action_below=query,
     'action_above'='query',
     'context'='BaseKB',
     'flang'='CLIF','fa'='tHumanHead','xref'='Overlap','POS'='N',
     'humanLang'='EnglishLanguage','olang'='CLIF','sExprs'='0','webDebug'='1',
     'displayStart'='0','displayMax'='100000']),
  xlisting_web:ain(baseKB:param_default_value(N,V))).


combo_default_value(logic_lang_name,2,'CLIF').
%% logic_lang_name( ?ARG1, ?ARG2) is det.
%
% Logic Language Name.
%
logic_lang_name('E2C',"Logicmoo English (E2C)").
logic_lang_name('CLIF',"Common Logic (CLIF)").
logic_lang_name('CycL',"CycL").
logic_lang_name('Prolog',"Prolog").
logic_lang_name('CGIF',"CG-Logic (CGIF)").
logic_lang_name('SUO-KIF',"SUO-KIF").
logic_lang_name('TPTP',"TPTP (fof/cnf)").
logic_lang_name('OWL',"OWL").



combo_default_value(prover_name,2,'proverPTTP').
%% prover_name( ?ARG1, ?ARG2) is det.
%
% Prover Name.
%
:- kb_global(prover_name/2).
prover_name(proverCyc,"CycL (LogicMOO)").
prover_name(proverPFC,"PFC").
prover_name(proverPTTP,"PTTP (LogicMOO)").
prover_name(proverDOLCE,"DOLCE (LogicMOO)").



/*
combo_default_value(partOfSpeech,2,'N').
%% partOfSpeech( ?ARG1, ?ARG2) is det.
%
% Part Of Speech.
%
:- kb_shared(partOfSpeech/2).
partOfSpeech("N","Noun").
partOfSpeech("V","Verb").
partOfSpeech("J","Adjective").
partOfSpeech("Z","Adverb").
*/

%% search_filter_name_comment( ?ARG1, ?ARG2, ?ARG3) is det.
%
% Search Filter Name Comment.
%

% :- kb_global(search_filter_name_comment/3).

%:- xlisting_web:dynamic(xlisting_web:search_filter_name_comment/3).
%:- baseKB:import(xlisting_web:search_filter_name_comment/3).
search_filter_name_comment(hideMeta,'Hide Meta/BookKeeping','1').
search_filter_name_comment(hideSystem,'Skip System','0').
search_filter_name_comment(hideTriggers,'Hide Triggers','1').
search_filter_name_comment(skipLarge,'No Large','0').
search_filter_name_comment(showHyperlink,'Hyperlink','1').
search_filter_name_comment(showFilenames,'Filenames','1').
search_filter_name_comment(showHUGE,'showHUGE','1').
search_filter_name_comment(wholePreds,'Whole Preds','1').
search_filter_name_comment(skipVarnames,'Skip Varnames','0').
search_filter_name_comment(hideClauseInfo,'Skip ClauseInfo','0').
search_filter_name_comment(hideXRef,'Skip XREF','1').
search_filter_name_comment(showAll,'Show All','0').
  

%:- add_import_module(baseKB, xlisting_web,end).


% % %
search_filter_name_comment(N,_,D)==>baseKB:param_default_value(N,D).

combo_default_value(is_context,2,'BaseKB').

:- kb_global(xlisting_web:xaction_menu_item/2).

combo_default_value(xaction_menu_item,2,'query').

%arg2Isa(xaction_menu_item,xtPrologString).

%% xaction_menu_item( ?ARG1, ?ARG2) is det.
%
% Action Menu Item.
%

xaction_menu_item('Find',"Find $item").
xaction_menu_item('Forward',"Forward Direction").
xaction_menu_item('Backward',"Backward Direction").
xaction_menu_item('query',"Query $item").
xaction_menu_item('repropagate',"Repropagate $item (ReAssert)").
xaction_menu_item('remove',"Remove $item(Unassert)").   
xaction_menu_item('Code',"Assume Theorem (Disable $item)").
xaction_menu_item('prologSingleValued',"Make $item Single Valued").
xaction_menu_item('prologBuiltin',"Impl $item in Prolog").
xaction_menu_item('prologPTTP',"Impl $item in PTTP").
xaction_menu_item('prologDRA',"Impl $item in DRA").
xaction_menu_item('prologPfc',"Impl $item in PFC").
xaction_menu_item('Monotonic',"Treat $item Monotonic").
xaction_menu_item('NonMonotonic',"Treat $item NonMonotonic").   


