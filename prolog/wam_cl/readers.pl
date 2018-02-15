/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(r3d3rz, []).

:- include('header').


wl:symbol_has_prop_set_get(sys_xx_stdin_xx,claz_prolog_input_stream,set_input,current_input).
wl:symbol_has_prop_set_get(sys_xx_stderr_xx,claz_prolog_output_stream,set_current_error,current_error_stream).
wl:symbol_has_prop_set_get(sys_xx_stderr_xx,claz_prolog_output_stream,set_output,current_output).


%$xx_standard_input_xx

% *DEBUG-IO*, *ERROR-OUTPUT*, *QUERY-IO*, *STANDARD-INPUT*, *STANDARD-OUTPUT*, *TRACE-OUTPUT*

% &optional INPUT_STREAM EOF_ERROR_P EOF_VALUE RECURSIVE_P => OBJECT

wl:init_args(0,read).

%f_read(Obj):-
%  current_input(Input),
%  pl_lisp_read(Input,t,[],[],Obj).

f_read(Options,Obj):- wdmsg(Options), 
  current_input(Input),
  pl_lisp_read(Input,t,[],[],Obj).
%f_read(Input,Obj):-  pl_lisp_read(Input,t,[],[],Obj).

pl_lisp_read(INPUT_STREAM,_EOF_ERROR_P, _EOF_VALUE, _RECURSIVE_P,OBJECT):-   
    lisp_read_typed(INPUT_STREAM, Forms0),!,   
    quietly_sreader((always(to_untyped(Forms0,OBJECT)))).


:- fixup_exports.


end_of_file.


[22:11] <MrBusiness> So uh
[22:11] <MrBusiness> this is a game about prolog?
[22:11] <MrBusiness> Is it actively being played at all?
[22:11] <MrBusiness> It sounds really fun and creative!
[22:12] <MrBusiness> i've always wanted to really play around with prolog, but the language itself seems to lend itself to specific applications of the non-game variety from the majority of what I've seen.
[22:18] <MrBusiness> Guess it's dierent than that, or I just haven't figured out how to runt he client. Been years since I played with a MUD or anything.-
[04:35] <dmiles> currently server is down.. but should have things going by mid-January
[04:35] <dmiles> So in Logicmoo a user can create realms and make them available for other people to play. 
[04:36] <dmiles> What prolog is very good at is blending differnt types of Knoweldge representations
[04:37] <dmiles> It is a database language that is not limited by the convertions that most people are used to
[04:38] <dmiles> the way Norvig's PAIP operated (in lisp) is a good genral preview of what the language does
[04:39] <dmiles> Prolog requires far less predicated structure.. jusut like in Lisp most things can have a List/Sequence/Slots point of view.. prolog has that a well 
[04:40] <dmiles> That is that prolog sees all datastructures (including its own programs) something it can makes deductions about and transformations
[04:42] <dmiles> Back to logicmoo. the idea is much like Inform7s Guncho https://www.guncho.com/
[04:43] <dmiles> instead of a controlled english i have been concentrating on a controlled higher order logic (taken from McCArthys idea of elaboration tollerant worlds
[04:44] <dmiles> the benefiuts is the world simulator can be as simple or detailed as one wants
[04:45] <dmiles> you can describe things with great and very little details
[04:46] <dmiles> the system doesnt need to recompile.. it just augments the existing code and database
[04:47] <dmiles> i been taqking too long at the infenrce engine (the thing that creates new worlds from ideas) so i decided to convert the source code of CYC to prolog
[04:47] <dmiles> (Cycorp implemented McCarhties ideas)
[04:48] <dmiles> right now i been focusing on the new tranlator that lets me port lisp applicaitons (like CYC, Daydreamer, etc) to prolog
[04:48] <dmiles> https://github.com/TeamSPoon/wam_common_lisp/blob/master/t/sanity-test.lisp_load.md#compiled--ufifteen
[04:49] <dmiles> that is an example output of the tranapiter.. i am haivng it generate .md files so i can find little bugs
[04:50] <dmiles> i hav eot take a 6 hour nap before resumeing work
[04:50] <dmiles> hope to talk soon
[07:04] * aindilis (~aindilis@172-12-3-117.lightspeed.sgnwmi.sbcglobal.net) Quit (Ping timeout: 252 seconds)
[16:23] <MrBusiness> that's an interesting idea, and I  am convinced of its viability
[16:25] <MrBusiness> I had a loosely related idea for a similar system based on puzzlescript, which has many prolog db/predicate analogues
[16:28] <MrBusiness> it would take an inform7 code, then cull the pscript db for matching rules/objects that maps to nouns & actions
[16:31] <MrBusiness> it relies on a bit of crowdsourcing, metrics, and a cache of the puzzlescripts and inform stories combined, along with an element of nondeterministic randomness and some kind of system for culling/adding rules/objects from specific constituent codes
[16:32] <MrBusiness> and a more deterministic "rating" system that somehow lets people decide, perhaps with a five star system, which combination sets work best
[16:33] <MrBusiness> but prolog predicates and pscript rules are very similar
[16:33] <MrBusiness> which is, I'd say, part of what makes a good puzzlescript so hard to use
[16:34] <MrBusiness> ultimately though, I think the end product would benefit from either an inform-style typed action command line.
[16:34] <MrBusiness> or a SCUMM-like subwindow of established verbs
[16:35] <MrBusiness> ultimately, I don't know that I'm strong enough in statistics to really make it work.
[16:40] <MrBusiness> Guncho is very intriguing
[16:41] <MrBusiness> I guess my original idea was that the player would start as a cat in a room with some objects. Like a cat, one does not know, at a glance, what much of anything is.
[16:43] <MrBusiness> only through interaction and exploration would actions be discovered, objects named, and so on. I reckon I must have been out of my mind, but having been around cats, I've observed that they do seem to discover and ascribe their own meanings to things in the world.
[17:01] <dmiles> *nod* part of what makes a good puzzlescript/inform7/prolog so hard is it can seem to be all rules and not much action .. it taken me a very long time to get as comfortable as i am with prolog
[17:01] <dmiles> iteresting what gets around some of this ..  like in Inform7 programmers drop to inserting inform6 code in {-   varx = varx * 2;  -} into the middle of Inform7 
[17:02] <dmiles> for prolog, i've used a lot of forard chaining to create/craft state
[17:02] <dmiles> foreword*
[17:03] <dmiles> the command line is sort of a way to see and manipulate that
[17:04] <dmiles> your description about cats is much what what i mean be "level of detail" accribed to objects
[17:05] <dmiles> be/by
[17:06] <dmiles> the lack of action thing has lead me slightly to a PDDL view of the world
[17:07] <dmiles> https://www.cs.toronto.edu/~sheila/2542/s14/A1/introtopddl2.pdf
[17:09] <dmiles> which gives a way to describe how actions afftect the world
[17:11] <dmiles> still everthing is declarative which is what makes it easy for the system and slightly harder for human programmers
[17:13] <dmiles> but at least humans can still ellaborate on the declared
[17:14] <dmiles> and interact with a declared world
[17:16] <dmiles> i been toying with having non player agents with goals that have been declared try to acommplish them.. my cat right now is out of sorts because he saw another cat outside and feels something is not right
[17:16] <dmiles> so he walked over to his food dish and decided the big problem muct be that the dish is not covered up
[17:16] <dmiles> must be*
[17:17] <dmiles> the reason he walked over to it is not out of thi9nking of protecting it.. but because he is hungry
[17:18] <dmiles> i knew he really didnt want it covered but he was trying to cover it.. so i helped him complete his task
[17:19] <dmiles> but getting a newpaper sheet and doing it.. he walked away contently
[17:19] <dmiles> but then 30 seconds later he came back and uncovered it and started eating
[17:22] <dmiles> that sort of interaction probably would happen much in the game world
[17:23] <dmiles> my cats actual issue was when he got to the food dish he was still feeling anxious about the outside cat .. the dish was only 1/2 full.. he is not used to seeing it low
[17:24] <dmiles> he thinks when its uncovered it always has lots of food
[17:24] <dmiles> so he needed to cover it
[17:24] <dmiles> (so it can be uncovered)
[17:26] <dmiles> after a few seconds of eating .. he let his mind drift to thinking aobut the outdoor cat and his anxiousness took away the hunger (ok maybe he was never hungry) 
[17:26] <dmiles> so he wnet over the scratching post and started running arround the house shrieking.. that was what he needed all along
[17:27] <dmiles> he crashed out just now on my desk (for feeling love and safety)
[17:30] <dmiles> i wanted a world that can be elaborted on .. just like real life
[17:32] <dmiles> for a while i thought that games that looked liek the real world would be helpfull
[17:33] <dmiles> but after expedning a lot of resources in 3d worlds, i realized i was using them to avoid the tough issues of representations that are not 3D
[17:34] <dmiles> the military was good for funding and supporting that research and would love it to continjue
[17:35] <dmiles> but i knew it was a way to sneak away from the harder tasks (for me)
[17:36] <dmiles> other Embodiment AI-ists are getting lost in that maze
[17:37] <dmiles> (that is my excuse for having wasted 12 years)
[17:38] <dmiles> why logicmoo is still close to being at the level it was in 2001
[17:39] <dmiles> to..  http://logicmoo.sourceforge.net/
[17:43] <dmiles> just mentioning sinc ei feel guitly it was not further along
[17:51] <dmiles> then another excuse (this might be apply to societys research as well) is we been looking for towards "Rule of least power"
[17:52] <dmiles> In programming, the rule of least power is a design principle that "suggests choosing the least powerful [computer] language suitable for a given purpose". ... This rule is an application of the principle of least privilege to protocol design.
[17:53] <dmiles> " The Semantic Web is an attempt, largely, to map large quantities of existing data onto a common language so that the data can be analyzed in ways never dreamed of by its creators. If, for example, a web page with weather data has RDF describing that data, a user can retrieve it as a table, perhaps average it, plot it, deduce things from it in combination with other information"
[17:54] <dmiles> the last project i was on was attempting to create PrologMUD states in all RDF
[17:56] <dmiles> after we spent a great deal of investor money on that i felt guilty and had to go off and start working on PrologMUD for free (they are continuing to work on it in RDF)
[17:57] <dmiles> other "Nomic Projects" like AutoNomic and IDNI are attempting to create a Logicmoo bassed on "Rule of least power"
[17:59] <dmiles> (Logicmoo is a "Nomic Game")
[18:00] <dmiles> Nomic is a game in which changing the rules is a move. In that respect it differs from almost every other game. The primary activity of Nomic is proposing changes in the rules, debating the wisdom of changing them in that way, voting on the changes, deciding what can and cannot be done afterwards, and doing it. Even this core of the game, of course, can be changed.
[20:30] * aindilis (~aindilis@172-12-3-117.lightspeed.sgnwmi.sbcglobal.net) has joined #logicmoo



