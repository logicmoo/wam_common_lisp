# daydreamer
DAYDREAMER goal-based agent

http://www.amazon.com/gp/product/1478137266/

## Legal

Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004, 2015 Erik T. Mueller.

DAYDREAMER is a trademark of Erik T. Mueller.

All brand names and product names are the trademarks or registered
trademarks of their respective owners and/or manufacturers.

DAYDREAMER/GATE is free software. You can redistribute it and/or
modify it under the terms of the GNU General Public License Version 2 as
published by the Free Software Foundation.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along
with this software; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

Any rights not expressly granted herein are reserved.

## Introduction

DAYDREAMER is a goal-based agent that models daydreaming, emotions,
planning, and serendipity. Just give DAYDREAMER some goals and some
input events, and it will be off and running in a stream of thought
and action, which are monologuized in English.

DAYDREAMER does many things. Some of the most interesting are:
DAYDREAMER reflects on its past experiences. If DAYDREAMER is upset
about something, it thinks about something happy. Or, it may produce a
rationalization. DAYDREAMER makes plans, forms intentions, and later
acts on them. DAYDREAMER discovers creative solutions serendipitously,
in response to internal or external triggers.

Technically, DAYDREAMER is composed of the following components:
emotion-driven control loop, personal goals and metagoals, analogical
planner, hierarchical planner, episodic memory, and English generator.

Help DAYDREAMER do more! Extend DAYDREAMER, with your own planning rules,
inference rules, action effect rules, concern initiation rules, personal
goals, emotional responses, generation templates, initial facts, input
phrases, and episodes. See Tutorial below.

## Examples

Simply by typing `(load "dd")`, you can run the LOVERS1 experience.

Here is how this trace begins:

```
  > (load "dd")
  ...
  DAYDREAMER 3.5, Common Lisp version of 2004-12-20
  Initialize DAYDREAMER
  Performing first-time initialization
  Creating primal reality...
  ...
  Creating initial reality context...
  State changes from SUSPENDED to DAYDREAMING
```

DAYDREAMER first creates an initial reality context: the context
designated as containing the state of the simulated "real world" as
seen by DAYDREAMER. Various initial facts are asserted into this
context: DAYDREAMER has a job, is not romantically involved, is
currently at home, and so on.

DAYDREAMER then starts out in daydreaming mode.

```
  Run inferences in #{CX.6: (CX)}, bp = (#{ME: (FEMALE-PERSON first-name "Sarah"......)})
  ...
  ******************
  LOVERS-THEME LOVERS-THEME fired as inference in CX.6
  -------------------------------------------------------
  IF   self not LOVERS with anyone 
  THEN ACTIVE-GOAL for LOVERS with some person 
  -------------------------------------------------------
  ?SELF = #{ME: (FEMALE-PERSON first-name "Sarah"......)}
  ******************
  Activate top-level goal #{OB.2212: (ACTIVE-GOAL obj (LOVERS actor ......)......)} in #{CX.6: (CX)}
  Assert #{OB.2212: (ACTIVE-GOAL obj (LOVERS actor ......)......)} in CX.6
  ==================================================
   I want to be going out with someone.
   I feel really interested in going out with
   someone.
  ==================================================
  Personal goal concern OB.2212: LOVERS motiv 0.9 status HALTED
```

DAYDREAMER activates a top-level LOVERS goal because it is not
involved in a LOVERS relationship, and one or more of its need states
subsumed by the relationship are unsatisfied. A new concern is created
and a positive motivating emotion of *interest* is created and
associated with the new concern. The intrinsic importance of the goal
is 0.9. This becomes the magnitude of the motivating emotion, as well
as the current value for the motivation of the concern.

Whenever facts are asserted into a context, they are converted into
English and produced as output.

```
  ******************
  ENTERTAINMENT-THEME ENTERTAINMENT-THEME fired as inference in CX.6
  -------------------------------------------------------
  IF   level of satisfaction of ENTERTAINMENT need 
       below threshold 
  THEN ACTIVE-GOAL for ENTERTAINMENT 
  -------------------------------------------------------
  ?SELF = #{ME: (FEMALE-PERSON first-name "Sarah"......)}
  ******************
  Activate top-level goal #{OB.2217: (ACTIVE-GOAL obj (ENTERTAINMENT...)......)} in #{CX.6: (CX)}
  Assert #{OB.2217: (ACTIVE-GOAL obj (ENTERTAINMENT...)......)} in CX.6
  ==================================================
   I want to be entertained.
   I feel interested in being entertained.
  ==================================================
  Personal goal concern OB.2217: ENTERTAINMENT motiv 0.6 status RUNABLE
  Personal goal concern OB.2212: LOVERS motiv 0.9 status HALTED
```

Next DAYDREAMER activates an ENTERTAINMENT goal because its need for
entertainment is unsatisfied. Another concern and associated
motivating emotion are activated.

```
  Running emotion-driven control loop...
  :Switching to new top-level goal #{OB.2217: (ACTIVE-GOAL obj (ENTERTAINMENT...)......)}
  ...
  ******************
  ENTERTAINMENT-PLAN1 fired as plan 
  for #{OB.2217: (ACTIVE-GOAL obj (ENTERTAINMENT...)......)}
  in CX.6 sprouting CX.7
  -------------------------------------------------------
  IF   ACTIVE-GOAL for ENTERTAINMENT 
  THEN ACTIVE-GOAL for M-MOVIE with self 
  -------------------------------------------------------
  ?SELF = #{ME: (FEMALE-PERSON first-name "Sarah"......)}
  Assert #{OB.2227: (ORDERING)} in CX.7
  Instantiate and activate subgoals
  Activate subgoal for #{OB.2217: (ACTIVE-GOAL obj (ENTERTAINMENT...)......)} obj #{OB.2228: (M-MOVIE actor ME)} in #{CX.7: (CX)}
  Assert #{OB.2230: (INTENDS linked-from (ACTIVE-GOAL...)......)} in CX.7
  Assert #{OB.2229: (ACTIVE-GOAL obj (M-MOVIE actor ......)......)} in CX.7
  ==================================================
   I have to go see a movie.
  ==================================================
```

The top-level control loop is invoked once DAYDREAMER has gotten off
the ground by applying inferences and, in this case, creating two
concerns. The most highly motivated nonhalted concern is ENTERTAINMENT
and therefore a unit of planning is performed for this
concern. DAYDREAMER has a rule which states that a goal for
ENTERTAINMENT may be achieved by achieving an M-MOVIE subgoal. Thus a
new context is sprouted in which the top-level goal for ENTERTAINMENT
is connected to a subgoal for M-MOVIE. This context becomes the new
reality context.

To see the full trace, type `(load "dd")`.

## Instructions

GATE and DAYDREAMER run best compiled. To compile GATE, do
`(load "gate_compile")`. To compile DAYDREAMER, do `(load "dd_compile")`.

To run the LOVERS1 experience, a rationalization daydream, and a revenge
daydream, type `(load "dd")`.

Alternatively, to run a GATE test suite, do `(load "gate_test")`. To load
GATE, do `(load "gate_get")`. To load DAYDREAMER, first load GATE and then
do `(load "dd_get")`. Then to run DAYDREAMER, type `(daydreamer)`.

## Tutorial

Let's extend DAYDREAMER with the *need to hack*, which is satisfied
by FTPing the DAYDREAMER source code. Here is how we would do this.

First, download DAYDREAMER and `cd` to the DAYDREAMER
directory.

Then enter the following into a file called `hello_world.cl`:

```
  ; Load Gate and DAYDREAMER.
  
  (setq *gate-load-options* '(sample))
  (load "gate_get.cl")
  (load "dd_get.cl")
  
  ; Define types.
  ; This says that a FILE is a kind of OBJECT with a name slot,
  ; HACK is a NEED with a strength slot, and
  ; FTP is an ACTION with slots actor and obj.

  (ty$fcreate 'FILE '(OBJECT) '(name))
  (ty$fcreate 'HACK '(NEED) '(strength))
  (ty$fcreate 'FTP '(ACTION) '(actor obj))

  ; Define objects.
  ; This says that File1 is the DAYDREAMER source code, which is
  ; a FILE.

  (ob$fcreate '(FILE name "the DAYDREAMER source code" obname File1))
  
  ; Define needs.
  ; This says that DAYDREAMER has only two needs: to HACK and to
  ; be ENTERTAINED.
  
  (setq *needs* (list (ob$fcreate '(HACK)) (ob$fcreate '(ENTERTAINMENT))))
  
  ; Define concern initiation, planning, and action effect rules.

  (define-rule Hack-Theme (sample)
    (RULE subgoal (UAND (HACK) (UPROC 'Less-Need-Thresh?))
          goal (ACTIVE-GOAL (HACK strength (UPROC 'Need-Satisfied?)))
          is 'inference-only
          emotion (POS-EMOTION strength 0.6)
          inf-comments '(if "level of satisfaction of HACK need below"
                            "threshold"
                         then "ACTIVE-GOAL to HACK")
          plausibility 1.0))

  ; This says that one plan for hacking is to FTP File1, which is
  ; the DAYDREAMER source code.  

  (define-rule Hack-Plan (sample)
    (RULE subgoal (FTP actor ?Self obj File1)
          goal (HACK)
          plan-comments '(if "ACTIVE-GOAL to HACK"
                          then "ACTIVE-GOAL to FTP DAYDREAMER source code")
          is 'plan-only
          plausibility 1.0))

  ; This says that the FTP action has no preconditions.
  
  (define-rule Ftp-Plan (sample)
    (RULE subgoal (RTRUE)
          goal (FTP actor ?Self obj ?File)
          plan-comments '(if "ACTIVE-GOAL to FTP"
                          then "ACTIVE-GOAL for RTRUE")
          is 'action-plan
          plausibility 1.0))
  
  ; Define English generation rules.
  
  (define-gen HACK nil
    (gen-need-obj con stream switches context bp 'hack " some code"))
  
  (define-gen FTP nil
    (let ((subject (ob$gets con 'actor)))
       (gen-subject subject stream switches context bp)
       (gen-verb 'download subject stream switches (neg? con))
       (gen (ob$get con 'obj) stream switches context bp)
       subject))
```

Now start up Lisp and type the following:

```
  > (load "hello_world.cl")
  Adding rule HACK-THEME
  Adding rule HACK-PLAN
  Adding rule FTP-PLAN
  ...
  > (daydreamer)
```

DAYDREAMER will produce the following output:

```
  DAYDREAMER 3.5, Common Lisp version of 2004-12-20
  Initialize DAYDREAMER
  Performing first-time initialization
  Creating primal reality...
  Assert #{OB.285: (ROMANTIC-INTEREST obj MOVIE-STAR1......)} in CX.3
  Creating initial reality context...
  #{CX.3: (CX)} --> #{CX.4: (CX)}
  Assert #{OB.1575: (HACK strength 0.1)} in CX.4
  Assert #{OB.1576: (ENTERTAINMENT strength 0.1......)} in CX.4
  State changes from SUSPENDED to DAYDREAMING
  Run inferences in #{CX.4: (CX)}, bp = (#{ME: (FEMALE-PERSON first-name "Sarah"......)})
  ******************
  HACK-THEME HACK-THEME fired as inference in CX.4
  -------------------------------------------------------
  IF   level of satisfaction of HACK need below 
       threshold 
  THEN ACTIVE-GOAL to HACK 
  -------------------------------------------------------
  ?SELF = #{ME: (FEMALE-PERSON first-name "Sarah"......)}
  ******************
  Activate top-level goal #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)} in #{CX.4: (CX)}
  Assert #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)} in CX.4
  ==================================================
   I want to hack some code.
  ==================================================
  Add dependency from #{OB.1601: (POS-EMOTION strength 0.6)} to #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)} in #{CX.4: (CX)}
  Assert #{OB.1602: (DEPENDENCY linked-from (POS-EMOTION...)......)} in CX.4
  Assert #{OB.1601: (POS-EMOTION strength 0.6)} in CX.4
  ==================================================
   I feel interested in hacking some code.
  ==================================================
  Personal goal concern OB.1599: HACK motiv 0.6 status RUNABLE
  Running emotion-driven control loop...
  :Switching to new top-level goal #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)}
  ----------------------CX.4--------------------
  Running rules for #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)}
  setting last sprout concept = #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)} in #{CX.3: (CX)}
  Run inferences in #{CX.4: (CX)}, bp = (#{ME: (FEMALE-PERSON first-name "Sarah"......)})
  Running p-goals in #{CX.4: (CX)}
  Running plans in #{CX.4: (CX)} for #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)} bp (#{ME: (FEMALE-PERSON first-name "Sarah"......)})
  Run plan for #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)} in #{CX.4: (CX)}
  Try fact plans
  Try rules and episodes
  Find candidate rules for obj #{OB.1600: (HACK strength (UPROC proc ......))} in #{CX.4: (CX)}
  Order candidates ((#{HACK-PLAN: (RULE subgoal (FTP actor ?Self......)......)}
                     (T
                      (SELF
                       #{ME: (FEMALE-PERSON first-name "Sarah"......)}))))
  Run generic plan #{HACK-PLAN: (RULE subgoal (FTP actor ?Self......)......)} for #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)} in #{CX.4: (CX)}
  #{CX.4: (CX)} --> #{CX.5: (CX)}
  HACK-PLAN Debugging resumed.
  Pruning possibilities from (#{CX.5: (CX)})
  :----------------------CX.5--------------------
  Running rules for #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)}
  setting last sprout concept = #{OB.1611: (ACTIVE-GOAL obj (FTP actor ......)......)} in #{CX.4: (CX)}
  ******************
  HACK-PLAN fired as plan 
  for #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)}
  in CX.4 sprouting CX.5
  -------------------------------------------------------
  IF   ACTIVE-GOAL to HACK 
  THEN ACTIVE-GOAL to FTP DAYDREAMER source code 
  -------------------------------------------------------
  ?SELF = #{ME: (FEMALE-PERSON first-name "Sarah"......)}
  Assert #{OB.1609: (ORDERING)} in CX.5
  Instantiate and activate subgoals
  Activate subgoal for #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)} obj #{OB.1610: (FTP actor ME obj FILE1)} in #{CX.5: (CX)}
  Assert #{OB.1612: (INTENDS linked-from (ACTIVE-GOAL...)......)} in CX.5
  Assert #{OB.1611: (ACTIVE-GOAL obj (FTP actor ......)......)} in CX.5
  ==================================================
   I have to download the DAYDREAMER source code.
  ==================================================
  Run inferences in #{CX.5: (CX)}, bp = (#{ME: (FEMALE-PERSON first-name "Sarah"......)})
  Running p-goals in #{CX.5: (CX)}
  Running plans in #{CX.5: (CX)} for #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)} bp (#{ME: (FEMALE-PERSON first-name "Sarah"......)})
  Run plan for #{OB.1611: (ACTIVE-GOAL obj (FTP actor ......)......)} in #{CX.5: (CX)}
  Try rules and episodes
  Find candidate rules for obj #{OB.1610: (FTP actor ME obj FILE1)} in #{CX.5: (CX)}
  Order candidates ((#{FTP-PLAN: (RULE subgoal (RTRUE) goal ......)}
                     (T
                      (FILE
                       #{FILE1: (FILE name "the DAYDREAMER source code"......)})
                      (SELF
                       #{ME: (FEMALE-PERSON first-name "Sarah"......)}))))
  Run generic plan #{FTP-PLAN: (RULE subgoal (RTRUE) goal ......)} for #{OB.1611: (ACTIVE-GOAL obj (FTP actor ......)......)} in #{CX.5: (CX)}
  #{CX.5: (CX)} --> #{CX.6: (CX)}
  FTP-PLAN Debugging resumed.
  Pruning possibilities from (#{CX.6: (CX)})
  :----------------------CX.6--------------------
  Running rules for #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)}
  setting last sprout concept = NIL in #{CX.5: (CX)}
  ******************
  FTP-PLAN fired as plan 
  for #{OB.1611: (ACTIVE-GOAL obj (FTP actor ......)......)}
  in CX.5 sprouting CX.6
  -------------------------------------------------------
  IF   ACTIVE-GOAL to FTP 
  THEN ACTIVE-GOAL for RTRUE 
  -------------------------------------------------------
  ?FILE = #{FILE1: (FILE name "the DAYDREAMER source code"......)}
  ?SELF = #{ME: (FEMALE-PERSON first-name "Sarah"......)}
  Retract OB.1609 in CX.6
  Assert #{OB.1616: (ORDERING)} in CX.6
  Instantiate and activate subgoals
  Activate subgoal for #{OB.1611: (ACTIVE-GOAL obj (FTP actor ......)......)} obj #{OB.1617: (RTRUE)} in #{CX.6: (CX)}
  Assert #{OB.1619: (INTENDS linked-from (ACTIVE-GOAL...)......)} in CX.6
  Assert #{OB.1618: (ACTIVE-GOAL obj (RTRUE) top-level-goal ......)} in CX.6
  ******************
  Goal #{OB.1618: (ACTIVE-GOAL obj (RTRUE) top-level-goal ......)} succeeds in #{CX.6: (CX)}
  Retract OB.1618 in CX.6
  Assert #{OB.1620: (ACTIVE-GOAL obj (RTRUE))} in CX.6
  Retract OB.1619 in CX.6
  Assert #{OB.1622: (INTENDS linked-from (ACTIVE-GOAL...)......)} in CX.6
  Retract OB.1620 in CX.6
  Assert #{OB.1620: (SUCCEEDED-GOAL obj (RTRUE...)......)} in CX.6
  Subgoals of #{OB.1611: (ACTIVE-GOAL obj (FTP actor ......)......)} completed
  About to perform real action but not in performance mode
  Change status of OB.1599: HACK to WAITINGRun inferences in #{CX.6: (CX)}, bp = (#{ME: (FEMALE-PERSON first-name "Sarah"......)})
  :Taking optional object or concept input
  Enter concepts in #{CX.4: (CX)}
  Parser>
```

At this point, DAYDREAMER is asking for input. Just type `end` and
hit Enter.

```
  end
  End of parser input
  No more goals to run; switching to performance mode
  Change status of OB.1599: HACK to RUNABLEState changes from DAYDREAMING to PERFORMANCE
  :----------------------CX.6--------------------
  Running rules for #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)}
  setting last sprout concept = NIL in #{CX.5: (CX)}
  Subgoals of #{OB.1611: (ACTIVE-GOAL obj (FTP actor ......)......)} completed
  Perform external action
  Perform action goal #{OB.1611: (ACTIVE-GOAL obj (FTP actor ......)......)} in #{CX.6: (CX)}
  ******************
  Goal #{OB.1611: (ACTIVE-GOAL obj (FTP actor ......)......)} succeeds in #{CX.6: (CX)}
  Retract OB.1611 in CX.6
  Assert #{OB.1625: (ACTIVE-GOAL obj (FTP actor ......)......)} in CX.6
  Retract OB.1622 in CX.6
  Assert #{OB.1627: (INTENDS linked-from (ACTIVE-GOAL...)......)} in CX.6
  Retract OB.1612 in CX.6
  Assert #{OB.1628: (INTENDS linked-from (ACTIVE-GOAL...)......)} in CX.6
  Retract OB.1625 in CX.6
  Assert #{OB.1625: (SUCCEEDED-GOAL obj (FTP actor ......)......)} in CX.6
  Assert #{OB.1626: (FTP actor ME obj FILE1)} in CX.6
  ==================================================
   I download the DAYDREAMER source code.
  ==================================================
  Run inferences in #{CX.6: (CX)}, bp = (#{ME: (FEMALE-PERSON first-name "Sarah"......)})
  Taking optional concept input
  Enter concepts in #{CX.6: (CX)}
  Parser> end
  End of parser input
  Subgoals of #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)} completed
  ******************
  Goal #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)} succeeds in #{CX.6: (CX)}
  Replace obj of #{OB.1599: (ACTIVE-GOAL obj (HACK strength ......)......)} with (T)
  Retract OB.1599 in CX.6
  Assert #{OB.1599: (SUCCEEDED-GOAL obj (HACK strength ......)......)} in CX.6
  ==================================================
   I succeed at hacking some code.
  ==================================================
  Assert #{OB.1631: (HACK strength 1.0)} in CX.6
  ==================================================
   I hack some code.
  ==================================================
  Run inferences in #{CX.6: (CX)}, bp = (#{ME: (FEMALE-PERSON first-name "Sarah"......)})
  Running p-goals in #{CX.6: (CX)}
  Running plans in #{CX.6: (CX)} for #{OB.1599: (SUCCEEDED-GOAL obj (HACK strength ......)......)} bp (#{ME: (FEMALE-PERSON first-name "Sarah"......)})
  Terminating planning for top-level goal #{OB.1599: (SUCCEEDED-GOAL obj (HACK strength ......)......)}
  Leaf context #{CX.6: (CX)}
  [OB.1599: (SG. (HACK strength 1.0))]
    [OB.1625: (SG. (FTP actor ME obj FILE1))]
      [OB.1620: (SG. (RTRUE))]
  Removing motivating emotions of #{OB.1599: (SUCCEEDED-GOAL obj (HACK strength ......)......)} in #{CX.6: (CX)}
  Retract OB.1602 in CX.6
  Retract OB.1601 in CX.6
  Emotional responses for #{OB.1599: (SUCCEEDED-GOAL obj (HACK strength ......)......)} in #{CX.6: (CX)}
  Add dependency from #{OB.1599: (SUCCEEDED-GOAL obj (HACK strength ......)......)} to #{OB.1634: (POS-EMOTION)} in #{CX.6: (CX)}
  Assert #{OB.1635: (DEPENDENCY linked-from (SUCCEEDED-GOAL...)......)} in CX.6
  Assert #{OB.1634: (POS-EMOTION strength 0.6)} in CX.6
  ==================================================
   I feel pleased about hacking some code.
  ==================================================
  Store episode #{OB.1599: (SUCCEEDED-GOAL obj (HACK strength ......)......)} in #{CX.6: (CX)}
  Assess scenario desirability in #{CX.6: (CX)}
  #{OB.1599: (SUCCEEDED-GOAL obj (HACK strength ......)......)} (0.6)
  Scenario desirability = 0.6
  Store goal of episode #{OB.1599: (SUCCEEDED-GOAL obj (HACK strength ......)......)}, realism 1.0
  Store goal of episode #{OB.1625: (SUCCEEDED-GOAL obj (FTP actor ......)......)}, realism 1.0
  Store goal of episode #{OB.1620: (SUCCEEDED-GOAL obj (RTRUE...)......)}, realism 1.0
  Make episode for goal #{OB.1625: (SUCCEEDED-GOAL obj (FTP actor ......)......)}
  Storing #{EPISODE.1: (EPISODE rule FTP-PLAN goal ......)} under #{FTP-PLAN: (RULE subgoal (RTRUE) goal ......)}
  Make episode for goal #{OB.1599: (SUCCEEDED-GOAL obj (HACK strength ......)......)}
  Storing #{EPISODE.2: (EPISODE rule HACK-PLAN goal ......)} under #{HACK-PLAN: (RULE subgoal (FTP actor ?Self......)......)}
  Storing #{EPISODE.2: (EPISODE rule HACK-PLAN goal ......)} under #{OB.1634: (POS-EMOTION strength 0.6)}
  Assert #{OB.1634: (POS-EMOTION strength 0.6)} in EPISODIC-MEMORY
  Storing #{EPISODE.2: (EPISODE rule HACK-PLAN goal ......)} under #{FILE1: (FILE name "the DAYDREAMER source code"......)}
  Assert #{FILE1: (FILE name "the DAYDREAMER source code"......)} in EPISODIC-MEMORY
  Activate index #{HACK-PLAN: (RULE subgoal (FTP actor ?Self......)......)}
  Activate index #{FILE1: (FILE name "the DAYDREAMER source code"......)}
  ...
```

## Book

Essential reading for anyone who wants to understand, use, or
extend DAYDREAMER: *Daydreaming in Humans and Machines*
http://www.amazon.com/gp/product/1478137266/
