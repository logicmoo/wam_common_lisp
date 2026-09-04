

once_per_request(G):- must_run_html(G),!.

ensure_readable_html:-
  once_per_request(format('~s',['
<style>
body {
  //background: #000; 
  //mix-blend-mode: difference;
}

p {
	background: #ffffff;
	background-image: url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAMgAAADICAMAAACahl6sAAAAA1BMVEVilQmZw+RvAAAAAXRSTlOF3TSvyQAAAD1JREFUeNrtwQENAAAAwqD3T20PBxQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAPBmnQgAAd4aVNwAAAAASUVORK5CYII=");
	background-repeat: repeat-y;
    background-size: 0% auto;
	color: #ffffff;
	padding: 5px;
	text-align: center;
	border: 1px solid #3E8096;
	display: block; 
}
</style>'])).

ensure_swish_app_html:- in_pp(http),!.
ensure_swish_app_html:- in_pp(swish),!.
ensure_swish_app_html:- !.
ensure_swish_app_html:-
  once_per_request(format('\n<div id="hidden_swish_app" style="display:none; visibility:hidden">
		<header class="navbar navbar-default">
			<div class="container pull-left">
				<div class="navbar-header">
					<a href="/" class="pengine-logo">&nbsp;</a>
					<a href="/" class="swish-logo">&nbsp;</a>
				</div>
				<nav id="navbar"></nav>
			</div>
		</header>
		<div id="content" class="container">
		  <div class="tile horizontal" data-split="60%">
			<div class="prolog-editor"></div>
			<div class="tile vertical" data-split="70%">
			  <div class="prolog-runners"></div>
			  <div class="prolog-query"></div>
			</div>
		  </div>
		</div>
   </div>')).


add_context_menu:-
once_per_request((
write('
<div>
<cssmenu class="cssmenu">
  <li class="cssmenu-item">
    <button type="button" class="cssmenu-btn"> <i class="fa fa-folder-open"></i> <span class="cssmenu-text">Open</span> </button>
  </li>
  <li class="cssmenu-item disabled">
    <button type="button" class="cssmenu-btn"> <span class="cssmenu-text">Open in New Window</span> </button>
  </li>
  <li class="cssmenu-separator"></li>
  <li class="cssmenu-item">
    <button type="button" class="cssmenu-btn"> <i class="fa fa-reply"></i> <span class="cssmenu-text">Reply</span> </button>
  </li>
  <li class="cssmenu-item">
    <button type="button" class="cssmenu-btn"> <i class="fa fa-star"></i> <span class="cssmenu-text">Favorite</span> </button>
  </li>
  <li class="cssmenu-item subcssmenu">
    <button type="button" class="cssmenu-btn"> <i class="fa fa-users"></i> <span class="cssmenu-text">Social</span> </button>
    <cssmenu class="cssmenu">
      <li class="cssmenu-item">
        <button type="button" class="cssmenu-btn"> <i class="fa fa-comment"></i> <span class="cssmenu-text">Comment</span> </button>
      </li>
      <li class="cssmenu-item subcssmenu">
        <button type="button" class="cssmenu-btn"> <i class="fa fa-share"></i> <span class="cssmenu-text">Share</span> </button>
        <cssmenu class="cssmenu">
          <li class="cssmenu-item">
            <button type="button" class="cssmenu-btn"> <i class="fa fa-twitter"></i> <span class="cssmenu-text">Twitter</span> </button>
          </li>
          <li class="cssmenu-item">
            <button type="button" class="cssmenu-btn"> <i class="fa fa-facebook-official"></i> <span class="cssmenu-text">Facebook</span> </button>
          </li>
          <li class="cssmenu-item">
            <button type="button" class="cssmenu-btn"> <i class="fa fa-google-plus"></i> <span class="cssmenu-text">Google Plus</span> </button>
          </li>
          <li class="cssmenu-item">
            <button type="button" class="cssmenu-btn"> <i class="fa fa-envelope"></i> <span class="cssmenu-text">Email</span> </button>
          </li>
        </cssmenu>
      </li>
    </cssmenu>
  </li>
  <li class="cssmenu-separator"></li>
  <li class="cssmenu-item">
    <button type="button" class="cssmenu-btn"> <i class="fa fa-download"></i> <span class="cssmenu-text">Save</span> </button>
  </li>
  <li class="cssmenu-item">
    <button type="button" class="cssmenu-btn"> <i class="fa fa-edit"></i> <span class="cssmenu-text">Rename</span> </button>
  </li>
  <li class="cssmenu-item">
    <button type="button" class="cssmenu-btn"> <i class="fa fa-trash"></i> <span class="cssmenu-text">Delete</span> </button>
  </li>
</cssmenu>
<div class="container">
  <h1>JS & CSS Context Cssmenu</h1>
</div>
<script>
var cssmenu = document.querySelector(".cssmenu");

function showCssmenu(x, y){
    cssmenu.style.left = x + "px";
    cssmenu.style.top = y + "px";
    cssmenu.classList.add("show-cssmenu");
}

function hideCssmenu(){
    cssmenu.classList.remove("show-cssmenu");
}

function onContextCssmenu(e){
    e.preventDefault();
    showCssmenu(e.pageX, e.pageY);
    document.addEventListener("click", onClick, false);
}

function onClick(e){
    hideCssmenu();
    document.removeEventListener("click", onClick);
}

document.addEventListener("contextmenu", onContextCssmenu, false);
</script>
<style type="text/css">
html {
 // width: 100%;
 // height: 100%;
 // background: radial-gradient(circle, #fff 0%, #a6b9c1 100%) no-repeat;
}

//.container {
//  left: 0;
//  margin: auto;
//  position: absolute;
//  top: 20%;
//  width: 100%;
//  text-align: center;
//}

//h1, h2 { color: #555; }

/* Cssmenu */


.cssmenu {
  position: absolute;
  width: 200px;
  padding: 2px;
  margin: 0;
  border: 1px solid #bbb;
  background: #eee;
  background: -webkit-linear-gradient(to bottom, #fff 0%, #e5e5e5 100px, #e5e5e5 100%);
  background: linear-gradient(to bottom, #fff 0%, #e5e5e5 100px, #e5e5e5 100%);
  z-index: 100;
  border-radius: 3px;
  box-shadow: 1px 1px 4px rgba(0,0,0,.2);
  opacity: 0;
  -webkit-transform: translate(0, 15px) scale(.95);
  transform: translate(0, 15px) scale(.95);
  transition: transform 0.1s ease-out, opacity 0.1s ease-out;
  pointer-events: none;
}

.cssmenu-item {
  display: block;
  position: relative;
  margin: 0;
  padding: 0;
  white-space: nowrap;
}

.cssmenu-btn {
  background: none;
  line-height: normal;
  overflow: visible;
  -webkit-user-select: none;
  -moz-user-select: none;
  -ms-user-select: none;
  display: block;
  width: 100%;
  color: #444;
  font-family: "Roboto", sans-serif;
  font-size: 13px;
  text-align: left;
  cursor: pointer;
  border: 1px solid transparent;
  white-space: nowrap;
  padding: 6px 8px;
  border-radius: 3px;
}
 .cssmenu-btn::-moz-focus-inner, .cssmenu-btn::-moz-focus-inner {
 border: 0;
 padding: 0;
}

.cssmenu-text { margin-left: 25px; }

.cssmenu-btn .fa {
  position: absolute;
  left: 8px;
  top: 50%;
  -webkit-transform: translateY(-50%);
  transform: translateY(-50%);
}

.cssmenu-item:hover > .cssmenu-btn {
  color: #fff;
  outline: none;
  background-color: #2E3940;
  background: -webkit-linear-gradient(to bottom, #5D6D79, #2E3940);
  background: linear-gradient(to bottom, #5D6D79, #2E3940);
  border: 1px solid #2E3940;
}

.cssmenu-item.disabled {
  opacity: .5;
  pointer-events: none;
}

.cssmenu-item.disabled .cssmenu-btn { cursor: default; }

.cssmenu-separator {
  display: block;
  margin: 7px 5px;
  height: 1px;
  border-bottom: 1px solid #fff;
  background-color: #aaa;
}

.cssmenu-item.subcssmenu::after {
  content: "";
  position: absolute;
  right: 6px;
  top: 50%;
  -webkit-transform: translateY(-50%);
  transform: translateY(-50%);
  border: 5px solid transparent;
  border-left-color: #808080;
}

.cssmenu-item.subcssmenu:hover::after { border-left-color: #fff; }

.cssmenu .cssmenu {
  top: 4px;
  left: 99%;
}

.show-cssmenu, .cssmenu-item:hover > .cssmenu {
  opacity: 1;
  -webkit-transform: translate(0, 0) scale(1);
  transform: translate(0, 0) scale(1);
  pointer-events: auto;
}

.cssmenu-item:hover > .cssmenu {
  -webkit-transition-delay: 100ms;
  transition-delay: 300ms;
}
</style>
</div>

'))).



add_blur_menu:-
once_per_request((
write('
    <div id="content">
        <h1>jQuery Social Selection Examples</h1><div id="carbon-block"></div>
        <div id="search" class="demo">
            <h3>Search Engines</h3>
            <p>Lorem ipsum dolor sit amet </p>
        </div>
        <div id="share" class="demo">
            <h3>Social Share</h3>
            <p>Lorem ipsum dolor sit amet </p>
        </div>
        <div id="visit" class="demo">
            <h3>Quick links</h3>
            <p>Lorem ipsum dolor sit amet </p>
        </div>
        <div class="demo">
            <h3>Customize The Plugin</h3>
            <div id="custom0" class="demo demoGroup">
                <h4>Default</h4>
                <p>Lorem ipsum dolor sit amet </p>
            </div>
            <div id="custom1" class="demo demoGroup">
                <h4>Custom Icons</h4>
                <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>
            </div>
            <div id="custom2" class="demo demoGroup">
                <h4>5 Icons Per Row</h4>
                <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>
            </div>
            <div id="custom3" class="demo demoGroup">
                <h4>Change Search Keywords</h4>
                <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>
            </div>
            <div id="custom4" class="demo demoGroup">
                <h4>Localization</h4>
                <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>
            </div>
            <div id="custom5" class="demo demoGroup">
                <h4>Add New Platform</h4>
                <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>
            </div>
        </div>
        <div id="new" class="demo">
            <h3>QR Code</h3>
            <p>Lorem ipsum dolor sit amet  </p>
        </div>
        <div id="html" class="demo">
            <h3>HTML</h3>
            <p>You can also use pure HTML to initiation the plugin.</p>
            <selecton-tooltip data-target="#html" data-comps="foo">
                <component-search>
                    <google></google>
                </component-search>
                <component-share>
                    <facebook></facebook>
                </component-share>
                <component-visit data-github="abc"></component-visit>
                <component-foo data-icon-path="/swish/lm_xref/pixmapx/selected/img/qrcode.svg" data-text="123">
                    <bar1 data-name="Bar1" data-link="{{bar1}}"  data-icon-path="/swish/lm_xref/pixmapx/selected/img/wechat.svg" data-require="qrcode"></bar1>
                </component-foo>
            </selecton-tooltip>
        </div>
    </div>


<script type="text/javascript">

(function() {
    let search = {
        components: {
            search: {
                enabled: "all"
            },
            share: {
                enabled: "none"
            },
            visit: {
                enabled: "none"
            },
        }
    }
    let share = {
        components: {
            search: {
                enabled: "none"
            },
            share: {
                enabled: "all"
            },
            visit: {
                enabled: "none"
            },
        }
    }
    let visit = {
        dev: true,
        components: {
            search: {
                enabled: "none"
            },
            share: {
                enabled: "none"
            },
            visit: {
                enabled: "all"
            },
        }
    }
    let custom = {
        components: {
            search: {
                enabled: "all"
            },
            share: {
                enabled: "none"
            },
            visit: {
                enabled: "none"
            },
        }
    }
    let newComp = {
        components: {
            search: {
                enabled: ["google", "bing"]
            },
            share: {
                enabled: ["facebook", "line"]
            },
            visit: {
                facebook: "default",
                youtube: "default"
            },

            foo: {
                enabled: ["bar1"],
                url: "google.com",
                iconPath: "/swish/lm_xref/pixmapx/selected/img/qrcode.svg",
                platforms: {
                    bar1: {
                        link: "{{url}}",
                        iconPath: "/swish/lm_xref/pixmapx/selected/img/wechat.svg",
                        require: ["qrcode"]
                    }
                }
            }
        },
        locale: {
            components: {
                foo: {
                    title: "Share QR Code to {{platform}}",
                    platforms: {
                        bar1: "Bar1"
                    },
                }
            }
        },
    }
    let customConfig = [
        {},
        {
            components: {
                search: {
                    iconPath: "/swish/lm_xref/pixmapx/selected/img/wikipedia.svg",
                    platforms: {
                        wikipedia: {
                            iconPath: "/swish/lm_xref/pixmapx/selected/img/search.svg"
                        }
                    }
                },
            }
        },
        { itemsPerColumn: 5 },
        {
            components: {
                search: {
                    text: "Hello World",
                }
            }
        },
        {
            locale: {
                components: {
                    search: {
                        title: "{{platform}} Custom Search",
                    }
                },
                platforms: {
                    google: "New Google"
                }
            }
        },
        {
            components: {
                search: {
                    platforms: {
                        google2: {
                            link: "https://www.google.com/search?q={{text}}",
                            name: "Google2",
                            iconPath: "https://www.google.com/images/branding/googlelogo/2x/googlelogo_color_272x92dp.png"
                        },
                    },
                }
            }
        },
    ]
    $("#default").socialSelection(newComp);
    // $("#body").socialSelection(newComp);
    //$("#table4").socialSelection(newComp);

    $("#search").socialSelection(search);
    $("#share").socialSelection(share);
    $("#visit").socialSelection(visit);
    $("#new").socialSelection(newComp);
    customConfig.map((v, i) => {
        $("#custom" + i).socialSelection($.fn.deepExtend(v, custom));
   })
   
</script>

'))).


one_2_three_blur:-
once_per_request((
write('
<script type="text/javascript">

if (!window.x) {
    window.x = {};
}

window.x.Selector = {};
window.x.Selector.getSelected = function() {
    var t = "";
    if (window.getSelection) {
        t = window.getSelection();
    } else if (document.getSelection) {
        t = document.getSelection();
    } else if (document.selection) {
        t = document.selection.createRange().text;
    }
    return t;
}

var pageX;
var pageY;
var selectedX;
var lastSelectedText;

$(document).ready(function() {
    $(document).bind("mouseup", function() {


    var selectedText = x.Selector.getSelected();

    if(selectedText != ""){            

			document.execCommand("copy")
			lastSelectedText = selectedText;
	 
      $("div.selectionTooltip:first").css({"left": pageX - 105,"top" : pageY - 175}).fadeIn(200);  
      $newdiv1.prepend($("div.selectionTooltip").clone())
      $newdiv1.prepend("<p>"+selectedText+"</p>");
            $newdiv1.css({
                "left": pageX - 55,
                "top" : pageY - 155
            }).fadeIn(200);
            
           
        } else {
           // $("ul.tools").fadeOut(200);
            //$("div.selectionTooltip").fadeOut(200);            
        }
    });

    $(document).on("mousedown", function(e){
        pageX = e.pageX;
        pageY = e.pageY;
            if (e.buttons == 2) {
				//debugger;
				if(lastSelectedText!=null)  {
					//document.execCommand("Paste", null, null); // 
				    document.execCommand("paste")
					//lastSelectedText = null;
				}
			}
	
    
    });


  function getClipboard() {
    var pasteTarget = document.createElement("div");
    pasteTarget.contentEditable = true;
    var actElem = document.activeElement.appendChild(pasteTarget).parentNode;
    pasteTarget.focus();
    document.execCommand("Paste", null, null);
    var paste = pasteTarget.innerText;
    actElem.removeChild(pasteTarget);
    return paste;
  };
});
</script>
'))).







ensure_collapsable_styles:- 
once_per_request(pformat_write('<style>
.collapsible {
  background-color: #777;
  color: white;
  cursor: pointer;
  padding: 18px;
  width: 100%;
  border: none;
  text-align: left;
  outline: none;
  font-size: 15px;
}

.active, .collapsible:hover {
  background-color: #555;
}

.collapsible:after {
  content: "\\002B";
  color: white;
  font-weight: bold;
  float: right;
  margin-left: 5px;
}

.active:after {
  content: "\\2212";
}

.collapsed-c {
  padding: 0 18px;
  max-height: 0;
  overflow: hidden;
  transition: max-height 0.2s ease-out;
  background-color: #f1f1f1;
}

</style>

')).

ensure_collapsable_script:- 
once_per_request(pformat_write('
<script>
var coll = document.getElementsByClassName("collapsible");
var i;

for (i = 0; i < coll.length; i++) {
  coll[i].addEventListener("click", function(e) {
    this.classList.toggle("active");
    e.preventDefault();
    var content = this.nextElementSibling;
    if (content.style.maxHeight){
      content.style.maxHeight = null;
    } else {
      content.style.maxHeight = content.scrollHeight + "px";
    }     
  });
}
</script> ')).






show_menu_types:-
 (once_per_request(write('  	

		<div class="displayBox">

			<div class="header"><h1>Example Pop-Up Icon Menus</h1></div>

			<div class="icon-box-top">
				<div id="defaultTest" style="margin-left: 75px">
					<i class="fa fa-cog leftBorder rightBorder"></i>
				</div>
				<p class="icon-text">
					Menu set with default settings and nothing passed through to the plugin. Will result in a menu with 2 items that give information on how to set up menus.
					<br />
					Contains 3 icons. Uses Font Awesome icons.
				</p>
			</div>

			<div class="icon-box">
				<div id="myPopUp" name="menu1">
					<i class="fa fa-cog leftBorder rightBorder"></i>
				</div>
				<p class="icon-text">
					Menu using a settings icon. Default styling color, position on top, activated by click, animation: bounce, disappears on click outside of the menu. Set to show an alert box but could be set to a link.
					<br />
					Contains 4 icons. Uses Font Awesome icons.
				</p>
			</div>

			<div class="icon-box">
				<div id="myPopUp2">
					<i class="fa fa-bars leftBorder rightBorder"></i>
				</div>
				<p class="icon-text">
					Menu using a hamburger icon. Red theme styling, position to the left, activated by click, animation: flip, disappears on click outside of the menu. Set to show/hide an area of the screen.
					<br />
					Contains 3 icons. Uses Font Awesome icons.
				</p>
			</div>

			<div class="icon-box">
				<div id="myPopUp3">
					<i class="fa fa-thumbs-up leftBorder rightBorder"></i>
				</div>
				<p class="icon-text">
					Menu using a thumbs-up icon. Custom styling, position to the right, activated by hover, animation: grow, appears on mouseover the menu and disappears on mouse leaving the popup menu. Set to append a div to the screen.
					<br />
					Contains 5 icons. Uses Google Material Icons. Button can be moved around the screen for a movable menu thanks to jQuery"s "draggable" ui (if you link it).
				</p>
			</div>
<p/>
			<div class="icon-box">
				<div id="myPopUp4">
					<i class="material-icons leftBorder rightBorder">more_vert</i>          
				</div>
				<p class="icon-text">
					Menu using 3 vertical dots. Green theme styling, position to the bottom, activated by click, animation: standard, disappears only when clicked again (not outside of the icon). Set to show an alert box but could be set to a link.
					<br />
					Contains 5 icons. Uses Google Material Icons. Button can be moved around the screen for a movable menu thanks to jQuery"s "draggable" ui (if you link it).
				</p>
			</div>

			<div class="icon-box">
				<p id="textPopup">Menu</p>
				<p class="menuText">
					A text type menu. Blue theme styling, position to the bottom, activated by hover, animation: standard, appears on mouseover the menu and disappears on mouse leaving the popup menu. Set to show an alert box but could be set to a link.
					<br />
					Contains 4 icons. Uses Font Awesome icons.
				</p>
			</div>

		</div>

		<div id="starWarsIcons" title="Click the icon to clear it.">
			<div id="rebel">
				<i class="fa fa-ra swIcon"></i>
				<p>Rebel Alliance</p>
			</div>

			<div id="empire">
				<i class="fa fa-empire swIcon"></i>
				<p>Empire</p>
			</div>

			<div id="firstOrderIcon">
				<i class="fa fa-first-order swIcon"></i>
				<p>First Order</p>
			</div>

		</div>'))).


world_snap:- once_per_request((world_snap(X),print_html_term_tree(X))).

world_snap([perceptq(x(player,i1),[]),props(x(cup,i1),[traits([cup,object]),
  shape=cup,default_rel=in,opened=f,can_be(open,t),has_rel(in,t),inherited(container),inherited(physical),
  can_be(move,t),can_be(touch,t),cleanliness=clean,class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),
  inherited(fully_corporial),can_be(examine,t),inherited(thinkable),inherited(moveable),inherited(flask),inherited(cup),
  co([shape=cup,traits([cup]),inherit(cup,t),sp(nouns,[cup])])]),props(x(cabinate,i1),[shape=cabinate,traits([cabinate]),
default_rel=in,opened=f,can_be(open,t),has_rel(in,t),inherited(container),can_be(take,f),
class_desc(["kind is an Immobile Object","kind is corporial","kind is normally thinkable","kind is furnature"]),
inherited(untakeable),can_be(touch,t),inherited(fully_corporial),has_rel(on,t),cleanliness=clean,inherited(surface),
can_be(examine,t),inherited(thinkable),inherited(physical),inherited(furnature),volume_capacity=10,inherited(cabinate),
 co([shape=cabinate,traits([cabinate]),inherit(cabinate,t),sp(nouns,[cabinate])])]),props(x(plate,i1),[traits([plate,object]),
 shape=plate,has_rel(on,t),default_rel=on,inherited(surface),inherited(physical),can_be(move,t),can_be(touch,t),cleanliness=clean,
 class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),inherited(fully_corporial),
  can_be(examine,t),inherited(thinkable),inherited(moveable),volume_capacity=2,breaks_into=shards,inherited(plate),
  co([shape=plate,traits([plate]),inherit(plate,t),sp(nouns,[plate])])]),props(x(sink,i1),[traits([sink,object]),shape=sink,
 cleanliness=dirty,opened=t,can_be(close,f),can_be(open,f),inherited(uncloseable),default_rel=in,has_rel(in,t),inherited(container),
  can_be(move,t),class_desc(["kind is an Movable Object","kind is an Immobile Object","kind is corporial","kind is normally thinkable","kind is furnature"]),inherited(moveable),inherited(flask),can_be(take,f),inherited(untakeable),can_be(touch,t),inherited(fully_corporial),has_rel(on,t),inherited(surface),can_be(examine,t),inherited(thinkable),inherited(physical),inherited(furnature),volume_capacity=5,inherited(sink),co([shape=sink,traits([sink]),inherit(sink,t),sp(nouns,[sink])])]),props(x(lamp,i1),[traits([lamp,object]),shape=lamp,name="shiny brass lamp",powered=t,can_be(switch,t),inherited(light),inherited(brass),inherited(shiny),inherited(physical),can_be(move,t),can_be(touch,t),cleanliness=clean,class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),inherited(fully_corporial),can_be(examine,t),inherited(thinkable),inherited(moveable),emitting(see,light),effect(switch(on),setprop(x(lamp,i1),emitting(see,light))),effect(switch(off),delprop(x(lamp,i1),emitting(see,light))),breaks_into=broken_lamp,inherited(lamp),co([shape=lamp,traits([lamp]),inherit(lamp,t),sp(nouns,[lamp])])]),props(x(flour,i1),[traits([flour,object]),shape=flour,can_be(eat,t),inherited(physical),can_be(move,t),can_be(touch,t),cleanliness=clean,class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),inherited(fully_corporial),can_be(examine,t),inherited(thinkable),inherited(moveable),inherited(food),ammount=some,inherited(measurable),inherited(flour),co([shape=flour,traits([flour]),inherit(flour,t),sp(nouns,[flour])])]),props(x(bowl,i1),[traits([bowl,object]),shape=bowl,opened=t,can_be(close,f),can_be(open,f),inherited(uncloseable),default_rel=in,has_rel(in,t),inherited(container),inherited(physical),can_be(move,t),can_be(touch,t),cleanliness=clean,class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),inherited(fully_corporial),can_be(examine,t),inherited(thinkable),inherited(moveable),inherited(flask),volume_capacity=2,breaks_into=shards,name="porcelain bowl",desc="This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface.",inherited(bowl),co([shape=bowl,traits([bowl]),inherit(bowl,t),sp(nouns,[bowl])])]),props(x(box,i1),[traits([box,object]),shape=box,volume_capacity=11,default_rel=in,opened=f,can_be(open,t),has_rel(in,t),inherited(container),inherited(physical),can_be(move,t),can_be(touch,t),cleanliness=clean,class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),inherited(fully_corporial),can_be(examine,t),inherited(thinkable),inherited(moveable),can_be(burn,t),inherited(paper),inherited(cardboard),inherited(box),co([shape=box,traits([box]),inherit(box,t),sp(nouns,[box])])]),props(x(table,i1),[shape=(table),traits([table]),has_rel(on,t),cleanliness=clean,inherited(surface),inherited(physical),default_rel=on,inherited(table),co([shape=(table),traits([table]),inherit(table,t),sp(nouns,[table])])]),props(x(table_leg,i1),[shape=table_leg,traits([table_leg]),inherited(table_leg),co([shape=table_leg,traits([table_leg]),inherit(table_leg,t),sp(nouns,[table_leg])])]),props(x(brklamp,i1),[traits([brklamp,object]),shape=brklamp,name="definately broken",effect(switch(on),true),effect(switch(off),true),nominals(broken),inherited(dented),inherited(broken),effect(hit,[print_("Hit brklamp"),setprop(x(brklamp,i1),inherit(broken))]),powered=t,can_be(switch,t),inherited(light),inherited(brass),inherited(shiny),inherited(physical),can_be(move,t),can_be(touch,t),cleanliness=clean,class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),inherited(fully_corporial),can_be(examine,t),inherited(thinkable),inherited(moveable),emitting(see,light),breaks_into=broken_lamp,inherited(lamp),inherited(brklamp),co([shape=brklamp,traits([brklamp]),inherit(brklamp,t),sp(nouns,[brklamp])])]),props(x(apple,i1),[shape=apple,traits([apple]),inherited(apple),co([shape=apple,traits([apple]),inherit(apple,t),sp(nouns,[apple])])]),props(x(crate,i1),[traits([crate,object]),shape=crate,default_rel=in,opened=f,can_be(open,t),has_rel(in,t),inherited(container),inherited(physical),can_be(move,t),can_be(touch,t),cleanliness=clean,class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),inherited(fully_corporial),can_be(examine,t),inherited(thinkable),inherited(moveable),volume_capacity=13,breaks_into=splinters,can_be(burn,t),inherited(wooden),inherited(crate),co([shape=crate,traits([crate]),inherit(crate,t),sp(nouns,[crate])])]),props(x(screendoor,i1),[shape=screendoor,traits([screendoor]),door_to(kitchen),door_to(garden),class_desc(["kind is an Immobile Object","kind is furnature","kind is normally thinkable","kind is corporial"]),inherited(untakeable),has_rel(on,t),default_rel=on,inherited(surface),inherited(physical),inherited(furnature),can_be(open,t),can_be(close,t),opened=f,can_be(touch,t),can_be(examine,t),inherited(thinkable),cleanliness=clean,inherited(fully_corporial),can_be(take,f),inherited(door),inherited(screendoor),co([shape=screendoor,traits([screendoor]),inherit(screendoor,t),sp(nouns,[screendoor])])]),props(x(fireplace,i1),[shape=fireplace,traits([fireplace]),has_rel(on,f),has_rel(over,t),opened=t,can_be(close,f),can_be(open,f),default_rel=in,has_rel(in,t),
 inherited(container),inherited(uncloseable),volume_capacity=20,can_be(take,f),class_desc(["kind is an Immobile Object","kind is corporial","kind is normally thinkable","kind is furnature"]),inherited(untakeable),can_be(touch,t),inherited(fully_corporial),cleanliness=clean,inherited(surface),can_be(examine,t),inherited(thinkable),inherited(physical),inherited(furnature),inherited(fireplace),co([shape=fireplace,traits([fireplace]),inherit(fireplace,t),sp(nouns,[fireplace])])]),props(x(videocamera,i1),[shape=videocamera,traits([videocamera]),inherited(memorize_perceptq),can_be(switch,t),effect(switch(on),setprop(x(videocamera,i1),powered=t)),effect(switch(off),setprop(x(videocamera,i1),powered=f)),powered=t,has_sense(see),breaks_into=broken_videocam,inherited(videocamera),co([shape=videocamera,traits([videocamera]),inherit(videocamera,t),sp(nouns,[videocamera])])]),memories(x(videocamera,i1),[spropOf(memories,x(videocamera,i1)),structure_label(mem(x(videocamera,i1))),timestamp(0,5848.7),current_goals(_29854,[]),goals_skipped(_29854,[]),goals_satisfied(_29854,[]),intent(_29854,[act3(look,x(videocamera,i1),[])]),inst(x(videocamera,i1))]),perceptq(x(videocamera,i1),[]),props(x(shovel,i1),[shape=shovel,traits([shovel]),inherited(shovel),co([shape=shovel,traits([shovel]),inherit(shovel,t),sp(nouns,[shovel])])]),props(x(mushroom,i1),[traits([mushroom,object]),shape=mushroom,name="speckled mushroom",inherited(physical),can_be(move,t),can_be(touch,t),cleanliness=clean,class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),inherited(fully_corporial),can_be(examine,t),inherited(thinkable),inherited(moveable),ammount=some,inherited(measurable),inherited(food),inherited(fungus),inherited(toadstool),inherited(speckled),initial("A speckled mushroom grows out of the sodden earth, on a long stalk."),desc="The mushroom is capped with blotches, and you aren't at all sure it's not a toadstool.",can_be(eat,t),before(eat,(random100=<30,die("It was poisoned!");"yuck!")),after(take,(initial,"You pick the mushroom, neatly cleaving its thin stalk.")),inherited(mushroom),co([shape=mushroom,traits([mushroom]),inherit(mushroom,t),sp(nouns,[mushroom])])]),props(x(water,i1),[shape=water,traits([water]),inherited(water),co([shape=water,traits([water]),inherit(water,t),sp(nouns,[water])])]),props(x(fountain,i1),[traits([fountain,object]),shape=fountain,volume_capacity=150,desc="this is a place",inherited(here),can_be(move,f),has_rel(fn(exit,_30660),t),inherited(place),cleanliness=dirty,opened=t,can_be(close,f),can_be(open,f),inherited(uncloseable),default_rel=in,has_rel(in,t),inherited(container),class_desc(["kind is an Movable Object","kind is an Immobile Object","kind is corporial","kind is normally thinkable","kind is furnature"]),inherited(moveable),inherited(flask),can_be(take,f),inherited(untakeable),can_be(touch,t),inherited(fully_corporial),has_rel(on,t),inherited(surface),can_be(examine,t),inherited(thinkable),inherited(physical),inherited(furnature),inherited(sink),inherited(fountain),co([shape=fountain,traits([fountain]),inherit(fountain,t),sp(nouns,[fountain])])]),props(x(rock,i1),[shape=rock,traits([rock]),inherited(rock),co([shape=rock,traits([rock]),inherit(rock,t),sp(nouns,[rock])])]),props(x(locker,i1),[traits([locker,object]),shape=locker,default_rel=in,can_be(open,t),has_rel(in,t),inherited(container),inherited(physical),can_be(move,t),can_be(touch,t),cleanliness=clean,class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),inherited(fully_corporial),can_be(examine,t),inherited(thinkable),inherited(moveable),volume_capacity=13,can_be(burn,f),inherited(metal),opened=f,inherited(locker),co([shape=locker,traits([locker]),inherit(locker,t),sp(nouns,[locker])])]),props(x(shelf,i1),[shape=shelf,traits([shelf]),can_be(take,f),class_desc(["kind is an Immobile Object","kind is corporial","kind is normally thinkable","kind is furnature"]),inherited(untakeable),can_be(touch,t),inherited(fully_corporial),has_rel(on,t),default_rel=on,cleanliness=clean,inherited(surface),can_be(examine,t),inherited(thinkable),inherited(physical),inherited(furnature),inherited(shelf),co([shape=shelf,traits([shelf]),inherit(shelf,t),sp(nouns,[shelf])])]),props(x(living_room,i1),[shape=living_room,traits([living_room]),volume_capacity=10000,default_rel=in,desc="this is a place",has_rel(in,t),inherited(here),can_be(move,f),can_be(take,f),has_rel(fn(exit,_31872),t),inherited(place),inherited(living_room),co([shape=living_room,traits([living_room]),inherit(living_room,t),sp(nouns,[living_room])])]),props(x(dining_room,i1),[shape=dining_room,traits([dining_room]),volume_capacity=10000,default_rel=in,desc="this is a place",has_rel(in,t),inherited(here),can_be(move,f),can_be(take,f),has_rel(fn(exit,_32090),t),inherited(place),inherited(dining_room),co([shape=dining_room,traits([dining_room]),inherit(dining_room,t),sp(nouns,[dining_room])])]),props(x(garden,i1),[shape=garden,traits([garden]),volume_capacity=10000,default_rel=in,desc="this is a place",has_rel(in,t),inherited(here),can_be(move,f),can_be(take,f),has_rel(fn(exit,_32308),t),inherited(place),cant_go($agent,up,"You lack the ability to fly."),inherited(garden),co([shape=garden,traits([garden]),inherit(garden,t),sp(nouns,[garden])])]),props(x(basement,i1),[shape=basement,traits([basement]),volume_capacity=10000,default_rel=in,desc="this is a place",has_rel(in,t),inherited(here),can_be(move,f),can_be(take,f),has_rel(fn(exit,_32544),t),inherited(place),dark=t,inherited(basement),co([shape=basement,traits([basement]),inherit(basement,t),sp(nouns,[basement])])]),props(x(wrench,i1),[traits([wrench,object]),shape=wrench,inherited(physical),can_be(move,t),class_desc(["kind is an Movable Object","kind is normally thinkable","kind is corporial"]),inherited(moveable),can_be(touch,t),can_be(examine,t),inherited(thinkable),cleanliness=clean,inherited(fully_corporial),inherited(shiny),inherited(wrench),co([shape=wrench,traits([wrench]),inherit(wrench,t),sp(nouns,[wrench])])]),props(x(coins,i1),[traits([coins,object]),shape=coins,inherited(physical),can_be(move,t),class_desc(["kind is an Movable Object","kind is normally thinkable","kind is corporial"]),inherited(moveable),can_be(touch,t),can_be(examine,t),inherited(thinkable),cleanliness=clean,inherited(fully_corporial),inherited(shiny),ammount=some,inherited(measurable),inherited(coins),co([shape=coins,traits([coins]),inherit(coins,t),sp(nouns,[coins])])]),props(x(bag,i1),[traits([bag,object]),shape=bag,volume_capacity=10,default_rel=in,opened=f,can_be(open,t),has_rel(in,t),inherited(container),inherited(physical),can_be(move,t),can_be(touch,t),cleanliness=clean,class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),inherited(fully_corporial),can_be(examine,t),inherited(thinkable),inherited(moveable),inherited(bag),co([shape=bag,traits([bag]),inherit(bag,t),sp(nouns,[bag])])]),props(x(watch,i1),[shape=watch,traits([watch]),inherited(watch),co([shape=watch,traits([watch]),inherit(watch,t),sp(nouns,[watch])])]),props(x(player,i1),[traits([player,console]),shape=player,name=x(player,i1),look_depth=3,user_mode=2,access_level=admin,inherited(physical),inherited(console),knows_verbs(eat,t),has_rel(worn_by,t),has_rel(held_by,t),model_depth=3,mass=50,volume=50,has_sense(see),knows_verbs(examine,t),inherited(actor),inherited(perceptive),class_desc(["When entering a new area the Agent will automatically\n            get an overview of the env (without purposeful looking)","Sensory percepts that discover new objects request further details (notice dirty plates are in the sink)","kind is corporial","kind is normally thinkable","direct inheriters are completely noncorporial","kind is both partly corporial and non-corporial"]),inherited(autolook),inherited(autoscan),can_be(touch,t),cleanliness=clean,inherited(fully_corporial),inherit(fully_corporial,t),can(examine)=f,can_be(examine,t),inherited(thinkable),inherited(noncorporial),inherited(partly_noncorporial),inherited(character),can_be(switch(off),f),powered=t,inherited(humanoid),inherited(player),co([shape=player,traits([player]),inherit(player,t),sp(nouns,[player])])]),memories(x(player,i1),[spropOf(memories,x(player,i1)),structure_label(mem(x(player,i1))),timestamp(0,5848.6),current_goals(_34146,[]),goals_skipped(_34146,[]),goals_satisfied(_34146,[]),intent(_34146,[act3(look,x(player,i1),[])]),inst(x(player,i1))]),props(x(kitchen,i1),[shape=kitchen,traits([kitchen]),volume_capacity=10000,default_rel=in,desc="this is a place",has_rel(in,t),inherited(here),can_be(move,f),can_be(take,f),has_rel(fn(exit,_34354),t),inherited(place),inherited(kitchen),co([shape=kitchen,traits([kitchen]),inherit(kitchen,t),sp(nouns,[kitchen])])]),props(x(floyd,i1),[traits([floyd,object]),shape=floyd,name="Floyd the robot",powered=f,knows_verbs(eat,f),inherited(impulsive),class_desc(["like Floyd the robot will, instances will automatically use its planner\n        about planning to decide on what to do","kind is an Movable Object","When entering a new area the Agent will automatically\n            get an overview of the env (without purposeful looking)","Sensory percepts that discover new objects request further details (notice dirty plates are in the sink)","kind is corporial","kind is normally thinkable","direct inheriters are completely noncorporial","kind is both partly corporial and non-corporial"]),inherited(autonomous),emitting(see,light),mass=200,inherited(metallic),desc="Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever.",can_be(switch,t),iza(memorizer),inherited(physical),can_be(move,t),inherited(moveable),inherited(shiny),has_rel(worn_by,t),has_rel(held_by,t),model_depth=3,volume=50,has_sense(see),knows_verbs(examine,t),inherited(actor),inherited(perceptive),inherited(autolook),inherited(autoscan),can_be(touch,t),cleanliness=clean,inherited(fully_corporial),inherit(fully_corporial,t),can(examine)=f,can_be(examine,t),inherited(thinkable),inherited(noncorporial),inherited(partly_noncorporial),inherited(character),effect(switch(on),setprop(x(floyd,i1),powered=t)),effect(switch(off),setprop(x(floyd,i1),powered=f)),inherited(robot),inherited(floyd),co([shape=floyd,traits([floyd]),inherit(floyd,t),sp(nouns,[floyd])])]),perceptq(x(floyd,i1),[]),memories(x(floyd,i1),[spropOf(memories,x(floyd,i1)),structure_label(mem(x(floyd,i1))),timestamp(0,5848.6),current_goals(_35136,[]),goals_skipped(_35136,[]),goals_satisfied(_35136,[]),intent(_35136,[act3(look,x(floyd,i1),[])]),inst(x(floyd,i1))]),props(x(pantry,i1),[shape=pantry,traits([pantry]),volume_capacity=1000,inherited(closet),nominals(kitchen),desc="You're in a dark kitchen pantry.",dark=t,default_rel=in,has_rel(in,t),inherited(here),can_be(move,f),can_be(take,f),has_rel(fn(exit,_35376),t),inherited(place),inherited(pantry),co([shape=pantry,traits([pantry]),inherit(pantry,t),sp(nouns,[pantry])])]),h(spatial,in,x(floyd,i1),x(pantry,i1)),h(spatial,in,x(player,i1),x(kitchen,i1)),h(spatial,worn_by,x(watch,i1),x(player,i1)),h(spatial,held_by,x(bag,i1),x(player,i1)),h(spatial,in,x(coins,i1),x(bag,i1)),h(spatial,held_by,x(wrench,i1),x(floyd,i1)),h(spatial,fn(exit,north),x(kitchen,i1),x(pantry,i1)),h(spatial,fn(exit,south),x(pantry,i1),x(kitchen,i1)),h(spatial,fn(exit,down),x(pantry,i1),x(basement,i1)),h(spatial,fn(exit,up),x(basement,i1),x(pantry,i1)),h(spatial,fn(exit,south),x(kitchen,i1),x(garden,i1)),h(spatial,fn(exit,north),x(garden,i1),x(kitchen,i1)),h(spatial,fn(exit,west),x(kitchen,i1),x(dining_room,i1)),h(spatial,fn(exit,east),x(dining_room,i1),x(kitchen,i1)),h(spatial,fn(exit,west),x(dining_room,i1),x(living_room,i1)),h(spatial,fn(exit,east),x(living_room,i1),x(dining_room,i1)),h(spatial,in,x(shelf,i1),x(pantry,i1)),h(spatial,in,x(locker,i1),x(pantry,i1)),h(spatial,in,x(rock,i1),x(garden,i1)),h(spatial,in,x(rock,2),x(garden,i1)),h(spatial,in,x(fountain,i1),x(garden,i1)),h(spatial,in,x(water,i1),x(fountain,i1)),h(spatial,in,x(mushroom,i1),x(garden,i1)),h(spatial,in,x(shovel,i1),x(basement,i1)),h(spatial,in,x(videocamera,i1),x(living_room,i1)),h(spatial,in,x(fireplace,i1),x(living_room,i1)),h(spatial,in,x(screendoor,i1),x(kitchen,i1)),h(spatial,in,x(crate,i1),x(kitchen,i1)),h(spatial,in,x(apple,i1),x(crate,i1)),h(spatial,in,x(screendoor,i1),x(garden,i1)),h(spatial,in,x(brklamp,i1),x(garden,i1)),h(spatial,on,x(table,i1),x(table_leg,i1)),h(spatial,on,x(box,i1),x(table,i1)),h(spatial,in,x(bowl,i1),x(box,i1)),h(spatial,in,x(flour,i1),x(bowl,i1)),h(spatial,in,x(table,i1),x(kitchen,i1)),h(spatial,on,x(lamp,i1),x(table,i1)),h(spatial,in,x(sink,i1),x(kitchen,i1)),h(spatial,in,x(plate,i1),x(sink,i1)),h(spatial,in,x(cabinate,i1),x(kitchen,i1)),h(spatial,in,x(cup,i1),x(cabinate,i1)),type_props(broken_videocam,[can_be(switch,f),powered=f,inherit(videocamera,t)]),type_props(videocamera,[inherit(memorizer,t),inherit(perceptq,t),inherit(memorize_perceptq,t),can_be(switch,t),effect(switch(on),setprop($self,powered=t)),effect(switch(off),setprop($self,powered=f)),powered=t,has_sense(see),breaks_into=broken_videocam]),type_props(wrench,[inherit(shiny,t)]),type_props(table,[inherit(surface,t),inherit(physical,t),default_rel=on]),type_props(shelf,[inherit(surface,t),inherit(physical,t),inherit(furnature,t)]),type_props(surface,[has_rel(on,t),default_rel=on,inherit(physical,t),cleanliness=clean]),type_props(broken_lamp,[name="dented brass lamp",inherit(light,t),traits(brass),inherit(broken,t),inherit(dented,t),can_be(switch,t),effect(switch(on),true),effect(switch(off),true)]),type_props(lamp,[name="shiny brass lamp",powered=t,can_be(switch,t),inherit(light,t),inherit(brass,t),inherit(shiny,t),inherit(moveable,t),emitting(see,light),effect(switch(on),setprop($self,emitting(see,light))),effect(switch(off),delprop($self,emitting(see,light))),breaks_into=broken_lamp]),type_props(flour,[inherit(food,t),inherit(measurable,t)]),type_props(coins,[inherit(shiny,t),inherit(measurable,t)]),type_props(shiny,[inherit(shiny,t),inherit(moveable,t),inherit(fully_corporial,t)]),type_props(measurable,[inherit(measurable,t),ammount=some]),type_props(fountain,[volume_capacity=150,inherit(place,t),inherit(sink,t)]),type_props(cabinate,[inherit(container,t),inherit(furnature,t),volume_capacity=10]),type_props(uncloseable,[opened=t,can_be(close,f),can_be(open,f),inherit(container,t)]),type_props(sink,[cleanliness=dirty,inherit(uncloseable,t),inherit(flask,t),inherit(furnature,t),volume_capacity=5]),type_props(paper,[can_be(burn,t)]),type_props(cardboard,[inherit(paper,t)]),type_props(metal,[can_be(burn,f)]),type_props(wooden,[breaks_into=splinters,can_be(burn,t)]),type_props(locker,[inherit(container,t),inherit(moveable,t),volume_capacity=13,inherit(metal,t),opened=f]),type_props(crate,[inherit(container,t),inherit(moveable,t),volume_capacity=13,inherit(wooden,t),opened=t]),type_props(box,[opened=f,volume_capacity=11,inherit(container,t),inherit(moveable,t),inherit(cardboard,t)]),type_props(fireplace,[has_rel(on,f),has_rel(over,t),inherit(uncloseable,t),volume_capacity=20,inherit(furnature,t)]),type_props(plate,[inherit(surface,t),inherit(moveable,t),volume_capacity=2,breaks_into=shards,cleanliness=dirty]),type_props(bowl,[inherit(uncloseable,t),inherit(flask,t),volume_capacity=2,breaks_into=shards,cleanliness=dirty,name="porcelain bowl",desc="This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface."]),type_props(flask,[inherit(physical,t),inherit(container,t),opened=t,inherit(moveable,t)]),type_props(cup,[inherit(flask,t)]),type_props(bag,[volume_capacity=10,inherit(container,t),inherit(moveable,t)]),type_props(container,[default_rel=in,opened=f,can_be(open,t),has_rel(in,t)]),type_props(place,[volume_capacity=10000,default_rel=in,desc="this is a place",has_rel(in,t),inherit(here,t),can_be(move,f),can_be(take,f),has_rel(fn(exit,_38384),t)]),type_props(perceptq,[inherit(no_perceptq,f)]),type_props(no_perceptq,[inherit(perceptq,f)]),type_props(natural_force,[knows_verbs(eat,f),can_be(touch,f),has_rel(held_by,f),has_rel(worn_by,f),has_sense(see),inherit(noncorporial,t),inherit(actor,t)]),type_props(robot,[knows_verbs(eat,f),inherit(autonomous,t),emitting(see,light),volume=50,mass=200,inherit(robot,t),inherit(metallic,t),desc="Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever.",can_be(switch,t),iza(memorizer),inherit(robot,t),inherit(shiny,t),inherit(character,t),powered=t,effect(switch(on),setprop($self,powered=t)),effect(switch(off),setprop($self,powered=f))]),type_props(actor,[knows_verbs(examine,t),inherit(partly_noncorporial,t)]),type_props(character,[has_rel(worn_by,t),has_rel(held_by,t),model_depth=3,mass=50,volume=50,has_sense(see),inherit(perceptq,t),inherit(memorizer,t),inherit(actor,t),inherit(autoscan,t),inherit(partly_noncorporial,t)]),type_props(autolook,[inherit(autolook,t),class_desc(["When entering a new area the Agent will automatically\n            get an overview of the env (without purposeful looking)"])]),type_props(autoscan,[inherit(perceptive,t),inherit(autolook,t),class_desc(["Sensory percepts that discover new objects request further details (notice dirty plates are in the sink)"])]),type_props(decider_plugin,[traits(decider),inherit(nomicmu_plugin,t),class_desc(["plugins that contain decide_action hooks"])]),type_props(nomicmu_plugin,[inherit(plugin,t),inherit(unthinkable,t),prefix='$error'("required config var"),class_desc(["Nomicmu plugin"])]),type_props(decider,[class_desc(["agents of this type/class call decide_action/3 hooks (and per plugin)"])]),type_props(autonomous,[inherit(autoscan,t),inherit(impulsive,t),class_desc(["like Floyd the robot will, instances will automatically use its planner\n        about planning to decide on what to do"])]),type_props(humanoid,[knows_verbs(eat,t),volume=50,mass=50,inherit(character,t),inherit(memorizer,t),can_be(switch(off),f),powered=t]),type_props(console,[inherit(physical,t),traits([console]),inherit(player,t)]),type_props(player,[name= $self,model_depth=3,inherit(character,t),inherit(autoscan,t),look_depth=3,user_mode=2,access_level=admin,inherit(console,t),inherit(humanoid,t)]),type_props(telnet,[inherit(remote,t),inherit(player,t),inherit(player,t)]),type_props(floyd,[name="Floyd the robot",powered=f,inherit(autonomous,t),inherit(robot,t)]),type_props(furnature,[can_be(examine,t),inherit(furnature,t),inherit(untakeable,t),inherit(fully_corporial,t),inherit(surface,t),inherit(thinkable,t),inherit(physical,t),class_desc(["kind is furnature"])]),type_props(untakeable,[inherit(untakeable,t),can_be(take,f),class_desc(["kind is an Immobile Object"])]),type_props(moveable,[can_be(examine,t),inherit(physical,t),inherit(moveable,t),traits(object),can_be(move,t),inherit(fully_corporial,t),inherit(thinkable,t),class_desc(["kind is an Movable Object"])]),type_props(fully_corporial,[can_be(touch,t),can_be(examine,t),inherit(thinkable,t),cleanliness=clean,inherit(fully_corporial,t),class_desc(["kind is corporial"])]),type_props(partly_noncorporial,[inherit(fully_corporial,t),inherit(partly_noncorporial,t),inherit(noncorporial,t),class_desc(["kind is both partly corporial and non-corporial"])]),type_props(only_conceptual,[inherit(only_conceptual,t),inherit(noncorporial,t),inherit(thinkable,t),class_desc(["kind is only conceptual"])]),type_props(noncorporial,[can(examine)=f,can_be(touch,f),inherit(thinkable,t),inherit(noncorporial,t),inherit(fully_corporial,f),class_desc(["direct inheriters are completely noncorporial"])]),type_props(thinkable,[can_be(examine,t),inherit(thinkable,t),class_desc(["kind is normally thinkable"])]),type_props(unthinkable,[can_be(examine,f),inherit(unthinkable,t),class_desc(["kind is normally unthinkable"])]),type_props(door,[inherit(furnature,t),can_be(open,t),can_be(close,t),opened=f,inherit(door,t),inherit(fully_corporial,t),can_be(take,f)]),type_props(mushroom,[name="speckled mushroom",inherit(food,t),inherit(mushroom,t),inherit(fungus,t),inherit(toadstool,t),inherit(speckled,t),initial("A speckled mushroom grows out of the sodden earth, on a long stalk."),desc="The mushroom is capped with blotches, and you aren't at all sure it's not a toadstool.",can_be(eat,t),before(eat,(random100=<30,die("It was poisoned!");"yuck!")),after(take,(initial,"You pick the mushroom, neatly cleaving its thin stalk."))]),type_props(broken,[name="definately broken",effect(switch(on),true),effect(switch(off),true),can_be(switch,t),nominals(broken),inherit(broken,t),inherit(dented,t)]),type_props(screendoor,[door_to(kitchen),door_to(garden),opened=f,inherit(door,t)]),type_props(brklamp,[inherit(broken,t),name="possibly broken lamp",effect(switch(on),print_(_40786,"Switch is flipped")),effect(hit,[print_("Hit brklamp"),setprop($self,inherit(broken))]),inherit(lamp,t)]),type_props(pantry,[volume_capacity=1000,inherit(closet,t),nominals(kitchen),desc="You're in a dark kitchen pantry.",dark=t,inherit(place,t)]),type_props(living_room,[inherit(place,t)]),type_props(kitchen,[inherit(place,t),desc="cooking happens here"]),type_props(garden,[inherit(place,t),cant_go($agent,up,"You lack the ability to fly."),desc="this is the garden",cant_go($agent,_41050,"The fence surrounding the garden is too tall and solid to pass.")]),type_props(dining_room,[inherit(place,t)]),type_props(basement,[inherit(place,t),desc="This is a very dark basement.",dark=t]),
 type_props(food,[can_be(eat,t),inherit(moveable,t),inherit(measurable,t)]),structure_label(istate)]).

show_map_legend :- once_per_request(write_html(
'<table border=0 cellpadding=5 bgcolor="#000000"><tr><td>
<pre><div style="background-color:#000000;float:left"><code><font size=2 face="Courier New, FixedSys, Lucida Console, Courier New, Courier"><font color="#0">
</font><font color="#C0C0C0">The map key is:

        </font><font color="#FF00FF">#</font><font color="#C0C0C0">  - You                         --- - North/south wall
        </font><font color="#FF0000">*</font><font color="#C0C0C0">  - Other players                |  - East/west wall
        </font><font color="#FFFF00">!</font><font color="#C0C0C0">  - Mobiles                      +  - Door (closed)
        </font><font color="#00FFFF">!</font><font color="#C0C0C0">  - Pet/other charmed mob        </font><font color="#0000FF">+</font><font color="#C0C0C0">  - Door (locked)
        </font><font color="#FF0000">!</font><font color="#C0C0C0">  - Angry mob (with Sense        &gt;  - Up exit
             Anger cast)                  </font><font color="#808000">&gt;</font><font color="#C0C0C0">  - Up exit (closed)
        </font><font color="#00FF00">!</font><font color="#C0C0C0">  - Unkillable Mob               &lt;  - Down exit
        </font><font color="#00FF00">$</font><font color="#C0C0C0">  - Shopkeeper                   </font><font color="#808000">&lt;</font><font color="#C0C0C0">  - Down exit (closed)
       </font><font color="#00FFFF">[</font><font color="#FFFFFF">?</font><font color="#00FFFF">]</font><font color="#C0C0C0"> - Area exit                    </font><font color="#800000">#</font><font color="#C0C0C0">  - PK-flagged room             
       </font><font color="#00FF00">[</font><font color="#FFFFFF">?</font><font color="#00FF00">]</font><font color="#C0C0C0"> - Clan public hall exit        </font><font color="#FF0000">D</font><font color="#C0C0C0">  - Donation room

Other characters on the map represent the terrain of the local area. Some 
of the major terrains are:

        [</font><font color="#FF00FF"> </font><font color="#C0C0C0">]   Inside             .</font><font color="#FF00FF"> </font><font color="#C0C0C0">.   City
        </font><font color="#008000">,</font><font color="#FF00FF"> </font><font color="#008000">`</font><font color="#C0C0C0">   Field              </font><font color="#00FF00">;</font><font color="#FF00FF"> </font><font color="#00FF00">;</font><font color="#C0C0C0">   Hills
        </font><font color="#808000">/</font><font color="#FF00FF"> </font><font color="#808000">\\</font><font color="#C0C0C0">   Mountain           </font><font color="#0000FF">~~</font><font color="#FF00FF"> </font><font color="#0000FF">~~</font><font color="#C0C0C0">   Water
        </font><font color="#0000FF">~~</font><font color="#FF00FF"> </font><font color="#0000FF">~~</font><font color="#C0C0C0">   Waternoswim        </font><font color="#008080">.</font><font color="#FF00FF"> </font><font color="#008080">.</font><font color="#C0C0C0">   Air
        </font><font color="#808000">~~</font><font color="#FF00FF"> </font><font color="#808000">~~</font><font color="#C0C0C0">   Desert             </font><font color="#FFFF00">%</font><font color="#FF00FF"> </font><font color="#FFFF00">%</font><font color="#C0C0C0">   Quicksand
        </font><font color="#000080">~~</font><font color="#FF00FF"> </font><font color="#000080">~~</font><font color="#C0C0C0">   Underwater         </font><font color="#00FFFF">~~</font><font color="#FF00FF"> </font><font color="#00FFFF">~~</font><font color="#C0C0C0">   Ice
        </font><font color="#0000FF">.</font><font color="#FF00FF"> </font><font color="#0000FF">.</font><font color="#C0C0C0">   Underground        -</font><font color="#FF00FF"> </font><font color="#C0C0C0">-   East/West road
        . .   North/South road   </font><font color="#00FFFF">~~ ~~</font><font color="#C0C0C0">   River
        </font><font color="#FF0000">/</font><font color="#FF00FF"> </font><font color="#FF0000">\\</font><font color="#C0C0C0">   Volcano            </font><font color="#000080">%</font><font color="#FF00FF"> </font><font color="#000080">%</font><font color="#C0C0C0">   Cave
        # #   Dungeon            </font><font color="#008000">( *</font><font color="#C0C0C0">   Forest

Other terrain types not listed here are for aesthetic purposes only, such
as </font><font color="#008080">[ ]</font><font color="#C0C0C0"> for temples, </font><font color="#FFFF00">* *</font><font color="#C0C0C0"> for shops, etc.
</font></font></code></div></pre></td></tr></table>')),!.

