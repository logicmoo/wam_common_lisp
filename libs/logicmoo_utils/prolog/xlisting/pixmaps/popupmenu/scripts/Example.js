/*jshint multistr: true */

//this is a sample .js file that shows how you might set up the popup menus

;(function($) {
	$(document).ready(function() {
		//putting the html for each popup menu into variables
		var transport 	= `<div>
							  <a href="#"><i id="plane" class="fa fa-plane"></i></a>
							  <a href="#"><i id="car" class="fa fa-car"></i></a>
							  <a href="#"><i id="bus" class="fa fa-bus"></i></a>
							  <a href="#"><i id="bicycle" class="fa fa-bicycle"></i></a>
						  </div>`;

		var starwars  	= `<div>
							  <a id="rebs" href="#rebel"><i id="rebels" class="fa fa-ra"></i></a>
							  <a id="imperial" href="#empire"><i id="imperials" class="fa fa-empire"></i></a>
						      <a id="firstO" href="#firstOrderIcon"><i id="firstOrder" class="fa fa-first-order"></i></a>
						  </div>`;

		var smileys   	= `<div>
							  <a href="#"><i id="v-hap" class="material-icons">sentiment_very_satisfied</i></a>
							  <a href="#"><i id="hap" class="material-icons">sentiment_satisfied</i></a>
							  <a href="#"><i id="neut" class="material-icons">sentiment_neutral</i></a>
							  <a href="#"><i id="unhap" class="material-icons">sentiment_dissatisfied</i></a>
							  <a href="#"><i id="v-unhap" class="material-icons">sentiment_very_dissatisfied</i></a>
						  </div>`;

		var homeCallPic = `<div>
							  <a href="#"><i id="home" class="material-icons">home</i></a>
							  <a href="#"><i id="call" class="material-icons">call</i></a>
							  <a href="#"><i id="chat" class="material-icons">chat</i></a>
							  <a href="#"><i id="mail" class="material-icons">mail_outline</i></a>
							  <a href="#"><i id="camera" class="material-icons">camera_alt</i></a>
						  </div>`;

//call popup, pass through options
		//call popup on the element the popup will be attached to
		//this one calls all menu elements with a name="menu1" and would attach the same menu to each.
		$('[name="menu1"]').popup({
			content 	: transport,//the menu to be appended

			position 	: "top",	//where the popup will show by default- top, right, bottom, or left

			theme 		: "popupTheme",//Menu Element theme. If left out, defaults to popupTheme, but custom class can be set instead

			style 		: "",		//colour theme for the popup menu, "" for default, Blue, Red, Green, Custom (all Uppercase first letter)

			animation 	: "bounce",	//choose how the popup will appear/disappear,
									//standard, flip, grow, bounce

			event 		: "click",	//activate popup by click or by hover

			hideOnClick : true,		//if event is click, set to true to close menu when clicking off of it	

			zIndex 		: 100,		//make the popup appear above other elements

			popItemClick: function(globalthis) {

				var content;				
				var container = $(event.target).attr("id");
				
				switch (container) {
					case "plane":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>You chose the Plane!<br /><i class='fa fa-plane'></i></p>"
						}
						globalthis.alertBox(content);
						break;
					case "car":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>You chose the Car!<br /><i class='fa fa-car'></i></p>"
						}
						globalthis.alertBox(content);
						break;
					case "bus":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>You chose the Bus!<br /><i class='fa fa-bus'></i></p>"
						}
						globalthis.alertBox(content);
						break;
					case "bicycle":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>You chose the Bike!<br /><i class='fa fa-bicycle'></i></p>"
						}
						globalthis.alertBox(content);
						break;
					default:
						content = {
							type 	: "danger",
							heading : "Error",
							text 	: "<p>Error!</p>"
						}
						globalthis.alertBox(content);
				}
			}
		});

		$('#myPopUp2').popup({	
			content 	: starwars,	

			position 	: "left",	

			theme 		: "popupTheme popupThemeRed",

			style 		: "Red",		

			animation 	: "flip",	

			event 		: "click",		

			hideOnClick : true,	

			zIndex 		: 100,

			popItemClick: function(globalthis) {

				var content;				
				var container = $(event.target).attr("id");
				
				switch (container) {
					case "rebels":
						$('#rebel').show();
						$('#empire, #firstOrderIcon').hide();
						break;
					case "imperials":
						$('#rebel, #firstOrderIcon').hide();
						$('#empire').show();
						break;
					case "firstOrder":
						$('#rebel, #empire').hide();
						$('#firstOrderIcon').show();
						break;
					default:
						content = {
							type 	: "danger",
							heading : "Error",
							text 	: "<p>Error!</p>"
						}
						globalthis.alertBox(content);
				}

				$('#rebel, #empire, #firstOrderIcon').off('click').on('click', function() {
					$('#rebel, #empire, #firstOrderIcon').hide();
				});

			}
		});

		$('#myPopUp3').popup({	
			content 	: smileys,	

			position 	: "right",	

			theme 		: "popupTheme popupThemeCustom",

			style 		: "Custom",		

			animation 	: "grow",	

			event 		: "hover",		

			hideOnClick : true,	

			zIndex 		: 100,

			popItemClick: function(globalthis) {
				var self = this;
				var loc;
				var content;				
				var container = $(event.target).attr("id");

				$(document).off('click').on('click', function(event) {
					if (($(event.target).attr("id") != container)) {
						$('.moods').remove();						
					} else {
						if ($('.moods').length) {
							$('.moods').remove();
						}

						self.moody = $('<div class="moods" />')
						.append('<div class="pop-mood" />').append('<i class="material-icons"></i>')
						.appendTo('body').css('opacity', 0).hide();

						loc = self.moody.find("i");

						switch (container) {
							case "v-hap":
								content = ('sentiment_very_satisfied');
								loc.html(content);
								self.moody.find('i').addClass('very-happy');
								self.moody.css('opacity', 1).fadeIn('slow');
								break;
							case "hap":
								content = ('sentiment_satisfied');
								loc.html(content);
								self.moody.find('i').addClass('happy');
								self.moody.css('opacity', 1).fadeIn('slow');
								break;
							case "neut":
								content = ('sentiment_neutral');
								loc.html(content);
								self.moody.find('i').addClass('neutral');
								self.moody.css('opacity', 1).fadeIn('slow');
								break;
							case "unhap":
								content = ('sentiment_dissatisfied');
								loc.html(content);
								self.moody.find('i').addClass('unhappy');
								self.moody.css('opacity', 1).fadeIn('slow');
								break;
							case "v-unhap":
								content = ('sentiment_very_dissatisfied');
								loc.html(content);
								self.moody.find('i').addClass('upset');
								self.moody.css('opacity', 1).fadeIn('slow');
								break;
							default:
								content = {
									type 	: "danger",
									heading : "Error",
									text 	: "<p>Error!</p>"
								}
								globalthis.alertBox(content);
						}
					}
				});
			}
		}).draggable({containment: "document"});

		$('#myPopUp4').popup({	
			content 	: homeCallPic,	

			position 	: "bottom",	

			theme 		: "popupTheme popupThemeGreen",

			style 		: "Green",		

			animation 	: "standard",	

			event 		: "click",		

			hideOnClick : false,	

			zIndex 		: 100,

			popItemClick: function(globalthis) {

				var content;				
				var container = $(event.target).attr("id");

				switch (container) {
					case "home":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>Go Home!<br /><i class='material-icons'>home</i></p>"
						}
						globalthis.alertBox(content);
						break;
					case "call":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>Call Me!<br /><i class='material-icons'>call</i></p>"
						}
						globalthis.alertBox(content);
						break;
					case "chat":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>Chat to Me!<br /><i class='material-icons'>chat</i></p>"
						}
						globalthis.alertBox(content);
						break;
					case "mail":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>Email Me!<br /><i class='material-icons'>mail_outline</i></p>"
						}
						globalthis.alertBox(content);
						break;
					case "camera":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>Take a Picture!<br /><i class='material-icons'>camera_alt</i></p>"
						}
						globalthis.alertBox(content);
						break;
					default:
						content = {
							type 	: "danger",
							heading : "Error",
							text 	: "<p>Error!</p>"
						}
						globalthis.alertBox(content);
				}
			}
		}).draggable({containment: "document"});
										

		$('#textPopup').popup({	
			content 	: transport,	

			position 	: "bottom",	

			theme 		: "textPopup", //this menu button is not using the popupTheme, so must be styled separately

			style 		: "Blue",		

			animation 	: "flip",	

			event 		: "hover",		

			hideOnClick : true,	

			zIndex 		: 100,

			popItemClick: function(globalthis) {

				var content;				
				var container = $(event.target).attr("id");

				switch (container) {
					case "plane":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>You chose the Plane!<br /><i class='fa fa-plane'></i></p>"
						}
						globalthis.alertBox(content);
						break;
					case "car":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>You chose the Car!<br /><i class='fa fa-car'></i></p>"
						}
						globalthis.alertBox(content);
						break;
					case "bus":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>You chose the Bus!<br /><i class='fa fa-bus'></i></p>"
						}
						globalthis.alertBox(content);
						break;
					case "bicycle":
						content = {
							type 	: "info",
							heading : "Info",
							text 	: "<p style='text-align:center'>You chose the Bike!<br /><i class='fa fa-bicycle'></i></p>"
						}
						globalthis.alertBox(content);
						break;
					default:
						content = {
							type 	: "danger",
							heading : "Error",
							text 	: "<p>Error!</p>"
						}
						globalthis.alertBox(content);
				}
			}
		}); 

		$('#defaultTest').popup();

	});
}(jQuery));