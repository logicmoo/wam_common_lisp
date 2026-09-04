;(function($) {

	/* Popup Menu plugin
	** author Chris Muster
	** created 04/12/2017
	** updated 08/05/2020
	** description - A plugin that, when called on an element, returns and attaches a customisable popup menu
	**		
	*/

	class Popup {

		constructor(options,elem) {
			var self = this;

			var defaultPopupMenu = `<div>
										<a href="#"><i id="faInfo" class="fa fa-info"></i></a>
										<a href="#"><i id="faQuest" class="fa fa-question"></i></a>
										<a href="#" title="Link to example.com"><i id="faLink" class="fa fa-external-link"></i></a>
									</div>`;

			this.defaultOptions = {
				content 	: defaultPopupMenu,	//this option MUST be set when new options passed through, or only the default menu will show
				position 	: "top",			//where the popup will show by default- top. Other options: right, bottom, or left
				theme 		: "popupTheme", 	//Menu Element theme. Defaults to popupTheme, but custom class can be set instead
				style 		: "",				//Popup Menu Style. Default no style, will revert to default colours. Other options: Blue, Red, Green, Custom
				animation 	: "standard",		//Standard animation by default. Other options: flip, grow, bounce
				event 		: "click",			//Default set to "click", can also be set to hover
				hideOnClick : true,				//When true, clicking off the menu closes it. When false, only clicking on the menu closes it
				zIndex 		: 100,				//Individual z-index can be set for each menu for layering if necessary

				//function to handle actions when clicking on popup menu icons. MUST be set when options are passed through or an error or default menu actions will occur 
				popItemClick: function(globalthis) {
					//Default actions
					var twentyEightSpaces = `&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
											&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
											&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`;
					
					var twentyFourSpaces = `&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
											&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
											&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`;
					
					var eightSpaces = `&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`;
					
					var sixteenSpaces = `&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
										&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`;

					var content;				
					var container = $(event.target).attr("id");

					switch (container) {
						case "faInfo":
							content = {
								type 	: "info",
								heading : "Information",
								text 	: `<p>To set a new menu when calling .popup() on an element,  
											you must set a variable that holds a string with the html for that menu, then 
											pass that variable through as the "content" part of the options. For example: </p>
											<p>var myMenu = '&lt;div&gt;&#92; <br />
											${twentyEightSpaces}&lt;a href="#''&gt;&lt;i id="faInfo" class="fa fa-info"&gt;&lt;/i&gt;&lt;/a&gt;&#92;<br />
											${twentyFourSpaces}&lt;/div&gt;'; </p>
											<p>would create a menu with one item, and just add more '&lt;a&gt;' tags with icons inside the '&lt;div&gt;' tags to add more menu items. </p> 
											<p>Then add it to the content when calling the popup: </p>
											<p>$("#myPopUp").popup({ <br />
											${eightSpaces}content: myMenu, <br />
											${eightSpaces}popItemClick(globalthis) { <br />
											${sixteenSpaces}...new actions here... <br />
											${eightSpaces}} <br />
											});</p>
											<p>You must set new actions in the "popItemClick" function for your menu 
											in the options you pass or it will throw an error.</p>`
							}
							globalthis.alertBox(content);
							break;
						case "faQuest":
							content = {
								type 	: "info",
								heading : "Question",
								text 	: `<p>Why is this being shown?</p>
										   <p>Because you need to set a popup menu of your own (and the popItemClick() function) or you get this default menu.</p>
										   <p>If you set the popup menu but don't change the popItemClick() function, you will get an error.</p>
										   <p>Click the "i" button for more info.</p>`
							}
							globalthis.alertBox(content);
							break;
						case "faLink":
							window.open("http://example.com/");
							break;
						default:
							content = {
								type 	: "danger",
								heading : "Error",
								text 	: `<p>Error! You have set a new menu without changing the 'popItemClick' function. 
									   	   The 'popItemClick' function must be set to new menu actions.</p>`
							}
							globalthis.alertBox(content);
					}
				}
			}

			this.elem    = elem;
			this.$elem   = $(elem);
			this.options = $.extend({}, this.defaultOptions, options);

			if (!this.$elem.hasClass(this.options.theme)) {
				this.$elem.addClass(this.options.theme);
			}

			this.init();
		}

		init() {

			this.popup = $('<div class="pop-cont" />')
			.addClass('pop-' + this.options.position)
			.addClass('popupTheme' + this.options.style)
			.append('<div class="pop-items" />')
			.appendTo('body').css("opacity", 0).hide();

			this.setContent();
			this.setTriggers();
		}

		setContent() {
			var self     = this;
			var location = this.popup.find(".pop-items");
			var content;

			if ((this.options.position == 'top') || (this.options.position == 'bottom')) {

				content = $(this.options.content).find("a").addClass("pop-item");
				location.html(content);
				this.popup.find("i").first().addClass("leftBorder");
				this.popup.find("i").last().addClass("rightBorder");

			} else if ((this.options.position == 'left') || (this.options.position == 'right')) {

				content = $(this.options.content).find("a").addClass("pop-item").addClass('item-side');
				location.html(content);
				this.popup.find("i").first().addClass("topBorder");
				this.popup.find("i").last().addClass("bottomBorder");

			}

		//popItemClick callback****************************************
			location.find('.pop-item').on('click', function(event) {
				event.preventDefault();
				self.options.popItemClick.call(this, self);
			});
		}

		setTriggers() {
			var self = this;

			if (this.options.event === 'click') {
				this.$elem.on('click', function(event) {
					event.preventDefault();
					if (self.$elem.hasClass('pressed')) {
						self.pophide();
					} else {
						self.popshow();
					}
				});
			}

			if (this.options.event === 'hover') {
				this.$elem.on('mouseenter', function(event) {
					setTimeout(function() {
						self.popshow();
						self.popup = $(self.popup[0]);
				    	}, 250);
					});

				$(this.popup).on('mouseleave', function(event) {
					setTimeout(function() {
						self.pophide();
					}, 1000);
				});
			}

			if (this.options.hideOnClick === true) {
				$('html').on('click.popup', function(event) {
					if (event.target != self.elem && self.$elem.has(event.target).length === 0 && 
						self.popup.has(event.target).length === 0 && self.popup.is(":visible")) {
						self.pophide();
					}
				});
			}
		}

		pophide() {
			var self      = this;
			var animation = {opacity: 0};
			this.$elem.removeClass('pressed');

			switch (this.options.position) {
				case 'top': 
					animation.top = '+=20';
					break;
				case 'left':
					animation.left = '+=20';
					break;
				case 'right':
					animation.left = '-=20';
					break;
				case 'bottom':
					animation.top = '-=20';
					break;
			}
			this.popup.animate(animation, 200, function() {
				self.popup.hide();
			});
		}

		popshow() {
			this.$elem.addClass('pressed');
			this.setPosition();
			this.popup.show().css({opacity: 1}).addClass('animate-' + this.options.animation);
		}

		setPosition() {
			var self      = this;
			this.coords   = this.$elem.offset();
			var x         = this.coords.left;
			var y         = this.coords.top;
			var popWidth  = this.popup.width();
			var popHeight = this.popup.height();
			var adjLeft   = popWidth / 2;
			var adjTop    = popHeight / 2;

			this.testy = $('<div class="test" />')
			.css({display: 'inline-block', margin: '0px', padding: '0px'})
			.appendTo('body');

			var measure = this.$elem.clone().css({padding: "0px", margin: "0px"});
			var loc     = this.testy;
			loc.html(measure);

			var textWidth  = this.testy.width();
			var textHeight = this.testy.height();
			this.testy.remove();

			var adjMenuWidth  = textWidth / 2;
			var adjMenuHeight = textHeight / 2;
			var up            = y - (popHeight + 7);
			var down          = y + textHeight;

			if (this.popup.hasClass('pop-top')){
					this.popup.css({
						top  : up + "px",
					 	left : (x - adjLeft + adjMenuWidth + 5) + "px",
					 	right: "auto", 'z-index': this.options.zIndex
					});
			}

			if (this.popup.hasClass('pop-bottom')) {
					this.popup.css({
						top  : (down + 7) + "px",
					 	left : (x - adjLeft + adjMenuWidth + 5) + "px",
					 	right: "auto", 'z-index': this.options.zIndex
					});
			}

			if (this.popup.hasClass('pop-left')) {
				this.popup.css({
					top  : (y - adjTop + adjMenuHeight + 5) + "px",
				 	left : (x - popWidth - 2) + "px",
				 	right: "auto", 'z-index': this.options.zIndex});
			}

			if (this.popup.hasClass('pop-right')) {
				this.popup.css({
					top  : (y - adjTop + adjMenuHeight + 5) + "px",
				 	left : (x + textWidth + 12) + "px",
				 	right: "auto", 'z-index': this.options.zIndex});
			}
		}

		alertBox(content) {
			var self = this;
			var myAlert =  `<div id="alertBox" class="alert">
								<div class="alert-content">
									<div class="alert-header">
										<h2></h2>
									</div>
									<div class="alert-body"></div>
									<div class="alert-footer">
										<button class="alert-close">OK</button>
									</div>
								</div>
							</div>`;

			$('body').append(myAlert);

			this.alert     = $('#alertBox');
			this.header    = this.alert.find('div.alert-header');
			this.heading   = this.header.find('h2');
			this.alertBody = this.alert.find('div.alert-body');
			this.footer    = this.alert.find('div.alert-footer');
			this.close     = this.footer.find('button.alert-close');


			this.heading.append(content.heading);
			this.alertBody.append(content.text);

			switch (content.type) {
				case "info":
					this.header.addClass("info");
					this.footer.addClass("info");
					this.close.addClass("info");
					break;
				case "success":
					this.header.addClass("success");
					this.footer.addClass("success");
					this.close.addClass("success");
					break;
				case "danger":
					this.header.addClass("danger");
					this.footer.addClass("danger");
					this.close.addClass("danger");
					break;
				case "warning":
					this.header.addClass("warning");
					this.footer.addClass("warning");
					this.close.addClass("warning");
					break;
				default:
					break;
			}

			this.alert.show();

			var closeBtn = $("button.alert-close");

			closeBtn.on("click", function() {
			    self.alert.remove();
			});

			$(document).on("click", function(event) {
				event.preventDefault();
			    if (event.target == self.alert[0]) {
			    	self.alert.remove();
			    }
			});
		}
	};

//Set $.fn.popup so it returns an instance of the Popup class when called*******************************
	$.fn.popup = function(options) {
		return this.each(function() {
			var popobject = new Popup(options, this);
		});
	};

}(jQuery));
