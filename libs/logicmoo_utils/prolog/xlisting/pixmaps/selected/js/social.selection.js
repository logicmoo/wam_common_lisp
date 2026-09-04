(function ($) {
    $.fn.deepExtend = function () {
        let args = [...arguments];
        let extend = (objA, objB) => {
            if (typeof objA === "undefined" && typeof objB === "undefined") {
                return {}
            } else if (typeof objA === "undefined") {
                return objB
            } else if (typeof objB === "undefined") {
                return objA
            }
            for (var item in objB) {
                if (item.indexOf("_") != -1 && item in objA) {} else if (Array.isArray(objB[item])) {
                    objA[item] = objB[item]
                } else if (typeof objB[item] !== "object") {
                    objA[item] = objB[item]
                } else {
                    if (!(item in objA)) {
                        objA[item] = {}
                    }
                    objA[item] = extend(objA[item], objB[item])
                }
            }
            return objA
        };

        if (args.length == 0) {
            return {};
        } else {
            let index = 1;
            while (index < args.length) {
                args[0] = extend(args[0], args[index]);
                index++;
            }
            return args[0];
        }
    }

    $.fn.socialSelection = function (opts) {
        let self = this;
        let $this = $(this);
        let setting = {
            delay: 400,
            itemsPerColumn: 4,
            dev: true,
            icons: {
                dir: '/swish/lm_xref/pixmapx/selected/img',
                ext: 'svg',
            },
            components: {
                search: {
                    platforms: {
                        ask: {
                            link: 'https://www.ask.com/web?q={{text}}'
                        },
                        baidu: {
                            link: 'https://www.baidu.com/s?wd={{text}}'
                        },
                        bing: {
                            link: 'https://www.bing.com/search?q={{text}}'
                        },
                        duckduckgo: {
                            link: 'https://duckduckgo.com/?q={{text}}'
                        },
                        google: {
                            link: 'https://www.google.com/search?q={{text}}',
                        },
                        wikipedia: {
                            link: 'https://wikipedia.org/w/index.php?search={{text}}'
                        },
                        wolframalpha: {
                            link: 'https://www.wolframalpha.com/input/?i={{text}}',
                        },
                        yahoo: {
                            link: 'https://search.yahoo.com/search?q={{text}}',
                        },
                        yandex: {
                            link: 'https://yandex.com/search?text={{text}}',
                        },
                    },
                    _selText: true,
                    url: location.href,
                    enabled: ['google', 'bing', 'duckduckgo'],
                },
                share: {
                    platforms: {
                        blogger: {
                            link: 'https://www.blogger.com/blog-this.g?u={{url}}&t={{text}}',
                            extra: {
                                title: 'n={{title}}',
                            }
                        },
                        digg: {
                            link: 'https://digg.com/submit?url={{url}}',
                        },
                        douban: {
                            link: 'https://www.douban.com/share/service/?href={{url}}&name={{text}}',
                        },
                        evernote: {
                            link: 'https://www.evernote.com/clip.action?url={{url}}&title={{text}}',
                        },
                        facebook: {
                            link: 'https://www.facebook.com/sharer/sharer.php?u={{url}}',
                        },
                        gmail: {
                            link: 'https://mail.google.com/mail/?view=cm&body={{url}}%0D%0A{{text}}',
                            extra: {
                                email: 'to={{email}}',
                                title: 'su={{title}}'
                            }
                        },
                        line: {
                            link: 'https://lineit.line.me/share/ui?url={{url}}&text={{text}}',
                        },
                        linkedin: {
                            link: 'https://www.linkedin.com/shareArticle?url={{url}}&summary={{text}}&mini=true',
                            extra: {
                                title: 'title={{title}}',
                                src: 'source={{src}}'
                            }
                        },
                        pinterest: {
                            link: 'https://pinterest.com/pin/create/button/?url={{url}}&description={{text}}',
                            extra: {
                                image: 'media={{image}}',
                            }
                        },
                        qrcode: {
                            link: '{{url}}',
                            require: ['qrcode']
                        },
                        qshare: {
                            link: 'https://connect.qq.com/widget/shareqq/index.html?url={{url}}&summary={{text}}&desc={{text}}',
                            extra: {
                                title: 'title={{title}}',
                                image: 'pics={{image}}',
                            }
                        },
                        qzone: {
                            link: 'https://sns.qzone.qq.com/cgi-bin/qzshare/cgi_qzshare_onekey?url={{url}}&summary={{text}}',
                            extra: {
                                title: 'title={{title}}',
                                image: 'pics={{image}}',
                            }
                        },
                        reddit: {
                            link: 'https://www.reddit.com/submit?url={{url}}&title={{text}}',
                        },
                        renren: {
                            link: 'https://widget.renren.com/dialog/share?resourceUrl={{url}}&srcUrl={{url}}&description={{text}}',
                            extra: {
                                title: 'title={{title}}',
                                image: 'pic={{image}}',
                            }
                        },
                        sms: {
                            link: 'sms:{{phone}}?body={{text}}'
                        },
                        telegram: {
                            link: 'https://t.me/share/url?url={{url}}&text={{text}}',
                            extra: {
                                phone: 'to={{phone}}'
                            },
                        },
                        tumblr: {
                            link: 'https://www.tumblr.com/widgets/share/tool?canonicalUrl={{url}}&caption={{text}}',
                            extra: {
                                hash: 'tags={{hash}}'
                            },
                        },
                        twitter: {
                            link: 'https://twitter.com/intent/tweet?url={{url}}&text={{text}}',
                            extra: {
                                hash: 'hashtags={{hash}}',
                                via: 'via={{via}}'
                            }
                        },
                        weibo: {
                            link: 'https://service.weibo.com/share/share.php?url={{url}}&title={{text}}'
                        },
                        whatsapp: {
                            link: 'https://wa.me/{{phone}}?text={{url}}%20{{text}}',
                        },
                    },
                    _selText: true,
                    url: location.href,
                    enabled: ['facebook', 'twitter', 'reddit', 'whatsapp'],
                },
                visit: {
                    platforms: {
                        blogger: {
                            link: "http://{{blogger}}.blogspot.com/"
                        },
                        codepen: {
                            link: "https://codepen.io/{{codepen}}"
                        },
                        discord: {
                            link: "https://discord.gg/{{discord}}"
                        },
                        facebook: {
                            link: "https://www.facebook.com/{{facebook}}"
                        },
                        github: {
                            link: "https://github.com/{{github}}"
                        },
                        instagram: {
                            link: "https://www.instagram.com/{{instagram}}"
                        },
                        line: {
                            link: "https://line.me/ti/p/{{line}}"
                        },
                        linkedin: {
                            link: "https://www.linkedin.com/in/{{linkedin}}"
                        },
                        patreon: {
                            link: "https://www.patreon.com/{{patreon}}"
                        },
                        pinterest: {
                            link: "https://www.pinterest.com/{{pinterest}}"
                        },
                        reddit: {
                            link: "https://www.reddit.com/r/{{reddit}}"
                        },
                        soundcloud: {
                            link: "https://soundcloud.com/{{soundcloud}}"
                        },
                        stackoverflow: {
                            link: "https://stackoverflow.com/users/{{stackoverflow}}"
                        },
                        telegram: {
                            link: "https://t.me/{{telegram}}"
                        },
                        tumblr: {
                            link: "https://{{tumblr}}.tumblr.com"
                        },
                        twitch: {
                            link: "https://www.twitch.tv/{{twitch}}"
                        },
                        twitter: {
                            link: "https://twitter.com/{{twitter}}"
                        },
                        wechat: {
                            link: "https://mp.weixin.qq.com/mp/profile_extaction=home&__biz={{wechat}}#wechat_redirect",
                            require: [
                                "qrcode"
                            ]
                        },
                        weibo: {
                            link: "https://www.weibo.com/{{weibo}}"
                        },
                        whatsapp: {
                            link: "https://wa.me/{{whatsapp}}"
                        },
                        wikipedia: {
                            link: "https://wikipedia.org/wiki/{{wikipedia}}"
                        },
                        xing: {
                            link: "https://www.xing.com/profile/{{xing}}"
                        },
                        yelp: {
                            link: "https://www.yelp.com/biz/{{yelp}}"
                        },
                        youtube: {
                            link: "https://www.youtube.com/{{youtube}}"
                        }
                    },
                    enabled: [],
                },
            },
            plugins: {
                qrcode: function (text) {
                    $qrcode = $('<div></div>');
                    $qrcode.qrcode({
                        width: 128,
                        height: 128,
                        text: text
                    })
                    $qrcode.css({
                        borderBottom: '1px solid #ddd',
                        padding: '10px'
                    })

                    let $tooltip = $('div.selectionTooltip:not(.hide)');


                    let top = Math.max(parseInt($tooltip.css('top')) - 148 * ($tooltip.hasClass('bottom') ? 0 : 1), 0)

                    setTimeout(() => {
                        $tooltip.data('self').showTooltip()
                        $tooltip.css({
                            top: top + 'px'
                        }).prepend($qrcode)
                        $('html,body').animate({
                           // scrollTop: $tooltip.offset().top
                        }, 300);
                    }, 200)
                }
            },
            locale: {
                components: {
                    share: {
                        title: 'Share to {{platform}}',
                    },
                    search: 
                    {
                        title: 'Search it on {{platform}}',
                    },
                    visit: 
                    {
                        title: 'Visit me on {{platform}}',
                    },
                },
                error: {
                    invalidComponent: 'Invalid Configuration: {{items}}\nPlease check the required fields for Component configuration and corresponding locale setting.',
                    invalidPlatform: 'Invalid Configuration: {{items}}\n"link" should be specified in every platform configuartion. Related feature of those platforms are disabled. Please fix it to ensure the functionality.',
                    unknownPlatform: 'Invalid platforms: {{items}}\nUnknown or unsupported platforms enabled. Please specify a supported platform or add custom config for the the above platforms. Please also check if the corresponding locale configuration is set properly',
                    missingPlugin: 'Plugin Missing: {{items}}\nRequired plugins not included or not loaded correctly. Please check and include the correct script for the the above plugins immediately to ensure the functionality.'
                },
                warning: {
                    invalidPlatformName: 'Platform Name Missing: {{items}}\nThis only affects the title shown on hovering the platform icon. Update the locale would fix this warning.',
                    noValidPlatform: 'There is no valid platform exists. Please select at least one platform or check if the configuration are correct.',
                },
                platforms: {
                    ask: 'Ask',
                    baidu: 'Baidu',
                    bing: 'Bing',
                    blogger: 'Blogger',
                    codepen: 'CodePen',
                    digg: 'Digg',
                    discord: 'Discord',
                    douban: 'Douban',
                    duckduckgo: 'DuckDuckGo',
                    evernote: 'EverNote',
                    facebook: 'Facebook',
                    github: 'GitHub',
                    google: 'Google',
                    gmail: 'GMail',
                    instagram: 'Instagram',
                    line: 'Line',
                    linkedin: 'LinkedIn',
                    pinterest: 'Pinterest',
                    patreon: 'Patreon',
                    qrcode: 'QR Code',
                    qshare: 'QQ Share',
                    qzone: 'QQ Zone',
                    reddit: 'Reddit',
                    renren: 'RenRen',
                    soundcloud: 'SoundCloud',
                    sms: 'SMS',
                    stackoverflow: 'Stack Overflow',
                    telegram: 'Telegram',
                    tumblr: 'Tumblr',
                    twitch: 'Twitch',
                    twitter: 'Twitter',
                    weibo: 'Weibo',
                    wechat: 'WeChat',
                    whatsapp: 'WhatsApp',
                    wikipedia: 'Wikipedia',
                    wolframalpha: 'WolframAlpha',
                    xing: 'Xing',
                    yahoo: 'Yahoo',
                    yandex: 'Yandex',
                    yelp: 'Yelp',
                    youtube: 'YouTube',
                },
            },
        }
        self.init = () => {
            $this.get().map(($elem, i) => {
                if (i == 0) {
                    if (self.bindedTooltip() || !self.preprocess()) return false;
                    self.uniqueId = Math.floor(Math.random() * 999999)

                    setting.icons.path = setting.icons.dir + '/{{itemName}}.' + setting.icons.ext;
                    self.$selectionTooltip = $("<div class='selectionTooltip selectionTooltip" + self.uniqueId + " hide'></div>");
                    self.customCSS = "<style>{{css}}</style>";
                    let css = "";
                    for (var comp in setting.components) {
                        let compSetting = setting.components[comp];
                        let backgroundImage = self.replaceRegexp(setting.icons.path, {
                            itemName: comp
                        })
                        if ('iconPath' in setting.components[comp]) {
                            backgroundImage = compSetting.iconPath;
                        }
                        css += "div.selectionTooltip" + self.uniqueId + " div.compsgroup." + comp + "::before{background-image: url('" + backgroundImage + "');}"
                        self[comp + 'All'] = self.createComps(comp)
                    }

                    $(self.replaceRegexp(self.customCSS, {
                        css: css
                    })).appendTo('head')
                    self.$selectionTooltip.appendTo($elem)
                    self.$selectionTooltip.data({
                        setting: setting,
                        uniqueId: self.uniqueId,
                        self: self
                    })
                    document.onselectionchange = self.triggerTooltip
                }
            })
            $this.data('selectionTooltip', self.uniqueId)
        }


        self.createComps = (comp) => {
            let compSetting = setting.components[comp];
            if (typeof compSetting.text !== 'undefined') {
                compSetting._selText = false
            }
            let $compsGroup = $("<div class='compsgroup " + comp + " columns' style='max-width: " + setting.itemsPerColumn * 40 + "px'></div>")

            let comps = compSetting.enabled.map((platform) => {
                let $comp = false;
                let iconPath = self.replaceRegexp(setting.icons.path, {
                    itemName: platform
                });
                if (platform in compSetting.platforms) {
                    let platformSetting = compSetting.platforms[platform];
                    let titleObj = {
                        platform: setting.locale.components[comp].platforms[platform]
                    };
                    if ('name' in platformSetting) {
                        titleObj.platform = platformSetting.name
                    }
                    $comp = $("<div class='comps selectionTooltip" + self.uniqueId + "_" + comp + "_" + platform + "'></div>");
                    if ('iconPath' in platformSetting) {
                        iconPath = platformSetting.iconPath;
                    }
                    if ('require' in platformSetting) {
                        $comp.data('require', platformSetting.require.join(','))
                    }
                    $comp.data('platform', platform)
                    $comp.data('component', comp)
                    $comp.attr('title', self.replaceRegexp(setting.locale.components[comp].title, titleObj))
                    $comp.css({
                        backgroundImage: 'url("' + iconPath + '")',
                    })
                    $comp.appendTo($compsGroup)
                    $comp.mouseover(() => {
                        self.$selectionTooltip.data('isHover', self.uniqueId + '_' + comp + '_' + platform)
                    }).mouseout(() => {
                        self.$selectionTooltip.data('isHover', '')
                    })
                }
                return $comp;
            }).filter((v) => v !== false);

            if (comps.length > 0) {
                $compsGroup.appendTo(self.$selectionTooltip)
            }

            return comps;
        }

        self.bindedTooltip = () => {
            return $this.triggerHandler('initsocialSelection')
        }

        self.preprocess = () => {
            $this.on('initsocialSelection', () => {
                return true
            })
            self.initLocale();
            if (typeof opts !== "undefined") {
                setting = $.fn.deepExtend(setting, opts)
            }
            for (var comp in setting.components) {
                let compSetting = setting.components[comp];
                if ('enabled' in compSetting && typeof compSetting.enabled === "string") {
                    if (compSetting.enabled == "none") {
                        setting.components[comp].enabled = [];
                    } else if (compSetting.enabled == "all") {
                        setting.components[comp].enabled = Object.keys(compSetting.platforms);
                    }
                }
                if (comp == "visit") {
                    for (var platform in compSetting.platforms) {
                        if (platform in compSetting && compSetting.enabled.indexOf(platform) == -1) {
                            setting.components[comp].enabled.push(platform)
                        }
                    }
                    
                    if (!setting.dev) {
                        setting.components[comp].enabled = setting.components[comp].enabled.filter(platform => platform in compSetting)
                    }
                    setting.components[comp].enabled.map((platform) => {
                        if (!(platform in setting.components[comp]) && setting.dev) {
                            setting.components[comp][platform] = "default";
                        }
                    })
                }
            }
            return self.validateConfigs();
        }

        self.initLocale = () => {
            if (opts && 'locale' in opts) {
                setting.locale = $.fn.deepExtend(setting.locale, opts.locale)
            }
            for (var comp in setting.components) {
                if (!(comp in setting.locale.components)) {
                    setting.locale.components[comp] = {};
                }
                if (!('platforms' in setting.locale.components[comp])) {
                    setting.locale.components[comp].platforms = {};
                }
                for (var platform in setting.components[comp].platforms) {
                    if (platform in setting.locale.platforms) {
                        setting.locale.components[comp].platforms[platform] = setting.locale.platforms[platform]
                    }
                }
            }
        }

        self.triggerTooltip = () => {
            let selection = false;
            if (window.getSelection) {
                selection = window.getSelection();
            } else if (document.getSelection) {
                selection = document.getSelection();
            }


            if (selection && !selection.isCollapsed) {
                let range = selection.getRangeAt(0);
                let rect = range.getBoundingClientRect();

                let $target = $(range.startContainer.parentElement)
                while ($target.find('.selectionTooltip').length == 0 && typeof $target.data('selectionTooltip') === "undefined") {
                    $target = $target.parent()
                }
                let $tooltip = $target.find('.selectionTooltip')
                if ($tooltip.length == 0 && typeof $target.data('selectionTooltip') !== "undefined") {
                    $tooltip = $($(document).find('.selectionTooltip').get().filter((v) => {
                        let _self = $(v).data('self')
                        if (_self.uniqueId == $target.data('selectionTooltip')) {
                            return v
                        }
                    })[0])
                }
                if ($tooltip.length != 1 || typeof $target.data('selectionTooltip') === "undefined") return false
                let _setting = $tooltip.data('setting')
                let _self = $tooltip.data('self')

                for (var comp in _setting.components) {
                    if (_setting.components[comp]._selText) {
                        _setting.components[comp].text = selection.toString();
                    }
                }
                
                for (var comp in _setting.components) {
                    _self[comp + 'All'].map(($v) => {
                        $v.data('uri', _self.buildPlatformLink(comp, $v.data('platform')))
                    })
                }

                _self.$selectionTooltip.css(_self.tooltipPosition(rect))
                _self.delayShow = setTimeout(_self.showTooltip, _setting.delay)
            } else {
                $(document).find('div.selectionTooltip').get().map((tooltip) => {
                    let isHover = $(tooltip).data('isHover');
                    let _setting = $(tooltip).data('setting');
                    let _self = $(tooltip).data('self');
                    if (isHover) {
                        hide = false;
                        $target = $(tooltip).find('div.comps.selectionTooltip' + isHover + '');
                        if ('statistics' in _setting) {
                            let component = $target.data('component');
                            let platform = $target.data('platform');
                            $.ajax({
                                url: _setting.statistics.url,
                                data: {
                                    location: location.pathname,
                                    component: component,
                                    platform: platform
                                }
                            })
                        }
                        if ($target.data('require')) {
                            $target.data('require').split(',').map((plugin) => {
                                if (plugin in _setting.plugins) {
                                    _setting.plugins[plugin]($target.data('uri'))
                                }
                            })
                            return false;
                        } else {
                            $(tooltip).data('isHover', '')							
                            window.open($target.data('uri'))
                        }
                    } else {
                        _self.hideTooltip()
                    }
                })
            }
        }
        self.tooltipPosition = (rect) => {


            let offset = ['relative', 'absolute', 'fixed'].indexOf($this.css('position')) != -1;
            let top = rect.top - 20 - self.$selectionTooltip.outerHeight();
            let left = window.scrollX - $this.offset().left;
            let center = window.scrollX - $this.offset().left + (rect.right + rect.left) / 2;
            self.$selectionTooltip.removeClass('bottom')
            if (top < 0 && (rect.bottom + self.$selectionTooltip.outerHeight() < window.innerHeight || window.scrollY < self.$selectionTooltip.outerHeight())) {
                top = window.scrollY + rect.bottom + 5
                self.$selectionTooltip.addClass('bottom')
            } else {
                top += window.scrollY;
            }
            if (offset) {
                top -= $this.offset().top
            }
            if (center + self.$selectionTooltip.outerWidth() / 2 > $this.innerWidth()) {
                left = $this.innerWidth() - self.$selectionTooltip.outerWidth()
                self.$selectionTooltip.addClass('right').removeClass('center left')
            } else if (center - self.$selectionTooltip.outerWidth() / 2 < 0) {
                left = 0
                self.$selectionTooltip.addClass('left').removeClass('center right')
            } else {
                left = center - self.$selectionTooltip.outerWidth() / 2;
                self.$selectionTooltip.addClass('center').removeClass('right left')
            }
            if ($this.css('display').indexOf("inline") != -1) {
                left += $this.position().left;
            }

			
            
		
	 
            top = top - 320;
            return {
                top: Math.round(top) + 'px',
                left: Math.round(left) + 'px'
            }
        }

        self.validateConfigs = () => {
            let error = {
                invalidComponent: [],
                invalidPlatform: [],
                unknownPlatform: [],
                missingPlugin: [],
            };
            let warning = {
                invalidPlatformName: [],
            };
            let validPlatformCount = 0;

            for (var comp in setting.components) {
                let invalidPlatforms = [];
                let compSetting = setting.components[comp];
                let platformSetting = compSetting.platforms;
                let localeSetting = setting.locale.components[comp];

                if (!('title' in localeSetting) || !('platforms' in localeSetting)) {
                    error.invalidComponent.push(comp);
                } else {
                    compSetting.enabled.filter((platform) => !(platform in platformSetting) && !(platform in localeSetting.platforms)).map((platform) => {
                        error.unknownPlatform.push(comp + "-" + platform)
                        invalidPlatforms.push(platform)
                    })
                    validPlatformCount += Object.keys(platformSetting).filter((platform) => compSetting.enabled.indexOf(platform) != -1 && invalidPlatforms.indexOf(platform) == -1).map((platform) => {
                        if (!(platform in localeSetting.platforms) && !('name' in platformSetting[platform])) {
                            warning.invalidPlatformName.push(comp + "-" + platform)
                        }
                        if (!('link' in platformSetting[platform])) {
                            error.invalidPlatform.push(comp + "-" + platform)
                            setting.components[comp].enabled = setting.components[comp].enabled.filter(v => v != platform)
                        }
                        if ('require' in platformSetting[platform]) {
                            platformSetting[platform].require.map((plugin) => {
                                if (!(plugin in $.fn) && error.missingPlugin.indexOf(plugin) == -1) {
                                    error.missingPlugin.push(comp + "-" + plugin);
                                }
                            })
                        }
                    }).length
                }
            }
            for (var err in error) {
                if (error[err].length > 0) {
                    self.showError(self.replaceRegexp(setting.locale.error[err], {
                        items: error[err].join(', ')
                    }))
                }
            }
            for (var warn in warning) {
                if (warning[warn].length > 0) {
                    self.showError(self.replaceRegexp(setting.locale.warning[warn], {
                        items: warning[warn].join(', ')
                    }), true)
                }
            }

            if (validPlatformCount == 0) {
                self.showError(setting.locale.warning.noValidPlatform, true)
            }

            return error.unknownPlatform.length == 0 && error.invalidComponent.length == 0 && validPlatformCount > 0
        }

        self.showTooltip = () => {
            self.$selectionTooltip.removeClass('hide')
            if (self.$selectionTooltip.position().top < window.scrollY) {
                $('html,body').animate({
                    // scrollTop: self.$selectionTooltip.offset().top
                }, 300);
            }
			self.$selectionTooltip.position().top = mouseY;
        }

        self.hideTooltip = () => {
            setTimeout(() => {
                self.$selectionTooltip.addClass('hide');
                self.$selectionTooltip.find('div:not(.comps):not(.compsgroup)').remove();
            }, 100)
            clearTimeout(self.delayShow)


        }

	    var mouseX, mouseY;

		window.onmousemove = function (e) {
			mouseX = e.clientX;
			mouseY = e.clientY;
		};

        self.buildPlatformLink = (comp, platform) => {
            let config = setting.components[comp].platforms[platform]
            let link = config.link;
            for (var item in config.extra) {
                if (item in setting.components[comp]) {
                    link += '&' + config.extra[item]
                }
            }
            return self.replaceRegexp(link, setting.components[comp]);
        }

        self.replaceRegexp = (text, object) => {
            let param = $.fn.deepExtend({}, object);
            param['[A-z0-9]+'] = '';
            for (var name in param) {
                let regex = new RegExp('{{' + name + '}}', 'g');
                text = text.replace(regex, param[name])
            }
            return text;
        }

        self.showError = (msg, warn) => {
            if (setting.dev) {
                warn ? console.warn(msg) : console.error(msg)
            }
        }
        self.init()
        return this
    }

    $.fn.selectionTooltip = function () {
        let $this = $(this);
        let components = {
            search: {},
            share: {},
            visit: {},
        }
        let options = $this.data();
        if ('comps' in options) {
            options.comps.split(",").map((v) => {
                components[v] = {};
                if (!('locale' in options)) options.locale = {};
                if (!('components' in options.locale)) options.locale.components = {};
                options.locale.components[v] = {
                    title: "{{platform}}",
                    platforms: {}
                };
            })
            delete options.comps;
        }
        let target = 'body';
        if ('target' in options) {
            target = options.target;
            delete options.target;
        }
        if (!('components' in options)) {
            options.components = {};
        }
        for (var compName in components) {
            let $comp = $this.find('component-' + compName);
            options.components[compName] = {
                enabled: []
            };
            if ($comp.length > 0) {
                options.components[compName] = $.fn.deepExtend(options.components[compName], $comp.data());
                if ($comp.children().length > 0) {
                    $comp.children().get().map((platform) => {
                        let platformName = $(platform).prop('tagName').toLowerCase()
                        if (!('platforms' in options.components[compName])) options.components[compName].platforms = {};
                        options.components[compName].enabled.push(platformName)
                        options.components[compName].platforms[platformName] = JSON.parse(JSON.stringify($(platform).data()));
                        if ('require' in options.components[compName].platforms[platformName]) {
                            options.components[compName].platforms[platformName].require = options.components[compName].platforms[platformName].require.split(',')
                        }
                    })
                }
            }
        }
        $(this).remove();
        $(target).socialSelection(options);
    }
    $(function () {
        if ($('selecton-tooltip').length > 0)
            $('selecton-tooltip').selectionTooltip();
    })
}(jQuery))

