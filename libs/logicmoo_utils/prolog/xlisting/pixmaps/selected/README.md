# jQuery Social Selection

## Description
This jQuery plugin supports users to **Search**, **Share** and **Visit Your Personal Page** when highlighting any words in your page. You can also keep track on how many people used this tool with some setting.

## Features
- Quick Search
The Search Component allows user to search the text they highlighted or the specific keywords you chose

- Quick Share
The Share Component allows user to share to different social media platform with your website link and description

- Quick Visit
If you are maintaining multiple personal pages, you can setup a shortcut for user to visit them easily

- Customizable Component
If you want more than this plugin provide, you can always write your own component or overwrite some functions.

Check the [demo](https://chingcm.github.io/social-selection-tooltip/example/example.html) to see how it works.

## Setup
Include the jQuery script and any other jQuery plugins you need to use for this Plugin:
```
<script src="https://code.jquery.com/jquery-3.4.1.min.js"></script>
```
Then include the plugin script and stylesheet:
```
<script src="/js/social.selection.js"></script>
<link rel="stylesheet" type="text/css" href="/css/social.selection.css">
```
## Usage
Follow this way to use the plugin immediately:
```
$("#selection").socialSelection();
```
Or you can use pure HTML code:
```
<div id="test"></div>
<selecton-tooltip data-target="#test">
    <selecton-search>
        <google></google>
    </selecton-search>
</selecton-tooltip>
```
## Documentation

### Supported Platforms
- **Search**
    - Ask
    - Baidu
    - Bing
    - DuckDuckGo
    - Google
    - Wikipedia
    - WolframAlpha
    - Yahoo
    - Yandex
- **Share**
    - Blogger
    - Digg
    - Douban
    - EverNote
    - Facebook
    - GMail
    - Line
    - LinkedIn
    - Pinterest
    - QR Code
    - QQ Share
    - QQ Zone
    - Reddit
    - RenRen
    - SMS
    - Telegram
    - Tumblr
    - Twitter
    - Weibo
    - WhatsApp
- **Visit**
    - Blogger
    - CodePen
    - Discord
    - Facebook
    - GitHub
    - Instagram
    - Line
    - LinkedIn
    - Patreon
    - Pinterest
    - Reddit
    - SoundCloud
    - Stack Overflow
    - Telegram
    - Tumblr
    - Twitch
    - Twitter
    - WeChat
    - Weibo
    - WhatsApp
    - Wikipedia
    - Xing
    - Yelp
    - YouTube


### **Basic Usage**
##### `components` (object) All components
- **`search`** (object) Search Component
    - `platforms` (object): All platforms configuration
    - `enabled` (string / array): specified which platfrom to used
        - `[platfrom_list]`
        - `all`
        - `none`
    - `iconPath` (string) (optional): specified location of component icon
    - `text` (string) (optional): specified keywords to search
- **`share`** (object) Share Component
    - `platforms` (object): All platforms configuration
    - `enabled` (string / array): specified which platfrom to used
        - `[platfrom_list]`
        - `all`
        - `none`
    - `iconPath` (string) (optional): specified location of component icon
    - `text` (string) (optional): specified keywords to share
    - `url` (string) (optional): specified link to share
    - `[params]`: different platform may accept extra parameters during sharing, check platform list for all acceptable parameters
- **`visit`** (object) Visit Component
    - `platforms` (object): All platforms configuration
    - `enabled` (string / array) **(optional)**: specified which platfrom to used, scripts will exclude the platform if it has not specify platform_unique_id
        - `[platfrom_list]`
        - `all` 
        - `none`
    - `iconPath` (string) (optional): specified location of component icon
    - `[platfroms]` (string): use platform name as key and platform_unique_id as value
##### `delay` (integer) Delay show time
Wait time before showing the tooltip after selection event is fired (in milliseconds) eg. 400 => 0.4s
##### `itemsPerColumn` (integer) Number of icons in a row
##### `dev` (boolean) Developer Mode
##### `icons` (object) Default icons setting
- `dir` (string): Icon Folder
- `ext` (string): File Extension eg. png, svg
##### `plugins` (object) External plugins used
- `[plugin_name]` (function): Your custom function using other plugin
##### `locale` (object) Language Setting
- **`components`** (object): All language configuration for components
    - `[component_name]` (object)
        - `title` (string): Message show when hovering the icon
- `platforms` (object): Master control of all platform name, same platform across different components share the same name
    - `[platform]` (string): Platform name
#### `statistics` (object) Statistics Tracking
- `url` (string): tracking url

### Locale Setting
To change the name of a platform under a component, directly update `components.[COMPONENT].platforms.[PLATFORM].name`:
```
// components > foo > platforms > bar > name
components: {
    foo: {
        platforms: {
            bar: {
                name: "New Name"
            }
        }
    }
}
```
To change the name of a platform across all components, directly update `locale.platforms.[PLATFORM]`:
```
// locale > platforms > bar
locale: {
    platforms: {
        bar: "New Name"
    }
}
```
To change the message show when hovering an icon for a component, directly update `locale.components.[COMPONENT].title`:
```
// locale > components > foo > title
locale: {
    components: {
        foo: {
            title: "Custom Text {{platform}}", // {{platform}} will be replaced by the platform name automatically
        }
    }
}
```

### **Advanced Usage**
#### Create a Regexp Link
All the `link` value will be processed before visiting it. Using the pattern `{{pattern_name}}` to replace the word you need to change. All patterns left unprocessed will be dropped.
**Default Pattern**
- `text`: Define the text to search/share
- `url`: Define the URL to share
- `title`: Define the title of the message (Blogger, GMail, LinkedIn, QQ Share, QQ Zone, RenRen only)
- `email`: Recipant of this email (GMail only)
- `src`: Your webiste/application name (LinkedIn only)
- `image`: URL of image to be shared (Pinterest, QQ Share, QQ Zone, RenRen only)
- `phone`: Phone number to contact with (SMS, Telegram, Whatsapp only)
- `hash`: Hashtags on the share message (Twitter, Tumblr only)
- `via`: Twitter username to associate with (Twitter only)

If you are searching **Google**, the link
```
www.google.com/search?q={{text}}
```
will change to 
```
www.google.com/search?q=Google
```

You may want to add optional parameters by specifying `extra` in the configuration or append directly to the `link`.
So, when your component setting are like this:
```
components: {
    share: {
        text: "Google",
        foo: "foo",
        platforms: {
            google: {
                link: "www.google.com/search?q={{text}}&bar={{bar}}",
                extra: {
                    foo: "foo={{foo}}"
                }
            }
        }
    }
}
```
The output url would be:
```
www.google.com/search?q=Google&bar=&foo=foo
```
**Strongly recommend**: Do not add optional parameter directly to the link. Some website may takes those parameter as empty value and process the request differently.

#### Create new platform
**ALL PLATFORMS** must specified `link` as the base url to process
**Search** and **Share** component takes `{{text}}` as the keywords to search/share.
**Share** takes `{{url}}` as the sharing website
**Visit** takes `{{platform}}` as the platform unique id
Search Example:
```
{
    platforms: {
        example: {
            link: "my_example_website/?search={{text}}", // your desired link
        }
    },
}
```

Share Example:
```
{
    platforms: {
        example: {
            link: "my_example_website/?shareText={{text}}&shareURL={{url}}, // your desired link
            extra: {
                hash: "hashtag={{hash}}",
            }
        }
    },
    hash: "hashtag",
}
```

Visit Example:
```
{
    platforms: {
        example: {
            link: "my_example_website/{{example}}, // your desired link
        },
        example2: {
            link: "my_example2_website/{{example2}}, // your desired link
        }
    },
    example: "exampleID",
    example2: "exampleID2",
},
```

New Component Example:
```
components: {
    foo: {
        platforms: {
            bar1: {
                link: "bar1_link",
            },
            bar2: {
                link: "bar2_link"
            }
        }
    }
}
```
**Remember**: Check the locale setting after adding new platform and component as most people always forget it. See Locale setting for more detail.

### Pure HTML
Using HTML code can also generate basic component. Some features may not work if you use pure HTML like change Locale setting. 
**\*** You can use pure HTML and Javascript to initiate the plugin at the same time but **DO NOT** target it to the same element or child nodes of initiated element.
**\*\*** Pure HTML only works when the code is loaded completely before the plugin run. Thus, it won't work during runtime.

Use `<selecton-tooltip></selecton-tooltip>` to wrap all the components. Specify data attributes for more feature:
- `data-target` (valid jQuery Selector string) (optional): apply this plugin to the target element (default apply to whole page).
- `data-comps` (string) (optional/required): Specify any custom component created by yourself. Use comma (,) to join multiple components.

Use `<component-[component_name]></component-[component_name]>` to wrap all platforms to be enable. Options assigned by Javascript can be modified to `data-attributes` form to achieve those features.
For `visit` component, only specify the data value can enable the plugin. eg. `data-facebook="1234567"`
**Remember**: The attribute name will change to lowercase. So, `iconPath` in Javascript becomes `data-icon-path` in HTML form.

Here is an example for applying the plugin (with new component `foo`) to element `$('#html')`:
```
<selecton-tooltip data-target="#html" data-comps="foo">
    <component-search>
        <google></google>
    </component-search>
    <component-share>
        <facebook></facebook>
    </component-share>
    <component-visit data-github="abc"></component-visit>
    <component-foo data-icon-path="../img/qrcode.svg" data-text="123">
        <bar1 data-name="Bar1" data-link="{{bar1}}"  data-icon-path="../img/wechat.svg" data-require="qrcode"></bar1>
    </component-foo>
</selecton-tooltip>
```

### Statistics
You can specify `statistics` to keep track on which platform did the user click
```
statistics: {
    url: 'my_statistics_url'
}
```

## License
Release under [MIT License](https://opensource.org/licenses/MIT)
