var bgExit;
var t1;
if (typeof Object.assign !== 'function') {
    // Must be writable: true, enumerable: false, configurable: true
    Object.defineProperty(Object, "assign", {
      value: function assign(target, varArgs) { // .length of function is 2
        'use strict';
        if (target === null || target === undefined) {
          throw new TypeError('Cannot convert undefined or null to object');
        }
  
        var to = Object(target);
  
        for (var index = 1; index < arguments.length; index++) {
          var nextSource = arguments[index];
  
          if (nextSource !== null && nextSource !== undefined) { 
            for (var nextKey in nextSource) {
              // Avoid bugs when hasOwnProperty is shadowed
              if (Object.prototype.hasOwnProperty.call(nextSource, nextKey)) {
                to[nextKey] = nextSource[nextKey];
              }
            }
          }
        }
        return to;
      },
      writable: true,
      configurable: true
    });
  }
function bannerInit() {
    document.getElementsByTagName('body')[0].className += ' init';    
    bgExit = document.getElementById('background_exit_ad');
    addListeners();
    checkready();
    if (typeof window['animate'] == "function"  ){
        animate();      
    }
    if (typeof window['domainAd'] == "function"  ){
        domainAd();      
    }
}

function checkready() {
    if (window.screenready) {
        window.screenready()
    }    
    else {
        setTimeout(function(){
            checkready()
        },50);
    }
}

addListeners = function() {
    if (bgExit) {
        bgExit.addEventListener('click', bgExitHandler, false);
    }
}

bgExitHandler = function(e) {
    //Call Exits
    if (Enabler) {
        Enabler.exit('HTML5_Background_Clickthrough');
    }
    window.open(clickTag);
}

function animate() {
    t1 = new TimelineMax();
    function _setup() {
        for (var key in animation_setup.frames) {
            t1.add(key, animation_setup.frames[key].time);
        }
        for (var key in animation_setup.states) {
            var p_spec = animation_setup.states[key];
            if (p_spec.use_timeline) {
                display_state(key);
            }
        }
    }

    function display_state(stateName) {
        var p_state = animation_setup.states[stateName];
        if (p_state) {
            for (var ii = 0; ii < p_state.items.length; ii++) {
                var anim = p_state.items[ii];

                if (display_state[anim.verb]) {
                    display_state[anim.verb](anim, p_state.use_timeline);
                }
            }
        }
    }
    display_state.write = function(verb, args, use_timeline, offset) {
        if (use_timeline) {
            if (offset) {
                args.push(offset);
            }

            t1[verb].apply(t1, args);
        } else {
            TweenMax[verb].apply(null, args);
        }
    }

    display_state.set = function(spec, use_timeline) {
        var func_args = [spec.target, Object.assign({}, spec.values)];
        display_state.write('set', func_args, use_timeline, spec.offset);
    }
    display_state.addClass = function(spec, use_timeline) {

        t1.call(function(target, className) {
            className.split(' ').forEach(function(el, idx){
                var p_el = document.querySelector(target);
                if (p_el) {
                    p_el.classList.add(el);
                }
            });
        }, [spec.target, spec.value], null, spec.offset);
    }
    display_state.to = function(spec, use_timeline) {
        var func_args = [spec.target, spec.duration, Object.assign({}, spec.values)];
        display_state.write('to', func_args, use_timeline, spec.offset);
    }
    display_state.fromTo = function(spec, use_timeline) {
        var func_args = [spec.target, spec.duration, Object.assign({}, spec.values)];
        display_state.write('set', [spec.target, Object.assign({}, spec.initial)], use_timeline, spec.offset);
        display_state.write('to', func_args, use_timeline, spec.offset);
    }
    display_state.staggerFromTo = function(spec, use_timeline) {
        var func_args = [spec.target, spec.duration, Object.assign({}, spec.values), spec.stagger];
        display_state.write('set', [spec.target, Object.assign({}, spec.initial)], use_timeline, spec.offset);
        display_state.write('staggerTo', func_args, use_timeline, spec.offset);
    }

    _setup();
}function domainAd() {
    var has_searched = false;
    bgExit = null;

    var cta = document.querySelector("#cta"),

        // Created a var for the btn element
        sea = document.querySelector('#btn'),
        // Setting delay time for the button to re-activate
        delayTime = 1000;
        // Another variable to account for enter key
        enterPressed = false;
        // End of added vars for double search

         // Variable to help with if statement
         searchedtwo = false;
         rightSize = false;
         //end of added vars to catch two results on small ad size


        searchMore = document.querySelector('#searchMore'),
        wait = document.querySelector('#wait'),
        container = document.querySelector("#domainList"),
        girl = document.querySelector("#girl"),
        logo = document.querySelector("#gd_logo"),
        disclaimer = document.querySelector(".disclaimer"),
        successMsg = "<h5> Finders, keepers! Your domain is available. Claim it while you still can.</h5>",
        suggestMsg = "<h5> That domain is spoken for, but we have lots more. What about these?</h5>",
        errorMsg = "Sorry, something's gone wrong. Please try again.";


        document.querySelector('#privacy_link').addEventListener('click', function(){
            Enabler.exit('DOMSEARCH_PRIVACY');
        })

        
    function _setup() {
        for (var key in animation_setup.frames) {
            t1.add(key, animation_setup.frames[key].time);
        }
        for (var key in animation_setup.states) {
            var p_spec = animation_setup.states[key];
            if (p_spec.use_timeline) {
                display_state(key);
            }
        }
    }
    function display_state(stateName) {
        var p_state = animation_setup.states[stateName];
        if (p_state) {
            for (var ii = 0; ii < p_state.items.length; ii++) {
                var anim = p_state.items[ii];
                if (display_state[anim.verb]) {
                    display_state[anim.verb](anim, p_state.use_timeline);
                }
            }
        }
    }
    display_state.write = function (verb, args, use_timeline, offset) {
        if (use_timeline) {
            if (offset) {
                args.push(offset);
            }
            t1[verb].apply(t1, args);
        }
        else {
            TweenMax[verb].apply(null, args);
        }
    }

    display_state.set = function (spec, use_timeline) {
        var func_args = [spec.target, Object.assign({}, spec.values)];
        display_state.write('set', func_args, use_timeline, spec.offset);
    }
    display_state.to = function (spec, use_timeline) {
        var func_args = [spec.target, spec.duration, Object.assign({}, spec.values)];
        display_state.write('to', func_args, use_timeline, spec.offset);
    }
    display_state.fromTo = function (spec, use_timeline) {
        var func_args = [spec.target, spec.duration, Object.assign({}, spec.values)];
        display_state.write('set', [spec.target, Object.assign({}, spec.initial)], use_timeline, spec.offset);
        display_state.write('to', func_args, use_timeline, spec.offset);
    }
    display_state.staggerFromTo = function (spec, use_timeline) {
        var func_args = [spec.target, spec.duration, Object.assign({}, spec.values), spec.stagger];
        display_state.write('set', [spec.target, Object.assign({}, spec.initial)], use_timeline, spec.offset);
        display_state.write('staggerTo', func_args, use_timeline, spec.offset);
    }

    _setup();

    //    display_state('starting');
    document.querySelector('#content_ad').style.display = "block";

    function animateIntro() {
        t1.addLabel('intro')
            .staggerFrom([dco_text1, dco_text2], 0.5, { x: -300 }, 0.3, 'intro+=0.5')
            .from([logo, disclaimer], 0.5, { y: 100 }, 'intro+=0.6')
            .to(girl, 1, { opacity: 1 }, 'intro+=0.5')
    }

    function animateOutro() {
        t1.to(cont, 0.5, { opacity: 1 }, 'now')
            .to(girl, 0.5, { opacity: 0 }, 'now')
    }

    function clearMessages() {
        document.querySelector('#successMsg').innerHtml = '';
        document.querySelector('#errorMsg').innerHtml = '';
        document.querySelector('#suggestMsg').innerHtml = '';
        container.innerHTML = "";
        searchMore.style.display = 'none';
    }

    function hideCues() {
        document.querySelector('.results-container').style.display = 'none';
    }

    hideCues();

    function loadJSON(callback, dom) {
        var xobj = new XMLHttpRequest();
        xobj.overrideMimeType("application/json");
        xobj.open('GET', "https://www.godaddy.com/api/ads/domains/search?domainname=" + dom + "&marketid=EN-us", true),
            xobj.onreadystatechange = function () {
                if (xobj.readyState == 4 && xobj.status == "200") {
                    callback(xobj);
                }
                if (xobj.readyState == 4 && xobj.status == "204") {
                    callback(xobj);
                }
            };
        xobj.send(null);
    }

    var dom_text;
    if (dom_text = document.querySelector("#domain")) {
        dom_text.addEventListener("keyup", function (event) {
            
            // Added and additional statement to catch a double enter tap
            
            if (event.key === "Enter" && !enterPressed) {
               clean_values();
               enterPressed = true;
            }else{
                setTimeout(function(){
                    enterPressed = false;
                }, delayTime);
            }
        });

    }
    function clean_values() {
        var search = document.querySelector('#domain').value,
            dom = search.replace(/[\s]/g, '-'),
            dom = dom.replace(/[^a-zA-Z0-9.-]/g, ''),
            findDot = dom.indexOf(".");

        event.preventDefault();

        if (findDot == '-1') {
            dom += ".ca";
        }

        if (findDot == (dom.length - 1)) {
            dom += ".ca";
        }

        //        animateOutro();

        clearMessages();

        if (!has_searched) {
            Enabler.counter('SEARCHED_ONCE');
            has_searched = true;

            var v6DomainAnim = new TimelineMax()

            switch (G_AD_SETUP.size) {
              case '300x250':
                v6DomainAnim.to('.v5frame.v5frame-2', 0.2, {y: -260}, 0);
                v6DomainAnim.to('.domain-search-input-results-wrap', 0.2, {y: -101}, 0.1);
                v6DomainAnim.to('.textbox-headline', 0.2, {opacity: 0}, 0.1);
                v6DomainAnim.to('.logo', 0.1, {opacity: 0}, 0);
                break;

              case '970x250':
                v6DomainAnim.to('.domain-search-input-results-wrap', 0.2, {y: -109}, 0.0);
                v6DomainAnim.to('.textbox-headline', 0.2, {opacity: 0}, 0.1);
                break;

              case '728x90':
                v6DomainAnim.to('.domain-search-input-results-wrap', 0.2, {y: -35}, 0.0);
                v6DomainAnim.to('.disclaimer', 0.2, {y: -60}, 0.0);

                // Added to put a unique class onto this size
                document.querySelector('.results-container-inner').className += ' test';
                rightSize = true;
                //end of additional code to handle two results on smaller ad size
                break;

              // case '300x600':
                // v6DomainAnim.to('.v5frame.v5frame-2', 0.2, {y: -253}, 0);
                // v6DomainAnim.to('.frame-container.foreground .frame-end .content', 0.2, {y: -101}, 0.1);
                // v6DomainAnim.to('.logo', 0.1, {opacity: 0}, 0);
                // break;
            }

            v6DomainAnim.play();

        }
        Enabler.counter('SEARCH');

        loadJSON(function (response) {
            if (response.responseText.length == 0) {
                Enabler.counter('SEARCH_MISS');
                display_state('has_items');

                document.querySelector('.results-container').style.display = 'block';
                document.querySelector('#successMsg').innerHTML = '';
                document.querySelector('#errorMsg').innerHTML = '<h5>Sorry, couldn\'t find a match. Visit <a id="dynamicLink" style="text-decoration: underline">GoDaddy.com</a> for more options and your chance to start your new Web presence.</h5>';
                document.querySelector('#dynamicLink').addEventListener('click', function () {
                    Enabler.counter('SEARCH_LEARNMORE');
                    Enabler.exit('HTML5_Background_Clickthrough');
                    //            window.open(clickTag);
                });

                // too simple of a match.
            }
            else {
                display_state('has_items');
                var data = JSON.parse(response.responseText);

                switch (data.match) {
                    case true:
                        // exact match
                        Enabler.counter('SEARCH_HIT');
                        document.querySelector('.results-container').style.display = 'block';
                        document.querySelector('#successMsg').innerHTML = successMsg;
                        document.querySelector('#errorMsg').innerHTML = '';
                        break;

                    case false:
                        // got back spins
                        Enabler.counter('SEARCH_NEAR');

                        document.querySelector('.results-container').style.display = 'block';
                        document.querySelector('#successMsg').innerHTML = suggestMsg;
                        document.querySelector('#errorMsg').innerHTML = '';
                        break;
                }

                showDomains(data);
            }
        }, dom);
    }

    sea.addEventListener('click', function (event) {
        clean_values();

        // Code added to deal with a double result appearing
        sea.disabled = true;
        setTimeout(function(){
            sea.disabled = false;
        }, delayTime);
        // End of added code for double result. 

    });


    function queryStringToJson(qs) {
        if (qs.indexOf('?') === 0) {
            qs = qs.slice(1);
        }

        var pairs = qs.split('&');
        var result = {};
        pairs.forEach(function (p) {
            var pair = p.split('=');
            var key = decodeURIComponent(pair[0]);
            var value = decodeURIComponent(pair[1] || '');

            if (result[key]) {
                if (Object.prototype.toString.call(result[key]) === '[object Array]') {
                    result[key].push(value);
                } else {
                    result[key] = [result[key], value];
                }
            } else {
                result[key] = value;
            }
        });

        return JSON.parse(JSON.stringify(result));
    };
    
    function jsonToQueryString(json) {
        var out = [];
        for (var key in json) {
            out.push(encodeURIComponent(key) + '=' + encodeURIComponent(json[key]));
        }
        return '?' + out.join('&');
    }

    function showDomains(data) {
        //        Enabler.exit('HTML5_Background_Clickthrough');
        //        window.open(clickTag);


        container.innerHTML = " ";

         // if statement for two results changes the flex box to fix spacing between list items
         if(data.domains.length <= 2 && rightSize){

            document.querySelector('.test').style.display = 'inline-flex'; 
            searchedtwo = true;      
       }else if (rightSize){
           
        document.querySelector('.test').style.display = 'flex';
         }
       //End fix for two results on 728x90



        for (var i = 0; i < data.domains.length; i++) {
            var thisOne = data.domains[i],
                dataString = JSON.stringify(thisOne),
                outerDiv = document.createElement("div"),
                domainName = document.createElement("h3"),
                buyLink = document.createElement("a"),
                buyButton = document.createElement("button");

            outerDiv.className = "domain";

            //Attaches a second class to the second search result to add styling elements
            if(data.domains.length <=2 && searchedtwo && i == 1){
                outerDiv.className += " domainshort";
            }
            //End additional code to catch two search results on smaller ad size.


            buyButton.textContent = "Buy Now";
            domainName.textContent = thisOne.name;
            domainName.title = thisOne.name;
            buyLink.href = "#";

            buyLink.addEventListener('click', function () {
                Enabler.counter('SEARCH_CONVERT');
                Enabler.exitQueryString("HTML5_Search", "domainToCheck=" + this.domain);
            }.bind({
                domain: thisOne.name
            }));

            outerDiv.appendChild(domainName);
            buyLink.appendChild(buyButton);
            outerDiv.appendChild(buyLink);

            container.appendChild(outerDiv);
        }
    }
}


/*!
 * VERSION: 0.8.8
 * DATE: 2017-01-17
 * UPDATES AND DOCS AT: http://greensock.com
 *
 * @license Copyright (c) 2008-2017, GreenSock. All rights reserved.
 * MorphSVGPlugin is a Club GreenSock membership benefit; You must have a valid membership to use
 * this code without violating the terms of use. Visit http://greensock.com/club/ to sign up or get more details.
 * This work is subject to the software agreement that was issued with your membership.
 * 
 * @author: Jack Doyle, jack@greensock.com
 */
var _gsScope="undefined"!=typeof module&&module.exports&&"undefined"!=typeof global?global:this||window;(_gsScope._gsQueue||(_gsScope._gsQueue=[])).push(function(){"use strict";var a=Math.PI/180,b=180/Math.PI,c=/[achlmqstvz]|(-?\d*\.?\d*(?:e[\-+]?\d+)?)[0-9]/gi,d=/(?:(-|-=|\+=)?\d*\.?\d*(?:e[\-+]?\d+)?)[0-9]/gi,e=/(^[#\.]|[a-y][a-z])/gi,f=/[achlmqstvz]/gi,g=/[\+\-]?\d*\.?\d+e[\+\-]?\d+/gi,h=_gsScope._gsDefine.globals.TweenLite,i=function(a){_gsScope.console&&console.log(a)},j=function(b,c){var d,e,f,g,h,i,j=Math.ceil(Math.abs(c)/90),k=0,l=[];for(b*=a,c*=a,d=c/j,e=4/3*Math.sin(d/2)/(1+Math.cos(d/2)),i=0;j>i;i++)f=b+i*d,g=Math.cos(f),h=Math.sin(f),l[k++]=g-e*h,l[k++]=h+e*g,f+=d,g=Math.cos(f),h=Math.sin(f),l[k++]=g+e*h,l[k++]=h-e*g,l[k++]=g,l[k++]=h;return l},k=function(c,d,e,f,g,h,i,k,l){if(c!==k||d!==l){e=Math.abs(e),f=Math.abs(f);var m=g%360*a,n=Math.cos(m),o=Math.sin(m),p=(c-k)/2,q=(d-l)/2,r=n*p+o*q,s=-o*p+n*q,t=e*e,u=f*f,v=r*r,w=s*s,x=v/t+w/u;x>1&&(e=Math.sqrt(x)*e,f=Math.sqrt(x)*f,t=e*e,u=f*f);var y=h===i?-1:1,z=(t*u-t*w-u*v)/(t*w+u*v);0>z&&(z=0);var A=y*Math.sqrt(z),B=A*(e*s/f),C=A*-(f*r/e),D=(c+k)/2,E=(d+l)/2,F=D+(n*B-o*C),G=E+(o*B+n*C),H=(r-B)/e,I=(s-C)/f,J=(-r-B)/e,K=(-s-C)/f,L=Math.sqrt(H*H+I*I),M=H;y=0>I?-1:1;var N=y*Math.acos(M/L)*b;L=Math.sqrt((H*H+I*I)*(J*J+K*K)),M=H*J+I*K,y=0>H*K-I*J?-1:1;var O=y*Math.acos(M/L)*b;!i&&O>0?O-=360:i&&0>O&&(O+=360),O%=360,N%=360;var P,Q,R,S=j(N,O),T=n*e,U=o*e,V=o*-f,W=n*f,X=S.length-2;for(P=0;X>P;P+=2)Q=S[P],R=S[P+1],S[P]=Q*T+R*V+F,S[P+1]=Q*U+R*W+G;return S[S.length-2]=k,S[S.length-1]=l,S}},l=function(a){var b,d,e,f,h,j,l,m,n,o,p,q,r,s=(a+"").replace(g,function(a){var b=+a;return 1e-4>b&&b>-1e-4?0:b}).match(c)||[],t=[],u=0,v=0,w=s.length,x=2,y=0;if(!a||!isNaN(s[0])||isNaN(s[1]))return i("ERROR: malformed path data: "+a),t;for(b=0;w>b;b++)if(r=h,isNaN(s[b])?(h=s[b].toUpperCase(),j=h!==s[b]):b--,e=+s[b+1],f=+s[b+2],j&&(e+=u,f+=v),0===b&&(m=e,n=f),"M"===h)l&&l.length<8&&(t.length-=1,x=0),u=m=e,v=n=f,l=[e,f],y+=x,x=2,t.push(l),b+=2,h="L";else if("C"===h)l||(l=[0,0]),l[x++]=e,l[x++]=f,j||(u=v=0),l[x++]=u+1*s[b+3],l[x++]=v+1*s[b+4],l[x++]=u+=1*s[b+5],l[x++]=v+=1*s[b+6],b+=6;else if("S"===h)"C"===r||"S"===r?(o=u-l[x-4],p=v-l[x-3],l[x++]=u+o,l[x++]=v+p):(l[x++]=u,l[x++]=v),l[x++]=e,l[x++]=f,j||(u=v=0),l[x++]=u+=1*s[b+3],l[x++]=v+=1*s[b+4],b+=4;else if("Q"===h)o=e-u,p=f-v,l[x++]=u+2*o/3,l[x++]=v+2*p/3,j||(u=v=0),u+=1*s[b+3],v+=1*s[b+4],o=e-u,p=f-v,l[x++]=u+2*o/3,l[x++]=v+2*p/3,l[x++]=u,l[x++]=v,b+=4;else if("T"===h)o=u-l[x-4],p=v-l[x-3],l[x++]=u+o,l[x++]=v+p,o=u+1.5*o-e,p=v+1.5*p-f,l[x++]=e+2*o/3,l[x++]=f+2*p/3,l[x++]=u=e,l[x++]=v=f,b+=2;else if("H"===h)f=v,l[x++]=u+(e-u)/3,l[x++]=v+(f-v)/3,l[x++]=u+2*(e-u)/3,l[x++]=v+2*(f-v)/3,l[x++]=u=e,l[x++]=f,b+=1;else if("V"===h)f=e,e=u,j&&(f+=v-u),l[x++]=e,l[x++]=v+(f-v)/3,l[x++]=e,l[x++]=v+2*(f-v)/3,l[x++]=e,l[x++]=v=f,b+=1;else if("L"===h||"Z"===h)"Z"===h&&(e=m,f=n,l.closed=!0),("L"===h||Math.abs(u-e)>.5||Math.abs(v-f)>.5)&&(l[x++]=u+(e-u)/3,l[x++]=v+(f-v)/3,l[x++]=u+2*(e-u)/3,l[x++]=v+2*(f-v)/3,l[x++]=e,l[x++]=f,"L"===h&&(b+=2)),u=e,v=f;else if("A"===h){if(q=k(u,v,1*s[b+1],1*s[b+2],1*s[b+3],1*s[b+4],1*s[b+5],(j?u:0)+1*s[b+6],(j?v:0)+1*s[b+7]))for(d=0;d<q.length;d++)l[x++]=q[d];u=l[x-2],v=l[x-1],b+=7}else i("Error: malformed path data: "+a);return t.totalPoints=y+x,t},m=function(a,b){var c,d,e,f,g,h,i,j,k,l,m,n,o,p,q=0,r=.999999,s=a.length,t=b/((s-2)/6);for(o=2;s>o;o+=6)for(q+=t;q>r;)c=a[o-2],d=a[o-1],e=a[o],f=a[o+1],g=a[o+2],h=a[o+3],i=a[o+4],j=a[o+5],p=1/(Math.floor(q)+1),k=c+(e-c)*p,m=e+(g-e)*p,k+=(m-k)*p,m+=(g+(i-g)*p-m)*p,l=d+(f-d)*p,n=f+(h-f)*p,l+=(n-l)*p,n+=(h+(j-h)*p-n)*p,a.splice(o,4,c+(e-c)*p,d+(f-d)*p,k,l,k+(m-k)*p,l+(n-l)*p,m,n,g+(i-g)*p,h+(j-h)*p),o+=6,s+=6,q--;return a},n=function(a){var b,c,d,e,f="",g=a.length,h=100;for(c=0;g>c;c++){for(e=a[c],f+="M"+e[0]+","+e[1]+" C",b=e.length,d=2;b>d;d++)f+=(e[d++]*h|0)/h+","+(e[d++]*h|0)/h+" "+(e[d++]*h|0)/h+","+(e[d++]*h|0)/h+" "+(e[d++]*h|0)/h+","+(e[d]*h|0)/h+" ";e.closed&&(f+="z")}return f},o=function(a){for(var b=[],c=a.length-1,d=0;--c>-1;)b[d++]=a[c],b[d++]=a[c+1],c--;for(c=0;d>c;c++)a[c]=b[c];a.reversed=a.reversed?!1:!0},p=function(a){var b,c=a.length,d=0,e=0;for(b=0;c>b;b++)d+=a[b++],e+=a[b];return[d/(c/2),e/(c/2)]},q=function(a){var b,c,d,e=a.length,f=a[0],g=f,h=a[1],i=h;for(d=6;e>d;d+=6)b=a[d],c=a[d+1],b>f?f=b:g>b&&(g=b),c>h?h=c:i>c&&(i=c);return a.centerX=(f+g)/2,a.centerY=(h+i)/2,a.size=(f-g)*(h-i)},r=function(a){for(var b,c,d,e,f,g=a.length,h=a[0][0],i=h,j=a[0][1],k=j;--g>-1;)for(f=a[g],b=f.length,e=6;b>e;e+=6)c=f[e],d=f[e+1],c>h?h=c:i>c&&(i=c),d>j?j=d:k>d&&(k=d);return a.centerX=(h+i)/2,a.centerY=(j+k)/2,a.size=(h-i)*(j-k)},s=function(a,b){return b.length-a.length},t=function(a,b){var c=a.size||q(a),d=b.size||q(b);return Math.abs(d-c)<(c+d)/20?b.centerX-a.centerX||b.centerY-a.centerY:d-c},u=function(a,b){var c,d,e=a.slice(0),f=a.length,g=f-2;for(b=0|b,c=0;f>c;c++)d=(c+b)%g,a[c++]=e[d],a[c]=e[d+1]},v=function(a,b,c,d,e){var f,g,h,i,j=a.length,k=0,l=j-2;for(c*=6,g=0;j>g;g+=6)f=(g+c)%l,i=a[f]-(b[g]-d),h=a[f+1]-(b[g+1]-e),k+=Math.sqrt(h*h+i*i);return k},w=function(a,b,c){var d,e,f,g=a.length,h=p(a),i=p(b),j=i[0]-h[0],k=i[1]-h[1],l=v(a,b,0,j,k),m=0;for(f=6;g>f;f+=6)e=v(a,b,f/6,j,k),l>e&&(l=e,m=f);if(c)for(d=a.slice(0),o(d),f=6;g>f;f+=6)e=v(d,b,f/6,j,k),l>e&&(l=e,m=-f);return m/6},x=function(a,b,c){for(var d,e,f,g,h,i,j=a.length,k=99999999999,l=0,m=0;--j>-1;)for(d=a[j],i=d.length,h=0;i>h;h+=6)e=d[h]-b,f=d[h+1]-c,g=Math.sqrt(e*e+f*f),k>g&&(k=g,l=d[h],m=d[h+1]);return[l,m]},y=function(a,b,c,d,e,f){var g,h,i,j,k,l=b.length,m=0,n=Math.min(a.size||q(a),b[c].size||q(b[c]))*d,o=999999999999,p=a.centerX+e,r=a.centerY+f;for(h=c;l>h&&(g=b[h].size||q(b[h]),!(n>g));h++)i=b[h].centerX-p,j=b[h].centerY-r,k=Math.sqrt(i*i+j*j),o>k&&(m=h,o=k);return k=b[m],b.splice(m,1),k},z=function(a,b,c,d){var e,f,g,h,j,k,l,n=b.length-a.length,p=n>0?b:a,v=n>0?a:b,z=0,A="complexity"===d?s:t,B="position"===d?0:"number"==typeof d?d:.8,C=v.length,D="object"==typeof c&&c.push?c.slice(0):[c],E="reverse"===D[0]||D[0]<0,F="log"===c;if(v[0]){if(p.length>1&&(a.sort(A),b.sort(A),k=p.size||r(p),k=v.size||r(v),k=p.centerX-v.centerX,l=p.centerY-v.centerY,A===t))for(C=0;C<v.length;C++)p.splice(C,0,y(v[C],p,C,B,k,l));if(n)for(0>n&&(n=-n),p[0].length>v[0].length&&m(v[0],(p[0].length-v[0].length)/6|0),C=v.length;n>z;)h=p[C].size||q(p[C]),g=x(v,p[C].centerX,p[C].centerY),h=g[0],j=g[1],v[C++]=[h,j,h,j,h,j,h,j],v.totalPoints+=8,z++;for(C=0;C<a.length;C++)e=b[C],f=a[C],n=e.length-f.length,0>n?m(e,-n/6|0):n>0&&m(f,n/6|0),E&&!f.reversed&&o(f),c=D[C]||0===D[C]?D[C]:"auto",c&&(f.closed||Math.abs(f[0]-f[f.length-2])<.5&&Math.abs(f[1]-f[f.length-1])<.5?"auto"===c||"log"===c?(D[C]=c=w(f,e,0===C),0>c&&(E=!0,o(f),c=-c),u(f,6*c)):"reverse"!==c&&(C&&0>c&&o(f),u(f,6*(0>c?-c:c))):!E&&("auto"===c&&Math.abs(e[0]-f[0])+Math.abs(e[1]-f[1])+Math.abs(e[e.length-2]-f[f.length-2])+Math.abs(e[e.length-1]-f[f.length-1])>Math.abs(e[0]-f[f.length-2])+Math.abs(e[1]-f[f.length-1])+Math.abs(e[e.length-2]-f[0])+Math.abs(e[e.length-1]-f[1])||c%2)?(o(f),D[C]=-1,E=!0):"auto"===c?D[C]=0:"reverse"===c&&(D[C]=-1),f.closed!==e.closed&&(f.closed=e.closed=!1));return F&&i("shapeIndex:["+D.join(",")+"]"),D}},A=function(a,b,c,d){var e=l(a[0]),f=l(a[1]);z(e,f,b||0===b?b:"auto",c)&&(a[0]=n(e),a[1]=n(f),("log"===d||d===!0)&&i('precompile:["'+a[0]+'","'+a[1]+'"]'))},B=function(a,b,c){return b||c||a||0===a?function(d){A(d,a,b,c)}:A},C=function(a,b){if(!b)return a;var c,e,f,g=a.match(d)||[],h=g.length,i="";for("reverse"===b?(e=h-1,c=-2):(e=(2*(parseInt(b,10)||0)+1+100*h)%h,c=2),f=0;h>f;f+=2)i+=g[e-1]+","+g[e]+" ",e=(e+c)%h;return i},D=function(a,b){var c,d,e,f,g,h,i,j=0,k=parseFloat(a[0]),l=parseFloat(a[1]),m=k+","+l+" ",n=.999999;for(e=a.length,c=.5*b/(.5*e-1),d=0;e-2>d;d+=2){if(j+=c,h=parseFloat(a[d+2]),i=parseFloat(a[d+3]),j>n)for(g=1/(Math.floor(j)+1),f=1;j>n;)m+=(k+(h-k)*g*f).toFixed(2)+","+(l+(i-l)*g*f).toFixed(2)+" ",j--,f++;m+=h+","+i+" ",k=h,l=i}return m},E=function(a){var b=a[0].match(d)||[],c=a[1].match(d)||[],e=c.length-b.length;e>0?a[0]=D(b,e):a[1]=D(c,-e)},F=function(a){return isNaN(a)?E:function(b){E(b),b[1]=C(b[1],parseInt(a,10))}},G=function(a,b){var c=_gsScope.document.createElementNS("http://www.w3.org/2000/svg","path"),d=Array.prototype.slice.call(a.attributes),e=d.length;for(b=","+b+",";--e>-1;)-1===b.indexOf(","+d[e].nodeName+",")&&c.setAttributeNS(null,d[e].nodeName,d[e].nodeValue);return c},H=function(a,b){var c,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y=a.tagName.toLowerCase(),z=.552284749831;return"path"!==y&&a.getBBox?(i=G(a,"x,y,width,height,cx,cy,rx,ry,r,x1,x2,y1,y2,points"),"rect"===y?(g=+a.getAttribute("rx")||0,h=+a.getAttribute("ry")||0,e=+a.getAttribute("x")||0,f=+a.getAttribute("y")||0,m=(+a.getAttribute("width")||0)-2*g,n=(+a.getAttribute("height")||0)-2*h,g||h?(o=e+g*(1-z),p=e+g,q=p+m,r=q+g*z,s=q+g,t=f+h*(1-z),u=f+h,v=u+n,w=v+h*z,x=v+h,c="M"+s+","+u+" V"+v+" C"+[s,w,r,x,q,x,q-(q-p)/3,x,p+(q-p)/3,x,p,x,o,x,e,w,e,v,e,v-(v-u)/3,e,u+(v-u)/3,e,u,e,t,o,f,p,f,p+(q-p)/3,f,q-(q-p)/3,f,q,f,r,f,s,t,s,u].join(",")+"z"):c="M"+(e+m)+","+f+" v"+n+" h"+-m+" v"+-n+" h"+m+"z"):"circle"===y||"ellipse"===y?("circle"===y?(g=h=+a.getAttribute("r")||0,k=g*z):(g=+a.getAttribute("rx")||0,h=+a.getAttribute("ry")||0,k=h*z),e=+a.getAttribute("cx")||0,f=+a.getAttribute("cy")||0,j=g*z,c="M"+(e+g)+","+f+" C"+[e+g,f+k,e+j,f+h,e,f+h,e-j,f+h,e-g,f+k,e-g,f,e-g,f-k,e-j,f-h,e,f-h,e+j,f-h,e+g,f-k,e+g,f].join(",")+"z"):"line"===y?c="M"+a.getAttribute("x1")+","+a.getAttribute("y1")+" L"+a.getAttribute("x2")+","+a.getAttribute("y2"):("polyline"===y||"polygon"===y)&&(l=(a.getAttribute("points")+"").match(d)||[],e=l.shift(),f=l.shift(),c="M"+e+","+f+" L"+l.join(","),"polygon"===y&&(c+=","+e+","+f+"z")),i.setAttribute("d",c),b&&a.parentNode&&(a.parentNode.insertBefore(i,a),a.parentNode.removeChild(a)),i):a},I=function(a,b,c){var f,g,j="string"==typeof a;return(!j||e.test(a)||(a.match(d)||[]).length<3)&&(f=j?h.selector(a):a&&a[0]?a:[a],f&&f[0]?(f=f[0],g=f.nodeName.toUpperCase(),b&&"PATH"!==g&&(f=H(f,!1),g="PATH"),a=f.getAttribute("PATH"===g?"d":"points")||"",f===c&&(a=f.getAttributeNS(null,"data-original")||a)):(i("WARNING: invalid morph to: "+a),a=!1)),a},J="Use MorphSVGPlugin.convertToPath(elementOrSelectorText) to convert to a path before morphing.",K=_gsScope._gsDefine.plugin({propName:"morphSVG",API:2,global:!0,version:"0.8.8",init:function(a,b,c,d){var e,g,h,j,k;return"function"!=typeof a.setAttribute?!1:("function"==typeof b&&(b=b(d,a)),e=a.nodeName.toUpperCase(),k="POLYLINE"===e||"POLYGON"===e,"PATH"===e||k?(g="PATH"===e?"d":"points",("string"==typeof b||b.getBBox||b[0])&&(b={shape:b}),j=I(b.shape||b.d||b.points||"","d"===g,a),k&&f.test(j)?(i("WARNING: a <"+e+"> cannot accept path data. "+J),!1):(j&&(this._target=a,a.getAttributeNS(null,"data-original")||a.setAttributeNS(null,"data-original",a.getAttribute(g)),h=this._addTween(a,"setAttribute",a.getAttribute(g)+"",j+"","morphSVG",!1,g,"object"==typeof b.precompile?function(a){a[0]=b.precompile[0],a[1]=b.precompile[1]}:"d"===g?B(b.shapeIndex,b.map||K.defaultMap,b.precompile):F(b.shapeIndex)),h&&(this._overwriteProps.push("morphSVG"),h.end=j,h.endProp=g)),!0)):(i("WARNING: cannot morph a <"+e+"> SVG element. "+J),!1))},set:function(a){var b;if(this._super.setRatio.call(this,a),1===a)for(b=this._firstPT;b;)b.end&&this._target.setAttribute(b.endProp,b.end),b=b._next}});K.pathFilter=A,K.pointsFilter=E,K.subdivideRawBezier=m,K.defaultMap="size",K.pathDataToRawBezier=function(a){return l(I(a,!0))},K.equalizeSegmentQuantity=z,K.convertToPath=function(a,b){"string"==typeof a&&(a=h.selector(a));for(var c=a&&0!==a.length?a.length&&a[0]&&a[0].nodeType?Array.prototype.slice.call(a,0):[a]:[],d=c.length;--d>-1;)c[d]=H(c[d],b!==!1);return c},K.pathDataToBezier=function(a,b){var c,d,e,f,g,i,j,k,m=l(I(a,!0))[0]||[],n=0;if(b=b||{},k=b.align||b.relative,f=b.matrix||[1,0,0,1,0,0],g=b.offsetX||0,i=b.offsetY||0,"relative"===k||k===!0?(g-=m[0]*f[0]+m[1]*f[2],i-=m[0]*f[1]+m[1]*f[3],n="+="):(g+=f[4],i+=f[5],k&&(k="string"==typeof k?h.selector(k):k&&k[0]?k:[k],k&&k[0]&&(j=k[0].getBBox()||{x:0,y:0},g-=j.x,i-=j.y))),c=[],e=m.length,f)for(d=0;e>d;d+=2)c.push({x:n+(m[d]*f[0]+m[d+1]*f[2]+g),y:n+(m[d]*f[1]+m[d+1]*f[3]+i)});else for(d=0;e>d;d+=2)c.push({x:n+(m[d]+g),y:n+(m[d+1]+i)});return c}}),_gsScope._gsDefine&&_gsScope._gsQueue.pop()(),function(a){"use strict";var b=function(){return(_gsScope.GreenSockGlobals||_gsScope)[a]};"function"==typeof define&&define.amd?define(["TweenLite"],b):"undefined"!=typeof module&&module.exports&&(require("../TweenLite.js"),module.exports=b())}("MorphSVGPlugin");var animation_setup = {
    "frames": {
        "frame0": {
            "time": 0.1,
            "state": "f0"
        },
        "frame1": {
            "time": 4,
            "state": "f1" 
        },
        "frame2": {
            "time": 4,
            "state": "f2" 
        }
    },
    "states": {}
};
animation_setup.states.f0 = {
    "use_timeline": true,
    "items": [
        {
            "target" : "#content_ad",
            "value": "showing",
            "verb": "addClass",
            "offset": "frame0"
        },
        {
            "target": ".v5frame-1",
            "value": "reversed showing",
            "verb": "addClass",
            "offset": "frame0"
        },
        {
            "target" : ".foreground .frame.frame-1 span, .foreground .frame.frame-2 span, .foreground .frame.frame-end",
            "values" : {
                "opacity" : 0
            },
            "verb": "set",
            "offset": "frame0"
        },
        {
            "target": ".v5frame-2",
            "values": {
                "x": 0
            },
            "verb": "set",
            "offset": "frame0"
        },
        {
            "target": ".texture-1",
            "values": {
                "y": 48
            },
            "verb": "set",
            "offset": "frame0"
        },
        {
            "target": ".texture-2",
            "values": {
                "y": 48
            },
            "verb": "set",
            "offset": "frame0"
        },
        {
        },
        {
            "target" : ".imgframe-2",
            "values" : {
                "x" : 300,
                "opacity": 1
            },
            "verb": "set",
            "offset": "frame0"
        },
        {
            "target" : ".imgframe-1",
            "values" : {
                "y" : 250
            },
            "verb": "set",
            "offset": "frame0"
        },
        {
            "target": ".v5frame-2",
            "values": {
                "x": 330,
                "opacity": 1
            },
            "duration" : 0.6,
            "verb": "to",
            "offset": "frame0+=0"
        },
        {
            "target" : ".imgframe-1",
            "values" : {
                "y" : 0
            },
            "duration" : 1,
            "verb": "to",
            "offset": "frame0+=0.1"
        },
        {
            "target": ".texture-1",
            "values": {
                "y": 0
            },
            "duration" : 0.6,
            "verb": "to",
            "offset": "frame0+0.2"
        },
        {
            "target" : ".foreground .frame.frame-1",
            "values" : {
                "opacity" : 1
            },
            "duration" : 0.2,
            "verb": "to",
            "offset": "frame0+=0.3"
        },
        {
            "target" : ".foreground .frame.frame-1 span",

            "values" : {
                "opacity" : 1
            },
            "duration" : 0.4,
            "verb": "to",
            "offset": "frame0+=1.6"
        },
        {
            "target": ".v5frame-2",
            "values": {
                "x": -568
            },
            "duration" : 0.5,
            "verb": "to",
            "offset": "frame1+=0.3"
        },
        {
            "target" : ".foreground .frame.frame-1 .textholder, .imgframe-1",
            "values" : {
                "opacity" : 0
            },
            "duration" : 0.3,
            "verb": "to",
            "offset": "frame1+=0"
        },
        {
            "target": ".foreground .frame.frame-2 .text",
            "value": "reversed",
            "verb": "addClass",
            "offset": "frame1"
        },
        {
            "target": ".texture-1",
            "values": {
                "opacity": 0
            },
            "duration" : 0.6,
            "verb": "to",
            "offset": "frame1"
        },
        {
            "target": ".texture-2",
            "values": {
                "y": 0
            },
            "duration" : 0.6,
            "verb": "to",
            "offset": "frame1+0.2"
        },
        {
            "target" : ".foreground .frame.frame-2",
            "values" : {
                "opacity" : 1
            },
            "duration" : 0.2,
            "verb": "to",
            "offset": "frame1+=0.3"
        },

        {
            "target" : ".imgframe-2",
            "values" : {
                "x" : 0,
                "opacity" : 1
            },
            "duration" : 1,
            "verb": "to",
            "offset": "frame1+=0.1"
        },
        {
            "target" : ".foreground .frame.frame-2 span",
            "values" : {
                "opacity" : 1
            },
            "duration" : 0.4,
            "verb": "to",
            "offset": "frame1+=0.6"
        },
        {
            "target": ".foreground .frame.frame-end .text",
            "value": "reversed",
            "verb": "addClass",
            "offset": "frame2"
        },

        {
            "target" : ".foreground .frame.frame-2, .imgframe-1",
            "values" : {
                "opacity" : 0
            },
            "duration" : 0.3,
            "verb": "to",
            "offset": "frame2+=0"
        },
        {
            "target": ".imgframe-2",
            "values": {
                "x": 300
            },
            "duration" : 0.2,
            "verb": "to",
            "offset": "frame2+=0.2"
        },
        {
            "target" : ".foreground .frame.frame-end",
            "values" : {
                "opacity" : 1
            },
            "duration" : 0.2,
            "verb": "to",
            "offset": "frame2+=0.6"
        },
        {
            "target" : ".frame-container.foreground .frame-end .has-domain-search .domain-search-input input[type='text']",
            "values" : {
                "opacity": 0
            },
            "verb": "set",
            "offset": "frame0"
        },
        {
            "target" : ".frame-container.foreground .frame-end .has-domain-search .domain-search-input input[type='text']",
            "values": {
                "opacity" : 1
            },
            "duration" : 0.3,
            "verb": "to",
            "offset": "frame2+=0.8"
        },
        {
            "target" : ".frame-container.foreground .frame-end .has-domain-search .domain-search-input button",
            "values" : {
                "opacity": 0
            },
            "verb": "set",
            "offset": "frame0"
        },
        {
            "target" : ".frame-container.foreground .frame-end .has-domain-search .domain-search-input button",
            "values": {
                "opacity" : 1
            },
            "duration" : 0.3,
            "verb": "to",
            "offset": "frame2+=0.8"
        },
        {
            "target" : ".frame-container.foreground .frame-end .has-domain-search .textbox-headline",
            "values" : {
                "opacity": 0
            },
            "verb": "set",
            "offset": "frame0"
        },
        {
            "target" : ".frame-container.foreground .frame-end .has-domain-search .textbox-headline",
            "values": {
                "opacity" : 1
            },
            "duration" : 0.5,
            "verb": "to",
            "offset": "frame2+=0.6"
        }
    ]
}

if (G_AD_SETUP.vars.new_logo) {
  animation_setup.states.f0.items.push({
    "target" : ".logo #gd_logo #heart-path-2",
    "values" : {
      "strokeDasharray" : "0 1114.03125 0",
      "stroke" : G_AD_SETUP.color1,
      "opacity" : 1,
    },
    "verb": "set",
    "offset": "frame0+=0.3"
  });

  animation_setup.states.f0.items.push({
    "target" : ".logo #gd_logo .letter",
    "values" : {
        "fill" : G_AD_SETUP.color1
    },
    "verb": "set",
    "offset": "frame0+=0.3"
  });

  animation_setup.states.f0.items.push({
    "target" : ".logo #gd_logo #heart-path-2",
    "values" : {
        "strokeDasharray" : "1114.03125 1114.03125 0"
    },
    "duration" : 2,
    "verb": "to",
    "offset": "frame0+=0.3"
  });

  animation_setup.states.f0.items.push({
    "target" : ".logo #gd_logo .letter",
    "initial" : {
        "opacity" : 0
    },
    "values" : {
        "opacity" : 1
    },
    "duration" : 0.2,
    "stagger" : 0.2,
    "verb": "staggerFromTo",
    "offset": "frame0+=0.8"
  });

  animation_setup.states.f0.items.push({
    "target" : ".logo #gd_logo .letter",
    "values" : {
        "fill" : G_AD_SETUP.color2
    },
    "duration" : 0.2,
    "verb": "to",
    "offset": "frame2+=0.3"
  });

  animation_setup.states.f0.items.push({
    "target" : ".logo #gd_logo #heart-path-2",
    "values" : {
        "stroke" : G_AD_SETUP.color2
    },
    "duration" : 0.2,
    "verb": "to",
    "offset": "frame2+=0.3"
  });

} else {
  // animation_setup.states.f0.items.push({
  //   "target": ".logo",
  //   "values": {
  //       "x": 0
  //   },
  //   "verb": "set",
  //   "offset": "frame0"
  // });

  // animation_setup.states.f0.items.push({
  //   "target": ".logo",
  //   "values": {
  //       "x" : 0
  //   },
  //   "duration" : 0.2,
  //   "verb": "to",
  //   "offset": "frame1+=0.3"
  // });

  animation_setup.states.f0.items.push({
    "target" : ".logo #gd_logo path",
    "initial" : {
        "opacity" : 0
    },
    "values" : {
        "opacity" : 1
    },
    "duration" : 0.2,
    "stagger" : 0.2,
    "verb": "staggerFromTo",
    "offset": "frame0+=0.3"
  });

  animation_setup.states.f0.items.push({
    "target" : ".logo #gd_logo path",
    "values" : {
        "fill" : G_AD_SETUP.color2
    },
    "duration" : 0.2,
    "verb": "to",
    "offset": "frame2+=0.3"
  });
}
// Image template animations are done in patch files only.


var COLORSCHEME = {
	one : '#4095e8',
	two : '#fbd9ed'
}
