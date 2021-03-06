  var EzoicMash = function () {
  };



    onmessagefunc = function(e) {

        //console.log("onmessagefunc ", e, " args ", arguments);

        if(typeof(e.data) == 'undefined') {
            e.data = e;
        }
        
        var event = e.data[0];
        var ad = e.data[1];

        //console.log('ww: event ', event, ' ad ', ad);

        switch(event) {
            case "ad_loaded":
            // ad was viewed
            ezoic_mash.AdLoadedEvent(ad);
            break;
            case "clear":
            // passes window information over.
            ezoic_mash.ClearIntervalAndTimeout();
            break;
            case "idle":
            // passes window information over.
            if(ezoic_mash.idle_count >= 0) {
                ezoic_mash.idle_count = e.data[2];
            }
            if(ezoic_mash.IsReadyForRefresh()) {
                ezoic_mash.RefreshQueuedSlots();
            }
            break;
            case "init":
            // passes window information over.
            ezoic_mash.InitExternalData(e.data[2]);
            break;
            case "set_bfh":
            // passes window information over.
            ezoic_mash.ezoibfh = e.data[2];
            break;
            case "viewed":
            // ad was viewed
            ezoic_mash.AdViewedEvent(ad);
            break;
            default:
            // code block
            //console.log("ww: unsupported event ", event);
        }

    };

    if (typeof(window) == "undefined" || window.document === undefined) {
        onmessage = onmessagefunc;
    } else {
        EzoicMash.prototype.onmessage = onmessagefunc;
        EzoicMash.prototype.postMessage = onmessagefunc;
    }

  EzoicMash.prototype.out = function() {
    if(false) {
        var msg = "ww: ";
        for (var i = 0; i < arguments.length; i++) {
            if (typeof arguments[i] !== 'undefined' && arguments[i] !== null) {
                if(typeof(arguments[i]) == "string") {
                    msg += arguments[i];
                } else {
                    msg += JSON.stringify(arguments[i]);
                }
                
            }
        }
        console.log(msg);
    }
  };

  EzoicMash.prototype.init = function() {
    ezoic_mash.initVars();
    ezoic_mash.startIdleFailsafe();
  };

  EzoicMash.prototype.InitExternalData = function(vars) {
    for(var key in vars) {
        //ezoic_mash.out("init var ", key);
        ezoic_mash[key] = vars[key];
    }
  }

  EzoicMash.prototype.initVars = function() {

    //filled from the window
    this.ezoadxnc = ""; //adx network code
    this._ebcids = []; //empty creative ids for exchange bidding
    this.__banger_pmp_deals = {}; //private marketplace deals
    this.ezorbf = {}; //server side set bid floors
    this.ezoibfh = {}; //bid floor hash keys
    this.ezbfcr = null; //local currency
    this.ezbflclcr = 1.0; //currency conversion rate
    this.ezslots = {}; // array of ezslot names

    this.viewed_slots = {} //keeps track of which slots have been viewed since the latest ad loaded [slot_name] = 0|1
    this.intervalIds = [], //keep track of interval ids
    this.timeoutIds = [] // keep track of timeout ids
    this.slot_retries = {}, //the number of times we have retried loading each slot. [slot_name] = count
	this.slot_max_retries = {}, //the max number of times we want to retry loading each slot. [slot_name] = count
    this.lazy_load_slots = {}, //stores the value of the ll targeting param by slot. [slot_name] = 0|1
	this.slots_available_for_refresh = {}, //stores the slots that can be refreshed and what bid floor they should be refreshed at. [slot_name] = bid_int
	this.slots_eb_wait_time = {}, //stores the amount of time we have waited for a new floor key to come back for bfhash
    this.slot_top_location = {}, //the pixel location on the page of the top of the ad [slot_name] = top_px
    this.slot_order = [], //the slots on the page ordered from top to bottom.
    this.ad_load_failsafe_interval = {}, //the variable to set and clear the interval for making sure that all the ads load.
    this.slot_positions = {}, //the positioning on the page of slots. [slot_name] = obj.top obj.bottom, obj.left, etc.
    this.position_check_loop_control = 0, //used to prevent an infinite loop on  page that doesn't load all slots
    this.spacing_adjusted_slots = {}, // a list of the ads that we adjusted the spacing around to comply with ads on screen rules
    this.slots_initially_loaded = [], //a list of the slots that we have requested an ad for.
    this.slots_loaded_non_yet_refreshable = {}, // keeps track of slots that have loaded but cannot be refreshed yet
    this.anchor_slots = [], //slots that are anchor ads so on screen rules dont apply
    this.safe_labeled_slots = [], //stores which ad slots we have labelled
    this.conflict_slots = {}, //slot pairings that are too close keyed by "first_slot_name_X_second_slot_name"
    this.adblock_css_added = 0,
    this.disable_refresh_slots = {}, //stores which slots to disable refresh (used to prevent criteo from refreshing slots too many times)
    this.ads_loaded = 0,// count of ads that been loaded
    this.initial_ad_positions_stored = false, // flag to trigger individual ad pos stores
    this.slot_positions_stored = {}, // keep track of slot positions stored
    this.words_at_y_pos = {}, //keep track of word count at elem positions. [y_pos] = word_count
    this.words_at_y_pos_keys = [], //sorted keys
    this.words_before_stored = {}, //keep track of words_before that have been stored
    this.bfh = {}, // a mapping of bid floors to their hashes
    this.currently_refreshing = {}; // a list of slots that are refreshing this instant.
    this.slot_refresh_epochs = {}; //slots and the epoch time we last refreshed.
    this.init_time = Date.now();
    this.is_idle = false;
    this.slots_waiting_for_refresh = [] //slots that should refresh but we are waiting for idle
  };

  EzoicMash.prototype.SendMessage = function(e) {
    if (typeof(window) == "undefined" || window.document === undefined) {
        postMessage(e);
    } else if(typeof(this.EzoMainMessage) == "function") {
        this.EzoMainMessage(e);
    }
  }

  EzoicMash.prototype.ClearIntervalAndTimeout = function() {

        ezoic_mash.out("ClearIntervalAndTimeout ", this.intervalIds, this.timeoutIds);
        for(var i = 0; i < this.intervalIds.length; i++) {
            clearInterval(this.intervalIds[i]);
        }

        for(var i = 0; i < this.timeoutIds.length; i++) {
            clearTimeout(this.timeoutIds[i]);
        }
    };

    SetSlotTargeting = function(slot,k,v) {
        slot.UnSyncedTargeting[k] = v;
        slot.Targeting[k] = v;
        //ezoic_mash.out("SetSlotTargeting ", slot.ElementId, " ", k,"=",v, " Unsynced values of ", slot.UnSyncedTargeting);
    };


    EzoicMash.prototype.AdLoadedEvent = function (e) {

        // Check if is one of our ads
        if(ezoic_mash.isOwnedAd(e) === false) {
            ezoic_mash.out("dwayne is not owned: ", e);
            return;
        }

        //reset params so we don't refresh multiple times at ones
        ezoic_mash.viewed_slots[e.slot.ElementId] = 0;
        ezoic_mash.slots_available_for_refresh[e.slot.ElementId] = null;

        var slot_name = e.slot.ElementId;
        var dfp_adunit_path = e.slot.AdUnitPath

        // Previous bang was final bang
        if(typeof ezoic_mash.intbidfb[dfp_adunit_path] != "undefined" && ezoic_mash.intbidfb[dfp_adunit_path] === true){
            // Gave opportunity above int ad, and slot empty - award to int
            if(e.isEmpty) {
                ezoic_mash.SendMessage(['load_int_ad', e, ezoic_mash.intbids[dfp_adunit_path]]);
            }

            // reset for refreshes
            ezoic_mash.intbidfb[dfp_adunit_path] = false;
            // Decrement bid for refreshes
            ezoic_mash.intbids[dfp_adunit_path].bid = ezoic_mash.intbids[dfp_adunit_path].bid * 0.75;
            return;
        }

        ezoic_mash.out("dwayne ad load event ", slot_name, e);

        // For lazy loading, both slotResponseReceived and slotRenderEnded call AdLoadedEvent
        // If slot has already loaded, do not try storing data / refreshing, until refresh timer goes off
        if (ezoic_mash.slots_loaded_non_yet_refreshable[slot_name] === true) {
            ezoic_mash.out("dwayne already loaded", e.slot.ElementId);
            return;
        }

        ezoic_mash.out("dwayne 3", slot_name, e);


        ezoic_mash.currently_refreshing[slot_name] = false;
        ezoic_mash.disable_refresh_slots[slot_name] = false;

        

        var max_refreshes = 2; //max number of times to allow a single slot to reload
        var bid_decrease_amount = .3; //percentage to lower the floor each refresh

        //record that this ad tried to load
        if (ezoic_mash.slot_retries.hasOwnProperty(slot_name) === false) {
            ezoic_mash.slot_retries[slot_name] = 0;
        }


            var cbf = ezoic_mash.GetBidFloorInUSD(e.slot); //current bid floor
            var cbfh = e.slot.Targeting['eb_br']; //current bid floor hash
            var ad_position_id = e.slot.Targeting['ap'];
            var ad_location_id = e.slot.Targeting['al'];
            var compid = e.slot.Targeting['compid'];
            var refresh = e.slot.Targeting['reft'];
            var refresh_duration = e.slot.Targeting['refs'];
            var header_bid = e.slot.Targeting['hb_pb'];
            var header_ssid = e.slot.Targeting['hb_ssid'];
            var disable_google = e.slot.Targeting['disablegoogle'];
            var google_account = e.slot.Targeting['ga'];
            var banger_version = e.slot.Targeting['bv'];
            var backfill = e.slot.Targeting['at'];
            var compid = e.slot.Targeting['compid'];
            var brfloor = e.slot.Targeting['brf'];
            var google_adsense_account_link = e.slot.Targeting['gala'];
            var slnc = e.slot.AdUnitPath.split("/")[1]; //network code

            //todo
            if(typeof ezoic_mash.ezoadxnc != "undefined" && ezoic_mash.ezoadxnc.length > 3) {
                var nc = ezoic_mash.ezoadxnc
            } else {
                var nc = slnc
            }

            if(nc != slnc) {
                nc += "|"+slnc;
            }
            

            if(typeof refresh_duration == 'undefined' || refresh_duration == '') {
                refresh_duration = 30;
            }

            //ezoic_mash.out("ssid: "+header_ssid+" for: "+e.slot.ElementId);
            if(typeof header_ssid !== 'undefined' && header_ssid != ''){
                switch (header_ssid)
                {
                    case "10010":
                    case "10011":
                    case "10013":
                    case "10015":
                    case "10016":
                    case "10017":
                    case "10018":
                    case "10019":
                    case "10020":
                    case "10028":
                    case "10030":
                    case "10031":
                    case "10033":
                    case "10034":
                    case "10035":
                    case "10039":
                    case "10048":
                    case "10050":
                    case "10054":
                    case "10057":
                    case "10058":
                    case "10060":
                    case "10061":
                    case "10062":
                        refresh_duration = 60;
                        break;
                    default:
                        break;
                }
            }

            SetSlotTargeting(e.slot,'refs', refresh_duration.toString());

            if (typeof e.slot.Targeting['fpfl'] !== 'undefined') {
                SetSlotTargeting(e.slot,'fpfl', "2");
            }
            

            refresh_duration = refresh_duration * 1000;

            //if we are trying adx before linkunits the set banger_version to 100
            if (compid == "0" && google_adsense_account_link != undefined && backfill == "lu") {
                banger_version = '100';
            }
            //ezoic_mash.out("For "+e.slot.ElementId);
            //ezoic_mash.out("cbf: "+cbf);

            var refresh_settings = ezoic_mash.CalculateNewFloor(cbf, ezoic_mash.slot_retries[slot_name]+1, ad_location_id, banger_version, nc, cbfh, brfloor);

            var refresh_floor = refresh_settings['rf'];
            var max_refreshes = refresh_settings['mr'];

            var intbid = 0;
            if(typeof ezoic_mash.intbids[dfp_adunit_path] != "undefined"){
                intbid = ezoic_mash.intbids[dfp_adunit_path].bid;
            }

            // Allow chance to beat our bid
            if(intbid > refresh_floor && refresh_floor >= 0){
                refresh_floor = intbid+1;
                ezoic_mash.intbidfb[dfp_adunit_path] = true;
            }

            //set the max refreshes at the start, so if calcNewFloor changes it it doesn't fuck things up
            if (typeof ezoic_mash.slot_max_retries[slot_name] === "undefined") {
                ezoic_mash.slot_max_retries[slot_name] = max_refreshes;
            } else {
                max_refreshes = ezoic_mash.slot_max_retries[slot_name];
            }


            //prefetch next bidfloor hash if we using it
            if (typeof cbfh !== 'undefined') {
                //having cbfh here is fine, only need to know if its undefined or not
                //ezoic_mash.out("Prefetch For "+e.slot.ElementId);
                ezoic_mash.CalculateNewFloor(ezoic_mash.FormatBid(refresh_floor), ezoic_mash.slot_retries[slot_name]+2, ad_location_id, banger_version, nc, cbfh, brfloor);
            }


            //if the backfill is set to lu then use adx first, then load the link unit
            if (compid == "0" && google_adsense_account_link != undefined && backfill == "lu") {
                if (brfloor > refresh_floor)
                {
                    SetSlotTargeting(e.slot,'compid', "4");
                }
            }

            if(compid == "4") {
                refresh_floor = 5000;
            }


        //make sure the element exists so we at least don't get an error
        if (typeof ezoic_mash._ebcids == 'undefined') {
            //exch bid creative ids
            ezoic_mash._ebcids = [138231308856, 138231308940, 138231308949, 138231387842, 138231421744, 138231421759, 138231421774, 138231421783, 138231421789, 138231421792, 138242067587, 138242067590, 138242067602, 138242067605, 138242067608, 138242067614, 138242229406, 138242229415, 138242229421, 138242229430];
        }

        //ezoic_mash.out("calling contains with ",_ebcids);
        var is_empty_creative = ezoic_mash.ArrayContains(ezoic_mash._ebcids, e.creativeId);

        if(e.lineItemId == 5166837810) {
            is_empty_creative = true;
        }

        //retry if we got nothing back or if we got one of our eb creatives back
        var do_retry = (typeof cbf != 'undefined' && ( (e.creativeId == null && e.lineItemId == null && e.isEmpty === true) || is_empty_creative == true));

        if (do_retry === true && ezoic_mash.slot_retries[slot_name] <= max_refreshes) {
            e.isEmpty = true;
        }
        ezoic_mash.out("Retry is "+(do_retry ? "true":"false")+" for e.creativeId: " + e.creativeId + " slot "+slot_name+ " is empty creative: "+is_empty_creative);

        //the last bid floor was already zero.
        if(cbf == 0 && do_retry === true && ezoic_mash.slot_retries[slot_name] <= max_refreshes) {
            //ezoic_mash.out("empty and floor was already zero. no need to try again for slot ", slot_name);
            ezoic_mash.slot_retries[slot_name] = max_refreshes+1;
        }

        //not allowed to retry again and it's still empty - give adsense one last shot
        if(ezoic_mash.slot_retries[slot_name] == (max_refreshes+1) && e.isEmpty === true && compid != "1" && compid != "7" && ad_location_id != "2005" && ad_location_id != "1005" && ad_location_id != "3005" && ad_location_id != "1031" && ad_location_id != "1032") {
            //ezoic_mash.out("trying comp 1 in ", slot_name);
            SetSlotTargeting(e.slot,'compid', '1,4');
            SetSlotTargeting(e.slot,'nocompoverride', '1');
            max_refreshes++;
        }

        //not allowed to retry again and it's still empty - give adsense one last shot
        if(compid == "7" && ezoic_mash.slot_retries[slot_name] == (max_refreshes) && e.isEmpty === true &&  ad_location_id != "2005" && ad_location_id != "1005" && ad_location_id != "3005" && ad_location_id != "1031" && ad_location_id != "1032") {
            SetSlotTargeting(e.slot,'compid', '1');
            SetSlotTargeting(e.slot,'nocompoverride', '1');
            ezoic_mash.SetBidFloor(e.slot,0);
        }


        if(compid == "6" && (brfloor > refresh_floor)) {
            do_retry = false;
        }

        //retry the ad.
        if (do_retry === true && ezoic_mash.slot_retries[slot_name] <= max_refreshes) {


            ezoic_mash.currently_refreshing[slot_name] = true;


            //if this is a location where we want to disable google (due to violations)
            //and we have an alternative ad - then do so.
            if(disable_google == '1' && header_bid > 0 && (typeof google_account != 'undefined' && google_account != '0')) {
                //ezoic_mash.out("disabling google on retry "+ezoic_mash.slot_retries[slot_name]+" for slot "+slot_name);
                SetSlotTargeting(e.slot,'ga2',e.slot.Targeting['ga']); //disable google ads on this slot.
                SetSlotTargeting(e.slot,'ga','0'); //disable google ads on this slot.
                ezoic_mash.SetBidFloor(e.slot, 0);
            }

            //keep track of refresh tries
            ezoic_mash.slot_retries[slot_name]++;

            //if the next retry wil be our last, try to get the hash for bf 0 now
            if (typeof cbfh !== 'undefined' && ezoic_mash.slot_retries[slot_name] == max_refreshes) {
                ezoic_mash.FetchBidFloorHash(0, nc);
            }


            //if this is our last refresh try, set the bid floor to zero.
            if (ezoic_mash.slot_retries[slot_name] > max_refreshes) {
                //ezoic_mash.out("RefreshSlot: 0 "+e.slot.ElementId);
                ezoic_mash.RefreshSlot(e,0);

        // add all pmp deals -- __banger_pmp_deal_list<dealId, priceFloor> from Sol's dfpslot.go
        if(typeof ezoic_mash.__banger_pmp_deals != 'undefined') {
            ezoic_mash.out("pmp_deals ", e);
            var pmp_deals = e.slot.Targeting['deal1'] || [];
            for( var bangerSlotIndex in ezoic_mash.__banger_pmp_deals) {
                var bangerSlot = ezoic_mash.__banger_pmp_deals[bangerSlotIndex];
                if( bangerSlot['SlotName'] == dfp_adunit_path ) {
                // get a list of all deals that should be added to the targeting
                for( var dealIndex in bangerSlot.Deals ) {
                    var deal = bangerSlot.Deals[dealIndex];
                    if (pmp_deals.indexOf(deal.DealId) === -1) {
                    pmp_deals.push(deal.DealId);
                    }
                }
                // add the list to targeting
                if(typeof pmp_deals == 'undefined' || pmp_deals.length < 1) {
                    SetSlotTargeting(e.slot,'deal1', "");
                } else {
                    SetSlotTargeting(e.slot,'deal1', pmp_deals);
                }
                
                break;
                }
            }
        }
            } else {
                        //decrease the bid floor
                        //ezoic_mash.out("refresh_floor: "+refresh_floor);
                        var new_bid = refresh_floor;
                        var nbf = ezoic_mash.FormatBid(new_bid);
                        while(new_bid > 0 && nbf == cbf) {
                            new_bid = new_bid * .9;
                            nbf = ezoic_mash.FormatBid(new_bid);
                        }

                    // add pmp deals if they are now above the new price floor -- __banger_pmp_deal_list<dealId, priceFloor> from Sol's dfpslot.go
                    if(typeof ezoic_mash.__banger_pmp_deals != 'undefined') {
                        ezoic_mash.out("pmp_deals 2 ", e);
                        var pmp_deals = e.slot.Targeting['deal1'] || [];
                        for (var bangerSlotIndex in ezoic_mash.__banger_pmp_deals) {
                        var bangerSlot = ezoic_mash.__banger_pmp_deals[bangerSlotIndex];
                        if (bangerSlot['SlotName'] == dfp_adunit_path) {
                            // get a list of all deals that should be added to the targeting
                            for (var dealIndex in bangerSlot.Deals) {
                            var deal = bangerSlot.Deals[dealIndex];
                                if (new_bid < deal.Floor) {
                                if (pmp_deals.indexOf(deal.DealId) === -1) {
                                    pmp_deals.push(deal.DealId);
                                }
                                }
                            }
                            // add the list to targeting
                            if(typeof pmp_deals == 'undefined' || pmp_deals.length < 1) {
                                SetSlotTargeting(e.slot,'deal1', "");
                            } else {
                                SetSlotTargeting(e.slot,'deal1', pmp_deals);
                            }
                            break;
                        }
                        }
                    }

                //retries the slot with the new floor
                //ezoic_mash.out("RefreshSlot with new floor "+e.slot.ElementId);
                //ezoic_mash.out("RefreshSlot Bidfloor: "+nbf);
                ezoic_mash.RefreshSlot(e,nbf);
            }
        } else { // ad has loaded or we have hit out maximum refreshes

            //ezoic_mash.out("ad loaded ok: "+ad_location_id+" - retries - "+ezoic_mash.slot_retries[slot_name]+" max "+max_refreshes);
            //ezoic_mash.out("cbf: "+cbf+" creative id: "+e.creativeId+" lineitem id: "+e.lineItemId+" isempty: "+e.isEmpty+" slot "+e.slot.ElementId);


            

            ezoic_mash.out("ad loaded");
            if (e.isEmpty === false) {

                //todo: do this when ad fills in banger.
                //ezoic_mash.UpdatePageViewMetricCookie(cbf);
                ezoic_mash.SendMessage(['update_pv_metrics', e, cbf]);

                //records information about the ad load
                ezoic_mash.ads_loaded++;
                ezoic_mash.slots_loaded_non_yet_refreshable[e.slot.ElementId] = true;
                ezoic_mash.SendMessage(['store_data', e]);

                ezoic_mash.checkAllAdsLoaded();

                //if the ad isn't mediation and it's a real ad.
                //ezoic_mash.out("dwayne awesome fresh timer? ", refresh, " -- ", refresh_duration);
                if (ezoic_mash.IsRefreshOn(e.slot)) {
                    //start the timer so we know when it's allowed to refresh.
                    //30 seconds is google's rule.
                    ezoic_mash.StartRefreshAllowedTimer(e.slot, cbf, refresh_duration);
                }

                //the ad loaded so we reset the retry count so it starts at zero for refreshes.
                ezoic_mash.slot_retries[slot_name] = 0;
                    
                clearTimeout(ezoic_mash.ad_load_failsafe_interval);
                ezoic_mash.ad_load_failsafe_interval = ezoic_mash.SetTimeout(ezoic_mash.LoadUnloadedAds, 5000);
                

            } else {
                ezoic_mash.SendMessage(['collapse',e]);
            }
        }
    };

  EzoicMash.prototype.AdViewedEvent = function(e) {
    // Check if is one of our ads
    if(ezoic_mash.isOwnedAd(e) === false) {
        return;
    }
    ezoic_mash.viewed_slots[e.slot.ElementId] = 1;
    
    if (ezoic_mash.IsRefreshOn(e.slot)) {
        ezoic_mash.SetTimeout(function(){ezoic_mash.TimedRefresh(e)},15000);
    }
    ezoic_mash.SendMessage(["ez_pel", e, ["viewed", 1]]);
};


EzoicMash.prototype.ArrayContains = function(myArray, needle) {

	if(needle !== needle) {
		return false;
	}

	if(typeof myArray != "object") {
		return false;
	}

	if(typeof Array.prototype.includes == "function") {
		return myArray.includes(needle);
	}

    var arrayLength = myArray.length;
	for (var i = 0; i < arrayLength; i++) {
		if(myArray[i] == needle) {
			return true;
		}
	}

	return false;
};


EzoicMash.prototype.CalculateNewFloor = function(b, retry_count, al, bv, nc, bfh, brfloor) {

	//ezoic_mash.out("calc new floor: bid "+b+" retry count "+retry_count+" ad loc "+al+" banger ver "+b +"");

	var refresh_settings = {};
	refresh_settings['rf'] = b;
	refresh_settings['mr'] = 9;


	//check for server-side defined floors
	if(typeof ezoic_mash.ezorbf != "undefined" && typeof ezoic_mash.ezorbf[al] != "undefined" && ezoic_mash.ezorbf[al].length > 0) {
		refresh_settings['mr'] = ezoic_mash.ezorbf[al].length;

		//remove the first one, since we used that one already
		ezoic_mash.ezorbf[al].shift();

		if(ezoic_mash.ezorbf[al].length > 0) {
			refresh_settings['rf'] = ezoic_mash.ezorbf[al][0];
		} else {
			refresh_settings['rf'] = 0;
		}


		//ezoic_mash.out("all ranked floors ", al, " ", ezoic_mash.ezorbf[al]);
		//ezoic_mash.out("select ranked floor ", al, " ", refresh_settings['rf']);

	} else {

		switch(bv) {
		case "1":
			//ezoic_mash.out("version 1");
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				if(al == 1000 || al == 1001 || al == 1002 || al == 1003 || al == 2001 || al == 2002 || al == 3000 || al == 3001 || al == 3002) {
					refresh_settings['mr'] = 4;
					bid_decrease_amount = .2 * (retry_count - 1);
				} else {
					refresh_settings['mr'] = 9;
					bid_decrease_amount = .06 * (retry_count - 1);
				}
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "2":
			//ezoic_mash.out("version 2");
		//ads that are higher on the page - don't let them refresh as much so we can get faster load times
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				if(al == 1000 || al == 1001 || al == 1002 || al == 1003 || al == 2001 || al == 2002 || al == 3000 || al == 3001 || al == 3002) {
					refresh_settings['mr'] = 2;
					bid_decrease_amount = .3 * (retry_count - 1);
				} else {
					refresh_settings['mr'] = 9;
					bid_decrease_amount = .06 * (retry_count - 1);
				}
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "3":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				if(al == 1000 || al == 1001 || al == 1002 || al == 1003 || al == 2001 || al == 2002 || al == 3000 || al == 3001 || al == 3002) {
					refresh_settings['mr'] = 6;
					bid_decrease_amount = .15 * (retry_count - 1);
				} else {
					refresh_settings['mr'] = 9;
					bid_decrease_amount = .06 * (retry_count - 1);
				}
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "4":
			if(retry_count == 1) {
				bid_decrease_amount = .6;
			} else {
				if(al == 1000 || al == 1001 || al == 1002 || al == 1003 || al == 2001 || al == 2002 || al == 3000 || al == 3001 || al == 3002) {
					refresh_settings['mr'] = 2;
					bid_decrease_amount = .6;
				} else {
					refresh_settings['mr'] = 5;
					bid_decrease_amount = .16 * (retry_count - 1);
				}
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "5":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				if(al == 1000 || al == 1001 || al == 1002 || al == 1003 || al == 2001 || al == 2002 || al == 3000 || al == 3001 || al == 3002) {
					refresh_settings['mr'] = 3;
					bid_decrease_amount = .25 * (retry_count - 1);
				} else {
					refresh_settings['mr'] = 7;
					bid_decrease_amount = .12 * (retry_count - 1);
				}
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "11":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				refresh_settings['mr'] = 1;
				bid_decrease_amount = (0.8*((retry_count)/(refresh_settings['mr']+0.15)));
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "12":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				refresh_settings['mr'] = 2;
				bid_decrease_amount = (0.8*((retry_count)/(refresh_settings['mr']+0.15)));
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "13":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				refresh_settings['mr'] = 3;
				bid_decrease_amount = (0.8*((retry_count)/(refresh_settings['mr']+0.15)));
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "14":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				refresh_settings['mr'] = 4;
				bid_decrease_amount = (0.8*((retry_count)/(refresh_settings['mr']+0.15)));
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "15":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				refresh_settings['mr'] = 5;
				bid_decrease_amount = (0.8*((retry_count)/(refresh_settings['mr']+0.15)));
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "16":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				refresh_settings['mr'] = 6;
				bid_decrease_amount = (0.8*((retry_count)/(refresh_settings['mr']+0.15)));
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "17":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				refresh_settings['mr'] = 7;
				bid_decrease_amount = (0.8*((retry_count)/(refresh_settings['mr']+0.15)));
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "18":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				refresh_settings['mr'] = 8;
				bid_decrease_amount = (0.8*((retry_count)/(refresh_settings['mr']+0.15)));
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "19":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				refresh_settings['mr'] = 9;
				bid_decrease_amount = (0.8*((retry_count)/(refresh_settings['mr']+0.15)));
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "20":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				refresh_settings['mr'] = 10;
				bid_decrease_amount = (0.8*((retry_count)/(refresh_settings['mr']+0.15)));
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "21":
			if(retry_count < 5 && b > 1200) {
				bid_decrease_amount = .22;
			} else if (retry_count < 7 && b > 200) {
				bid_decrease_amount = .6;
			} else {
				refresh_settings['mr'] = 3;
				bid_decrease_amount = .6;
			}
			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "22":
			if(retry_count == 1) {
				bid_decrease_amount = .5;
			} else {
				refresh_settings['mr'] = 5;
				bid_decrease_amount = (0.8*((retry_count)/(refresh_settings['mr']+0.15)));
			}
			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "23":
			if(retry_count == 1) {
				bid_decrease_amount = .3;
			} else {
				refresh_settings['mr'] = 3;
				bid_decrease_amount = .1;
			}
			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "24":
			var increment = (b-brfloor)/2;
			refresh_settings['rf'] = (b - (increment*retry_count))+20;
			refresh_settings['mr'] = 2;
			break;
		case "100":
			//used only for adx before link units
			if(retry_count == 1) {
				bid_decrease_amount = .3;
			} else {
				refresh_settings['mr'] = 8;
				bid_decrease_amount = .22;
			}
			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
			break;
		case "9":
			refresh_settings['mr'] = 1;
			refresh_settings['rf'] = 0;
			break;
		default:
			//ezoic_mash.out("version default");
			//ads that are higher on the page - don't let them refresh as much so we can get faster load times
			if(al == 1000 || al == 1001 || al == 1002 || al == 1003 || al == 2001 || al == 2002 || al == 3000 || al == 3001 || al == 3002) {
				refresh_settings['mr'] = 2;
				bid_decrease_amount = .3 * (retry_count + 1);
			} else {
				refresh_settings['mr'] = 9;
				bid_decrease_amount = .06 * (retry_count + 1);
			}

			refresh_settings['rf'] = b * (1 - bid_decrease_amount);
		}
	}

	//try fetching the bidfloor's hash if using it
	if (typeof bfh !== 'undefined') {
		ezoic_mash.FetchBidFloorHash(refresh_settings['rf'], nc);
	} else {
		//ezoic_mash.out("bfh was undefined, so not fetching");
	}


	return refresh_settings;

};

//Gets the bid floors hash from our servers and then shoves it into bfh to be used later
EzoicMash.prototype.FetchBidFloorHash = function(rawbf, nc) {
	var bf = ezoic_mash.FormatBid(rawbf);
    
	if(typeof ezoic_mash.ezoibfh[bf] != "undefined") {
		ezoic_mash.out("got bid floor for ", bf, " from ezoicbfh. value: ", ezoic_mash.ezoibfh[bf]);
		ezoic_mash.bfh[bf] = ezoic_mash.ezoibfh[bf];
		return;
	} else {
		ezoic_mash.out("no ezoicbfh value for ", bf);
	}

	//if we already have it, do nothing
	if (typeof ezoic_mash.bfh[bf] !== "undefined") {
		return;
	}

	//ezoic_mash.out("Fetching: "+bf);

	//if we don't have it, just assume ours for now
	if (typeof nc === "undefined") {
		//todo: make this default a variable
		nc = "1254144";
	}

	//prefill this so we know that we are making this request already
	ezoic_mash.bfh[bf] = -1;

	//closure that will fill in bhf when the async request returns
	var makeCallback = function(v) {
		return function() {
		  if (this.readyState == 4 && this.status == 200) {

			var hash = "";
			try {
				hash = JSON.parse(this.responseText);
			} catch (e) {
				//ezoic_mash.out("fail hashjson parse");
				hash = this.responseText;
			}

			ezoic_mash.bfh[v] = hash;
			//ezoic_mash.out("Recevied hash for value: ",v," hash ", hash, " resp text ", this.responseText);
			}
	  };
	};

	//Make the async call
	var request = new XMLHttpRequest();
	var url = "//go.ezoic.net/bfhash/" + nc + "/" + bf;
	request.onreadystatechange = makeCallback(bf);
	request.open('GET', url);
	request.send();

};

// check that on every ad load to detect last one to get final x/y positions of ads
EzoicMash.prototype.checkAllAdsLoaded = function() {
    // ezoic_mash.out(ezoic_mash.ads_loaded, window.ezslots.length);
    if (ezoic_mash.initial_ad_positions_stored === false
            && ezoic_mash.ads_loaded === ezoic_mash.ezslots.length) {
        ezoic_mash.SendMessage(["store_positions"], {});
        ezoic_mash.initial_ad_positions_stored = true;
    }
};


//ensures that a bid is in the correct increments for our dfp setup.
EzoicMash.prototype.FormatBid = function(v) {
	v = v / 100;
	if (v <= 0) {
		v = 0;
	} else if (v <= 1) {
		v = Math.floor((v*10)+0.5)*10;
	} else if (v <= 3) {
		v = Math.floor(((v*100)/20)+0.5)*20;
	} else if (v <= 10) {
		v = Math.floor(((v*100)/50)+0.5)*50;
	} else if (v <= 30) {
		v = Math.floor(((v*100)/100)+0.5)*100;
	}else if (v <= 50) {
		v = Math.floor(((v*100)/200)+0.5)*200;
	}else if (v <= 120) {
		v = Math.floor(((v*100)/500)+0.5)*500;
	} else {
		v = Math.floor(((v*100)/1000)+0.5)*1000;
	}
	return v;
};


EzoicMash.prototype.GetBidFloorInUSD = function(slot) {
	bid_val = parseInt(slot.Targeting["br1"]);
	ezoic_mash.out("bid_val before: "+bid_val);
	if (typeof ezoic_mash.ezbfcr !== 'undefined') {
		bid_val = bid_val * ezoic_mash.ezbfcr;
		ezoic_mash.out("bid_val after: "+bid_val);
	}

    return bid_val;
};

EzoicMash.prototype.GetEzimKeyFromSlot = function(slot) {

    
    slotElName = slot.ElementId;

	if(typeof slotElName == 'string') {
		slotElName = slotElName.replace("div-gpt-ad-", "");
		if( slotElName.indexOf('-0_') !== -1 ) {
			slotElName = slotElName.replace('-0_', '_');
		}
		return slotElName;
	} else {
		return "";
	}
};

EzoicMash.prototype.IsRefreshOn = function(slot) {
    ezoic_mash.out("IsRefreshOn ", slot.ElementId, " -- ", slot.Targeting);
    if(slot.Targeting['compid'] != 1 && (slot.Targeting['reft'] == 't' || slot.Targeting['reft'] == 'tf') ) {
        return true;
    }
    return false;
}

EzoicMash.prototype.RefreshSlot = function(e, bf) {
    ezoic_mash.RefreshSlotWaitForBid(e, bf);   
}

EzoicMash.prototype.SetBidFloor = function(slot, bfUSD) {
	var br1 = bfUSD;
	if (typeof ezoic_mash.ezbfcr !== 'undefined' && ezoic_mash.ezbfcr != 0) {
		br1 = br1 / ezoic_mash.ezbfcr;
		if (isNaN(br1)) {
			br1 = bfUSD;
		}
	}

    br1 = ezoic_mash.FormatBid(br1);
    ezoic_mash.out("SetSlotTargeting lb to ", slot.Targeting['br1'], " ", br1);
	SetSlotTargeting(slot, 'lb', slot.Targeting['br1']);
	SetSlotTargeting(slot, 'br1', br1.toString());
	var br1_lcl = bfUSD;
	if (typeof ezoic_mash.ezbflclcr !== 'undefined' && ezoic_mash.ezbflclcr != 0) {
		br1_lcl = br1_lcl / ezoic_mash.ezbflclcr;
		if (isNaN(br1_lcl)) {
			br1_lcl = bfUSD;
		}
		br1_lcl = ezoic_mash.FormatBid(br1_lcl);
		SetSlotTargeting(slot, 'br1_lcl', br1_lcl.toString());
		//ezoic_mash.out("Setting lcl: "+br1_lcl);
    }
    
    	//empty bid, allow backfill ads to compete.
	if(br1 == 0) {
        SetSlotTargeting(slot,'at', 'bf');
        SetSlotTargeting(slot,'ss38', '1');
        SetSlotTargeting(slot,'ss9', '1');

		//record that it's out last try.
        var bra = slot.Targeting['bra'];
        if(typeof bra != 'undefined' && bra.length > 0) {
            SetSlotTargeting(slot,'bra', bra+'-2');
        }

        bf='0';
    }
	//ezoic_mash.out("Set BidFloor: "+bfUSD+" br1: "+br1);
};



EzoicMash.prototype.RefreshSlotWaitForBid = function(e,bf) {

    var slot = e.slot;

    if (typeof ezoic_mash.bfh[bf] !== 'undefined') {

        //if we've actually got the hash set it
        if (ezoic_mash.bfh[bf] !== -1) {
            SetSlotTargeting(slot, 'eb_br', ezoic_mash.bfh[bf]);
            //ezoic_mash.out("Changed targeting correctly for value "+bf+" for "+slot.getSlotElementId());
        } else {
            //we tried to get the hash but the request took longer than it took us to get here
            //we already wait a bit elsewhere, so if we still don't have it just continue
            //with a fake value this round.
            SetSlotTargeting(slot, 'eb_br', "waiting");
            //ezoic_mash.out("New Hash for value "+bf+" not in in time for "+slot.getSlotElementId());
        }
    } else {
        //we didn't even try to get the hash. keep going with a fake value this round
        SetSlotTargeting(slot, 'eb_br', "empty");
        //ezoic_mash.out("We don't have hash for value "+bf+" for "+slot.getSlotElementId());
    }


    var eb_br = slot.Targeting['eb_br']

	if (eb_br == "waiting") {
		ezoic_mash.out("waiting ", bf, ezoic_mash.bfh[bf]);
		if (typeof ezoic_mash.bfh[bf] !== 'undefined' && ezoic_mash.bfh[bf] != -1) {
			SetSlotTargeting(slot,'eb_br', ezoic_mash.bfh[bf]);
			eb_br = ezoic_mash.bfh[bf];
		}
	}

	if (eb_br == "waiting" && (typeof ezoic_mash.slots_eb_wait_time[slot.ElementId] == "undefined" || ezoic_mash.slots_eb_wait_time[slot.ElementId] < 300) ) {

		if(typeof ezoic_mash.slots_eb_wait_time[slot.ElementId] == "undefined") {
			ezoic_mash.slots_eb_wait_time[slot.ElementId] = 10;
		} else {
			ezoic_mash.slots_eb_wait_time[slot.ElementId] += 10;
		}

		setTimeout(function(){ezoic_mash.RefreshSlotWaitForBid(e);}, 10);
	}  else {

		if((eb_br == "waiting" || eb_br == "disable" || eb_br == "empty" ) && bf > 0) {
			SetSlotTargeting(slot,"eba", "0");
		}

        ezoic_mash.slots_eb_wait_time[slot.ElementId] = 0;
        //ezoic_mash.out("Refreshing Slot. Unsynced values of ", e.slot.UnSyncedTargeting);
        ezoic_mash.SetBidFloor(e.slot, bf);

        ezoic_mash.DelayedRefresh(e, false);
        

        
	}

}

EzoicMash.prototype.DelayedRefresh = function(e, isQueued) {

    var now = Date.now();
    

    //loading ads too quickly clogs up the primary js thread and slows down the page
    //this slows them down until the page is idle or we've waited for too long
    if(ezoic_mash.IsReadyForRefresh()) {  

        var reload_delay = 0;

        if(e.slot.RequestTime > 0) {
            last_load_time = e.slot.RequestTime;
        } else if(typeof(e.slot.Targeting['reqt']) != 'undefined') {
            var last_load_time = e.slot.Targeting['reqt'];
        } else {
            var last_load_time = ezoic_mash.init_time;
        }

        reload_delay = 500 - (now - last_load_time);
        ezoic_mash.out("reload_delay of "+reload_delay+ " for "+e.slot.ElementId, " now ", now, " last ", e.slot.LoadTime);
        ezoic_mash.slot_refresh_epochs[e.slot.ElementId] = now;

        if(reload_delay<0) {
            reload_delay = 0;
        }
        
        setTimeout(function() {
            ezoic_mash.SendMessage(["refresh_slot", e]);
        }, reload_delay );

        if(isQueued != true) {
            ezoic_mash.RefreshQueuedSlots();
        }

    } else {
        //queue up the refreshes
        ezoic_mash.slots_waiting_for_refresh.push(e);
        ezoic_mash.out("queueing slot ", e.slot.ElementId);
    }
};

EzoicMash.prototype.IsReadyForRefresh = function() {
    if(ezoic_mash.idle_count >= 3 || ezoic_mash.idle_count < 0) { 
        return true;
    } else {
        return false;
    }
}

EzoicMash.prototype.RefreshQueuedSlots = function() {
    if(ezoic_mash.IsReadyForRefresh()) {
        while(ezoic_mash.slots_waiting_for_refresh.length > 0) {
            var e = ezoic_mash.slots_waiting_for_refresh.pop();
            ezoic_mash.out("refreshing queued slot ", e);
            ezoic_mash.DelayedRefresh(e, true);
        }
    }
};

//failsafe just in case idle takes too long or never happens.
EzoicMash.prototype.startIdleFailsafe = function() {
    setTimeout(function(){
        ezoic_mash.idle_count = -1;
        ezoic_mash.RefreshQueuedSlots();
    }, 3500);
};

//Starts a timer that sets when an ad is available for refresh.
//this does not actually refresh an ad.
EzoicMash.prototype.StartRefreshAllowedTimer = function(slot,cb,t) {
	//ezoic_mash.out("dwayne setting refresh allowed timer ", slot.getSlotElementId());
	    SetSlotTargeting(slot,'reft', 't');
		ezoic_mash.SetTimeout(function(){
            //console.log("StartRefreshAllowedTimer ready!! ", slot.ElementId);
			var slot_name = slot.ElementId;
			ezoic_mash.slots_available_for_refresh[slot_name] = cb;
			ezoic_mash.slots_loaded_non_yet_refreshable[slot_name] = false;
	},t);
};

EzoicMash.prototype.TimedRefresh = function(e) {

    var rfr = false;
	if(typeof ezoic_mash.slots_available_for_refresh[e.slot.ElementId] != 'undefined' && ezoic_mash.slots_available_for_refresh[e.slot.ElementId] != null) {
		rfr = true;
    }
    
    var vw = false;
    if(ezoic_mash.viewed_slots[e.slot.ElementId] == 1) {
        vw = true;
    }

    ezoic_mash.out("TimedRefresh ", e.slot.ElementId, " - ", rfr, " ", vw);
    if(rfr == true && vw == true) {
        ezoic_mash.SendMessage(["timed_refresh", e]);
    } else {
        ezoic_mash.SetTimeout(function(){ezoic_mash.TimedRefresh(e)},2000);
    }

}

EzoicMash.prototype.isOwnedAd = function(e) {

    // Check it is one of our ads
    try {
        var ad_position_id = e.slot.Targeting['ap'];
    }catch(err) {
		ezoic_mash.out("Error: "+err);
    }

    if (typeof ad_position_id == "undefined" || ad_position_id.length == 0) {
        return false;
    }

    return true;
};

EzoicMash.prototype.SetTimeout = function(func, delay) {
    var timeoutId = setTimeout(func, delay);
    this.timeoutIds.push(timeoutId);
    return timeoutId;
};

var ezoic_mash = new EzoicMash();
ezoic_mash.init();